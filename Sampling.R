source("stansettings.R")
source("data_MBH.R")
library(utils)

N <- length(data_MBH[["FUVcolor"]])
yobs <-  data_MBH[["logMBH"]]
yerr <- data_MBH[["err1logMBH"]]
yerr1 <- data_MBH[["err1logMBH"]]
yerr2 <- data_MBH[["err2logMBH"]]


names <- c("FUVcolor","NUVcolor","Logsigma","LogRe","LogLk",
           "M","LogMassk")
errnames <- c("errFUVcolor","errNUVcolor","errLogsigma","errLogRe","errLogLk",
              "errM","errLogMassk")

pars <- c(6,8,10,14)
errpars <- pars+1
M <- length(pars)

xobs <- data_MBH[pars]
xerr <- data_MBH[errpars]

xobs <- t(data.matrix(xobs))
xerr <- t(data.matrix(xerr))

K <- 10000
standatajoint <- list(N=N,M=M,xobs=xobs,xerr=xerr,yobs=yobs,yerr1=yerr1,yerr2=yerr2,
                      iter=K,warmup=4000)

stanfit_joint <- stan("joint_fit.stan",data=standatajoint)
stanfit_joint_sum <- summary(stanfit_joint)$summary
stanfit_joint_sum_mean <- summary(stanfit_joint)$summary[,"mean"]
stanfit_joint_sum_sd <- summary(stanfit_joint)$summary[,"sd"]
yreal_joint_names <- vector(mode="character",length=N)
for(i in 1:N){yreal_joint_names[i] <- paste("yreal[",i,"]",sep="")}
yreal_joint <- stanfit_joint_sum_mean[yreal_joint_names]
yreal_joint_err <- stanfit_joint_sum_sd[yreal_joint_names]

yreal_sample <- extract(stanfit_joint, pars="yreal")$yreal
yreal_sample_df <- rbind(data.frame(t(types)),data.frame(yreal_sample))
colnames(yreal_sample_df) <- galaxynames
Rownames <- vector(mode="character",length=K+1)
Rownames[1] <- "Black Hole Observation Type"
for(i in 2:K+1){Rownames[i] <- paste("Iteration ",i,sep="")}
rownames(yreal_sample_df) <- Rownames
write.table(yreal_sample_df,file="BlackHoleMassSampling.txt",sep=",")


