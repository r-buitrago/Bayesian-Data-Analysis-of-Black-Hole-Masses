library("ggplot2")
library("gridExtra")

source("stansettings.R")
source("data_MBH.R")


names <- c("FUVcolor","NUVcolor","Logsigma","LogRe","LogLk",
           "M","LogMassk")
errnames <- c("errFUVcolor","errNUVcolor","errLogsigma","errLogRe","errLogLk",
              "errM","errLogMassk")

#JOINT FIT
pars <- c(4,6,8,10,12,14,16)
errpars <- pars+1

M <- length(pars)
N <- length(data_MBH[["logMBH"]])

xobs <- data_MBH[pars]
xerr <- data_MBH[errpars]

xobs <- t(data.matrix(xobs))
xerr <- t(data.matrix(xerr))

yobs <- data_MBH[[1]]
yerr1 <- data_MBH[[2]]
yerr2 <- data_MBH[[3]]

standatajoint <- list(N=N,M=M,xobs=xobs,xerr=xerr,yobs=yobs,yerr1=yerr1,yerr2=yerr2)

stanfit_joint <- stan("joint_fit.stan",data=standatajoint,iter=4000,warmup=2000)
stanfit_joint_sum_mean <- summary(stanfit_joint)$summary[,"mean"]
yreal_joint_names <- vector(mode="character",length=N)
for(i in 1:N){yreal_joint_names[i] <- paste("yreal[",i,"]",sep="")}
yreal_joint <- stanfit_joint_sum_mean[yreal_joint_names]
xreal_joint <- matrix(data=NA,nrow=N,ncol=length(names))
for(j in 1:length(names)){
  xreal_joint_names <- vector(mode="character",length=N)
  for(i in 1:N){xreal_joint_names[i] <- paste("xreal[",j,",",i,"]",sep="")}
  xreal_joint[,j] <- stanfit_joint_sum_mean[xreal_joint_names]
}
data_joint <- data.frame(xreal_joint)
names_joint <- paste(names,"_joint",sep="")
colnames(data_joint) <- names_joint
data_joint["yreal_joint"] <- yreal_joint

#We generate a column with identical values for the legend
data_joint["yreal"] <- "yreal"
data_joint["galaxynames"] <- galaxynames #this is for merging

data_joint_merged <- merge(data_MBH,data_joint,by="galaxynames")


#INDIVIDUAL FITS
yobs <- data_MBH[["logMBH"]]
N <- length(yobs)
yerr1 <- data_MBH[["err1logMBH"]]
yerr2 <- data_MBH[["err2logMBH"]]

plots <- list()
stanfits_ind <- list()
S <- 4
s <- 2
plotting_vars <- c(5,6,7) #these are the variables to plot
for(i in plotting_vars){ 
  xobs <- data_MBH[[names[i]]]
  xerr <- data_MBH[[errnames[i]]]
  
  standata <- list(N=N,M=1,yobs=yobs,yerr1=yerr1,yerr2=yerr2,xobs=xobs,xerr=xerr,iter=4000,warmup=2000)
  stanfit_ind <- stan("individual_fit.stan",data = standata,iter=4000,warmup=2000)
  
  stanfits_ind <- c(stanfits_ind, stanfit_ind)
  
  stanfit_ind_sum_mean <- summary(stanfit_ind)$summary[,"mean"]
  
  yreal_ind_names <- vector(mode="character",length=N)
  for(k in 1:N){yreal_ind_names[k] <- paste("yreal[",k,"]",sep="")}
  yreal_ind <- stanfit_ind_sum_mean[yreal_ind_names]
  xreal_ind_names <- vector(mode="character",length=N)
  for(k in 1:N){xreal_ind_names[k] <- paste("xreal[",k,"]",sep="")}
  xreal_ind <- stanfit_ind_sum_mean[xreal_ind_names]
  data_ind <- data.frame(yreal_ind,xreal_ind,row.names=NULL)
  data_ind["yreal"] <- "yreal" #this is for legend
  data_ind["galaxynames"] <- galaxynames #this is form merging
  data_ind_merged <- merge(data_MBH,data_ind,by="galaxynames")
  #Individual plot
  p <- ggplot(data=data_ind_merged)+
    geom_errorbarh(aes_string(xmin=paste(names[i],errnames[i],sep="-"),
                              xmax=paste(names[i],errnames[i],sep="+"),y="logMBH"),
                   linetype="dashed")+
    geom_errorbar(aes_string(x=names[i],ymin=paste("logMBH","err1logMBH",sep="-"),
                             ymax=paste("logMBH","err1logMBH",sep="+")),
                  linetype="dashed")+
    geom_point(aes_string(x=names[i],y="logMBH",color="types"),size=S)+
    xlab(names[i])+ylab("Black Hole Mass")+
    geom_abline(aes_string(slope=stanfit_ind_sum_mean[["beta1"]],intercept=stanfit_ind_sum_mean[["beta0"]]),
                size = 1.1, color = "green",linetype="longdash")+
    geom_segment(aes_string(x=names[i],y="logMBH",xend="xreal_ind",yend="yreal_ind"),
                 arrow=arrow(length=unit(0.2,"cm")),size=1.2)+
    ggtitle(paste("Individual fit for",names[i]))+
    scale_color_manual(name="Color legend",values=c("Yobs Type1"="cyan","Yobs upper bound"="red"))+
    theme(legend.text=element_text(size=11),
          legend.title=element_text(size=14),
          legend.position = "none",
          legend.justification = "left",
          legend.direction = "horizontal",
          title=element_text(size=14))
  
  plots <- c(plots,list(p)) 
  
  #Joint plot
  p <- ggplot(data=data_joint_merged)+
    geom_errorbarh(aes_string(xmin=paste(names[i],errnames[i],sep="-"),
                              xmax=paste(names[i],errnames[i],sep="+"),y="logMBH"),
                   linetype="dashed")+
    geom_errorbar(aes_string(x=names[i], ymin=paste("logMBH","err1logMBH",sep="-"),
                             ymax=paste("logMBH","err1logMBH",sep="+")),
                  linetype="dashed")+
    geom_point(aes_string(x=names[i],y="logMBH",color="types"),size=S)+
    xlab(names[i])+ylab("Black Hole Mass")+
    geom_abline(aes_string(slope=stanfit_joint_sum_mean[[paste("beta1[",i,"]",sep="")]],
                           intercept=stanfit_joint_sum_mean[[paste("beta0[",i,"]",sep="")]]),
                 size = 1.1, color = "green")+
    geom_abline(aes_string(slope=stanfit_ind_sum_mean[["beta1"]],intercept=stanfit_ind_sum_mean[["beta0"]]),
                size = 1.1, color = "green",linetype="longdash")+
    geom_segment(aes_string(x=names[i],y="logMBH",xend=names_joint[i],yend="yreal_joint"),
                 arrow=arrow(length=unit(0.2,"cm")),size=1.2)+
    ggtitle(paste("Joint fit for",names[i]))+
    scale_color_manual(name="Color legend",values=c("Yobs Type1"="cyan","Yobs upper bound"="red"))+
    theme(legend.text=element_text(size=11),
          legend.title=element_text(size=14),
          legend.position = "none",
          legend.justification = "left",
          legend.direction = "horizontal",
          title=element_text(size=14))

  
  plots <- c(plots,list(p))
}

do.call("grid.arrange", c(plots, ncol=2))

