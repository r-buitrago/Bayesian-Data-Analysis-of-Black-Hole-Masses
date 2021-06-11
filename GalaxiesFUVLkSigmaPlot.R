source("stansettings.R")
source("data_MBH.R")
library("ggplot2")

N <- length(data_MBH[["FUVcolor"]])
yobs <-  data_MBH[["logMBH"]]
yerr1 <- data_MBH[["err1logMBH"]]
yerr2 <- data_MBH[["err2logMBH"]]
type1 <- which(yerr1==yerr2)
type2 <- which(yerr1!=yerr2)
yobs1 <- yobs[type1]
yobs2 <- yobs[type2]


#FUV fit
standataFUVcolor <- list(N = N, xobs = data_MBH[["FUVcolor"]], 
                         xerr = data_MBH[["errFUVcolor"]], yobs = yobs,
                         yerr1 = yerr1, yerr2 = yerr2 )

stanfitFUV <- stan("individual_fit.stan",data=standataFUVcolor)

stanfit_FUV_sum <- summary(stanfitFUV)$summary
stanfit_FUV_mean <- stanfit_FUV_sum[,"mean"]

yrealFUV <- stanfit_FUV_mean[3:(N+2)]


#Lk fit
standataLk<- list(N = N, xobs = data_MBH[["LogLk"]], 
                         xerr = data_MBH[["errLogLk"]], yobs = yobs,
                         yerr1 = yerr1, yerr2 = yerr2)

stanfitLk <- stan("individual_fit.stan",data=standataLk)

stanfit_Lk_sum <- summary(stanfitLk)$summary
stanfit_Lk_mean <- stanfit_Lk_sum[,"mean"]

yrealLk <- stanfit_Lk_mean[3:(N+2)]


#SIGMA FIT
standatasigma <- list(N = N, xobs = data_MBH[["Logsigma"]],
                      xerr = data_MBH[["errLogsigma"]], yobs = yobs,
                      yerr1 = yerr1, yerr2 = yerr2 )

stanfitsigma <- stan("individual_fit.stan",data=standatasigma)

stanfit_sigma_sum <- summary(stanfitsigma)$summary
stanfit_sigma_mean <- stanfit_sigma_sum[,"mean"]

yrealsigma <- stanfit_sigma_mean[3:(N+2)]


#JOINT FIT
pars <- c(4,12,8)
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

stanfit <- stan("joint_fit.stan",data=standatajoint,iter=4000,warmup=2000)
stanfit_joint_sum <- summary(stanfit)$summary
stanfit_joint_sum_mean <- stanfit_joint_sum[,"mean"]
yrealjoint_names <- vector(mode="character",length=N)
for(i in 1:N){
  yrealjoint_names[i] <- paste("yreal[",i,"]",sep="")
}
yrealjoint <- stanfit_joint_sum_mean[yrealjoint_names]


#Ahora cogemos una muestra de K galaxias para visualizarlas
K <- 30
set.seed(20)
index1 <- sample(type1,K/2)
set.seed(24)
index2 <- sample(type2,K/2)
yobs1_p <- yobs[index1]
yobs2_p <- yobs[index2]
yrealFUV1_p <- yrealFUV[index1]
yrealFUV2_p <- yrealFUV[index2]
yrealFUV_p <- c(yrealFUV1_p,yrealFUV2_p)
yrealLk1_p <- yrealLk[index1]
yrealLk2_p <- yrealLk[index2]
yrealLk_p <- c(yrealLk1_p,yrealLk2_p)
yrealsigma1_p <- yrealsigma[index1]
yrealsigma2_p <- yrealsigma[index2]
yrealsigma_p <- c(yrealsigma1_p,yrealsigma2_p)
yrealjoint1_p <- yrealjoint[index1]
yrealjoint2_p <- yrealjoint[index2]
yrealjoint_p <- c(yrealjoint1_p,yrealjoint2_p)

yerr <- data_MBH[["err1logMBH"]]
yerr1_p <- yerr[index1]
yerr2_p <- yerr[index2]

data1 <- data.frame(x=1:(K/2),y=yobs1_p,yerr=yerr1_p)
data2 <- data.frame(x=(K/2+1):K,y=yobs2_p,yerr=yerr2_p)
dataFUVNUVsigmajoint <- data.frame(x=1:K,yFUV=yrealFUV_p,yLk=yrealLk_p,
                                   ysigma=yrealsigma_p,yjoint=yrealjoint_p)

S <- 6
s <- 5
ss <- 1
p <- ggplot() + geom_point(data=data1, aes(x=x,y=y,color="Yobs Type 1"),shape=16,size=S)+
  geom_errorbar(data=data1,aes(x=x,ymin=y-yerr,ymax=y+yerr),linetype="dashed",color="cyan",size=ss)+
  geom_point(data=data2,aes(x=x,y=y,color="Yobs Upper Bound"),shape=16,size=S)+
  geom_errorbar(data=data2,aes(x=x,ymax=y+yerr,ymin=y+yerr),linetype="dashed",color="red",size=ss)+ #This error bar, combined with the next two lines of code, generated one-sided error bars 
  geom_linerange(data=data2,aes(x=x,ymax=y+yerr,ymin=y),linetype="dashed",color="red")+
  geom_segment(data=data2,aes(x=x,y=y,xend=x,yend=y-yerr),arrow=arrow(),linetype="dashed",color="red")+
  geom_point(data=dataFUVNUVsigmajoint,aes(x=x,y=yFUV,shape="Yreal FUV"),size=s)+
  geom_point(data=dataFUVNUVsigmajoint,aes(x=x,y=yLk,shape="Yreal Lk"),size=s)+
  geom_point(data=dataFUVNUVsigmajoint,aes(x=x,y=ysigma,shape="Yreal sigma"),size=s)+
  geom_point(data=dataFUVNUVsigmajoint,aes(x=x,y=yjoint,color="Yreal joint"),size=S-2)+
  scale_color_manual(name="Color Legend",
                     values =c("Yobs Type 1"="cyan","Yobs Upper Bound"="red","Yreal joint"="gray"))+
  scale_shape_manual(name="Shape legend",
                     values =c("Yreal FUV"=0,"Yreal Lk"=2,"Yreal sigma"=11,"Yreal joint"=10))+
  xlab("Galaxies")+ylab("logMBH")+
  theme(legend.position="top",
        legend.direction="horizontal",
        legend.justification="left",
        legend.title=element_text(size=14),
        legend.text=element_text(size=11),
        title=element_text(size=14))

#Save as 1200x800
