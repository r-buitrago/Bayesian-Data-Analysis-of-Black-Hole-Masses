source("stansettings.R")
source("data_MBH.R")
library("ggplot2")


N <- length(data_MBH[["FUVcolor"]])
yobs <-  data_MBH[["logMBH"]]
yerr <- data_MBH[["err1logMBH"]]
yerr1 <- data_MBH[["err1logMBH"]]
yerr2 <- data_MBH[["err2logMBH"]]
type1 <- which(yerr1==yerr2)
type2 <- which(yerr1!=yerr2)
yobs1 <- yobs[type1]
yobs2 <- yobs[type2]


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

standatajoint <- list(N=N,M=M,xobs=xobs,xerr=xerr,yobs=yobs,yerr1=yerr1,yerr2=yerr2)

stanfit_joint <- stan("joint_fit.stan",data=standatajoint,iter=10000,warmup=2000)
stanfit_joint_sum <- summary(stanfit_joint)$summary
stanfit_joint_sum_mean <- summary(stanfit_joint)$summary[,"mean"]
stanfit_joint_sum_sd <- summary(stanfit_joint)$summary[,"sd"]
yreal_joint_names <- vector(mode="character",length=N)
for(i in 1:N){yreal_joint_names[i] <- paste("yreal[",i,"]",sep="")}
yreal_joint <- stanfit_joint_sum_mean[yreal_joint_names]
yreal_joint_err <- stanfit_joint_sum_sd[yreal_joint_names]

yreal_sample <- extract(stanfit_joint, pars="yreal")$yreal

orderyerr <- sort.list(yerr,decreasing=TRUE)

orderyerr_1 <- c()
orderyerr_2 <- c()
for(i in orderyerr){
  if(types_logical[i]){orderyerr_1 <- c(orderyerr_1,i)}
  else{orderyerr_2 <- c(orderyerr_2,i)}
}

indeces <- c(2,3,6,7)
mean_ind <- data.frame(matrix(NA,nrow=N,ncol=0))
sd_ind <- data.frame(matrix(NA,nrow=N,ncol=0))

for(i in indeces ){
  xobs <- data_MBH[[names[i]]]
  xerr <- data_MBH[[errnames[i]]]
  
  standata <- list(N=N,yobs=yobs,yerr1=yerr1,yerr2=yerr2,xobs=xobs,xerr=xerr)
  stanfit_ind <- stan("individual_fit.stan",data = standata,iter=4000,warmup=2000)
  
  mean_ind[names[i]] <- unname(summary(stanfit_ind)$summary[,"mean"][3:69])
  sd_ind[names[i]] <- unname(summary(stanfit_ind)$summary[,"sd"][3:69])
}

#Plotting
j <- 1
for(index in orderyerr_2){
  df <- data.frame(yreal_sample[,index])
  colnames(df) <- "yreal_sample"

  
  m <- min(yerr[index],min(sd_ind[index,]))
  scale <- ((2*pi)^(1/2)*m)^(-1)
  
  
  fun <- function(x){
    return(dnorm(x,mean=yobs[index],sd=yerr[index])/ scale )}
  
  funs <- list()
  funs[[1]] <- 
    function(x){
      return(dnorm(x,mean=mean_ind[index,1],sd=sd_ind[index,1])/scale)}
  funs[[2]] <- 
    function(x){
      return(dnorm(x,mean=mean_ind[index,2],sd=sd_ind[index,2])/scale)}
  funs[[3]] <- 
    function(x){
      return(dnorm(x,mean=mean_ind[index,3],sd=sd_ind[index,3])/scale)}
  funs[[4]] <- 
    function(x){
      return(dnorm(x,mean=mean_ind[index,4],sd=sd_ind[index,4])/scale)}
  
  indexstanfit <- paste("yreal[",index,"]",sep="")
  minx <- min(yobs[index]-3*yerr[index],
              stanfit_joint_sum_mean[[indexstanfit]]-3*stanfit_joint_sum_sd[[indexstanfit]],
              mean_ind[index,1]-0.7*sd_ind[index,1],
              mean_ind[index,2]-0.7*sd_ind[index,2],
              mean_ind[index,3]-0.7*sd_ind[index,3],
              mean_ind[index,4]-0.7*sd_ind[index,4])
  maxx <- max(yobs[index]+3*yerr[index],
              stanfit_joint_sum_mean[[indexstanfit]]+3*stanfit_joint_sum_sd[[indexstanfit]],
              mean_ind[index,1]+0.7*sd_ind[index,1],
              mean_ind[index,2]+0.7*sd_ind[index,2],
              mean_ind[index,3]+0.7*sd_ind[index,3],
              mean_ind[index,4]+0.7*sd_ind[index,4])
  p <- ggplot(data=df)+
    geom_histogram(aes(x=yreal_sample,y=..ncount..),bins=60,
                   fill="palegreen3",color="black")+
    geom_function(fun=fun,size=2.4, aes(color="Yobs Upper Bound"),
                  xlim=c(yobs[index],yobs[index]+3*yerr[index]+0.1))+
    geom_function(fun=function(x){return( ((2*pi)^(1/2)*yerr[index])^(-1)/scale ) },size=2.4,
                  aes(color="Yobs Upper Bound"),xlim=c(minx,yobs[index]))+
    xlim(minx,maxx+0.1)+xlab("logMBH")+
    ggtitle(paste(data_MBH[["galaxynames"]][index],"Upper bound observation",sep=", "))+
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank())+
    geom_function(fun=funs[[1]],size=1.3,aes(color=paste("Yreal ",names[indeces[1]],sep="")))+
    geom_function(fun=funs[[2]],size=1.3,aes(color=paste("Yreal ",names[indeces[2]],sep="")))+
    geom_function(fun=funs[[3]],size=1.3,aes(color=paste("Yreal ",names[indeces[3]],sep="")))+
    geom_function(fun=funs[[4]],size=1.3,aes(color=paste("Yreal ",names[indeces[4]],sep="")))+
    scale_color_manual(name="Color legend",values=c("Yobs Upper Bound"="black","Yreal LogMassk"="brown",
                                                    "Yreal Logsigma"="red","Yreal M"="purple",
                                                    "Yreal NUVcolor"="royalblue1"))+
    theme(legend.title=element_text(size=14),
          legend.text=element_text(size=13),
          title=element_text(size=14))
  
  filename <- paste("type2_",j,"_",data_MBH[["galaxynames"]][index],"_individual",".png",sep="")
  ggsave(filename=filename,plot=p,width=7,height=7)
  
  j <- j+1
}
