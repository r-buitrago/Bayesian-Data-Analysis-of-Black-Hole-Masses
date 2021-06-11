library(ggplot2)
try(dev.off())
source("stansettings.R")
#We will test our upper-bound approach with this simple example
#Suppose we have a dependency y_real =x_real + epsilon; epsilon ~ N(0,sigma)
#We will call the effect of sigma "real dispersion"
#We have N values for x
N <- 24
x <- 1:N

#Now suppose we have a vector yobs and two errors, yerr1 and yerr2, in which the yobs can be of two types
# Type 1: An estimation of yreal. This occurs when yerr1 = yerr2 = observation error
# Type 2: An upper bound of yreal. This occurs when yerr1 != yerr2. 
# In type 2, yerr1 is the observation error and yerr2=yobs

#With the first N/2 points, we generate values of yobs that
#are close to the line x_real. All the yobs_A will be Type 1 values
#We denote such values as yobs_0
sigma_0 <- 1.5
set.seed(39)
epsilon <- rnorm(N/2,mean=0,sd=sigma_0)
x_0 <- x[1:(N/2)]
yobs_0 <- x_0 + epsilon
yobs_0 <- x_0

#Now, we will manually design the second part of yobs_A such that we can 
#see the effect of Type 1 and Type 2 values
#We will denote yobs_1over, yobs_1below, yobs_2over, yobs_2below to denote
#observations of type 1 and 2 over and below the line y=x, respectively
x_1over <- x[(N/2+1):(N/2+N/8)]
x_2over <- x[(N/2+N/8+1):(N/2+2*N/8)]
x_1below <- x[(N/2+2*N/8+1):(N/2+3*N/8)]
x_2below <- x[(N/2+3*N/8+1):(N)]

k= 6 #we generate values y~x +/- k
yobs_1over <- x_1over + k
yobs_1below <- x_1below - k
yobs_2over <- x_2over + k
yobs_2below <- x_2below - k 

#We also manually design the error
sigmaobs <- 1.5
yerr_0 <- rep(sigmaobs,N/2)

err <- k/3
yerr_1over <- rep(err,N/8)
yerr_2over <- rep(err,N/8)
yerr_1below <- rep(err,N/8)
yerr_2below <- rep(err,N/8)


dataA <- data.frame(x_0,yobs_0,yerr_0)
dataB <- data.frame(x_1over,yobs_1over,x_1below,yobs_1below,
                    x_2over,yobs_2over,x_2below,yobs_2below,yerr_1over,
                    yerr_2over,yerr_1below,yerr_2below)

S <- 7
s <- 1
p1 <- ggplot()+geom_point(data=dataA,aes(x=x_0,y=yobs_0,color="Yobs Type 1"),size=S)+
  geom_errorbar(data=dataA,aes(x=x_0,ymin=yobs_0-yerr_0,ymax=yobs_0+yerr_0),linetype="dotted",size=s)+
  geom_point(data=dataB, aes(x=x_1over,y=yobs_1over,color="Yobs Type 1"),size=S)+
  geom_errorbar(aes(x=x_1over,ymin=yobs_1over-yerr_1over,ymax=yobs_1over+yerr_1over),linetype="dotted",size=s)+
  geom_point(data=dataB, aes(x=x_1below,y=yobs_1below,color="Yobs Type 1"),size=S)+
  geom_errorbar(aes(x=x_1below,ymin=yobs_1below-yerr_1below,ymax=yobs_1below+yerr_1below),linetype="dotted",size=s)+
  geom_point(data=dataB, aes(x=x_2over,y=yobs_2over,color="Yobs Upper Bound"),size=S)+
  geom_errorbar(aes(x=x_2over,ymin=yobs_2over-yerr_2over,ymax=yobs_2over+yerr_2over),linetype="dotted",size=s)+
  geom_point(data=dataB, aes(x=x_2below,y=yobs_2below,color="Yobs Upper Bound"),size=S)+
  geom_errorbar(aes(x=x_2below,ymin=yobs_2below-yerr_2below,ymax=yobs_2below+yerr_2below),linetype="dotted",size=s)

#Now we fit the model. We generate "B" errors, to simulate the data from
#our thesis
yerr_0B <- yerr_0
yerr_1overB <- yerr_1over
yerr_2overB <- yobs_2over
yerr_1belowB <- yerr_1below
yerr_2belowB <- yobs_2below

yobs <- c(yobs_0,yobs_1over,yobs_2over,yobs_1below,yobs_2below)
yerr <- c(yerr_0,yerr_1over,yerr_2over,yerr_1below,yerr_2below)
yerrB <- c(yerr_0B,yerr_1overB,yerr_2overB,yerr_1belowB,yerr_2belowB)


data_stan <- list(N=N, x=x, yobs=yobs, yerr=yerr, yerrB=yerrB)

#q <- ggplot(data_stan)+geom_point(aes(x=x,y=yobs))+
# geom_errorbar(aes(x=x,ymin=yobs-yerr,ymax=yobs+yerr))

stanfit <- stan("upper_bound.stan",data=data_stan,iter=4000,warmup=2000)
               

stanfit_sum <- summary(stanfit)$summary
stanfit_sum_mean <- stanfit_sum[,"mean"]
yreal <- stanfit_sum_mean[4:(N+4-1)]
sigmareal <- stanfit_sum[,"sd"][4:27]

dataC <- data.frame(x,yreal,sigmareal)

p2 <- p1 + geom_abline(aes(slope=stanfit_sum_mean[["beta1"]],
                          intercept=stanfit_sum_mean[["beta0"]], linetype="With upper bound"),lwd=1.6)+
  geom_point(data=dataC, aes(x=x,y=yreal,color="Yreal"),size=5.5)+
  geom_errorbar(data=dataC, aes(x=x,ymin=yreal-sigmareal,ymax=yreal+sigmareal),linetype="dotted",size=s,color="black")+
  geom_abline(aes(slope=1,intercept=0,linetype="Without upper bound"),lwd=1.2)+
  labs(x="x",y="y")+
    scale_color_manual(name="Color legend:",values=c("Yobs Type 1"="cyan","Yobs Upper Bound"="red","Yreal"="darkgreen"))+
  scale_linetype_manual(name="Regression models:",values=c("With upper bound"="solid","Without upper bound"="dashed"))+
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=17),
        legend.position = "top",
        legend.justification = "left",
        legend.direction = "horizontal",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

  
                      