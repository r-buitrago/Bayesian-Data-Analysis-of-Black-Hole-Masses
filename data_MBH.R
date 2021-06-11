
A <- read.table("datafile_bh.dat.txt", header = TRUE, skip = 41)

FUVcolor <- A[[5]]-A[[11]] #FUVcolor
errFUVcolor <- (A[[6]]^2+A[[12]]^2)^(1/2)

NUVcolor <- A[[8]]-A[[11]] #NUVcolor
errNUVcolor <- (A[[9]]^2+A[[12]]^2)^(1/2)

Logsigma <- A[[27]] #velocity dispersion (from Van den Bosch, 2016)
errLogsigma <- A[[28]]

LogRe <- A[[34]] #Effective radius in kpc (from Van den Bosch, 2016)
errLogRe <- A[[35]]

LogLk <- A[[29]] #log K-band luminosity [in solar unit](from Van den Bosch, 2016)
errLogLk <-A[[30]]

M <- A[[11]]-5*log(A[[36]])+5 #Absolute magnitude in band 3.6vm
errM <- (A[[12]]^2 +(5/A[[36]]*A[[37]])^2)^(1/2)

LogMassk <- A[[32]] #log Stellar mass (k-band), in solar masses
errLogMassk <- A[[33]]

BTuv <- A[[15]] #luminosity NUV/FUV Bulge-to-total ratio

logMBH <- A[[24]] #Black Hole mass
err1logMBH <- A[[25]]
err2logMBH <- A[[26]]

galaxynames <- A[[1]]

N <- length(galaxynames)

types_logical <- err1logMBH==err2logMBH

types <- rep("Yobs upper bound",N)
types[types_logical] <- "Yobs Type1"

data_MBH <- data.frame(logMBH,err1logMBH,err2logMBH,FUVcolor,errFUVcolor,NUVcolor,errNUVcolor,
                   Logsigma,errLogsigma,LogRe,errLogRe,LogLk,errLogLk,
                   M,errM,LogMassk,errLogMassk,BTuv,galaxynames,types)

data_MBH_type1 <- subset(data_MBH,types_logical)
data_MBH_type2 <- subset(data_MBH,!types_logical)



