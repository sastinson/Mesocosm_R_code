#set working directory
setwd("/Users/WeeRedLass/Desktop/")

#load packages
library(rattle)
library(reshape)
library(reshape2)

#read file
zp <- read.csv("Zooplankton normalized 3.17.16.csv")
#melt data
str(zp)
controls <- zp[zp$Treatment=="control",]
zp.control <- zp[zp$Treatment=="control" | zp$Treatment=="high diuron",]
zp.melt <- melt(zp.control, id.vars = c("Tank..", "Date", "Treatment"))
zp.melt <- melt(zp, id.vars = c("Tank..", "Date", "Treatment"))


#data summary
summary(zp)

logzp<- log(zp)

