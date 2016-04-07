#set working directory
setwd("/Users/WeeRedLass/Desktop/")

#load packages
library(car)
library(reshape)
library(reshape2)

#read file
inv <- read.csv("MacroInvert_logtrans_4.7.16.csv")

#melt data
str(inv)
controls <- inv[inv$Treatment=="control",]
inv.control <- inv[inv$Treatment=="control" | inv$Treatment=="high diuron",]
inv.melt <- melt(inv.control, id.vars = c("Tank..", "Date", "Treatment"))
inv.melt <- melt(inv, id.vars = c("Tank..", "Date", "Treatment"))

#data summary
summary(inv)

#repeated measures (rm) anova
#load ez package
library(ez)
library(lsr)

#model = ezANOVA(data, dependent variable, wid, within, detailed = TRUE, return_aov = TRUE)

#Ephemeroptera
model <- ezANOVA(inv.melt, dv = variable.names(), wid = 16, within = "Treatment", detailed = TRUE, return_aov = TRUE) 

                 
# Shapiro test for normality
shapiro.test(inv$Ephemeroptera)
shapiro.test(inv$Coleoptera)
shapiro.test(inv$Chironimidae)
shapiro.test(inv$Chaoboridae)
shapiro.test(inv$Culex)
shapiro.test(inv$Diptera)

#Levene's test for homogeneity of variance Ceriodaphnia by date *not working
#leveneTest(value ~ variable, dataset)
leveneTest(Ephemeroptera ~ Treatment, inv)

#high high
#one-way ANOVA
inv.aov <- aov(Ephemeroptera ~ Treatment, data=inv)
summary(inv.aov)

#+TukeyHSD
TukeyHSD(inv.aov)

#+bonferroni correction
#+new alpha value

#high low
#Welch-two sample test
t.test(Ephemeroptera ~ Treatment)

#low high
#use Kruskal-Wallis test
kruskal.test(Ephemeroptera ~ Treatment, inv)

#low low
#Wilcox-Mann Whitney
(wilcox.test(mpg ~ am, data=mtcars) )



#subset data frame - indv date
daph09.02 <- daph[65:80,]

#check date range:
daph09.02

# Shapiro test for normality Ceriodaphnia by date *not working, change date text to read as date?*
shapiro.test(as.numeric(daph$Ceriodaphnia))

#Levene's test for homogeneity of variance Ceriodaphnia by date *not working
#leveneTest(value ~ variable, dataset)
leveneTest(Ceriodaphnia ~ Treatment, daph09.02)


                 
                 

