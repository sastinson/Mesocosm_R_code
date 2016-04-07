#set working directory
setwd("/Users/WeeRedLass/Desktop/")

#read data 

#indv species 
daph1 <- read.csv("Daphnids9.09.csv", header=T)
attach(daph1)
names(daph1)

controls <- daph[daph$treatment=="control",]
daph.control <- daph[daph$treatment=="control" | phys$treatment=="high diuron",]
daph.melt <- melt(daph.control, id.vars = c("tank", "date", "treatment"))
daph.melt <- melt(daph, id.vars = c("tank", "date", "treatment"))

# Shapiro test for normality Ceriodaphnia by date *not working, change date text to read as date?*
shapiro.test(daph$Diptera)

#Levene's test for homogeneity of variance Ceriodaphnia by date *not working
#leveneTest(value ~ variable, dataset)
leveneTest(Diptera ~ treatment, daph)

#high high
#one-way ANOVA
daph.aov <- aov(Arthropoda ~ treatment, data=daph)
summary(daph.aov)

#+TukeyHSD
TukeyHSD(daph.aov)

#+bonferroni correction
#+new alpha value

#high low
#Welch-two sample test
t.test(Diptera ~ treatment)

#low high
#use Kruskal-Wallis test
kruskal.test(Rotifera ~ treatment, daph1)

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

#"Practice rm ANOVA 2.24.16"

#this is my practice repeated measures (rm) anova

#load ez package
install.packages("ez")
install.packages("lsr")
library(ez)
library(lsr)

model <- ezANOVA(data = zplstats, dv = Cyclopoida, + 
                   wid = Tank, + 
                   between = Treatment, + 
                   detailed = T, + 
                   within_full = .(Calanoida, Nauplia) +
                   return_aov = T

