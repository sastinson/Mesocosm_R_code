#set working directory
setwd("/Users/WeeRedLass/Desktop/")

#load packages
library(ggplot2)
library(scales)
library(timeSeries)
library(chron)

#read data
temp <-read.csv("subset1.csv")

temp$Date<-as.Date(temp$Date,"%m/%d/%y")

#plot
ggplot(temp, aes(x=Date, y=Temp, group=Temperatures, colour=Temperatures)) +
  geom_line() +
  geom_point() + 
  ggtitle("Daily Min/Max Temperatures") + ylab("Temperature (C)") + xlab("") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(10,32), breaks = seq(0, 32, 5), expand = c(0,0)) +
  scale_x_date(labels=date_format("%m/%d/%y")) +
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Temp_MinMax_3_24_16.pdf")
ggsave("Temp_MinMax_3_24_16.png", dpi = 400)
  