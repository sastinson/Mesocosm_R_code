#set working directory
setwd("/Users/WeeRedLass/Desktop/")

#load packages
library(ggplot2)
library(reshape)
library(reshape2)

#read data set
cover <- read.csv("percentcoverpractice.csv")

#melt data
controls <- cover[cover$Treatment=="control",]
cover.control <- cover[cover$Treatment=="control" | cover$Treatment=="high diuron",]
cover.melt <- melt(cover.control, id.vars = c("Tank..", "Date", "Treatment"))
cover.melt <- melt(cover, id.vars = c("Tank..", "Date", "Treatment"))

percent <- cover.melt[cover.melt$variable=="percentcover",]

cover.plot <- ggplot(data=cover, aes(x = Date, y = Percentcover, colour = Treatment)) +
  geom_boxplot() + ggtitle("Myriophyllum growth") + ylab("Percent Cover") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.1), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Macro_percent_3_23_16.pdf")
ggsave("Macro_percent_3_23_16.png", dpi = 400)
