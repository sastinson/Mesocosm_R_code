#set working directory
setwd("/Users/WeeRedLass/Desktop/")

#load packages
#load reshape, reshape2, ggplot

#melt data
nk <- read.csv("nk.csv")
str(nk)
controls <- nk[nk$Treatment=="control",]
nk.control <- nk[nk$Treatment=="control" | nk$Treatment=="high diuron",]
nk.melt <- melt(nk.control, id.vars = c("Tank..", "Date", "Treatment"))
nk.melt <- melt(nk, id.vars = c("Tank..", "Date", "Treatment"))

phosphate <- nk.melt[nk.melt$variable=="Phosphate",]
nk.melt$Phosphate=as.numeric(levels(nk.melt$Phosphate))[nk.melt$Phosphate]

#plot
nk.plot <- ggplot(data=nk, aes(x = Date, y = Phosphate, colour = Treatment)) +
  geom_boxplot() + ggtitle("Total Phosphate (mg/L)") + ylab("Phosphate (mg/L)") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,0.1), breaks = seq(0, 0.1, 0.01), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Phosphate_boxplot_3_24_16.pdf")
ggsave("Phosphate_boxplot_3_24_16.png", dpi = 400)
