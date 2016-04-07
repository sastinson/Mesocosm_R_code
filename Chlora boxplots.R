setwd("/Users/WeeRedLass/Desktop/")
chlor<- read.csv("Chlora for R.csv")

controls <- chlor[chlor$treatment=="control",]
chlor.control <- chlor[chlor$treatment=="control" | chlor$treatment=="high diuron",]
chlor.melt <- melt(chlor.control, id.vars = c("tank..", "date", "treatment"))
chlor.melt <- melt(chlor, id.vars = c("tank..", "date", "treatment"))

chlor <- chlor.melt[chlor.melt$variable=="chlorappb",]
chlor.plot <- ggplot(data=chlor, aes(x = date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Chlorophyll-a (ppb)") + ylab("Chlorophyll-a (ppb)") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,25), breaks = seq(0, 25, 5), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Chlor_boxplot_2_25_16.pdf")
ggsave("Chlor_boxplot_2_25_16.png", dpi = 400)
