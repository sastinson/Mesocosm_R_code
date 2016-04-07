setwd("/Users/WeeRedLass/Desktop/")
phys<- read.csv("Physicochemical data for R.csv")
str(phys)

controls <- phys[phys$treatment=="control",]
phys.control <- phys[phys$treatment=="control" | phys$treatment=="high diuron",]
phys.melt <- melt(phys.control, id.vars = c("tank..", "date", "treatment"))
phys.melt <- melt(phys, id.vars = c("tank..", "date", "treatment"))

#temperature (C)
temp <- phys.melt[phys.melt$variable=="temperature",]
temp.plot <- ggplot(data=temp, aes(x = date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Temperature (C)") + ylab("Temperature (C)") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(12,22), breaks = seq(12, 22, 5), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Temperature_boxplot_2_24_16.pdf")
ggsave("Temperature_boxplot_2_24_16.png", dpi = 400)

#pH
pH <- phys.melt[phys.melt$variable=="pH",]
pH.plot <- ggplot(data=pH, aes(x = date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("pH") + ylab("pH") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(8.5,10), breaks = seq(8.5, 12, 1), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("pH_boxplot_2_24_16.pdf")
ggsave("pH_boxplot_2_24_16.png", dpi = 400)

#DO
DO <- phys.melt[phys.melt$variable=="DOmg",]
DO.plot <- ggplot(data=DO, aes(x = date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Dissolved Oxygen (mg)") + ylab("DO (mg)") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(6,15), breaks = seq(6, 15, 1), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("DO_boxplot_2_24_16.pdf")
ggsave("DO_boxplot_2_24_16.png", dpi = 400)

#EC
EC <- phys.melt[phys.melt$variable=="EC",]
EC.plot <- ggplot(data=EC, aes(x = date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("EC (μS/cm)") + ylab("EC (μS/cm") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(500,900), breaks = seq(500, 900, 200), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("EC_boxplot_2_24_16.pdf")
ggsave("EC_boxplot_2_24_16.png", dpi = 400)

#SC
SC <- phys.melt[phys.melt$variable=="SC",]
SC.plot <- ggplot(data=SC, aes(x = date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("SC (S/M)") + ylab("SC (S/M") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(500,1100), breaks = seq(500, 1100, 200), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("SC_boxplot_2_24_16.pdf")
ggsave("SC_boxplot_2_24_16.png", dpi = 400)


