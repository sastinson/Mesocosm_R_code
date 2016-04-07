setwd("/Users/WeeRedLass/Desktop/")
zpc <- read.csv("Zooplankton combined categories 2.20.16.csv")
str(zpc)

controls <- zpc[zpc$treatment=="control",]
zpc.control <- zpc[zpc$treatment=="control" | zpc$treatment=="high diuron",]
zpc.melt <- melt(zpc.control, id.vars = c("Tank..", "Date", "treatment"))
zpc.melt <- melt(zpc, id.vars = c("Tank..", "Date", "treatment"))

#Copepoda
cpod <- zpc.melt[zpc.melt$variable=="Copepoda",]

#plot Copepoda
cpod.plot <- ggplot(data=cpod, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Copepoda") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,50), breaks = seq(0, 50, 10), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Copepoda_boxplot_2_20_16.pdf")
ggsave("Copepoda_boxplot_2_20_16.png", dpi = 400)

#Rotifera
rot <- zpc.melt[zpc.melt$variable=="Rotifera",]

#plot Rotifera
rot.plot <- ggplot(data=rot, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Rotifera") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,200), breaks = seq(0, 200, 50), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Rotifera_boxplot_2_20_16.pdf")
ggsave("Rotifera_boxplot_2_20_16.png", dpi = 400)

#arthrodopda/crustaceae
arth <- zpc.melt[zpc.melt$variable=="arthrodpodacrustaceae",]

#plot arthrodpoda crustaceae
arth.plot <- ggplot(data=arth, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Arthrodpoda/Crustaceae") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0, 300, 50), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Arthropoda_boxplot_2_20_16.pdf")
ggsave("Arthropoda_boxplot_2_20_16.png", dpi = 400)

invc <- read.csv("Macroinvertebrate combined categories 2.20.16.csv")

controls <- invc[invc$treatment=="control",]
invc.control <- invc[invc$treatment=="control" | invc$treatment=="high diuron",]
invc.melt <- melt(invc.control, id.vars = c("Tank..", "Date", "treatment"))
invc.melt <- melt(invc, id.vars = c("Tank..", "Date", "treatment"))

#Diptera
dip <- invc.melt[invc.melt$variable=="Diptera",]

#plot Diptera
dip.plot <- ggplot(data=dip, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Diptera") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,250), breaks = seq(0, 250, 50), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Diptera_boxplot_2_20_16.pdf")
ggsave("Diptera_boxplot_2_20_16.png", dpi = 400)

#Odonata
odo <- invc.melt[invc.melt$variable=="Odonata",]

#plot Odonata
odo.plot <- ggplot(data=odo, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Odonata") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,75), breaks = seq(0, 75, 25), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Odonata_boxplot_2_20_16.pdf")
ggsave("Odonata_boxplot_2_20_16.png", dpi = 400)

#Gastropoda
gas <- invc.melt[invc.melt$variable=="Gastropoda",]

#plot Gastropoda
gas.plot <- ggplot(data=gas, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Gastropoda") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, 10), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Gastropoda_boxplot_2_20_16.pdf")
ggsave("Gastropoda_boxplot_2_20_16.png", dpi = 400)
