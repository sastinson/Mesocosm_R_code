#boxplots of macroinverts
setwd("/Users/WeeRedLass/Desktop/")
#load macroinvert data
inv <- read.csv("macroinvertebrate data presentation 2.20.16.csv")
str(inv)

#data summary
summary(zp)

controls <- inv[inv$treatment=="control",]
inv.control <- inv[inv$treatment=="control" | inv$treatment=="high diuron",]
inv.melt <- melt(inv.control, id.vars = c("Tank..", "Date", "Treatment"))
inv.melt <- melt(inv, id.vars = c("Tank..", "Date", "Treatment"))

#facet wrap to repeat for all indv macroinvert species by treatment
inv.plot <- ggplot(data=inv.melt, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot()                    
inv.plot + facet_wrap(~variable) +
  geom_boxplot() + ggtitle("Macroinvertebrates") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=10, vjust=0.5)) +
  scale_y_continuous(limits = c(0,200), breaks = seq(0, 200, 50), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("macroinvertebrate_indvspecies_treatment_boxplot_2_20_16.pdf")
ggsave("macroinvertebrate_indvspecies_treatment_boxplot_2_20_16.png", dpi = 400)

#first subset species = Ephemeroptera
ephem <- inv.melt[inv.melt$variable=="Ephemeroptera",]

#plot Ephemeroptera
ephem.plot <- ggplot(data=ephem, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Ephemeroptera") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,75), breaks = seq(0, 75, 10), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Ephemeroptera_boxplot_2_20_16.pdf")
ggsave("Ephemeroptera_boxplot_2_20_16.png", dpi = 400)

#subset species = Chironomidae
chiro <- inv.melt[inv.melt$variable=="Chironominae",]

#plot Chironominae
chiro.plot <- ggplot(data=chiro, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Chironominae") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 25), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Chironominae_boxplot_2_20_16.pdf")
ggsave("Chironominae_boxplot_2_20_16.png", dpi = 400)

#subset species = Chaoboridae
chaob <- inv.melt[inv.melt$variable=="Chaoboridae",]

#plot Chaoboridae
chaob.plot <- ggplot(data=chaob, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Chaoboridae") + ylab("number of individuals") + xlab("") + 
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
ggsave("Chaoboridae_boxplot_2_20_16.pdf")
ggsave("Chaoboridae_boxplot_2_20_16.png", dpi = 400)

#subset species = Culexlarvae
culexl <- inv.melt[inv.melt$variable=="Culexlarvae",]

#plot Culexlarvae
culexl.plot <- ggplot(data=culexl, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Culex larvae") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,200), breaks = seq(0, 200, 25), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Culexlarvae_boxplot_2_20_16.pdf")
ggsave("Culexlarvae_boxplot_2_20_16.png", dpi = 400)

#subset species = Zygoptera
zyg <- inv.melt[inv.melt$variable=="Zygoptera",]

#plot Zygoptera
zyg.plot <- ggplot(data=zyg, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Zygoptera") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,75), breaks = seq(0, 75, 10), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Zygoptera_boxplot_2_20_16.pdf")
ggsave("Zygoptera_boxplot_2_20_16.png", dpi = 400)

#plot Zygoptera closeup
zyg.plot <- ggplot(data=zyg, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Zygoptera") + ylab("number of individuals") + xlab("") + 
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
ggsave("Zygoptera_boxplot_closeup_2_20_16.pdf")
ggsave("Zygoptera_boxplot_closeup_2_20_16.png", dpi = 400)

#subset species = Anisoptera
anis <- inv.melt[inv.melt$variable=="Anisoptera",]

#plot Anisoptera
anis.plot <- ggplot(data=anis, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Anisoptera") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0, 20, 10), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Anisoptera_boxplot_2_20_16.pdf")
ggsave("Anisoptera_boxplot_2_20_16.png", dpi = 400)

#subset species = Radix
rad <- inv.melt[inv.melt$variable=="Radix",]

#plot Radix
rad.plot <- ggplot(data=rad, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Radix") + ylab("number of individuals") + xlab("") + 
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
ggsave("Radix_boxplot_2_20_16.pdf")
ggsave("Radix_boxplot_2_20_16.png", dpi = 400)

#subset species = Hyalella azteca
haz <- inv.melt[inv.melt$variable=="Hyalellaazteca",]

#plot Hyalellaazteca
haz.plot <- ggplot(data=haz, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Hyalella azteca") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,150), breaks = seq(0, 150, 25), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Hyalellaazteca_boxplot_2_20_16.pdf")
ggsave("Hyalellaazteca_boxplot_2_20_16.png", dpi = 400)

#plot Hyalellaazteca closeup
haz.plot <- ggplot(data=haz, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Hyalella azteca") + ylab("number of individuals") + xlab("") + 
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
ggsave("Hyalellaazteca_boxplot_closeup_2_20_16.pdf")
ggsave("Hyalellaazteca_boxplot_closeup_2_20_16.png", dpi = 400)

#subset species = Nematoda
nem <- inv.melt[inv.melt$variable=="Nematoda",]

#plot Nematoda
nem.plot <- ggplot(data=nem, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Nematoda") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, 5), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Nematoda_boxplot_2_20_16.pdf")
ggsave("Nematoda_boxplot_2_20_16.png", dpi = 400)

#subset species = Hydrachnidae
hydrac <- inv.melt[inv.melt$variable=="Hydrachnidae",]

#plot Hydrachnidae
hydrac.plot <- ggplot(data=hydrac, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Hydrachnidae") + ylab("number of individuals") + xlab("") + 
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
ggsave("Hydrachnidae_boxplot_2_20_16.pdf")
ggsave("Hydrachnidae_boxplot_2_20_16.png", dpi = 400)