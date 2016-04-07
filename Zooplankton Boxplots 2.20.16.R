#set working directory
setwd("/Users/WeeRedLass/Desktop/")

#load packages
#load reshape, reshape2, ggplot

#melt data
zp <- read.csv("Zooplankton normalized 3.17.16.csv")
str(zp)
controls <- zp[zp$Treatment=="control",]
zp.control <- zp[zp$Treatment=="control" | zp$Treatment=="high diuron",]
zp.melt <- melt(zp.control, id.vars = c("Tank..", "Date", "Treatment"))
zp.melt <- melt(zp, id.vars = c("Tank..", "Date", "Treatment"))

#first subset sepcies = daphnids = cdaphnia + dmagna (combined in excel as Ceriodaphnia)
cdaph <- zp.melt[zp.melt$variable=="Ceriodaphnia",]

#plot daphnids
cdaph.plot <- ggplot(data=cdaph, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Daphnids") + ylab("number of individuals per m^3") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,5000), breaks = seq(0, 5000, 500), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Daphnid_boxplot_3_17_16.pdf")
ggsave("Daphnid_boxplot_3_17_16.png", dpi = 400)

#facet wrap to repeat for all species individual charts
zp.plot <- ggplot(data=zp.melt, aes(x = Date, y = value, colour = Treatment, group = variable)) +
  geom_boxplot()   
zp.plot + facet_wrap(~variable) +
  geom_boxplot() + ggtitle("Zooplankton") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,10000), breaks = seq(0, 10000, 5000), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Zooplankton_indvspecies_graph_2_20_16.pdf")
ggsave("Zooplankton_indvspecies_graph_2_20_16.png", dpi = 400)

#facet wrap to repeat for all indv zooplankton species by treatment
zp.plot <- ggplot(data=zp.melt, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot()                    
zp.plot + facet_wrap(~variable) +
  geom_boxplot() + ggtitle("Zooplankton") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=10, vjust=0.5)) +
  scale_y_continuous(limits = c(0,500), breaks = seq(0, 500, 100), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Zooplankton_indvspecies_treatment_graph_2_20_16.pdf")
ggsave("Zooplankton_indvspecies_treatment_graph_2_20_16.png", dpi = 400)

#facet wrap to repeat for each species, treatment closeup
zp.plot <- ggplot(data=zp.melt, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot()                    
zp.plot + facet_wrap(~variable) +
  geom_boxplot() + ggtitle("Zooplankton") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=10, vjust=0.5)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 25), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Zooplankton_indvspecies_treatment_graph_closeup_2_20_16.pdf")
ggsave("Zooplankton_indvspecies_treatment_graph_closeup_2_20_16.png", dpi = 400)

# violin plots with jitter points
cdaph.plot <- ggplot(data=cdaph, aes(x = Date, y = value, colour = treatment)) + 
  geom_violin(alpha=0.5, color="firebrick2") + geom_jitter(alpha=0.5, aes(color="firebrick2"), position = position_jitter(width = 0.1))

#Daphnids violin plot
cdaph.plot <- ggplot(data=cdaph, aes(x = Date, y = value, colour = treatment)) +
  geom_violin(alpha=0.5, aes(color=treatment)) + 
  ggtitle("Daphnids") + ylab("number of individuals") + xlab("") +
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,400), breaks = seq(0, 400, 50), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))
  
  #save data
ggsave("Daphnid_violin_2_20_16.pdf")
ggsave("Daphnid_violin_2_20_16.png", dpi = 400)

#Daphnid jitter points
cdaph.plot <- ggplot(data=cdaph, aes(x = Date, y = value, colour = treatment)) +
  geom_jitter(alpha=0.5, aes(color=treatment), position = position_jitter(width = 0.5)) +
  ggtitle("Daphnids") + ylab("number of individuals") + xlab("") +
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,400), breaks = seq(0, 400, 50), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Daphnid_jitter_2_20_16.pdf")
ggsave("Daphnid_jitter_2_20_16.png", dpi = 400)

# Daphnids violin plots + jitter points
cdaph.plot <- ggplot(data=cdaph, aes(x = Date, y = value, colour = treatment)) +
  geom_violin(alpha=0.5, aes(color=treatment)) + 
  geom_jitter(alpha=0.5, aes(color=treatment), position = position_jitter(width = 0.5)) +
  ggtitle("Daphnids") + ylab("number of individuals") + xlab("") +
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,400), breaks = seq(0, 400, 50), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Daphnid_violin_jitter_2_20_16.pdf")
ggsave("Daphnid_violin_jitter_2_20_16.png", dpi = 400)

#repeat subset species = Ostracoda
ostra <- zp.melt[zp.melt$variable=="Ostracoda",]

#plot Ostracoda
ostra.plot <- ggplot(data=ostra, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Ostracoda") + ylab("number of individuals") + xlab("") + 
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
ggsave("Ostracoda_boxplot_2_20_16.pdf")
ggsave("Ostracoda_boxplot_2_20_16.png", dpi = 400)

#repeat subset species = Cyclopoida
cyclo <- zp.melt[zp.melt$variable=="Cyclopoida",]

#plot Cyclopodia
cyclo.plot <- ggplot(data=cyclo, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Cyclopoida") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,80), breaks = seq(0, 80, 20), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Cyclopoida_boxplot_2_20_16.pdf")
ggsave("Cyclopoida_boxplot_2_20_16.png", dpi = 400)

#repeat subset species = Nauplia
naup <- zp.melt[zp.melt$variable=="Nauplia",]

#plot Nauplia
naup.plot <- ggplot(data=naup, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Nauplia") + ylab("number of individuals") + xlab("") + 
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
ggsave("Nauplia_boxplot_2_20_16.pdf")
ggsave("Nauplia_boxplot_2_20_16.png", dpi = 400)

#repeat subset species = Brachionus quadridentatus 
brachquad <- zp.melt[zp.melt$variable=="Brachionusquadridentatus",]

#plot Brachionus quadridentatus 
brachquad.plot <- ggplot(data=brachquad, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Brachionus quadridentatus") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,1000), breaks = seq(0, 1000, 100), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("brachquad_boxplot_2_20_16.pdf")
ggsave("brachquad_boxplot_2_20_16.png", dpi = 400)

#repeat subset species = Bosminidae 
bosm <- zp.melt[zp.melt$variable=="Bosminidae",]

#plot Bosminidae 
bosm.plot <- ggplot(data=bosm, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Bosminidae") + ylab("number of individuals") + xlab("") + 
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
ggsave("bosminidae_boxplot_2_20_16.pdf")
ggsave("bosminidae_boxplot_2_20_16.png", dpi = 400)

#repeat subset species = Chydoridae 
chyd <- zp.melt[zp.melt$variable=="Chydoridae",]

#plot Chydoridae 
chyd.plot <- ggplot(data=chyd, aes(x = Date, y = value, colour = treatment)) +
  geom_boxplot() + ggtitle("Chydoridae") + ylab("number of individuals") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,40), breaks = seq(0, 40, 10), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Chydoridae_boxplot_2_20_16.pdf")
ggsave("Chydoridae_boxplot_2_20_16.png", dpi = 400)