#set working directory
setwd("/Users/WeeRedLass/Desktop/")

#load packages
#load reshape, reshape2, ggplot

#melt data
zpa <- read.csv("zpsum_3.28.16.csv")
str(zp)
controls <- zpa[zpa$Treatment=="control",]
zpa.control <- zpa[zpa$Treatment=="control" | zpa$Treatment=="high diuron",]
zpa.melt <- melt(zpa.control, id.vars = c("Tank..", "Date", "Treatment"))
zpa.melt <- melt(zpa, id.vars = c("Tank..", "Date", "Treatment"))

#facet wrap to repeat for all zoo families by treatment
zp.plot <- ggplot(data=zp.melt, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot()                    
zp.plot + facet_wrap(~variable) +
  geom_boxplot() + ggtitle("Summary of Zooplankton abundance") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=8, vjust=0.5)) +
  scale_y_continuous(limits = c(0,5000), breaks = seq(0, 5000, 1000), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 16, face = "bold"))

#save data
ggsave("Zoo_Fam_Sum_3_28_16.pdf")
ggsave("Zoo_Fam_Sum_3_28_16.png", dpi = 400)

zp <- read.csv("zpnorm_3.28.16.csv")
str(zp)
controls <- zp[zp$Treatment=="control",]
zp.control <- zp[zp$Treatment=="control" | zp$Treatment=="high diuron",]
zp.melt <- melt(zp.control, id.vars = c("Tank..", "Date", "Treatment"))
zp.melt <- melt(zp, id.vars = c("Tank..", "Date", "Treatment"))

#first subset sepcies = daphnids = cdaphnia + dmagna 
daph <- zp.melt[zp.melt$variable=="Daphnidae",]

#plot daphnids boxplot
daph.plot <- ggplot(data=daph, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Daphnidae") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,6000), breaks = seq(0, 6000, 1000), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Daphnid_boxplot_3_28_16.pdf")
ggsave("Daphnid_boxplot_3_28_16.png", dpi = 400)


#plot daphnids line graph
daph.plot <- ggplot(data=daph, aes(x = Date, y = value, colour = Treatment)) +
  ggtitle("Daphnids") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,6000), breaks = seq(0, 6000, 1000), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Daphnid_line_3_28_16.pdf")
ggsave("Daphnid_line_3_28_16.png", dpi = 400)

#repeat subset species = Ostracoda
ostra <- zp.melt[zp.melt$variable=="Ostracoda",]

#plot Ostracoda
ostra.plot <- ggplot(data=ostra, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Ostracoda") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,1500), breaks = seq(0, 1500, 500), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Ostracoda_boxplot_3_28_16.pdf")
ggsave("Ostracoda_boxplot_3_28_16.png", dpi = 400)

#repeat subset species = Cyclopoida
cyclo <- zp.melt[zp.melt$variable=="Cyclopoida",]

#plot Cyclopodia
cyclo.plot <- ggplot(data=cyclo, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Cyclopoida") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,800), breaks = seq(0, 800, 200), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Cyclopoida_boxplot_3_28_16.pdf")
ggsave("Cyclopoida_boxplot_3_28_16.png", dpi = 400)

#repeat subset species = Nauplia
naup <- zp.melt[zp.melt$variable=="Nauplia",]

#plot Nauplia
naup.plot <- ggplot(data=naup, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Nauplia") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,1000), breaks = seq(0, 1000, 200), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Nauplia_boxplot_3_28_16.pdf")
ggsave("Nauplia_boxplot_3_28_16.png", dpi = 400)

#repeat subset species = Brachionus quadridentatus 
brachquad <- zp.melt[zp.melt$variable=="Brachionusquadridentatus",]

#plot Brachionus quadridentatus 
brachquad.plot <- ggplot(data=brachquad, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Brachionus quadridentatus") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,10000), breaks = seq(0, 10000, 2000), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("brachquad_boxplot_3_28_16.pdf")
ggsave("brachquad_boxplot_3_28_16.png", dpi = 400)

#repeat subset species = Bosminidae 
bosm <- zp.melt[zp.melt$variable=="Bosminidae",]

#plot Bosminidae 
bosm.plot <- ggplot(data=bosm, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Bosminidae") + ylab("Individuals/cubic meter") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,4500), breaks = seq(0, 4500, 1000), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("bosminidae_boxplot_3_20_16.pdf")
ggsave("bosminidae_boxplot_3_20_16.png", dpi = 400)

#repeat subset species = Chydoridae 
chyd <- zp.melt[zp.melt$variable=="Chydoridae",]

#plot Chydoridae 
chyd.plot <- ggplot(data=chyd, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Chydoridae") + ylab("Individuals/cubic meter") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,1200), breaks = seq(0, 1200, 500), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Chydoridae_boxplot_3_20_16.pdf")
ggsave("Chydoridae_boxplot_3_20_16.png", dpi = 400)

#melt Chydoridae and Bosminidae
chybos <- zp.melt[zp.melt$variable=="Chydoridae" | zp.melt$variable=="Bosminidae",]

#plot Chydoridae and Bosminidae
chybos.plot <- ggplot(data=chybos, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Chydoridae & Bosminidae") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,800), breaks = seq(0, 800, 200), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Chy_plus_Bos_boxplot_3_28_16.pdf")
ggsave("Chy_plus_Bos_boxplot_3_28_16.png", dpi = 400)

#all Cladocera
clado <- zpa.melt[zp.melt$variable=="Cladocera",]

#plot all Cladocera
clado.plot <- ggplot(data=clado, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Cladocera") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0,5000), breaks = seq(0, 5000, 1000), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save data
ggsave("Clado_boxplot_3_28_16.pdf")
ggsave("Clado_boxplot_3_28_16.png", dpi = 400)

#all Copepoda
cope <- zpa.melt[zpa.melt$variable=="Copepoda" ,]

#plot all Copepoda
cope.plot <- ggplot(data=cope, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Copepoda") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, 200), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save
ggsave("Cope_boxplot_3_28_16.pdf")
ggsave("Cope_boxplot_3_28_16.png", dpi = 400)

#all Rotifers *not working*
rot <- zpa.melt[zpa.melt$variable=="Rotifera" ,]

#plot all Copepoda
rot.plot <- ggplot(data=cope, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot() + ggtitle("Rotifera") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=12, vjust=0.5, face = "bold")) +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, 200), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 18, face = "bold"))

#save
ggsave("Rot_boxplot_3_28_16.pdf")
ggsave("Rot_boxplot_3_28_16.png", dpi = 400)

  
#facet wrap to repeat for all indv zoo species by treatment
rot.plot <- ggplot(data=rot.melt, aes(x = Date, y = value, colour = Treatment)) +
  geom_boxplot()                    
rot.plot + facet_wrap(~variable) +
  geom_boxplot() + ggtitle("Rotifera") + ylab("Individuals per L") + xlab("") + 
  scale_colour_manual(values=c("deepskyblue3","goldenrod1","firebrick2","magenta")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(axis.text.x = element_text(angle=50, size=8, vjust=0.5)) +
  scale_y_continuous(limits = c(0,3000), breaks = seq(0, 3000, 500), expand = c(0,0)) +
  theme(legend.title = element_blank()) + 
  theme(plot.title = element_text(size = 16, face = "bold"))

#save data
ggsave("Rotifer_Summary_3_24_16.pdf")
ggsave("Rotifer_Summary_3_24_16.png", dpi = 400)

