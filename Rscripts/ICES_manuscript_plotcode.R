#####################################################
################### Molly Morse #####################
####### Additional Plotting for Bluefin Stock #######
########### Assessment Simulation Testing ###########
#####################################################

library(RColorBrewer)
library(stringr)
library(ggplot2)
library(stringi)
library(reshape2)
library(extrafont)
library(gridExtra)
library(scales)
library(grid)
library(cowplot)
library(readr)
yrs <- 1974:2015
nyr <- 42
# run the following 3 lines to import "Times New Roman" and other fonts if haven't done already (must have extrafont package installed)
# font_import()
# y
# loadfonts(device = "win")







#### OM ####
setwd("C:/Users/mmorse1/Documents/Simulations_lomov/OM_output")

R_om <- as.matrix(read.csv("R.csv", header = T))
R_om <- array(R_om[,-1], c(42, 4, 7, 2), dimnames = list(year = 1974:2015, quarter = 1:4, zone = 1:7, unit = 1:2))
SSBp_om <- as.matrix(read.csv("T_Pssb.csv", header = T))

# East R #
plot(1974:2015, R_om[, 1, 1, 2], type = "l") #same as base OM


# East SSB #

#population (see above)
#stock
SSBs_om_e   <- as.matrix(read.csv("T_Essb.csv", header = T))

#combine
SSB_om_e <- as.data.frame(cbind(SSBp_om[, c(1, 4)], SSBs_om_e[, 4])) 
colnames(SSB_om_e) <- c("years","Population view","Stock view")
SSB_om_e <- melt(SSB_om_e, id.vars = "years")

#plot
SSB_om_e_plot <-
ggplot(data = SSB_om_e, aes(x=years,y=value,linetype=variable)) +
  #geom_line(aes(size = variable)) +
  geom_line(size = 1.5, color = 1) +
  theme_classic() +
  labs(y = "SSB (tonnes)", x="", title="East") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position = c(0.2, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 19),
        legend.key.width = unit(2, "cm")) +
  scale_size_manual(values = c(1,1,2)) +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,1000000,500000)) +
  coord_cartesian(ylim=c(0,1000000))


# West R #
#same as base OM

# West SSB #
#population (see above)

#stock
SSBs_om_w <- as.matrix(read.csv("T_Wssb.csv", header = T))
plot(1974:2015, SSBs_om_w[,3], type = "l", ylim = c(0, 600000))

#combine
SSB_om_w <- as.data.frame(cbind(SSBp_om[, c(1, 8)], SSBs_om_w[, 4]))
colnames(SSB_om_w) <- c("years","Population view","Stock view")
SSB_om_w <- melt(SSB_om_w, id.vars = "years")

#plot
SSB_om_w_plot <-
ggplot(data = SSB_om_w, aes(x=years,y=value,linetype=variable)) +
  # geom_line(aes(size = variable)) +
  geom_line(size = 1.5, color = 1) +
  theme_classic() +
  labs(y = "SSB (tonnes)", x="", title="West") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position = c(0.2, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 19),
        legend.key.width = unit(2, "cm")) +
  scale_size_manual(values = c(1,1,2)) +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,600000,300000)) +
  coord_cartesian(ylim=c(0,600000))

jpeg("C:/Users/mmorse1/Documents/Simulations_lomov/OM_output/OM_ssb_meanmove.jpeg",
     width = 4400, height = 2200, units = "px", quality = 100, res = 300)
grid.arrange(SSB_om_w_plot, SSB_om_e_plot,
             ncol = 2, nrow = 1)
dev.off()




## Population SSB by area (for stacked area charts) ##
e_area <- read_csv("C:/Users/mmorse1/Documents/Simulations_2/OM_Base_Output/Pssb-eastarea.csv")
w_area <- read_csv("C:/Users/mmorse1/Documents/Simulations_2/OM_Base_Output/Pssb-westarea.csv")
e_area <- melt(e_area, id.vars = "year")
w_area <- melt(w_area, id.vars = "year")

grob1 <- grobTree(textGrob("E", x = 0.05, y = 0.95, hjust = 0,
                           gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
w_area_plot <-
  ggplot(w_area, aes(year, value, fill = variable)) +
  geom_area(color = "black") +
  scale_fill_manual(values = c("black", "gray85")) +
  theme_classic() +
  labs(y = "SSB
(tonnes)", x = "") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        legend.position = c(0.08, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 17),
        # legend.key.width = unit(.8, "cm"),
        legend.key.size = unit(.8, "cm")) +
  scale_y_continuous(label = scientific_format(digits = 1), breaks = seq(0,100000,50000)) +
  scale_x_continuous(breaks = seq(1974,2015,10)) +
  coord_cartesian(ylim=c(0,100000)) +
  annotation_custom(grob1)


grob2 <- grobTree(textGrob("F", x = 0.05, y = 0.95, hjust = 0,
                           gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
e_area_plot <-
  ggplot(e_area, aes(year, value, fill =  variable)) +
  geom_area(color = "black") +
  scale_fill_manual(values = c("black", "gray85")) +
  theme_classic() +
  labs(y = "", x = "") +
  theme(axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        legend.position = c(0.08, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 17),
        # legend.key.width = unit(1, "cm")) +
        legend.key.size = unit(.8, "cm")) +
  scale_y_continuous(label = scientific_format(digits = 1), breaks = seq(0,1000000,500000)) +
  scale_x_continuous(breaks = seq(1974,2015,10)) +
  coord_cartesian(ylim=c(0,1000000)) +
  annotation_custom(grob2)







#### COMPARING OM to ICCAT ####

setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/Misc")
ICCAT <- read.csv("ICCATresults.csv")

# Read in data saved from OM #
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Base_Output/T_Essb.csv", header = TRUE) #east stock Q3 base
OM.E.SSB.S.base <- temp[,4]
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Base_Output/T_Pssb.csv", header = TRUE) #east population Q3 base
OM.E.SSB.P.base <- temp[,4]
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Alt_Output/T_Essb.csv", header = TRUE) #east stock Q3 alt
OM.E.SSB.S.alt <- temp[,4]
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Alt_Output/T_Pssb.csv", header = TRUE) #east population Q3 alt
OM.E.SSB.P.alt <- temp[,4]
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Base_Output/T_Wssb.csv", header = TRUE) #west stock Q3 base
OM.W.SSB.S.base <- temp[,4]
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Base_Output/T_Pssb.csv", header = TRUE) #west population Q3 base
OM.W.SSB.P.base <- temp[,8]
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Alt_Output/T_Wssb.csv", header = TRUE) #west stock Q3 alt
OM.W.SSB.S.alt <- temp[,4]
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Alt_Output/T_Pssb.csv", header = TRUE) #west population Q3 alt
OM.W.SSB.P.alt <- temp[,8]
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Base_Output/NAA-1.csv", header = TRUE) #east R base
OM.E.R.base <- temp$X1.1.7.1
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Alt_Output/NAA.csv", header = TRUE) #east R alt
OM.E.R.alt <- temp$X1.1.7.1
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Base_Output/NAA-1.csv", header = TRUE) #west R base
OM.W.R.base <- temp$X1.1.1.2
temp <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Alt_Output/NAA.csv", header = TRUE) #west R alt
OM.W.R.alt <- temp$X1.1.1.2



# SSB - East (stock) #
# OM.E.SSB <- BFTE.100.SSB[,103] #stock Q1
# OM.E.SSBP <- BFTE.100.SSB[,102] #population Q1
# OM.E.SSB.alt <- T_Essb[1:nyr,3] #stock Q3
# OM.E.SSBP.alt <- T_Pssb[1:nyr,3,1] #population Q3
# OM.E.SSB.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/E_SSBCI_data.csv", header = TRUE)[,6]
# OM.E.SSBP.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/E_SSBCI_data.csv", header = TRUE)[,5]
ICCAT.E.SSB <- ICCAT[,2]
# E.SSB <- cbind(1974:2015, OM.E.SSB.S.base, OM.E.SSB.P.base, OM.E.SSB.S.alt, OM.E.SSB.P.alt, ICCAT.E.SSB) #w/ alt OM + ICCAT
# E.SSB <- cbind(1974:2015, OM.E.SSB.P.base, OM.E.SSB.S.base, ICCAT.E.SSB) w/o alt OM and w/ ICCAT
E.SSB <- cbind(1974:2015, OM.E.SSB.P.base, OM.E.SSB.S.base) #w/o ICCAT
E.SSB.df <- as.data.frame(E.SSB)
# colnames(E.SSB.df) <- c("years","Population view","Stock view","ICCAT") #w/ ICCAT
colnames(E.SSB.df) <- c("years","Population","Stock") #w/o ICCAT
E.SSB_1 <- melt(E.SSB.df, id.vars = "years")

grob3 <- grobTree(textGrob("D", x = 0.05, y = 0.95, hjust = 0,
                           gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
E.SSB.OM.plot <-
  ggplot(data = E.SSB_1, aes(x = years, y = value)) +
  geom_line(aes(linetype = variable), size = 1.5) +  
  theme_classic() +
  labs(y = "", x = "", title = "") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold"),
        # axis.text.x = element_text(family = "Times New Roman",
        #                            size = 22),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        legend.position = c(0.1, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 17),
        legend.key.width = unit(1.6, "cm"),
        legend.key.size = unit(.8, "cm")) +
  # scale_size_manual(values = c(1, 1, 2)) +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_y_continuous(label = scientific_format(digits = 1), breaks = seq(0,1000000,500000)) +
  scale_x_continuous(breaks = seq(1974,2015,10)) +
  coord_cartesian(ylim=c(0,1000000)) +
  annotation_custom(grob3)
  




# SSB - West (stock) #
# OM.W.SSB <- BFTW.100.SSB[,103] #stock
# OM.W.SSBP <- BFTW.100.SSB[,102] #population
# OM.W.SSB.alt <- T_Wssb[1:nyr,3] #stock Q3
# OM.W.SSBP.alt <- T_Pssb[1:nyr,3,2] #population Q3
# OM.W.SSB.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 1/W_SSBCI_data.csv", header = TRUE)[,6]
# OM.W.SSBP.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 1/W_SSBCI_data.csv", header = TRUE)[,5]
ICCAT.W.SSB <- ICCAT[,3]
# W.SSB <- cbind(1974:2015, OM.W.SSB.S.base, OM.W.SSB.P.base, OM.W.SSB.S.alt, OM.W.SSB.P.alt, ICCAT.W.SSB) #w/ alt OM + ICCAT
# W.SSB <- cbind(1974:2015,  OM.W.SSB.P.base, OM.W.SSB.S.base,ICCAT.W.SSB) #w/o alt OM + w/ ICCAT
W.SSB <- cbind(1974:2015,  OM.W.SSB.P.base, OM.W.SSB.S.base) #w/o ICCAT
W.SSB.df <- as.data.frame(W.SSB)
# colnames(W.SSB.df) <- c("years","Population view","Stock view","ICCAT") #w/ ICCAT
colnames(W.SSB.df) <- c("years","Population","Stock")
W.SSB_1 <- melt(W.SSB.df, id.vars="years")

grob4 <- grobTree(textGrob("C", x = 0.05, y = 0.95, hjust = 0,
                           gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
W.SSB.OM.plot <-
  ggplot(data = W.SSB_1, aes(x = years,y = value)) +
  geom_line(aes(linetype = variable), size = 1.5) +
  theme_classic() +
  labs(y = "SSB
(tonnes)", x="", title="") +
  theme(plot.title = element_text(family ="Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold",
                                    margin = margin(r = 10)),
        # axis.text.x = element_text(family = "Times New Roman",
        #                            size = 22),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        legend.position = c(0.1, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 17),
        legend.key.width = unit(1.6, "cm"),
        legend.key.size = unit(.8, "cm")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  # scale_size_manual(values = c(1,1,2)) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,100000,50000)) +
  scale_x_continuous(breaks = seq(1974,2015,10)) +
  coord_cartesian(ylim=c(0,100000)) +
  annotation_custom(grob4)


## OM EAST RECRUITMENT ##
#OM.E.R <- BFTE.100.R[,102]
# OM.E.R.alt <- naa[1:nyr,1,1,7,1]
# OM.E.R.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs/BFTstockassess.csv", header = TRUE)[1:42,2]
ICCAT.E.R <- ICCAT[,4]
# E.R <- cbind(1974:2015, OM.E.R.base, OM.E.R.alt) #w/ alt OM
E.R <- cbind(1974:2015, OM.E.R.base) #w/o alt OM
E.R.df <- as.data.frame(E.R)
# colnames(E.R.df) <- c("years", "Base/ICCAT","Alt") #w/ alt OM
colnames(E.R.df) <- c("years") #w/o alt OM
E.R_1 <- melt(E.R.df, id.vars="years")

grob5 <- grobTree(textGrob("B", x = 0.05, y = 0.95, hjust = 0,
                           gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
E.R.OM.plot <-
  ggplot(data = E.R_1,aes(x = years, y = value)) +
  geom_line(size = 1.5) +
  theme_classic() +
  labs(y = "", x = "", title = "East") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold"),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 14),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman")) +
  scale_y_continuous(label=scientific_format(digits = 2), breaks = seq(0,6000000,2000000)) +
  scale_x_continuous(breaks = seq(1974,2015,10)) +
  coord_cartesian(ylim=c(0,6000000)) +
  annotation_custom(grob5)


## OM WEST RECRUITMENT ##
# OM.W.R.alt <- naa[1:nyr,1,1,1,2]
# OM.W.R.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs/BFTstockassess.csv", header = TRUE)[1:42,3]
ICCAT.W.R <- ICCAT[,5]
# W.R <- cbind(1974:2015, OM.W.R.base, OM.W.R.alt) #w/ alt OM
W.R <- cbind(1974:2015, OM.W.R.base) #w/o alt OM
W.R.df <- as.data.frame(W.R)
# colnames(W.R.df) <- c("years", "Base/ICCAT", "Alt") #w/ alt OM
colnames(W.R.df) <- c("years") #w/o alt OM
W.R_1 <- melt(W.R.df, id.vars="years")

# ICCAT.W.R <- ICCAT[,5]
# W.R <- cbind(1974:2015, OM.W.R.base)
# W.R.df <- as.data.frame(W.R)
# colnames(W.R.df) <- c("years", "Base/ICCAT")
# W.R_1 <- melt(W.R.df, id.vars="years")
grob6 <- grobTree(textGrob("A", x = 0.05, y = 0.95, hjust = 0,
                           gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
W.R.OM.plot <-
  ggplot(data=W.R_1,aes(x = years, y = value)) +
  geom_line(size = 1.5) +
  theme_classic() +
  labs(y = "Recruitment
(numbers)", x= "", title = "West") +
  theme(plot.title = element_text(family="Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5), 
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold",
                                    margin = margin(r = 10)),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 14),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        legend.position = "none",
        legend.title = element_blank()) +
  scale_y_continuous(label=scientific_format(digits = 2)) +
  scale_x_continuous(breaks = seq(1974,2015,10)) +
  coord_cartesian(xlim=c(1974:2015)) +
  annotation_custom(grob6)


## OM PLOT LAYOUTS ##
jpeg("C:/Users/mmorse1/Documents/Publishing/Revisions - Bluefin Tuna Simulations/ICES JMS Review/Figures/OMplots.jpeg",
     width = 4000, height = 4200, units = "px", quality = 100, res = 300)
# grid.arrange(W.R.OM.plot, E.R.OM.plot,
#              W.SSB.OM.plot, E.SSB.OM.plot,
#              w_area_plot, e_area_plot,
#              ncol = 2, nrow = 3)
plot_grid(W.R.OM.plot, E.R.OM.plot,
          W.SSB.OM.plot, E.SSB.OM.plot,
          w_area_plot, e_area_plot,
          ncol = 2, nrow = 3, align = "v")
dev.off()






#### BASE CASE vs. LOW MOVEMENT OM ####

# East #
df <- as.data.frame(cbind(SSBp_om[, c(1, 4)], SSBs_om_e[, 4], OM.E.SSB.P.base, OM.E.SSB.S.base)) 
colnames(df) <- c("years","low - pop","low - stock", "base - pop", "base - stock")
df.plot <- melt(df, id.vars = "years")

ggplot(data = df.plot, aes(x=years,y=value,linetype=variable, color= variable)) +
  geom_line(aes(size = variable)) +
  theme_classic() +
  labs(y = "SSB (tonnes)", x="", title="East") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 19),
        legend.key.width = unit(2, "cm")) +
  scale_size_manual(values = c(1,1,1,1)) +
  scale_linetype_manual(values = c("solid","dotted","solid","dotted")) +
  scale_color_manual(values = c(1, 1, 2, 2)) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,1000000,500000)) +
  coord_cartesian(ylim=c(0,1000000))


# West #
dfw <- as.data.frame(cbind(SSBp_om[, c(1, 8)], SSBs_om_w[, 4], OM.W.SSB.P.base, OM.W.SSB.S.base))
colnames(dfw) <- c("years","low - pop","low - stock", "base - pop", "base - stock")
dfw.plot <- melt(dfw, id.vars = "years")

ggplot(data = dfw.plot, aes(x=years,y=value,linetype=variable, color= variable)) +
  geom_line(aes(size = variable)) +
  theme_classic() +
  labs(y = "SSB (tonnes)", x="", title="West") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 19),
        legend.key.width = unit(2, "cm")) +
  scale_size_manual(values = c(1,1,1,1)) +
  scale_linetype_manual(values = c("solid","dotted","solid","dotted")) +
  scale_color_manual(values = c(1, 1, 2, 2)) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,600000,200000)) +
  coord_cartesian(ylim=c(0,600000))



#### VPA-2BOX CONVERGENCE ####


# West #
nsims <- 500
notconverge <- c()
folder <- paste("West")
for (n in 0:nsims) {
  wd <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Run", n, sep = "")
  setwd(wd)
  
  # VPA-2BOX.LOG file
  log.file <- as.data.frame(read.table(file = "VPA-2BOX.LOG",
                                       fill = T, col.names = 1:max(count.fields("VPA-2BOX.LOG"))))
  cormat <- log.file[(nrow(log.file)-3),4]
  # Parameter estimates file
  parest_file_name <- paste("BFTW2017_", n, ".e1", sep = "")
  parest.file <- as.data.frame(read.table(file = parest_file_name,
                                          fill = T, col.names = 1:max(count.fields(parest_file_name))))
  bounds <- parest.file[,ncol(parest.file)]
  boundsv <- as.vector(bounds)
  boundlogic <- grepl("BOUND", boundsv, fixed = TRUE)
  if (length(which(boundlogic==TRUE))>0) {
    numbound <- 2
  } else {
    numbound <-0
  }
  
  if (cormat!="-NaN" & numbound < 1) {  #i.e., convergence criteria met
    results.from <- paste("BFTW2017_", n, "_RESULTS.R", sep = "")
    results.to <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/BFTW2017_", n, "_RESULTS.R", sep = "")##Create "Converged" folder## 
    file.copy(results.from, results.to)  #copy converged results files to a new directory
  } else {  #i.e., convergence criteria not met
    notconverge <- append(notconverge,n)  #save nsim to remove from SSB, R results files
  }
}
length(notconverge)  #gives number of runs that DID NOT converge
100-(length(notconverge))/nsims*100  #gives % run that DID converge
notconverge
SSB.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/W_SSB_data.csv", sep = "")
SSB.result <- read.csv(SSB.flnm, header = TRUE)  #read in all SSB results
SSB.result <- SSB.result[1:42,-1]  #remove 1st col (years)
SSB.converge <- SSB.result[,-notconverge]  #remove SSB results from runs that didn't converge
SSB.write <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/W_SSB_data_converge.csv", sep = "")
write.csv(SSB.converge, SSB.write)  #write a new SSB results file only converged runs

SSB.bias.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/W_SSB_bias_data.csv", sep = "")
SSB.bias <- read.csv(SSB.bias.flnm, header = TRUE)  #read in all SSB results
SSB.bias <- SSB.bias[1:42,-1]  #remove 1st col (years)
SSB.bias.converge <- SSB.bias[,-notconverge]  #remove SSB results from runs that didn't converge
SSB.bias.write <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/W_SSB_bias_data_converge.csv", sep = "")
write.csv(SSB.bias.converge, SSB.bias.write)  #write a new SSB results file only converged runs

R.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/W_R_data.csv", sep = "")
R.result <- read.csv(R.flnm, header = TRUE)  #read in all R results
R.result <- R.result[1:42,-1]  #remove 1st col (years)
R.converge <- R.result[,-notconverge]  #remove R results from runs that didn't converge
R.write <- paste( "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/W_R_data_converge.csv", sep = "")
write.csv(R.converge, R.write)

R.bias.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/W_R_bias_data.csv", sep = "")
R.bias <- read.csv(R.bias.flnm, header = TRUE)  #read in all R results
R.bias <- R.bias[1:42,-1]  #remove 1st col (years)
R.bias.converge <- R.bias[,-notconverge]  #remove R results from runs that didn't converge
R.bias.write <- paste( "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/W_R_bias_data_converge.csv", sep = "")
write.csv(R.bias.converge, R.bias.write)

BFTW.100.SSB <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West - 100 Sims/W_SSB_data_converge.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.100.SSB[,-1], type = "l", col = c(rep(brewer.pal(5,"Set3"),(nsims/5)),1,1,2,4), lty = c(rep(1,nsims),1,3,1,1),
        lwd = c(rep(1,nsims),3,3,2,2), ylim=c(0, 150000), xlab = "Year", ylab = "SSB (mt)", main = "West VPA Testing - SSB",  xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 145000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model population", "operating model stock"),
       col = c(2,2,4,1,1), lty = c(1,1,1,1,3), lwd = c(1,2,2,3,3), cex = 0.7)
BFTW.500.SSB <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West - 500 Sims/Converged/W_SSB_data_converge.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.500.SSB[,-1], type = "l", col = c(rep("orchid1",(500-length(notconverge))),1,1,2,4), lty = c(rep(1,(500-length(notconverge))),1,3,1,1),
        lwd = c(rep(1,(500-length(notconverge))),3,3,2,2), ylim=c(0, 150000), xlab = "Year", ylab = "SSB (mt)", main = "",  xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 145000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model population", "operating model stock"),
       col = c("orchid1",2,4,1,1), lty = c(1,1,1,1,3), lwd = c(1,2,2,3,3), cex = 0.7)
BFTW.500.R <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West - 500 Sims/Converged/W_R_data_converge.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.500.R[,-1], type = "l", ylim=c(0, 2500000), col = c(rep("orchid1",(500-length(notconverge))),1,2,4), lty = 1,
        lwd = c(rep(1,(500-length(notconverge))),3,2,2), xlab = "Year", ylab = "Recruits (n)", main = "West", xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 2400000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model"),
       col = c("orchid1",2,4,1), lty = 1, lwd = c(1,2,2,3), cex = 0.8)

# East #
nsims <- 500
notconverge <- c()
folder <- paste("East - 500 Sims - 2")
for (n in 0:nsims) {
  wd <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Run", n, sep = "")
  setwd(wd)
  
  # VPA-2BOX.LOG file
  log.file <- as.data.frame(read.table(file = "VPA-2BOX.LOG",
                                       fill = T, col.names = 1:max(count.fields("VPA-2BOX.LOG"))))
  cormat <- log.file[(nrow(log.file)-3),4]
  # Parameter estimates file
  parest_file_name <- paste("BFTE2017_", n, ".e1", sep = "")
  parest.file <- as.data.frame(read.table(file = parest_file_name,
                                          fill = T, col.names = 1:max(count.fields(parest_file_name))))
  bounds <- parest.file[,ncol(parest.file)]
  boundsv <- as.vector(bounds)
  boundlogic <- grepl("BOUND", boundsv, fixed = TRUE)
  if (length(which(boundlogic==TRUE))>0) {
    numbound <- 2
  } else {
    numbound <-0
  }
  # Chi-square
  results_file_name <- paste("BFTE2017_", n, "_RESULTS.R", sep = "") 
  results.file <- as.data.frame(read.table(file = results_file_name,
                                           fill = T, col.names = 1:max(count.fields(results_file_name))))
  xsq <- results.file[15,4]
  
  if (cormat!="-NaN" & numbound < 1 & xsq != "-NaN") {  #i.e., convergence criteria met
    results.from <- paste("BFTE2017_", n, "_RESULTS.R", sep = "")
    results.to <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/BFTE2017_", n, "_RESULTS.R", sep = "")
    file.copy(results.from, results.to)  #copy converged results files to a new directory
  } else {  #i.e., convergence criteria not met
    notconverge <- append(notconverge,n)  #save nsim to remove from SSB, R results files
  }
}
length(notconverge)  #gives number of runs that did not converge
100-(length(notconverge))/nsims*100  #gives % run that DID converge
notconverge
E500.notconv <- notconverge
E500 <- c(1:500)
E500.conv <- E500[-E500.notconv]
SSB.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/E_SSB_data.csv", sep = "")
SSB.result <- read.csv(SSB.flnm, header = TRUE)  #read in all SSB results
SSB.result <- SSB.result[1:42,-1]  #remove 1st col (years)
SSB.converge <- SSB.result[,-notconverge]  #remove SSB results from runs that didn't converge
SSB.write <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/E_SSB_data_converge.csv", sep = "")
write.csv(SSB.converge, SSB.write)  #write a new SSB results file only converged runs

SSB.bias.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/E_SSB_bias_data.csv", sep = "")
SSB.bias <- read.csv(SSB.bias.flnm, header = TRUE)  #read in all SSB results
SSB.bias <- SSB.bias[1:42,-1]  #remove 1st col (years)
SSB.bias.converge <- SSB.bias[,-notconverge]  #remove SSB results from runs that didn't converge
SSB.bias.write <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/E_SSB_bias_data_converge.csv", sep = "")
write.csv(SSB.bias.converge, SSB.bias.write)  #write a new SSB results file only converged runs

R.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/E_R_data.csv", sep = "")
R.result <- read.csv(R.flnm, header = TRUE)  #read in all R results
R.result <- R.result[1:42,-1]  #remove 1st col (years)
R.converge <- R.result[,-notconverge]  #remove R results from runs that didn't converge
R.write <- paste( "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/E_R_data_converge.csv", sep = "")
write.csv(R.converge, R.write)

R.bias.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/E_R_bias_data.csv", sep = "")
R.bias <- read.csv(R.bias.flnm, header = TRUE)  #read in all R results
R.bias <- R.bias[1:42,-1]  #remove 1st col (years)
R.bias.converge <- R.bias[,-notconverge]  #remove R results from runs that didn't converge
R.bias.write <- paste( "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/", folder, "/Converged/E_R_bias_data_converge.csv", sep = "")
write.csv(R.bias.converge, R.bias.write)
# convergence plots #
#boxplot - ggplot2
SSB <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged/E_SSB_data_converge.csv")
SSB <- SSB[,-c(1,476:479)]
rownames(SSB) <- c(1974:2015)
#colnames(SSB) <- c(1:474,"OMP","OMS","mean","det")
colnames(SSB) <- c(1:474)
SSB.t <- t(SSB)
SSB.df <- as.data.frame(as.table(SSB.t)) #convert SSB matrix into data frame with 3 cols for easier ggplotting
colnames(SSB.df) <- c( "simulation", "year","spawningbiomass")
SSBplot <- ggplot(SSB.df, aes(year,spawningbiomass))
SSBplot + geom_boxplot(varwidth = T,fill = "plum")
#boxplot - base
SSB <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged/E_SSB_data_converge.csv")
SSB.t <- t(SSB[,2:475])
colnames(SSB.t) <- 1974:2015
SSB.t.df <- as.data.frame(SSB.t)
boxplot(SSB.t.df/1000, ylim=c(1,1100))
EpopSSB <- SSB[,476]/1000
EpopSSB.dft <- t(as.data.frame(EpopSSB))
colnames(EpopSSB.dft) <- c(1974:2015)
points(1:length(EpopSSB.dft),EpopSSB.dft[c(1974:2015)], pch = 16, col = "red")
boxplot((SSB.t/1000), ylim=c(0,1100), las = 1, col = "lightblue", ylab = "SSB ('000 tonnes)",
        xlab = "", outpch = 21, outcex = .5, outcol = "black", outbg = "lightblue", xaxs="i", yaxs="i")
points(x=1974:2015,y=EpopSSB, pch = 16, col = "red")
lines(x=1974:2015, y=EpopSSB, type = "l")
matplot(1974:2015, (SSB[,476:479]/1000), type = "b", col = 1, 
        lty = 1, lwd = 1, ylim=c(0, 1100), xlab = "", ylab = "SSB ('000 tonnes)", xaxs="i", yaxs="i",
        las = 1, add = TRUE)
axis(side=1, col = 0, mgp = c(3,0.3,1))
axis(side=2, col = 0, las = 1, at = c(200, 600, 1000), mgp = c(3,0.5,1))
box(which = "plot", col = "gray90")
grid(col = "gray90", lty = 1)










#### BIAS PLOTS ####

## EAST RECRUITMENT ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged")
E.R.biasdata <- as.data.frame(read.csv("E_R_bias_data_converge.csv", header = TRUE))
E.R.biasdata <- E.R.biasdata[1:38,2:475]
E.R.biasavg <- (rowMeans(E.R.biasdata[1:38,]))*100
E.R.biasavgdf <- as.data.frame(E.R.biasavg)
E.R.biasavgdf <- cbind(1974:2011,E.R.biasavgdf)
colnames(E.R.biasavgdf) <- c("years","vals")
E.baseR.PRB <- 
  ggplot(E.R.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-80,20,50),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2011,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-80,20)) +
  labs(y="", x="") +
  theme(#axis.title.y= element_text(family = "Times New Roman",
    #                           face = "bold",
    #                           size = 15),
    axis.title.y = element_blank(),    
    axis.text.y = element_text(family = "Times New Roman",
                               size = 20),
    axis.text.x = element_text(family = "Times New Roman",
                               size = 20,
                               margin = margin(t = 5)),
    plot.margin = unit(c(0,0,1,0), "cm"))



## EAST SSB ##
E.SSB.biasdata <- as.data.frame(read.csv("E_SSB_data_converge+popbias.csv", header = TRUE))
E.SSB.biasdata <- E.SSB.biasdata[1:42,2:475]
E.SSB.biasavg <- (rowMeans(E.SSB.biasdata))*100
E.SSB.biasavgdf <- as.data.frame(E.SSB.biasavg)
E.SSB.biasavgdf <- cbind(1974:2015,E.SSB.biasavgdf)
colnames(E.SSB.biasavgdf) <- c("years","vals")
E.baseSSB.PRB <- 
  ggplot(E.SSB.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-100,0,50),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2015,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-100, 0)) +
  labs(y="", x="") +
  theme(#axis.title.y= element_text(family = "Times New Roman",
    #                           face = "bold",
    #                           size = 15),
    axis.title.y = element_blank(),    
    axis.text.y = element_text(family = "Times New Roman",
                               size = 20),
    axis.text.x = element_text(family = "Times New Roman",
                               size = 20,
                               margin = margin(t = 5)),
    plot.margin = unit(c(0,0,1,0), "cm"))



## EAST SSB SELF-TEST ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged")
E.SSB.biasdata <- as.data.frame(read.csv("E_SSB_bias_data_converge.csv", header = TRUE))
E.SSB.biasdata <- E.SSB.biasdata[1:42,2:493]
E.SSB.biasavg <- (rowMeans(E.SSB.biasdata))*100
E.SSB.biasavgdf <- as.data.frame(E.SSB.biasavg)
E.SSB.biasavgdf <- cbind(1974:2015,E.SSB.biasavgdf)
colnames(E.SSB.biasavgdf) <- c("years","vals")
E.selfSSB.PRB <- ggplot(E.SSB.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-50,0,25),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2015,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-50, 0)) +
  labs(y="", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 15),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13))


## EAST APICAL F ##
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged"
setwd(wd)
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path = wd, pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums_e <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) #the ID numbers of runs that converged
E.all.F.ap <- array(NA, c(nyr,length(runnums_e)), dimnames = list(year = 1974:2015, run = runnums_e)) #storage for apical Fs for each year for each run

for (i in 1:length(runnums_e)) {
  result_file_name <- paste("BFTE2017_", runnums_e[i], "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:11], nrow = nyr, ncol = 10)
  F.res <- apply(F.res, c(1,2), as.numeric)
  
  for (j in 1:42) {
    E.all.F.ap[j, i] <- max(F.res[j, ])  #calculate and save apical Fs for each run
  }
}

E.F.OM <- array(NA, c(42,29),dimnames=list(year=1974:2015,age=1:29)) #Fs from OM
for (y in 1:42)
  for (a in 1:29) {
    E.F.OM[y, a] <- sum(Fa[y, a, 1:4, 4:7])  #sum over true Fs quarters and eastern zones
  }

E.F.OM.ap <- matrix(NA, nrow = 42, ncol = 1) #apical Fs from OM
for (i in 1:42) {
  E.F.OM.ap[i, 1] <- max(E.F.OM[i, ])  #take apical Fs
}

E.F.bias <- matrix(NA, nrow = nyr, ncol = length(runnums_e)) #percent relative bias of each run
for (j in 1:length(runnums_e)) 
  for (i in 1:nyr) {
    E.F.bias[i, j] <- ((E.all.F.ap[i, j] - E.F.OM.ap[i, 1])/E.F.OM.ap[i, 1]) * 100
  }

E.F.biasabs <- matrix(NA, nrow = nyr, ncol = length(runnums_e)) #absolute bias of each run
for (j in 1:length(runnums_e)) 
  for (i in 1:nyr) {
    E.F.biasabs[i, j] <- (E.all.F.ap[i, j] - E.F.OM.ap[i, 1])
  }

E.F.biasavg             <- rowMeans(E.F.bias)
E.F.biasavgdf           <- as.data.frame(cbind(1974:2015, E.F.biasavg))
colnames(E.F.biasavgdf) <- c("years", "vals")
# E.F.PRB <-
  ggplot(E.F.biasavgdf, aes(x = years, y = vals)) +
  geom_bar(stat = "identity", fill = "gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1000, 500), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1974, 2015, 10), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(y="", x="") +
  theme(
    axis.title.y = element_blank(),    
    axis.text.y = element_text(family = "Times New Roman",
                               size = 20),
    axis.text.x = element_text(family = "Times New Roman",
                               size = 20,
                               margin = margin(t = 5)),
    plot.margin = unit(c(0,0,1,0), "cm"))







## WEST RECRUITMENT ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West - 500 Sims - 2/Converged")
W.R.biasdata <- as.data.frame(read.csv("W_R_bias_data_converge.csv", header = TRUE))
W.R.biasdata <- W.R.biasdata[1:38,2:406]
W.R.biasavg <- (rowMeans(W.R.biasdata))*100
W.R.biasavgdf <- as.data.frame(W.R.biasavg)
W.R.biasavgdf <- cbind(1974:2011,W.R.biasavgdf)
colnames(W.R.biasavgdf) <- c("years","vals")

W.baseR.PRB <- 
  ggplot(W.R.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,700,350),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2011,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(0,700)) +
  labs(y="Relative bias
(%)", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 24,
                                   margin = margin(r = 10)),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20,
                                   margin = margin(t = 5)),
        plot.margin = unit(c(0,0,1,0), "cm"))



## WEST SSB ##
W.SSB.biasdata <- as.data.frame(read.csv("W_SSB_data_converge+popbias.csv", header = TRUE))
W.SSB.biasdata <- W.SSB.biasdata[1:42,2:406]
W.SSB.biasavg <- (rowMeans(W.SSB.biasdata))*100
W.SSB.biasavgdf <- as.data.frame(W.SSB.biasavg)
W.SSB.biasavgdf <- cbind(1974:2015,W.SSB.biasavgdf)
colnames(W.SSB.biasavgdf) <- c("years","vals")
W.baseSSB.PRB <- 
  ggplot(W.SSB.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,200,100),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2015,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(0,200)) +
  labs(y="Relative bias
(%)", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 24,
                                   margin = margin(r = 10)),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20,
                                   margin = margin(t = 5)),
        plot.margin = unit(c(0,0,1,0), "cm"))



## WEST APICAL F ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged")
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums_w <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
W.all.F.ap <- array(NA, c(nyr,length(runnums_w)), dimnames = list(year = 1974:2015, run = runnums_w))

for (i in 1:length(runnums_w)) {
  result_file_name <- paste("BFTW2017_", runnums_w[i], "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:17], nrow = nyr, ncol = 16)
  F.res <- apply(F.res, c(1,2), as.numeric)
  for (j in 1:42) {
    W.all.F.ap[j, i] <- max(F.res[j, ])
  }
}

W.F.OM <- array(NA, c(42,29),dimnames=list(year=1974:2015,age=1:29)) #Fs from OM
for (y in 1:42)
  for (a in 1:29) {
    W.F.OM[y, a] <- sum(Fa[y, a, 1:4, 1:3])
  }

W.F.OM.ap <- matrix(NA, nrow = 42, ncol = 1) #apical Fs from OM
for (i in 1:42) {
  W.F.OM.ap[i, ] <- max(W.F.OM[i, ])
}

W.F.bias <- matrix(NA, nrow = nyr, ncol = length(runnums_w)) #percent relative bias of each run
for (j in 1:length(runnums_w)) 
  for (i in 1:nyr) {
    W.F.bias[i, j] <- ((W.all.F.ap[i, j] - W.F.OM.ap[i, 1])/W.F.OM.ap[i, 1]) * 100
  }

W.F.biasabs <- matrix(NA, nrow = nyr, ncol = length(runnums_w)) #absolute bias of each run
for (j in 1:length(runnums_w)) 
  for (i in 1:nyr) {
    W.F.biasabs[i, j] <- (W.all.F.ap[i, j] - W.F.OM.ap[i, 1])
  }

W.F.biasavg             <- rowMeans(W.F.bias)
W.F.biasavgdf           <- as.data.frame(cbind(1974:2015, W.F.biasavg))
colnames(W.F.biasavgdf) <- c("years", "vals")
W.F.PRB <-
  ggplot(W.F.biasavgdf, aes(x = years, y = vals)) +
  geom_bar(stat = "identity", fill = "gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-100, 400, 250), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1974, 2015, 10), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-100, 400)) +
  labs(y="Relative bias
(%)", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),    
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20,
                                   margin = margin(t = 5)),
    plot.margin = unit(c(0,0,1,0), "cm"))
















#### ESTIMATION MODEL PLOTS ####

# EAST Recruitment Cross-test #
years <- matrix(1974:2011,nrow = 38,ncol=1)
E.R.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged/E_R_data_converge.csv", header = TRUE)
E.R.data[1:38,1] <- years
E.R.sims <- as.data.frame(E.R.data[1:38,1:475]) #create data frame of simulations data
colnames(E.R.sims) <- c("years",1:474)
E.R.om <- as.data.frame(E.R.data[1:38,c(1,476)]) #create data frame of operating model data
colnames(E.R.om) <- c("years","OM")
E.R.sims_1 <- melt(E.R.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(E.R.sims_1$years,E.R.om$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
E.R.sims_1$years <- factor(E.R.sims_1$years,levels=(allLevels))
E.R.om$years <- factor(E.R.om$years,levels=(allLevels))

# b&W
# E.R.plot <-
  ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y = "", x = "", title = "East") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,8000000,4000000)) +
  coord_cartesian(ylim = c(0,8000000)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        #axis.title.y = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 24),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20))


# color
grob1 <- grobTree(textGrob("B", x = 0.05, y = 0.95, hjust = 0,
                             gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
  
E.R.plot <-
  ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1,aes(x=years, y=value), color="lightblue4",fill="lightblue1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y = "", x = "", title = "East") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,8000000,4000000)) +
  coord_cartesian(ylim = c(0,8000000)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        #axis.title.y = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 24),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 20),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  annotation_custom(grob1)


# EAST SSB Cross-test #
years <- matrix(1974:2015,nrow = 42,ncol=1)
E.SSB.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged/E_SSB_data_converge.csv", header = TRUE)
E.SSB.data[,1] <- years
E.SSB.sims <- as.data.frame(E.SSB.data[1:42,1:475]) #create data frame of simulations data
colnames(E.SSB.sims) <- c("years",1:474)
E.SSB.omp <- as.data.frame(E.SSB.data[,c(1,476)]) #create data frame of operating model data
colnames(E.SSB.omp) <- c("years","OMP")
E.SSB.oms <- as.data.frame(E.SSB.data[,c(1,477)])
colnames(E.SSB.oms) <- c("years","OMS")
E.SSB.sims_1 <- melt(E.SSB.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(E.SSB.sims_1$years,E.SSB.omp$years,E.SSB.oms$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
E.SSB.sims_1$years <- factor(E.SSB.sims_1$years,levels=(allLevels))
E.SSB.omp$years <- factor(E.SSB.omp$years,levels=(allLevels))
E.SSB.oms$years <- factor(E.SSB.oms$years,levels=(allLevels))

# b&w
# E.SSB.plot <- 
  ggplot(data=E.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1,aes(x=years, y=value), color="black",fill="gray80") +
  geom_line(data=E.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1500000,500000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,1500000)) +
  theme(
    #axis.title.y = element_text(family = "Times New Roman",
    #                               face = "bold",
    #                              size = 14),
    #axis.text.x = element_text(family = "Times New Roman",
    #                           size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 20))

# color
grob2 <- grobTree(textGrob("D", x = 0.05, y = 0.95, hjust = 0,
                            gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
  
E.SSB.plot <-
  ggplot(data=E.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1,aes(x=years, y=value), color="lightblue4",fill="lightblue1") +
  geom_line(data=E.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1500000,500000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,1500000)) +
  theme(
    #axis.title.y = element_text(family = "Times New Roman",
    #                               face = "bold",
    #                              size = 14),
    axis.text.x = element_text(family = "Times New Roman",
                              size = 20),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 20)) +
  annotation_custom(grob2)



# EAST SSB Self-Test #
E.SSB.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged/E_SSB_data_converge.csv", header = TRUE)
E.SSB.data[,1] <- years
E.SSB.sims <- as.data.frame(E.SSB.data[1:42,1:493]) #create data frame of simulations data
colnames(E.SSB.sims) <- c("years",1:492)
E.SSB.omp <- as.data.frame(E.SSB.data[,c(1,494)]) #create data frame of operating model data
colnames(E.SSB.omp) <- c("years","OMP")
E.SSB.oms <- as.data.frame(E.SSB.data[,c(1,495)])
colnames(E.SSB.oms) <- c("years","OMS")
E.SSB.sims_1 <- melt(E.SSB.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(E.SSB.sims_1$years,E.SSB.omp$years,E.SSB.oms$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
E.SSB.sims_1$years <- factor(E.SSB.sims_1$years,levels=(allLevels))
E.SSB.omp$years <- factor(E.SSB.omp$years,levels=(allLevels))
E.SSB.oms$years <- factor(E.SSB.oms$years,levels=(allLevels))

E.SSB.plot <- ggplot(data=E.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1,aes(x=years, y=value), color="black",fill="gray80") +
  geom_line(data=E.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1500000,500000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,1500000)) +
  theme(
    #axis.title.y = element_text(family = "Times New Roman",
    #                               face = "bold",
    #                              size = 14),
    #axis.text.x = element_text(family = "Times New Roman",
    #                           size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 13))


## EAST APICAL F ##
E.F.ap     <- cbind(1974:2015, E.all.F.ap) %>% 
  as.data.frame()
E.F.ap.OM  <- cbind(1974:2015, E.F.OM.ap) %>%
  as.data.frame()

colnames(E.F.ap)    <- c("years", runnums_e)
colnames(E.F.ap.OM) <- c("years", "OM")
E.F.ap.gg             <- melt(E.F.ap, id.vars = "years")

allLevels        <- levels(factor(c(E.F.ap.gg$years, E.F.ap.OM$years)))
E.F.ap.gg$years  <- factor(E.F.ap.gg$years, levels = (allLevels))
E.F.ap.OM$years  <- factor(E.F.ap.OM$years, levels = (allLevels))

# b&w
# E.F.plot <-
  ggplot(data = E.F.ap.OM, aes(x = years, y = OM)) +
  geom_boxplot(data = E.F.ap.gg, aes(x = years, y = value), color = "black", fill = "gray80") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5, color = "black") +
  labs(y = "Fishing mortality rate", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 3, 1)) +
  coord_cartesian(ylim = c(0,3)) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20))

# color
grob3 <- grobTree(textGrob("F", x = 0.05, y = 0.95, hjust = 0,
                            gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
  
E.F.plot <-
  ggplot(data = E.F.ap.OM, aes(x = years, y = OM)) +
  geom_boxplot(data = E.F.ap.gg, aes(x = years, y = value), color = "lightblue4", fill = "lightblue1") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5, color = "black") +
  labs(y = "Fishing mortality rate", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 3, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,3)) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  annotation_custom(grob3)




## WEST Recruitment ##
years <- matrix(1974:2011,nrow = 38,ncol=1)
W.R.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged/W_R_data_converge.csv", header = TRUE)
W.R.data[1:38,1] <- years
W.R.sims <- as.data.frame(W.R.data[1:38,1:406]) #create data frame of simulations data
colnames(W.R.sims) <- c("years",1:405)
W.R.om <- as.data.frame(W.R.data[1:38,c(1,407)]) #create data frame of operating model data
colnames(W.R.om) <- c("years","OM")
W.R.sims_1 <- melt(W.R.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(W.R.sims_1$years,W.R.om$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
W.R.sims_1$years <- factor(W.R.sims_1$years,levels=(allLevels))
W.R.om$years <- factor(W.R.om$years,levels=(allLevels))

# b&w
# W.R.plot <- 
  ggplot(data=W.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y = "Recruitment 
(numbers)", x = "", title = "West") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  coord_cartesian(ylim = c(0,2000000)) +
  theme(plot.title = element_text(family = "Times New Roman", 
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold",
                                    margin = margin(r = 10)),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),    
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  scale_y_continuous(breaks = seq(0,2000000,1000000), label=scientific_format(digits = 2))

# color
grob4 <- grobTree(textGrob("A", x = 0.05, y = 0.95, hjust = 0,
                            gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
  
W.R.plot <- 
  ggplot(data=W.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="lightblue4",fill="lightblue1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y = "Recruitment 
(numbers)", x = "", title = "West") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  coord_cartesian(ylim = c(0,2000000)) +
  theme(plot.title = element_text(family = "Times New Roman", 
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold",
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 20),
        # axis.text.x = element_blank(),    
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  scale_y_continuous(breaks = seq(0,2000000,1000000), label=scientific_format(digits = 2)) +
  annotation_custom(grob4)


# WEST SSB #
years <- matrix(1974:2015,nrow = 42,ncol=1)
W.SSB.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged/W_SSB_data_converge.csv", header = TRUE)
W.SSB.data[,1] <- years
W.SSB.sims <- as.data.frame(W.SSB.data[1:42,1:406]) #create data frame of simulations data
colnames(W.SSB.sims) <- c("years",1:405)
W.SSB.omp <- as.data.frame(W.SSB.data[,c(1,407)]) #create data frame of operating model data
colnames(W.SSB.omp) <- c("years","OMP")
W.SSB.oms <- as.data.frame(W.SSB.data[,c(1,408)])
colnames(W.SSB.oms) <- c("years","OMS")
W.SSB.sims_1 <- melt(W.SSB.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(W.SSB.sims_1$years,W.SSB.omp$years,W.SSB.oms$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
W.SSB.sims_1$years <- factor(W.SSB.sims_1$years,levels=(allLevels))
W.SSB.omp$years <- factor(W.SSB.omp$years,levels=(allLevels))
W.SSB.oms$years <- factor(W.SSB.oms$years,levels=(allLevels))

# b&w
# W.SSB.plot <- 
  ggplot(data=W.SSB.omp, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1,aes(x=years, y=value), color="black",fill="gray80") +
  geom_line(data=W.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y = "SSB
(tonnes)", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,200000,100000)) +
  coord_cartesian(ylim = c(0,200000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20))

# color
grob5 <- grobTree(textGrob("C", x = 0.05, y = 0.95, hjust = 0,
                            gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
  
W.SSB.plot <- 
  ggplot(data=W.SSB.omp, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1,aes(x=years, y=value), color="lightblue4",fill="lightblue1") +
  geom_line(data=W.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y = "SSB
(tonnes)", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,200000,100000)) +
  coord_cartesian(ylim = c(0,200000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 20),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  annotation_custom(grob5)


## WEST APICAL F ##
W.F.ap     <- cbind(1974:2015, W.all.F.ap) %>% 
  as.data.frame()
W.F.ap.OM  <- cbind(1974:2015, W.F.OM.ap) %>%
  as.data.frame()

colnames(W.F.ap)    <- c("years", runnums_w)
colnames(W.F.ap.OM) <- c("years", "OM")
W.F.ap.gg             <- melt(W.F.ap, id.vars = "years")

allLevels        <- levels(factor(c(W.F.ap.gg$years, W.F.ap.OM$years)))
W.F.ap.gg$years  <- factor(W.F.ap.gg$years, levels = (allLevels))
W.F.ap.OM$years  <- factor(W.F.ap.OM$years, levels = (allLevels))

# b&w
# W.F.plot <-
  ggplot(data = W.F.ap.OM, aes(x = years, y = OM)) +
  geom_boxplot(data = W.F.ap.gg, aes(x = years, y = value), color = "black", fill = "gray80") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5, color = "black") +
  labs(y = "Fishing mortality 
rate", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 2, 1)) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20))

# color
grob6 <- grobTree(textGrob("E", x = 0.05, y = 0.95, hjust = 0,
                            gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
  
W.F.plot <-
  ggplot(data = W.F.ap.OM, aes(x = years, y = OM)) +
  geom_boxplot(data = W.F.ap.gg, aes(x = years, y = value), color = "lightblue4", fill = "lightblue1") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5, color = "black") +
  labs(y = "Fishing mortality 
rate", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 2, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  annotation_custom(grob6)






# w/ relative bias
grid.arrange(W.R.plot, E.R.plot,
             W.baseR.PRB, E.baseR.PRB,
             W.SSB.plot, E.SSB.plot,
             W.baseSSB.PRB, E.baseSSB.PRB,
             nrow = 4, ncol = 2,
             heights=c(5,3,5,3))
# w/o relative bias
grid.arrange(W.R.plot, E.R.plot,
             W.SSB.plot, E.SSB.plot,
             W.F.plot, E.F.plot,
             nrow = 3, ncol = 2)

jpeg("C:/Users/mmorse1/Documents/Publishing/Revisions - Bluefin Tuna Simulations/ICES JMS Review/Figures/EMplots.jpeg",
     width = 4000, height = 4200, units = "px", quality = 100, res = 300)
# w/ relative bias
# plot_grid(W.R.plot, E.R.plot,
#           W.baseR.PRB, E.baseR.PRB,
#           W.SSB.plot, E.SSB.plot,
#           W.baseSSB.PRB, E.baseSSB.PRB,
#           ncol = 2, nrow = 4, align = "v", rel_heights = c(7,3,7,3))
# w/o relative bias
plot_grid(W.R.plot, E.R.plot,
          W.SSB.plot, E.SSB.plot,
          W.F.plot, E.F.plot,
          ncol = 2, nrow = 3, align = "v")
dev.off()





#### SELF-TEST PLOTS ####

# SSB boxplots
years <- matrix(1974:2015,nrow = 42,ncol=1)
E.SSB.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged/E_SSB_data_converge.csv", header = TRUE)
E.SSB.data[,1] <- years
E.SSB.sims <- as.data.frame(E.SSB.data[1:42,1:493]) #create data frame of simulations data
colnames(E.SSB.sims) <- c("years",1:492)
E.SSB.omp <- as.data.frame(E.SSB.data[,c(1,494)]) #create data frame of operating model data
colnames(E.SSB.omp) <- c("years","OMP")
E.SSB.oms <- as.data.frame(E.SSB.data[,c(1,495)])
colnames(E.SSB.oms) <- c("years","OMS")
E.SSB.sims_1 <- melt(E.SSB.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(E.SSB.sims_1$years,E.SSB.omp$years,E.SSB.oms$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
E.SSB.sims_1$years <- factor(E.SSB.sims_1$years,levels=(allLevels))
E.SSB.omp$years <- factor(E.SSB.omp$years,levels=(allLevels))
E.SSB.oms$years <- factor(E.SSB.oms$years,levels=(allLevels))

# boxplot (grayscale)
# E.selfSSB.plot <- 
  ggplot(data=E.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1,aes(x=years, y=value), color="black",fill="gray80") +
  geom_line(data=E.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1500000,500000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,1500000)) +
  theme(axis.title.y = element_blank(),
        # axis.title.y = element_text(family = "Times New Roman",
        #                             face = "bold",
        #                             size = 24,
        #                             margin = margin(r = 10)),
        # axis.text.x = element_text(family = "Times New Roman",
        #                             size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22))

# color
grob7 <- grobTree(textGrob("D", x = 0.05, y = 0.95, hjust = 0,
                             gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
  
E.selfSSB.plot <- 
  ggplot(data=E.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1,aes(x=years, y=value), color = "lightblue4", fill = "lightblue1") +
  geom_line(data=E.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1500000,500000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,1500000)) +
  theme(axis.title.y = element_blank(),
        # axis.title.y = element_text(family = "Times New Roman",
        #                             face = "bold",
        #                             size = 24,
        #                             margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                    size = 20),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22)) +
  annotation_custom(grob7)




# SSB bias barplot
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged")
E.SSB.biasdata <- as.data.frame(read.csv("E_SSB_bias_data_converge.csv", header = TRUE))
E.SSB.biasdata <- E.SSB.biasdata[1:42,2:493]
E.SSB.biasavg <- (rowMeans(E.SSB.biasdata))*100
E.SSB.biasavgdf <- as.data.frame(E.SSB.biasavg)
E.SSB.biasavgdf <- cbind(1974:2015,E.SSB.biasavgdf)
colnames(E.SSB.biasavgdf) <- c("years","vals")
E.selfSSB.PRB <- ggplot(E.SSB.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-50,0,25),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2015,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-50, 0)) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        # axis.title.y = element_text(family = "Times New Roman",
        #                             face = "bold",
        #                             size = 24,
        #                             margin = margin(r = 28)),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   margin = margin(t = 10)))
plot_grid(E.SSB.plot, E.selfSSB.PRB, ncol = 1, nrow = 2, align = "v", rel_heights = c(7,3))

# R boxplots
years <- matrix(1974:2011,nrow = 38,ncol=1)
E.R.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged/E_R_data_converge.csv", header = TRUE)
E.R.data[1:38,1] <- years
E.R.sims <- as.data.frame(E.R.data[1:38,1:493]) #create data frame of simulations data
colnames(E.R.sims) <- c("years",1:492)
E.R.om <- as.data.frame(E.R.data[1:38,c(1,494)]) #create data frame of operating model data
colnames(E.R.om) <- c("years","OM")
E.R.sims_1 <- melt(E.R.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(E.R.sims_1$years,E.R.om$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
E.R.sims_1$years <- factor(E.R.sims_1$years,levels=(allLevels))
E.R.om$years <- factor(E.R.om$years,levels=(allLevels))

# grayscale
# E.selfR.plot <- 
  ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="", title = "East") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,8000000,4000000)) +
  coord_cartesian(ylim = c(0,8000000)) +
  theme(
        # axis.title.y = element_text(family = "Times New Roman",
        #                             face = "bold",
        #                             size = 24,
        #                             margin = margin(r = 10)),
        # axis.text.x = element_text(family = "Times New Roman",
        #                             size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        plot.title = element_text(family = "Times New Roman", 
                                  face = "bold", 
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)))

# color
grob8 <- grobTree(textGrob("B", x = 0.05, y = 0.95, hjust = 0,
                             gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))

E.selfR.plot <- 
  ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1,aes(x=years, y=value), color="lightblue4",fill="lightblue1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="", title = "East") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,8000000,4000000)) +
  coord_cartesian(ylim = c(0,8000000)) +
  theme(
    # axis.title.y = element_text(family = "Times New Roman",
    #                             face = "bold",
    #                             size = 24,
    #                             margin = margin(r = 10)),
    axis.text.x = element_text(family = "Times New Roman",
                                size = 20),
    axis.title.y = element_blank(),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 22),
    plot.title = element_text(family = "Times New Roman", 
                              face = "bold", 
                              size = 24,
                              hjust = 0.5,
                              margin = margin(b = 10))) +
  annotation_custom(grob8)

# R bias barplot
E.R.biasdata <- as.data.frame(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged/E_R_bias_data_converge.csv", header = TRUE))
E.R.biasdata <- E.R.biasdata[1:38,2:493]
E.R.biasavg <- (rowMeans(E.R.biasdata[1:38,]))*100
E.R.biasavgdf <- as.data.frame(E.R.biasavg)
E.R.biasavgdf <- cbind(1974:2011,E.R.biasavgdf)
colnames(E.R.biasavgdf) <- c("years","vals")
E.selfR.PRB <- ggplot(E.R.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-30,50,40),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2011,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-30,50)) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        # axis.title.y= element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 24,
        #                            margin = margin(r = 10)),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   margin = margin(t = 10)))

# F boxplot

Fa <- as.matrix(read.csv("C:/Users/mmorse1/Documents/Simulations_selftest/OM Output/Fa.csv", header = T))[, -1]
Fa <- array(Fa, c(42, 29, 2), dimnames = list(year = 1974:2015, age = 1:29, unit = 1:2))

#East OM (end at age 10)
F.OM.E <- array(NA, c(42,10),dimnames=list(year=1974:2015,age=1:10))
for (y in 1:42)
  for (a in 1:10) {
    F.OM.E[y,a] <- sum(Fa[y,a,1])
  }
#OM apical F
F.OM.ap <- matrix(NA, nrow = 42, ncol = 1)
for (i in 1:42) {
  F.OM.ap[i, ] <- max(F.OM.E[i, ])
}
# Compare apical F from stochastic runs to the OM
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)
all.F.ap <- array(NA, c(nyr,length(runnums)), dimnames = list(year = 1974:2015, run = runnums))
for (i in 1:length(runnums)) {
  result_file_name <- paste("BFTE2017_", runnums[i], "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:11], nrow = nyr, ncol = nageE)
  F.res <- apply(F.res, c(1,2), as.numeric)
  for (j in 1:42) {
    all.F.ap[j, i] <- max(F.res[j, ])
  }
}
F.ap     <- cbind(1974:2015,all.F.ap) %>% 
  as.data.frame()
F.ap.OM  <- cbind(1974:2015, F.OM.ap) %>%
  as.data.frame()
colnames(F.ap)    <- c("years", runnums)
colnames(F.ap.OM) <- c("years", "OM")
F.ap.gg <- melt(F.ap, id.vars = "years")
allLevels <- levels(factor(c(F.ap.gg$years, F.ap.OM$years)))
F.ap.gg$years  <- factor(F.ap.gg$years,levels=(allLevels))
F.ap.OM$years  <- factor(F.ap.OM$years,levels=(allLevels))

# grayscale
# E.F.self.plot <-
  ggplot(data=F.ap.OM, aes(x=years, y=OM)) +
  geom_boxplot(data=F.ap.gg,aes(x=years, y=value), color="black", fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5, color="black") +
  labs(y = "", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 3, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,3)) +
  theme(axis.title.y = element_blank(),
        # axis.title.y = element_text(family = "Times New Roman",
        #                                 face = "bold",
        #                                 size = 24),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 20),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22))

# color
grob9 <- grobTree(textGrob("F", x = 0.05, y = 0.95, hjust = 0,
                           gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
E.F.self.plot <-
  ggplot(data=F.ap.OM, aes(x=years, y=OM)) +
  geom_boxplot(data=F.ap.gg,aes(x=years, y=value), color="lightblue4", fill="lightblue1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5, color="black") +
  labs(y = "", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 3, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,3)) +
  theme(axis.title.y = element_blank(),
        # axis.title.y = element_text(family = "Times New Roman",
        #                                 face = "bold",
        #                                 size = 24),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 20),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22)) +
  annotation_custom(grob9)

# F bias barplot
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged")
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums_e <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) #the ID numbers of runs that converged
E.all.F.ap <- array(NA, c(nyr,length(runnums_e)), dimnames = list(year = 1974:2015, run = runnums_e)) #storage for apical Fs for each year for each run
for (i in 1:length(runnums_e)) {
  result_file_name <- paste("BFTE2017_", runnums_e[i], "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:11], nrow = nyr, ncol = nageE)
  F.res <- apply(F.res, c(1,2), as.numeric)
  for (j in 1:42) {
    E.all.F.ap[j, i] <- max(F.res[j, ])
  }
}
E.F.bias <- matrix(NA, nrow = nyr, ncol = length(runnums_e)) #percent relative bias of each run
for (j in 1:length(runnums_e)) 
  for (i in 1:nyr) {
    E.F.bias[i, j] <- ((E.all.F.ap[i, j] - F.OM.ap[i, 1])/F.OM.ap[i, 1]) * 100
  }
E.F.biasabs <- matrix(NA, nrow = nyr, ncol = length(runnums_e)) #absolute bias of each run
for (j in 1:length(runnums_e)) 
  for (i in 1:nyr) {
    E.F.biasabs[i, j] <- (E.all.F.ap[i, j] - F.OM.ap[i, 1])
  }
E.F.biasavg             <- rowMeans(E.F.bias)
E.F.biasavgdf           <- as.data.frame(cbind(1974:2015, E.F.biasavg))
colnames(E.F.biasavgdf) <- c("years", "vals")
E.selfF.PRB <-
  ggplot(E.F.biasavgdf, aes(x = years, y = vals)) +
  geom_bar(stat = "identity", fill = "gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1000, 500), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1974, 2015, 10), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),    
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   margin = margin(t = 10))#,
        #plot.margin = unit(c(0,0,1,0), "cm")
        )




## WEST ##
# SSB boxplots
years <- matrix(1974:2015,nrow = 42,ncol=1)
W.SSB.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged/W_SSB_data_converge.csv", header = TRUE)
W.SSB.data[,1] <- years
W.SSB.sims <- as.data.frame(W.SSB.data[1:42,1:405]) #create data frame of simulations data
colnames(W.SSB.sims) <- c("years",1:404)
W.SSB.omp <- as.data.frame(W.SSB.data[,c(1,406)]) #create data frame of operating model data
colnames(W.SSB.omp) <- c("years","OMP")
W.SSB.oms <- as.data.frame(W.SSB.data[,c(1,407)])
colnames(W.SSB.oms) <- c("years","OMS")
W.SSB.sims_1 <- melt(W.SSB.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(W.SSB.sims_1$years,W.SSB.omp$years,W.SSB.oms$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
W.SSB.sims_1$years <- factor(W.SSB.sims_1$years,levels=(allLevels))
W.SSB.omp$years <- factor(W.SSB.omp$years,levels=(allLevels))
W.SSB.oms$years <- factor(W.SSB.oms$years,levels=(allLevels))
# boxplot (grayscale)
W.selfSSB.plot <- ggplot(data=W.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1,aes(x=years, y=value), color="black",fill="gray80") +
  geom_line(data=W.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="SSB
(tonnes)",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,80000,40000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,80000)) +
  theme(
    axis.title.y = element_text(family = "Times New Roman",
                                face = "bold",
                                size = 24,
                                margin = margin(r = 10)),
    #axis.text.x = element_text(family = "Times New Roman",
    #                           size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 22))

# color
grob10 <- grobTree(textGrob("C", x = 0.05, y = 0.95, hjust = 0,
                           gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))
W.selfSSB.plot <- 
  ggplot(data=W.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1,aes(x=years, y=value), color="lightblue4",fill="lightblue1") +
  geom_line(data=W.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="SSB
(tonnes)",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,80000,40000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,80000)) +
  theme(
    axis.title.y = element_text(family = "Times New Roman",
                                face = "bold",
                                size = 24,
                                margin = margin(r = 10)),
    axis.text.x = element_text(family = "Times New Roman",
                              size = 20),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 22)) +
  annotation_custom(grob10)


# SSB bias barplot
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged")
W.SSB.biasdata <- as.data.frame(read.csv("W_SSB_bias_data_converge.csv", header = TRUE))
W.SSB.biasdata <- W.SSB.biasdata[1:42,2:405]
W.SSB.biasavg <- (rowMeans(W.SSB.biasdata))*100
W.SSB.biasavgdf <- as.data.frame(W.SSB.biasavg)
W.SSB.biasavgdf <- cbind(1974:2015,W.SSB.biasavgdf)
colnames(W.SSB.biasavgdf) <- c("years","vals")
W.selfSSB.PRB <- ggplot(W.SSB.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-10,20,10),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2015,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-10, 20)) +
  labs(y="Relative bias
(%)", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 24),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   margin = margin(t = 10)))
plot_grid(E.SSB.plot, E.selfSSB.PRB, ncol = 1, nrow = 2, align = "v", rel_heights = c(7,3))

# R boxplots
years <- matrix(1974:2011,nrow = 38,ncol=1)
W.R.data <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged/W_R_data_converge.csv", header = TRUE)
W.R.data[1:38,1] <- years
W.R.sims <- as.data.frame(W.R.data[1:38,1:405]) #create data frame of simulations data
colnames(W.R.sims) <- c("years",1:404)
W.R.om <- as.data.frame(W.R.data[1:38,c(1,406)]) #create data frame of operating model data
colnames(W.R.om) <- c("years","OM")
W.R.sims_1 <- melt(W.R.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(W.R.sims_1$years,W.R.om$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
W.R.sims_1$years <- factor(W.R.sims_1$years,levels=(allLevels))
W.R.om$years <- factor(W.R.om$years,levels=(allLevels))
# grayscale
W.selfR.plot <- ggplot(data=W.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="Recruitment
(numbers)",x="", title = "West") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,2000000,1000000)) +
  coord_cartesian(ylim = c(0,2000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        plot.title = element_text(family = "Times New Roman", 
                                  face = "bold", 
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)))

# color
grob11 <- grobTree(textGrob("A", x = 0.05, y = 0.95, hjust = 0,
                            gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))

W.selfR.plot <- 
  ggplot(data=W.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="lightblue4",fill="lightblue1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="Recruitment
(numbers)",x="", title = "West") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,2000000,1000000)) +
  coord_cartesian(ylim = c(0,2000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 20),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        plot.title = element_text(family = "Times New Roman", 
                                  face = "bold", 
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10))) +
  annotation_custom(grob11)

# R bias barplot
W.R.biasdata <- as.data.frame(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged/W_R_bias_data_converge.csv", header = TRUE))
W.R.biasdata <- W.R.biasdata[1:38,2:404]
W.R.biasavg <- (rowMeans(W.R.biasdata[1:38,]))*100
W.R.biasavgdf <- as.data.frame(W.R.biasavg)
W.R.biasavgdf <- cbind(1974:2011,W.R.biasavgdf)
colnames(W.R.biasavgdf) <- c("years","vals")
W.selfR.PRB <- ggplot(W.R.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,70,35),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2011,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(0,70)) +
  labs(y="Relative bias
(%)", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 24),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   margin = margin(t = 10)))

# F boxplot
#West OM (end at age 16)
F.OM.W <- array(NA, c(42,16),dimnames=list(year=1974:2015,age=1:16))
for (y in 1:42)
  for (a in 1:16) {
    F.OM.W[y,a] <- sum(Fa[y,a,2])
  }
#OM apical F
F.OM.W.ap <- matrix(NA, nrow = 42, ncol = 1)
for (y in 1:42) {
  F.OM.W.ap[y, ] <- max(F.OM.W[y, ])
}
# Compare apical F from stochastic runs to the OM
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)
all.F.ap <- array(NA, c(nyr,length(runnums)), dimnames = list(year = 1974:2015, run = runnums))
for (i in 1:length(runnums)) {
  result_file_name <- paste("BFTW2017_", runnums[i], "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:17], nrow = nyr, ncol = nageW)
  F.res <- apply(F.res, c(1,2), as.numeric)
  for (j in 1:42) {
    all.F.ap[j, i] <- max(F.res[j, ])
  }
}
F.ap.W     <- cbind(1974:2015,all.F.ap) %>% 
  as.data.frame()
F.ap.W.OM  <- cbind(1974:2015,F.OM.W.ap) %>%
  as.data.frame()
colnames(F.ap.W)    <- c("years", runnums)
colnames(F.ap.W.OM) <- c("years", "OM")
F.ap.W.gg <- melt(F.ap.W, id.vars = "years")
allLevels <- levels(factor(c(F.ap.W.gg$years, F.ap.W.OM$years)))
F.ap.W.gg$years  <- factor(F.ap.W.gg$years,levels=(allLevels))
F.ap.W.OM$years  <- factor(F.ap.W.OM$years,levels=(allLevels))
#plot grayscale
W.F.self.plot <-
  ggplot(data=F.ap.W.OM, aes(x=years, y=OM)) +
  geom_boxplot(data=F.ap.W.gg,aes(x=years, y=value), color="black", fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5, color="black") +
  labs(y = "Fishing mortality
rate", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 2, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 20),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22))

# color
grob12 <- grobTree(textGrob("E", x = 0.05, y = 0.95, hjust = 0,
                            gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2)))

W.F.self.plot <-
  ggplot(data=F.ap.W.OM, aes(x=years, y=OM)) +
  geom_boxplot(data=F.ap.W.gg,aes(x=years, y=value), color="lightblue4", fill="lightblue1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5, color="black") +
  labs(y = "Fishing mortality
rate", x = "") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 2, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 20),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22)) +
  annotation_custom(grob12)

# F bias barplot
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged")
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums_w <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) #the ID numbers of runs that converged
W.all.F.ap <- array(NA, c(nyr,length(runnums_w)), dimnames = list(year = 1974:2015, run = runnums_w)) #storage for apical Fs for each year for each run
for (i in 1:length(runnums_w)) {
  result_file_name <- paste("BFTW2017_", runnums_w[i], "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:17], nrow = nyr, ncol = nageW)
  F.res <- apply(F.res, c(1,2), as.numeric)
  for (j in 1:42) {
    W.all.F.ap[j, i] <- max(F.res[j, ])
  }
}
W.F.bias <- matrix(NA, nrow = nyr, ncol = length(runnums_w)) #percent relative bias of each run
for (j in 1:length(runnums_w)) 
  for (i in 1:nyr) {
    W.F.bias[i, j] <- ((W.all.F.ap[i, j] - F.OM.W.ap[i, 1])/F.OM.W.ap[i, 1]) * 100
  }
W.F.biasabs <- matrix(NA, nrow = nyr, ncol = length(runnums_w)) #absolute bias of each run
for (j in 1:length(runnums_w)) 
  for (i in 1:nyr) {
    W.F.biasabs[i, j] <- (W.all.F.ap[i, j] - F.OM.W.ap[i, 1])
  }
W.F.biasavg             <- rowMeans(W.F.bias)
W.F.biasavgdf           <- as.data.frame(cbind(1974:2015, W.F.biasavg))
colnames(W.F.biasavgdf) <- c("years", "vals")
W.selfF.PRB <-
  ggplot(W.F.biasavgdf, aes(x = years, y = vals)) +
  geom_bar(stat = "identity", fill = "gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 400, 200), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1974, 2015, 10), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 400)) +
  labs(y="Relative bias
(%)", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),    
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   margin = margin(t = 10))#,
        #plot.margin = unit(c(0,0,1,0), "cm")
        )




## Layouts ##
plot_grid(E.selfR.plot, E.selfR.PRB, ncol = 1, nrow = 2, align = "v", rel_heights = c(7,3))
grid.arrange(E.selfR.plot, E.SSB.plot)
plot_grid(E.selfR.plot, E.SSB.plot, nrow  = 2, ncol = 1, align = "v")

#EAST only
jpeg("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Publishing/CJFAS - Bluefin Tuna Simulations/Figures/selftestplots.jpeg",
     width = 1400, height = 800, units = "px", quality = 100)
grid.arrange(E.selfR.plot, E.selfSSB.plot,
             E.selfR.PRB, E.selfSSB.PRB,
             ncol = 2,
             heights = c(7,3))
dev.off()

#WEST only
jpeg("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/Wselftestplots.jpeg",
     width = 550, height = 1200, units = "px", quality = 100)
plot_grid(W.selfR.plot, W.selfR.PRB, W.selfSSB.plot, 
          W.selfSSB.PRB, W.F.self.plot, W.selfF.PRB,
          ncol = 1, nrow = 6, align = "v",
          rel_heights = c(7,3,7,3,7,3))
dev.off()

#EAST & WEST
jpeg("C:/Users/mmorse1/Documents/Publishing/Revisions - Bluefin Tuna Simulations/ICES JMS Review/Figures/selftestplots_EW.jpeg",
      width = 4000, height = 4200, units = "px", quality = 100, res = 300)
# w/bias plots
# plot_grid(W.selfR.plot,   E.selfR.plot,
#           W.selfR.PRB,    E.selfR.PRB,
#           W.selfSSB.plot, E.selfSSB.plot,
#           W.selfSSB.PRB,  E.selfSSB.PRB,
#           W.F.self.plot,  E.F.self.plot,
#           W.selfF.PRB,    E.selfF.PRB,
#           ncol = 2, nrow = 6, align = "v",
#           rel_heights = c(7,3,7,3,7,3))
#w/o bias plots
plot_grid(W.selfR.plot,   E.selfR.plot,
          W.selfSSB.plot, E.selfSSB.plot,
          W.F.self.plot,  E.F.self.plot,
          ncol = 2, nrow = 3, align = "v")
dev.off()



#### EAST SELECTIVITIES ####

# Pull out selectivities for each index modeled for each run of the VPA simulations
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)
# Set up objects for storing selectivities of each index
MOR.SP.TP <- matrix(NA, nrow = 5, ncol = 1)
#MOR.POR.TP <- c(NA)  #all selectivities =1 for age 10 only; no need to collate all
JPN.LL.EastMed <- matrix(NA, nrow = 5, ncol = 1)
JPN.LL1.NEA <- matrix(NA, nrow = 7, ncol = 1)
JPN.LL2.NEA <- matrix(NA, nrow = 7, ncol = 1)
SP.BB1 <- matrix(NA, nrow = 2, ncol = 1)
SP.BB2 <- matrix(NA, nrow = 4, ncol = 1)
FR.AER1 <- matrix(NA, nrow = 3, ncol = 1)
FR.AER2 <- matrix(NA, nrow = 3, ncol = 1)
WMED.LARV <- matrix(NA, nrow = 8, ncol = 1)

# Pull out selectivities from each VPA simulation
for (i in runnums) 
{
  result.filename <- paste("BFTE2017_", i, "_RESULTS.R", sep="")
  
  result.file <- as.data.frame(read.table(file = result.filename,
                                          fill = T, col.names = 1:max(count.fields(
                                            result.filename
                                          ))))
  
  MOR.SP.TP <- cbind(MOR.SP.TP, t(apply(result.file[266, 2:6], c(1,2), as.numeric)))
  #MOR.POR.TP <- append(MOR.POR.TP, result.file[316, 2])  #all selectivities =1 for age 10 only; no need to collate all
  JPN.LL.EastMed <- cbind(JPN.LL.EastMed, t(apply(result.file[370, 2:6], c(1,2), as.numeric)))
  JPN.LL1.NEA <- cbind(JPN.LL1.NEA, t(apply(result.file[440, 2:8], c(1,2), as.numeric)))
  JPN.LL2.NEA <- cbind(JPN.LL2.NEA, t(apply(result.file[481, 2:8], c(1,2), as.numeric)))
  SP.BB1 <- cbind(SP.BB1, t(apply(result.file[535, 2:3], c(1,2), as.numeric)))
  SP.BB2 <- cbind(SP.BB2, t(apply(result.file[591, 2:5], c(1,2), as.numeric)))
  FR.AER1 <- cbind(FR.AER1, t(apply(result.file[618, 2:4], c(1,2), as.numeric)))
  FR.AER2 <- cbind(FR.AER2, t(apply(result.file[643, 2:4], c(1,2), as.numeric)))
  WMED.LARV <- cbind(WMED.LARV, t(apply(result.file[673, 2:9], c(1,2), as.numeric)))

}


# Pull out selectivities used as inputs to the OM ("Selectivity.csv")
Selectivity <- (as.matrix(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/R Code + Inputs/Selectivity.csv"), header=TRUE))  
Sel.E <- (array(Selectivity[2:11,19:28],c(10,10),dimnames=list(age=1:10,gear=1:10)))


# Compare the average of the VPA simulations selectivities to the OM selectivities using R base plot functions
layout(matrix(c(1,2,3,4,
              5,6,7,8), 2, 4, byrow = TRUE))

MOR.SP.TP.avg <- c(rep(0,5), rowMeans(MOR.SP.TP[,-1]))
MOR.SP.TP.comp <- t(rbind(MOR.SP.TP.avg, Sel.E[,1]))
matplot(1:10, MOR.SP.TP.comp, type = "l", xlab = "age", ylab = "Selectivity", main = "MOR_SP_TP")
legend(x = 1, y = 1.0, legend = c("OM", "EM"), lty = c(2,1), col = c(2,1), cex = 0.8)

JPN.LL.EastMed.avg <- c(rep(0,5), rowMeans(JPN.LL.EastMed[,-1]))
JPN.LL.EastMed.comp <- t(rbind(JPN.LL.EastMed.avg, Sel.E[,3]))
matplot(1:10, JPN.LL.EastMed.comp, type = "l", xlab = "age", ylab = "Selectivity", main = "JPN_LL_EastMed")

JPN.LL1.NEA.avg <- c(rep(0,3), rowMeans(JPN.LL1.NEA[,-1]))
JPN.LL1.NEA.comp <- t(rbind(JPN.LL1.NEA.avg, Sel.E[,4]))
matplot(1:10, JPN.LL1.NEA.comp, type = "l", xlab = "age", ylab = "Selectivity", main = "JPN_LL1_NEA")

JPN.LL2.NEA.avg <- c(rep(0,3), rowMeans(JPN.LL2.NEA[,-1]))
JPN.LL2.NEA.comp <- t(rbind(JPN.LL2.NEA.avg, Sel.E[,5]))
matplot(1:10, JPN.LL2.NEA.comp, type = "l", xlab = "age", ylab = "Selectivity", main = "JPN_LL2_NEA")

SP.BB1.avg <- c(0, rowMeans(SP.BB1[,-1]), rep(0,7))
SP.BB1.comp <- t(rbind(SP.BB1.avg, Sel.E[,6]))
matplot(1:10, SP.BB1.comp, type = "l", xlab = "age", ylab = "Selectivity", main = "SP_BB1")

SP.BB2.avg <- c(0, 0, rowMeans(SP.BB2[,-1]), rep(0,4))
SP.BB2.comp <- t(rbind(SP.BB2.avg, Sel.E[,7]))
matplot(1:10, SP.BB2.comp, type = "l", xlab = "age", ylab = "Selectivity", main = "SP_BB2")

WMED.LARV.avg <- c(0, 0, rowMeans(WMED.LARV[,-1]))
WMED.LARV.comp <- t(rbind(WMED.LARV.avg, Sel.E[,10]))
matplot(1:10, WMED.LARV.comp, type = "l", xlab = "age", ylab = "Selectivity", main = "WMED_LARV")

#Boxplots with ggplot
ages <- matrix(1:10, nrow = 10, ncol=1)
MOR.SP.TP.dat <- rbind(matrix(0, nrow = 5, ncol = ncol(MOR.SP.TP)), MOR.SP.TP)
MOR.SP.TP.dat[1:10,1] <- ages
MOR.SP.TP.sims <- as.data.frame(MOR.SP.TP.dat)
colnames(MOR.SP.TP.sims) <- c("age", 1:474)
MOR.SP.TP.om <- as.data.frame(cbind(ages, Sel.E[,1]))
colnames(MOR.SP.TP.om) <- c("age", "OM")
MOR.SP.TP.sims_1 <- melt(MOR.SP.TP.sims, id.vars = "age")
allLevels <- levels(factor(c(MOR.SP.TP.sims_1$age, MOR.SP.TP.om$age)))
MOR.SP.TP.sims_1$age <- factor(MOR.SP.TP.sims_1$age, levels = (allLevels))
MOR.SP.TP.om$age <- factor(MOR.SP.TP.om$age, levels = (allLevels))
MOR.SP.TP.plot <- 
ggplot(data = MOR.SP.TP.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = MOR.SP.TP.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Morocco-Spanish Trap", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,10,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

JPN.LL.EastMed.dat <- rbind(matrix(0, nrow = 5, ncol = ncol(JPN.LL.EastMed)), JPN.LL.EastMed)
JPN.LL.EastMed.dat[1:10,1] <- ages
JPN.LL.EastMed.sims <- as.data.frame(JPN.LL.EastMed.dat)
colnames(JPN.LL.EastMed.sims) <- c("age", 1:474)
JPN.LL.EastMed.om <- as.data.frame(cbind(ages, Sel.E[,3]))
colnames(JPN.LL.EastMed.om) <- c("age", "OM")
JPN.LL.EastMed.sims_1 <- melt(JPN.LL.EastMed.sims, id.vars = "age")
allLevels <- levels(factor(c(JPN.LL.EastMed.sims_1$age, JPN.LL.EastMed.om$age)))
JPN.LL.EastMed.sims_1$age <- factor(JPN.LL.EastMed.sims_1$age, levels = (allLevels))
JPN.LL.EastMed.om$age <- factor(JPN.LL.EastMed.om$age, levels = (allLevels))
JPN.LL.EastMed.plot <- 
ggplot(data = JPN.LL.EastMed.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = JPN.LL.EastMed.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Japanese Longline
       (E Atlantic & Med)", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,10,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))


JPN.LL1.NEA.dat <- rbind(matrix(0, nrow = 3, ncol = ncol(JPN.LL1.NEA)), JPN.LL1.NEA)
JPN.LL1.NEA.dat[1:10,1] <- ages
JPN.LL1.NEA.sims <- as.data.frame(JPN.LL1.NEA.dat)
colnames(JPN.LL1.NEA.sims) <- c("age", 1:474)
JPN.LL1.NEA.om <- as.data.frame(cbind(ages, Sel.E[,4]))
colnames(JPN.LL1.NEA.om) <- c("age", "OM")
JPN.LL1.NEA.sims_1 <- melt(JPN.LL1.NEA.sims, id.vars = "age")
allLevels <- levels(factor(c(JPN.LL1.NEA.sims_1$age, JPN.LL1.NEA.om$age)))
JPN.LL1.NEA.sims_1$age <- factor(JPN.LL1.NEA.sims_1$age, levels = (allLevels))
JPN.LL1.NEA.om$age <- factor(JPN.LL1.NEA.om$age, levels = (allLevels))
JPN.LL1.NEA.plot <- 
ggplot(data = JPN.LL1.NEA.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = JPN.LL1.NEA.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Japanese Longline
       (NE Atlantic 1990-2009)", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,10,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

JPN.LL2.NEA.dat <- rbind(matrix(0, nrow = 3, ncol = ncol(JPN.LL2.NEA)), JPN.LL2.NEA)
JPN.LL2.NEA.dat[1:10,1] <- ages
JPN.LL2.NEA.sims <- as.data.frame(JPN.LL2.NEA.dat)
colnames(JPN.LL2.NEA.sims) <- c("age", 1:474)
JPN.LL2.NEA.om <- as.data.frame(cbind(ages, Sel.E[,5]))
colnames(JPN.LL2.NEA.om) <- c("age", "OM")
JPN.LL2.NEA.sims_1 <- melt(JPN.LL2.NEA.sims, id.vars = "age")
allLevels <- levels(factor(c(JPN.LL2.NEA.sims_1$age, JPN.LL2.NEA.om$age)))
JPN.LL2.NEA.sims_1$age <- factor(JPN.LL2.NEA.sims_1$age, levels = (allLevels))
JPN.LL2.NEA.om$age <- factor(JPN.LL2.NEA.om$age, levels = (allLevels))
JPN.LL2.NEA.plot <- 
ggplot(data = JPN.LL2.NEA.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = JPN.LL2.NEA.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Japanese Longline
       (NE Atlantic 2010-2015)", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,10,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

SP.BB1.dat <- rbind(matrix(0, nrow = 1, ncol = ncol(SP.BB1)), SP.BB1, matrix(0, nrow = 7, ncol = ncol(SP.BB1)))
SP.BB1.dat[1:10,1] <- ages
SP.BB1.sims <- as.data.frame(SP.BB1.dat)
colnames(SP.BB1.sims) <- c("age", 1:474)
SP.BB1.om <- as.data.frame(cbind(ages, Sel.E[,6]))
colnames(SP.BB1.om) <- c("age", "OM")
SP.BB1.sims_1 <- melt(SP.BB1.sims, id.vars = "age")
allLevels <- levels(factor(c(SP.BB1.sims_1$age, SP.BB1.om$age)))
SP.BB1.sims_1$age <- factor(SP.BB1.sims_1$age, levels = (allLevels))
SP.BB1.om$age <- factor(SP.BB1.om$age, levels = (allLevels))
SP.BB1.plot <- 
ggplot(data = SP.BB1.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = SP.BB1.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Spanish Baitboat
       (1974-2006)", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,10,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

SP.BB2.dat <- rbind(matrix(0, nrow = 2, ncol = ncol(SP.BB2)), SP.BB2, matrix(0, nrow = 4, ncol = ncol(SP.BB2)))
SP.BB2.dat[1:10,1] <- ages
SP.BB2.sims <- as.data.frame(SP.BB2.dat)
colnames(SP.BB2.sims) <- c("age", 1:474)
SP.BB2.om <- as.data.frame(cbind(ages, Sel.E[,7]))
colnames(SP.BB2.om) <- c("age", "OM")
SP.BB2.sims_1 <- melt(SP.BB2.sims, id.vars = "age")
allLevels <- levels(factor(c(SP.BB2.sims_1$age, SP.BB2.om$age)))
SP.BB2.sims_1$age <- factor(SP.BB2.sims_1$age, levels = (allLevels))
SP.BB2.om$age <- factor(SP.BB2.om$age, levels = (allLevels))
SP.BB2.plot <- 
  ggplot(data = SP.BB2.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = SP.BB2.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Spanish Baitboat
       (2007-2014)", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,10,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

WMED.LARV.dat <- rbind(matrix(0, nrow = 2, ncol = ncol(WMED.LARV)), WMED.LARV)
WMED.LARV.dat[1:10,1] <- ages
WMED.LARV.sims <- as.data.frame(WMED.LARV.dat)
colnames(WMED.LARV.sims) <- c("age", 1:474)
WMED.LARV.om <- as.data.frame(cbind(ages, Sel.E[,10]))
colnames(WMED.LARV.om) <- c("age", "OM")
WMED.LARV.sims_1 <- melt(WMED.LARV.sims, id.vars = "age")
allLevels <- levels(factor(c(WMED.LARV.sims_1$age, WMED.LARV.om$age)))
WMED.LARV.sims_1$age <- factor(WMED.LARV.sims_1$age, levels = (allLevels))
WMED.LARV.om$age <- factor(WMED.LARV.om$age, levels = (allLevels))
WMED.LARV.plot <- 
ggplot(data = WMED.LARV.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = WMED.LARV.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Western Mediterranean 
       Larval Survey", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,10,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

plot_grid(MOR.SP.TP.plot, JPN.LL.EastMed.plot, JPN.LL1.NEA.plot, JPN.LL2.NEA.plot, SP.BB1.plot,
          SP.BB2.plot, WMED.LARV.plot, ncol = 4, nrow = 2, align = "v")



## Eastern stock-wide selectivities (from VPAs)
stockFs_E <- matrix(NA, nrow = 1, ncol = 10)
for (i in runnums) 
{
  result.filename <- paste("BFTE2017_", i, "_RESULTS.R", sep="")
  
  result.file <- as.data.frame(read.table(file = result.filename,
                                          fill = T, col.names = 1:max(count.fields(
                                            result.filename
                                          ))))
  
  threeyrF <- apply(result.file[70:72, 2:11], c(1,2), as.numeric) #pull out stock-wide Fs 2012-2014
  threeyrF_avg <- colMeans(threeyrF)
  stockFs_E <- rbind(stockFs_E, threeyrF_avg)
  
}

stockFs_avg_E <- colMeans(stockFs_E[-1, ])
plot(stockFs_avg_E, type = "l")

## Eastern stock-wide selectivities (from OM, q3)
F.weight.e <- array(NA, c(3,29,4), dimnames = list(year=2012:2014, age=1:29, zone=4:7))
for (y in 39:41)
  for (a in 1:29)
    for (z in 4:7)
    {
      F.weight.e[y-38, a, z-3] <- sum(naa[y, a, 3, z, 1:2]) * Fannual[y, z, a]
    }
F.weight.avg.e <- rep(NA,29)
for (a in 1:29)
{
  F.weight.avg.e[a] <- sum(F.weight.e[1:3, a, 1:4]) / sum(naa[39:41, a, 3, 4:7, 1:2])
}

plot(F.weight.avg.e[1:10], type = "l")





#### WEST SELECTIVITIES ####

# Pull out selectivities for each index modeled for each run of the VPA simulations
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)

# Set up objects for storing selectivities of each index
CAN.GSL.Acoustic <- matrix(NA, nrow = 9, ncol = 1)
US.RR.145 <- matrix(NA, nrow = 5, ncol = 1)
US.RR.66.114 <- matrix(NA, nrow = 3, ncol = 1)
US.RR.115.144 <- matrix(NA, nrow = 3, ncol = 1)
US.RR.195 <- matrix(NA, nrow = 8, ncol = 1)
JLL.AREA.2.WEST <- matrix(NA, nrow = 15, ncol = 1)
LARVAL.ZERO.INFLATED <- matrix(NA, nrow = 9, ncol = 1)
GOM.PLL.1.6 <- matrix(NA, nrow = 9, ncol = 1)
JLL.GOM <- matrix(NA, nrow = 9, ncol = 1)
JLL.AREA.2.RECENT <- matrix(NA, nrow = 12, ncol = 1)

# Pull out selectivities from each VPA simulation
for (i in runnums) 
{
  result.filename <- paste("BFTW2017_", i, "_RESULTS.R", sep="")
  
  result.file <- as.data.frame(read.table(file = result.filename,
                                          fill = T, col.names = 1:max(count.fields(
                                            result.filename
                                          ))))

  CAN.GSL.Acoustic <- cbind(CAN.GSL.Acoustic, t(apply(result.file[261, 2:10], c(1,2), as.numeric)))
  US.RR.145 <- cbind(US.RR.145, t(apply(result.file[310, 2:6], c(1,2), as.numeric)))
  US.RR.66.114 <- cbind(US.RR.66.114, t(apply(result.file[360, 2:4], c(1,2), as.numeric)))
  US.RR.115.144 <- cbind(US.RR.115.144, t(apply(result.file[421, 2:4], c(1,2), as.numeric)))
  US.RR.195 <- cbind(US.RR.195, t(apply(result.file[473, 2:9], c(1,2), as.numeric)))
  JLL.AREA.2.WEST <- cbind(JLL.AREA.2.WEST, t(apply(result.file[540, 2:16], c(1,2), as.numeric)))
  LARVAL.ZERO.INFLATED <- cbind(LARVAL.ZERO.INFLATED, t(apply(result.file[633, 2:10], c(1,2), as.numeric)))
  GOM.PLL.1.6 <- cbind(GOM.PLL.1.6, t(apply(result.file[708, 2:10], c(1,2), as.numeric)))
  JLL.GOM <- cbind(JLL.GOM, t(apply(result.file[755, 2:10], c(1,2), as.numeric)))
  JLL.AREA.2.RECENT <- cbind(JLL.AREA.2.RECENT, t(apply(result.file[788, 2:13], c(1,2), as.numeric)))
  
}

# Pull out selectivities used as inputs to the OM ("Selectivity.csv")
Selectivity <- (as.matrix(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/R Code + Inputs/Selectivity.csv"), header=TRUE))  
Sel.W <- (array(Selectivity[2:17,2:18],c(16,17),dimnames=list(age=1:16,gear=1:17)))


agesW <- matrix(1:16, nrow = 16, ncol=1)


# Compare the average of the VPA simulations selectivities to the OM selectivities

layout(matrix(c(1,2,3,4,5,
                6,7,8,9,10), 2, 5, byrow = TRUE))

CAN.GSL.Acoustic.avg <- c(rep(0,7), rowMeans(CAN.GSL.Acoustic[,-1]))
CAN.GSL.Acoustic.comp <- t(rbind(CAN.GSL.Acoustic.avg, Sel.W[,2]))
matplot(1:16, CAN.GSL.Acoustic.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "CAN_GSL_Acoustic")
legend(x = 1, y = 1.0, legend = c("OM", "EM"), lty = c(2,1), col = c(2,1), cex = 0.8)

US.RR.145.avg <- c(rowMeans(US.RR.145[,-1]), rep(0,11))
US.RR.145.comp <- t(rbind(US.RR.145.avg, Sel.W[,3]))
matplot(1:16, US.RR.145.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "US_RR<145")

US.RR.66.114.avg <- c(0, rowMeans(US.RR.66.114[,-1]), rep(0,12))
US.RR.66.114.comp <- t(rbind(US.RR.66.114.avg, Sel.W[,4]))
matplot(1:16, US.RR.66.114.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "US_RR_66_114")

US.RR.115.144.avg <- c(rep(0,3), rowMeans(US.RR.115.144[,-1]), rep(0,10))
US.RR.115.144.comp <- t(rbind(US.RR.115.144.avg, Sel.W[,5]))
matplot(1:16, US.RR.115.144.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "US_RR_115_144")

US.RR.195.avg <- c(rep(0,8), rowMeans(US.RR.195[,-1]))
US.RR.195.comp <- t(rbind(US.RR.195.avg, Sel.W[,7]))
matplot(1:16, US.RR.195.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "US_RR>195")

JLL.AREA.2.WEST.avg <- c(0, rowMeans(JLL.AREA.2.WEST[,-1]))
JLL.AREA.2.WEST.comp <- t(rbind(JLL.AREA.2.WEST.avg, Sel.W[,10]))
matplot(1:16, JLL.AREA.2.WEST.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "JLL_AREA_2_(WEST)")

LARVAL.ZERO.INFLATED.avg <- c(rep(0,7), rowMeans(LARVAL.ZERO.INFLATED[,-1]))
LARVAL.ZERO.INFLATED.comp <- t(rbind(LARVAL.ZERO.INFLATED.avg, Sel.W[,13]))
matplot(1:16, LARVAL.ZERO.INFLATED.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "LARVAL_ZERO_INFLATED")

GOM.PLL.1.6.avg <- c(rep(0,7), rowMeans(GOM.PLL.1.6[,-1]))
GOM.PLL.1.6.comp <- t(rbind(GOM.PLL.1.6.avg, Sel.W[,14]))
matplot(1:16, GOM.PLL.1.6.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "GOM_PLL_1-6")

JLL.GOM.avg <- c(rep(0,7), rowMeans(JLL.GOM[,-1]))
JLL.GOM.comp <- t(rbind(JLL.GOM.avg, Sel.W[,15]))
matplot(1:16, JLL.GOM.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "JLL_GOM")

JLL.AREA.2.RECENT.avg <- c(rep(0,4), rowMeans(JLL.AREA.2.RECENT[,-1]))
JLL.AREA.2.RECENT.comp <- t(rbind(JLL.AREA.2.RECENT.avg, Sel.W[,17]))
matplot(1:16, JLL.AREA.2.RECENT.comp, type = "l",xlab = "age", ylab = "Selectivity", main = "JLL_RECENT")


CAN.GSL.Acoustic.dat <- rbind(matrix(0, nrow = 7, ncol = ncol(CAN.GSL.Acoustic[,-1])), CAN.GSL.Acoustic[,-1])
CAN.GSL.Acoustic.dat[1:16,1] <- agesW
CAN.GSL.Acoustic.sims <- as.data.frame(CAN.GSL.Acoustic.dat)
colnames(CAN.GSL.Acoustic.sims) <- c("age", 1:405)
CAN.GSL.Acoustic.om <- as.data.frame(cbind(agesW, Sel.W[,2]))
colnames(CAN.GSL.Acoustic.om) <- c("age", "OM")
CAN.GSL.Acoustic.sims_1 <- melt(CAN.GSL.Acoustic.sims, id.vars = "age")
allLevels <- levels(factor(c(CAN.GSL.Acoustic.sims_1$age, CAN.GSL.Acoustic.om$age)))
CAN.GSL.Acoustic.sims_1$age <- factor(CAN.GSL.Acoustic.sims_1$age, levels = (allLevels))
CAN.GSL.Acoustic.om$age <- factor(CAN.GSL.Acoustic.om$age, levels = (allLevels))
CAN.GSL.Acoustic.plot <- 
  ggplot(data = CAN.GSL.Acoustic.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = CAN.GSL.Acoustic.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Gulf of St Lawrence 
       Acoustic Survey", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

US.RR.145.dat <- rbind(US.RR.145, matrix(0, nrow = 11, ncol = ncol(US.RR.145)))
US.RR.145.dat[1:16,1] <- agesW
US.RR.145.sims <- as.data.frame(US.RR.145.dat)
colnames(US.RR.145.sims) <- c("age", 1:404)
US.RR.145.om <- as.data.frame(cbind(agesW, Sel.W[,3]))
colnames(US.RR.145.om) <- c("age", "OM")
US.RR.145.sims_1 <- melt(US.RR.145.sims, id.vars = "age")
allLevels <- levels(factor(c(US.RR.145.sims_1$age, US.RR.145.om$age)))
US.RR.145.sims_1$age <- factor(US.RR.145.sims_1$age, levels = (allLevels))
US.RR.145.om$age <- factor(US.RR.145.om$age, levels = (allLevels))
US.RR.145.plot <- 
  ggplot(data = US.RR.145.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = US.RR.145.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "US Rod & Reel 
       <145cm", y = "", x ="") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

US.RR.66.114.dat <- rbind(matrix(0, nrow = 1, ncol = ncol(US.RR.66.114)), US.RR.66.114, matrix(0, nrow = 12, ncol = ncol(US.RR.66.114)))
US.RR.66.114.dat[1:16,1] <- agesW
US.RR.66.114.sims <- as.data.frame(US.RR.66.114.dat)
colnames(US.RR.66.114.sims) <- c("age", 1:404)
US.RR.66.114.om <- as.data.frame(cbind(agesW, Sel.W[,4]))
colnames(US.RR.66.114.om) <- c("age", "OM")
US.RR.66.114.sims_1 <- melt(US.RR.66.114.sims, id.vars = "age")
allLevels <- levels(factor(c(US.RR.66.114.sims_1$age, US.RR.66.114.om$age)))
US.RR.66.114.sims_1$age <- factor(US.RR.66.114.sims_1$age, levels = (allLevels))
US.RR.66.114.om$age <- factor(US.RR.66.114.om$age, levels = (allLevels))
US.RR.66.114.plot <- 
  ggplot(data = US.RR.66.114.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = US.RR.66.114.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "US Rod & Reel 
       66-114cm", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

US.RR.115.144.dat <- rbind(matrix(0, nrow = 3, ncol = ncol(US.RR.115.144)), US.RR.115.144, matrix(0, nrow = 10, ncol = ncol(US.RR.115.144)))
US.RR.115.144.dat[1:16,1] <- agesW
US.RR.115.144.sims <- as.data.frame(US.RR.115.144.dat)
colnames(US.RR.115.144.sims) <- c("age", 1:404)
US.RR.115.144.om <- as.data.frame(cbind(agesW, Sel.W[,5]))
colnames(US.RR.115.144.om) <- c("age", "OM")
US.RR.115.144.sims_1 <- melt(US.RR.115.144.sims, id.vars = "age")
allLevels <- levels(factor(c(US.RR.115.144.sims_1$age, US.RR.115.144.om$age)))
US.RR.115.144.sims_1$age <- factor(US.RR.115.144.sims_1$age, levels = (allLevels))
US.RR.115.144.om$age <- factor(US.RR.115.144.om$age, levels = (allLevels))
US.RR.115.144.plot <- 
  ggplot(data = US.RR.115.144.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = US.RR.115.144.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "US Rod & Reel 
       115-144cm", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

US.RR.195.dat <- rbind(matrix(0, nrow = 8, ncol = ncol(US.RR.195)), US.RR.195)
US.RR.195.dat[1:16,1] <- agesW
US.RR.195.sims <- as.data.frame(US.RR.195.dat)
colnames(US.RR.195.sims) <- c("age", 1:404)
US.RR.195.om <- as.data.frame(cbind(agesW, Sel.W[,7]))
colnames(US.RR.195.om) <- c("age", "OM")
US.RR.195.sims_1 <- melt(US.RR.195.sims, id.vars = "age")
allLevels <- levels(factor(c(US.RR.195.sims_1$age, US.RR.195.om$age)))
US.RR.195.sims_1$age <- factor(US.RR.195.sims_1$age, levels = (allLevels))
US.RR.195.om$age <- factor(US.RR.195.om$age, levels = (allLevels))
US.RR.195.plot <- 
  ggplot(data = US.RR.195.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = US.RR.195.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "US Rod & Reel 
       <195cm", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

JLL.AREA.2.WEST.dat <- rbind(matrix(0, nrow = 1, ncol = ncol(JLL.AREA.2.WEST)), JLL.AREA.2.WEST)
JLL.AREA.2.WEST.dat[1:16,1] <- agesW
JLL.AREA.2.WEST.sims <- as.data.frame(JLL.AREA.2.WEST.dat)
colnames(JLL.AREA.2.WEST.sims) <- c("age", 1:404)
JLL.AREA.2.WEST.om <- as.data.frame(cbind(agesW, Sel.W[,10]))
colnames(JLL.AREA.2.WEST.om) <- c("age", "OM")
JLL.AREA.2.WEST.sims_1 <- melt(JLL.AREA.2.WEST.sims, id.vars = "age")
allLevels <- levels(factor(c(JLL.AREA.2.WEST.sims_1$age, JLL.AREA.2.WEST.om$age)))
JLL.AREA.2.WEST.sims_1$age <- factor(JLL.AREA.2.WEST.sims_1$age, levels = (allLevels))
JLL.AREA.2.WEST.om$age <- factor(JLL.AREA.2.WEST.om$age, levels = (allLevels))
JLL.AREA.2.WEST.plot <- 
  ggplot(data = JLL.AREA.2.WEST.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = JLL.AREA.2.WEST.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Japanese Longline
       (W Atlantic 1976-2009)", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

JLL.AREA.2.RECENT.dat <- rbind(matrix(0, nrow = 4, ncol = ncol(JLL.AREA.2.RECENT)), JLL.AREA.2.RECENT)
JLL.AREA.2.RECENT.dat[1:16,1] <- agesW
JLL.AREA.2.RECENT.sims <- as.data.frame(JLL.AREA.2.RECENT.dat)
colnames(JLL.AREA.2.RECENT.sims) <- c("age", 1:404)
JLL.AREA.2.RECENT.om <- as.data.frame(cbind(agesW, Sel.W[,17]))
colnames(JLL.AREA.2.RECENT.om) <- c("age", "OM")
JLL.AREA.2.RECENT.sims_1 <- melt(JLL.AREA.2.RECENT.sims, id.vars = "age")
allLevels <- levels(factor(c(JLL.AREA.2.RECENT.sims_1$age, JLL.AREA.2.RECENT.om$age)))
JLL.AREA.2.RECENT.sims_1$age <- factor(JLL.AREA.2.RECENT.sims_1$age, levels = (allLevels))
JLL.AREA.2.RECENT.om$age <- factor(JLL.AREA.2.RECENT.om$age, levels = (allLevels))
JLL.AREA.2.RECENT.plot <- 
  ggplot(data = JLL.AREA.2.RECENT.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = JLL.AREA.2.RECENT.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Japanese Longline
       (W Atlantic 2010-2015)", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

LARVAL.ZERO.INFLATED.dat <- rbind(matrix(0, nrow = 7, ncol = ncol(LARVAL.ZERO.INFLATED)), LARVAL.ZERO.INFLATED)
LARVAL.ZERO.INFLATED.dat[1:16,1] <- agesW
LARVAL.ZERO.INFLATED.sims <- as.data.frame(LARVAL.ZERO.INFLATED.dat)
colnames(LARVAL.ZERO.INFLATED.sims) <- c("age", 1:404)
LARVAL.ZERO.INFLATED.om <- as.data.frame(cbind(agesW, Sel.W[,13]))
colnames(LARVAL.ZERO.INFLATED.om) <- c("age", "OM")
LARVAL.ZERO.INFLATED.sims_1 <- melt(LARVAL.ZERO.INFLATED.sims, id.vars = "age")
allLevels <- levels(factor(c(LARVAL.ZERO.INFLATED.sims_1$age, LARVAL.ZERO.INFLATED.om$age)))
LARVAL.ZERO.INFLATED.sims_1$age <- factor(LARVAL.ZERO.INFLATED.sims_1$age, levels = (allLevels))
LARVAL.ZERO.INFLATED.om$age <- factor(LARVAL.ZERO.INFLATED.om$age, levels = (allLevels))
LARVAL.ZERO.INFLATED.plot <- 
  ggplot(data = LARVAL.ZERO.INFLATED.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = LARVAL.ZERO.INFLATED.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Gulf of Mexico 
       Larval Survey", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

GOM.PLL.1.6.dat <- rbind(matrix(0, nrow = 7, ncol = ncol(GOM.PLL.1.6)), GOM.PLL.1.6)
GOM.PLL.1.6.dat[1:16,1] <- agesW
GOM.PLL.1.6.sims <- as.data.frame(GOM.PLL.1.6.dat)
colnames(GOM.PLL.1.6.sims) <- c("age", 1:404)
GOM.PLL.1.6.om <- as.data.frame(cbind(agesW, Sel.W[,14]))
colnames(GOM.PLL.1.6.om) <- c("age", "OM")
GOM.PLL.1.6.sims_1 <- melt(GOM.PLL.1.6.sims, id.vars = "age")
allLevels <- levels(factor(c(GOM.PLL.1.6.sims_1$age, GOM.PLL.1.6.om$age)))
GOM.PLL.1.6.sims_1$age <- factor(GOM.PLL.1.6.sims_1$age, levels = (allLevels))
GOM.PLL.1.6.om$age <- factor(GOM.PLL.1.6.om$age, levels = (allLevels))
GOM.PLL.1.6.plot <- 
  ggplot(data = GOM.PLL.1.6.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = GOM.PLL.1.6.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "US Gulf of Mexico 
       Longline", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

JLL.GOM.dat <- rbind(matrix(0, nrow = 7, ncol = ncol(JLL.GOM)), JLL.GOM)
JLL.GOM.dat[1:16,1] <- agesW
JLL.GOM.sims <- as.data.frame(JLL.GOM.dat)
colnames(JLL.GOM.sims) <- c("age", 1:404)
JLL.GOM.om <- as.data.frame(cbind(agesW, Sel.W[,15]))
colnames(JLL.GOM.om) <- c("age", "OM")
JLL.GOM.sims_1 <- melt(JLL.GOM.sims, id.vars = "age")
allLevels <- levels(factor(c(JLL.GOM.sims_1$age, JLL.GOM.om$age)))
JLL.GOM.sims_1$age <- factor(JLL.GOM.sims_1$age, levels = (allLevels))
JLL.GOM.om$age <- factor(JLL.GOM.om$age, levels = (allLevels))
JLL.GOM.plot <- 
  ggplot(data = JLL.GOM.om, aes(x = factor(age), y = OM)) +
  geom_boxplot(data = JLL.GOM.sims_1, aes(x = age, y = value), fill = "gray80") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.5) +
  theme_classic() +
  labs(title = "Japanese Gulf of Mexico 
       Longline", y = "", x = "") +
  scale_x_discrete(breaks = seq(1,16,2)) +
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  theme(plot.title = element_text(family = "Times New Roman"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"))

plot_grid(CAN.GSL.Acoustic.plot, US.RR.145.plot, US.RR.66.114.plot,
          US.RR.115.144.plot, US.RR.195.plot, JLL.AREA.2.WEST.plot, JLL.AREA.2.RECENT.plot, 
          LARVAL.ZERO.INFLATED.plot, GOM.PLL.1.6.plot, JLL.GOM.plot, 
          MOR.SP.TP.plot, JPN.LL.EastMed.plot, JPN.LL1.NEA.plot, JPN.LL2.NEA.plot, SP.BB1.plot,
          SP.BB2.plot, WMED.LARV.plot, 
          ncol = 4, nrow = 5, align = "v")


## Western stock-wide selectivities
stockFs_W <- matrix(NA, nrow = 1, ncol = 16)
for (i in runnums) 
{
  result.filename <- paste("BFTW2017_", i, "_RESULTS.R", sep="")
  
  result.file <- as.data.frame(read.table(file = result.filename,
                                          fill = T, col.names = 1:max(count.fields(
                                            result.filename
                                          ))))
  
  threeyrF <- apply(result.file[70:72, 2:17], c(1,2), as.numeric) #pull out stock-wide Fs 2012-2014
  threeyrF_avg <- colMeans(threeyrF)
  stockFs_W <- rbind(stockFs_W, threeyrF_avg)
  
}

stockFs_avg_W <- colMeans(stockFs_W[-1, ])
plot(stockFs_avg_W, type = "l")

## Western stock-wide selectivities (from OM, q3)
F.weight.w <- array(NA, c(3, 29, 3), dimnames = list(year=2012:2014, age=1:29, zone=1:3))
for (y in 39:41)
  for (a in 1:29)
    for (z in 1:3)
    {
      F.weight.w[y-38, a, z] <- sum(naa[y, a, 3, z, 1:2]) * Fannual[y, z, a]
    }
F.weight.avg.w <- rep(NA,29)
for (a in 1:29)
{
  F.weight.avg.w[a] <- sum(F.weight.w[1:3, a, 1:3]) / sum(naa[39:41, a, 3, 1:3, 1:2])
}

plot(F.weight.avg.w[1:16], type = "l")
















#### FISHING MORTALITY CALCULATIONS ####

#East OM (end at age 10)
F.OM.E <- array(NA, c(42,10),dimnames=list(year=1974:2015,age=1:10))
for (y in 1:42)
  for (a in 1:10) {
    F.OM.E[y,a] <- sum(Fa[y,a,1:4,4:7])
  }

F.OM.ap <- matrix(NA, nrow = 42, ncol = 1)
for (y in 1:42) {
  F.OM.ap[y, ] <- max(F.OM.E[y, ])  #calc apical Fs (only to age 10; not summed over ages 10-29)
}

#East OM (age 10 as plus group)
F.OM.E2 <- array(NA, c(42,29),dimnames=list(year=1974:2015,age=1:29))
for (y in 1:42)
  for (a in 1:29) {
    F.OM.E2[y, a] <- sum(Fa[y, a, 1:4, 4:7])
  }

F.OM.E2pl <- array(NA, c(42,1), dimnames = list(year = 1974:2015, age=10))
for (y in 1:42) {
  F.OM.E2pl[y] <- sum(F.OM.E2[y, 10:29])  #sum all plus group ages Fs
}
F.OM.E2new <- cbind(F.OM.E2[, 1:9], F.OM.E2pl)  #bind ages 1-9 with plus group

F.OM.ap2 <- matrix(NA, nrow = 42, ncol = 1)
for (i in 1:42) {
  F.OM.ap2[i, ] <- max(F.OM.E2new[i, ])  #calc apical Fs
}


#West OM
F.OM.W <- array(NA, c(42,16),dimnames=list(year=1974:2015,age=1:16))
for (y in 1:42)
  for (a in 1:16) {
    F.OM.W[y,a] <- sum(Fa[y,a,1:4,1:3])
  }

#West OM (age 16 as plus group)
F.OM.W2 <- array(NA, c(42,29),dimnames=list(year=1974:2015,age=1:29))
for (y in 1:42)
  for (a in 1:29) {
    F.OM.W2[y, a] <- sum(Fa[y, a, 1:4, 1:3])
  }

F.OM.W2pl <- array(NA, c(42,1), dimnames = list(year = 1974:2015, age=16))
for (y in 1:42) {
  F.OM.W2pl[y] <- sum(F.OM.W2[y, 16:29])
}
F.OM.W2new <- cbind(F.OM.W2[, 1:15], F.OM.W2pl)

F.OM.ap <- matrix(NA, nrow = 42, ncol = 1)
for (i in 1:42) {
  F.OM.ap[i, ] <- max(F.OM.W[i, ])
}

F.OM.ap2 <- matrix(NA, nrow = 42, ncol = 1)
for (i in 1:42) {
  F.OM.ap2[i, ] <- max(F.OM.W2new[i, ])
}


# Compare averages of the stochastic F-at-age matrices to the OM
#East
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)
all.F.stoch <- array(NA, c(nyr,nageE,length(runnums)), dimnames = list(year=1974:2015, age=1:nageE, run=runnums))

for (i in runnums) 
{
  result_file_name <- paste("BFTE2017_", i, "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:11], nrow = nyr, ncol = nageE)  # fishing mortality results - will need to change these dimensions if number of years or ages changes
  all.F.stoch[,,(which(i == runnums))] <- apply(F.res, c(1,2), as.numeric)
}
F.stoch.avg <- matrix(NA, nrow = nyr, ncol = nageE)
for (y in 1:nyr)
  for (a in 1:nageE)
  {
    F.stoch.avg[y,a] <- mean(all.F.stoch[y,a,], ylim = c(0, 1))
  }

matplot(1974:2015, F.stoch.avg, type = "l")

#West
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West - 500 Sims - 2/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West - 500 Sims - 2/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)
all.F.stoch <- array(NA, c(nyr,nageW,length(runnums)), dimnames = list(year=1974:2015, age=1:nageW, run=runnums))

for (i in runnums) 
{
  result_file_name <- paste("BFTW2017_", i, "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:17], nrow = nyr, ncol = nageW) # fishing mortality results - will need to change these dimensions if number of years or ages changes
  all.F.stoch[,,(which(i == runnums))] <- apply(F.res, c(1,2), as.numeric)
}
F.stoch.avg <- matrix(NA, nrow = nyr, ncol = nageW)
for (y in 1:nyr)
  for (a in 1:nageW)
  {
    F.stoch.avg[y,a] <- mean(all.F.stoch[y,a,])
  }

matplot(1974:2015, F.stoch.avg, type = "l", ylim = c(0,0.6))



# Compare apical F from stochastic runs to the OM
#East
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 1/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)
all.F.ap <- array(NA, c(nyr,length(runnums)), dimnames = list(year = 1974:2015, run = runnums))

for (i in 1:length(runnums)) {
  
  result_file_name <- paste("BFTE2017_", runnums[i], "_RESULTS.R", sep="")
  
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  
  F.res <- as.matrix(Results[32:73, 2:11], nrow = nyr, ncol = nageE)
  F.res <- apply(F.res, c(1,2), as.numeric)
  
  for (j in 1:42) {
    
    all.F.ap[j, i] <- max(F.res[j, ])
  
  }

}

F.ap     <- cbind(1974:2015,all.F.ap) %>% 
  as.data.frame()
F.ap.OM  <- cbind(1974:2015, F.OM.ap) %>%
  as.data.frame()
F.ap2.OM <- cbind(1974:2015, F.OM.ap2) %>%
  as.data.frame()

colnames(F.ap)    <- c("years", runnums)
colnames(F.ap.OM) <- c("years", "OM")
colnames(F.ap2.OM) <- c("years", "OM2")
F.ap.gg <- melt(F.ap, id.vars = "years")

allLevels <- levels(factor(c(F.ap.gg$years, F.ap.OM$years, F.ap2.OM$years)))
F.ap.gg$years  <- factor(F.ap.gg$years,levels=(allLevels))
F.ap.OM$years  <- factor(F.ap.OM$years,levels=(allLevels))
F.ap2.OM$years <- factor(F.ap2.OM$years,levels=(allLevels))

# BOXPLOT FOR MANUSCRIPT
E.F.plot <-
  ggplot(data=F.ap.OM, aes(x=years, y=OM)) +
  geom_boxplot(data=F.ap.gg,aes(x=years, y=value), color="black", fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5, color="red") +
  geom_line(data=F.ap2.OM, aes(x=years, y=OM2, group = 1), color = "blue", size = 1.5) +
  labs(y = "Fishing mortality rate", x = "", title = "East") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 2, 0.5)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20))


#West
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West - 500 Sims - 2/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West - 500 Sims - 2/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)
all.F.ap <- array(NA, c(nyr,length(runnums)), dimnames = list(year = 1974:2015, run = runnums))

for (i in 1:length(runnums)) {
  
  result_file_name <- paste("BFTW2017_", runnums[i], "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:17], nrow = nyr, ncol = nageW)
  F.res <- apply(F.res, c(1,2), as.numeric)
  
  for (j in 1:42) {
    
    all.F.ap[j, i] <- max(F.res[j, ])
    
  }
}

F.ap     <- cbind(1974:2015,all.F.ap) %>% 
  as.data.frame()
F.ap.OM  <- cbind(1974:2015, F.OM.ap) %>%
  as.data.frame()
F.ap2.OM <- cbind(1974:2015, F.OM.ap2) %>%
  as.data.frame()

colnames(F.ap)    <- c("years", runnums)
colnames(F.ap.OM) <- c("years", "OM")
colnames(F.ap2.OM) <- c("years", "OM2")
F.ap.gg <- melt(F.ap, id.vars = "years")

allLevels <- levels(factor(c(F.ap.gg$years, F.ap.OM$years, F.ap2.OM$years)))
F.ap.gg$years  <- factor(F.ap.gg$years,levels=(allLevels))
F.ap.OM$years  <- factor(F.ap.OM$years,levels=(allLevels))
F.ap2.OM$years <- factor(F.ap2.OM$years,levels=(allLevels))

# BOXPLOT FOR MANUSCRIPT
W.F.plot <-
  ggplot(data=F.ap.OM, aes(x=years, y=OM)) +
  geom_boxplot(data=F.ap.gg,aes(x=years, y=value), color="black", fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5, color="red") +
  geom_line(data=F.ap2.OM, aes(x=years, y=OM2, group = 1), color = "blue", size = 1.5) +
  labs(y = "Fishing mortality rate", x = "", title = "West") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 2, 0.5)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20))



## Self-Test ##

setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/R Code + Inputs")
F_E_OM <- as.matrix(read.csv("F_E.csv"))[,-1]
F_W_OM <- as.matrix(read.csv("F_W.csv"))[,-1]

F_E_OM_ap <- matrix(NA, nrow = 42, ncol = 1)
F_W_OM_ap <- matrix(NA, nrow = 42, ncol = 1)

for (i in 1:42) {
  F_E_OM_ap[i, ] <- max(F_E_OM[i, ])
}

for (i in 1:42) {
  F_W_OM_ap[i, ] <- max(F_W_OM[i, ])
}

write.csv(F_E_OM_ap, "Fapical_E_OM.csv")
write.csv(F_W_OM_ap, "Fapical_W_OM.csv")

# East #

wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums)))[-1] # the ID numbers of runs that converged; remove run 0 (deterministic)
setwd(wd)
F_E_ap <- array(NA, c(nyr,length(runnums)), dimnames = list(year = 1974:2015, run = runnums))

for (i in 1:length(runnums)) {
  
  result_file_name <- paste("BFTE2017_", runnums[i], "_RESULTS.R", sep="")
  
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  
  F.res <- as.matrix(Results[32:73, 2:11], nrow = nyr, ncol = nageE)
  F.res <- apply(F.res, c(1,2), as.numeric)
  
  for (j in 1:42) {
    
    F_E_ap[j, i] <- max(F.res[j, ])
    
  }
  
}

write.csv(F_E_ap, "Fapical_E.csv")

F_E_ap_df     <- cbind(1974:2015, F_E_ap) %>% 
  as.data.frame()
F_E_OM_ap_df  <- cbind(1974:2015, F_E_OM_ap) %>%
  as.data.frame()

colnames(F_E_ap_df)    <- c("years", runnums)
colnames(F_E_OM_ap_df) <- c("years", "OM")
F_E_ap_gg <- melt(F_E_ap_df, id.vars = "years")

allLevels          <- levels(factor(c(F_E_ap_gg$years, F_E_OM_ap_df$years)))
F_E_ap_gg$years    <- factor(F_E_ap_gg$years,levels=(allLevels))
F_E_OM_ap_df$years <- factor(F_E_OM_ap_df$years,levels=(allLevels))

# BOXPLOT FOR MANUSCRIPT
# E.F.plot <-
  ggplot(data=F_E_OM_ap_df, aes(x=years, y=OM)) +
  geom_boxplot(data=F_E_ap_gg,aes(x=years, y=value), color="black", fill="gray80") + 
  geom_line(data=F_E_OM_ap_df, aes(x=years, y=OM, group = 1), color = "black", size = 1.5) +
  labs(y = "Fishing mortality rate", x = "", title = "East") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 3, 1)) +
  coord_cartesian(ylim = c(0,3)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20))

# West #
  
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                   list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/West/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums)))[-1] # the ID numbers of runs that converged; remove run 0 (deterministic)
setwd(wd)
F_W_ap <- array(NA, c(nyr,length(runnums)), dimnames = list(year = 1974:2015, run = runnums))
  
for (i in 1:length(runnums)) {
    
  result_file_name <- paste("BFTW2017_", runnums[i], "_RESULTS.R", sep="")
    
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                      result_file_name
                                      ))))
    
  F.res <- as.matrix(Results[32:73, 2:17], nrow = nyr, ncol = nageW)
  F.res <- apply(F.res, c(1,2), as.numeric)
    
  for (j in 1:42) {
      
    F_W_ap[j, i] <- max(F.res[j, ])
      
  }
    
}
  
write.csv(F_W_ap, "Fapical_W.csv")

F_W_ap_df     <- cbind(1974:2015, F_W_ap) %>% 
  as.data.frame()
F_W_OM_ap_df  <- cbind(1974:2015, F_W_OM_ap) %>%
  as.data.frame()

colnames(F_W_ap_df)    <- c("years", runnums)
colnames(F_W_OM_ap_df) <- c("years", "OM")
F_W_ap_gg <- melt(F_W_ap_df, id.vars = "years")

allLevels          <- levels(factor(c(F_W_ap_gg$years, F_W_OM_ap_df$years)))
F_W_ap_gg$years    <- factor(F_W_ap_gg$years,levels=(allLevels))
F_W_OM_ap_df$years <- factor(F_W_OM_ap_df$years,levels=(allLevels))
  
# BOXPLOT FOR MANUSCRIPT
# W.F.plot <-
ggplot(data=F_W_OM_ap_df, aes(x=years, y=OM)) +
  geom_boxplot(data=F_W_ap_gg,aes(x=years, y=value), color="black", fill="gray80") + 
  geom_line(data=F_W_OM_ap_df, aes(x=years, y=OM, group = 1), color = "black", size = 1.5) +
  labs(y = "Fishing mortality rate", x = "", title = "West") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0, 2, 1)) +
  coord_cartesian(ylim = c(0,2)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20))



## For Revisions - Apical F ##

# Cross-test #
# East #

# true apical Fs
Fa.read <- as.matrix(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Base_Output/Fa.csv", header = TRUE))
Fa      <- array(Fa.read[, -1], c(nyr, nage, 4, 7), dimnames = list(year = 1974:2015, age = 1:nage, quarter = 1:4, zone = 1:7))
F.OM.E  <- array(NA, c(42,10), dimnames = list(year = 1974:2015, age = 1:10))
for (y in 1:42)
  for (a in 1:10) {
    F.OM.E[y, a] <- sum(Fa[y, a, 1:4, 4:7])
  }

F.OM.ap.E <- matrix(NA, nrow = 42, ncol = 1)
for (y in 1:42) {
  F.OM.ap.E[y, ] <- max(F.OM.E[y, ]) 
}


# estimated apical Fs
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path = wd, pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
setwd(wd)
all.F.ap.E <- array(NA, c(nyr, length(runnums)), dimnames = list(year = 1974:2015, run = runnums)) #array to store apical Fs from all runs

for (i in 1:length(runnums)) {
  
  result_file_name <- paste("BFTE2017_", runnums[i], "_RESULTS.R", sep="")
  
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  
  F.res <- as.matrix(Results[32:73, 2:11], nrow = nyr, ncol = nageE) #pull out F results matrix
  F.res <- apply(F.res, c(1,2), as.numeric) #convert to numeric
  
  for (j in 1:42) {
    
    all.F.ap.E[j, i] <- max(F.res[j, ]) #calculate apical Fs for this run, add to a matrix of all runs
    
  }
  
}


write.csv(cbind(all.F.ap.E, F.OM.ap.E), paste(wd, "/E_apicalF_data_converge.csv", sep = ""))


# West #
# true apical Fs
F.OM.W  <- array(NA, c(42, 16), dimnames = list(year = 1974:2015, age = 1:16))
for (y in 1:42)
  for (a in 1:16) {
    F.OM.W[y, a] <- sum(Fa[y, a, 1:4, 1:3])
  }

F.OM.ap.W <- matrix(NA, nrow = 42, ncol = 1)
for (y in 1:42) {
  F.OM.ap.W[y, ] <- max(F.OM.W[y, ]) 
}


# estimated apical Fs
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged" #switch folder
setwd(wd)
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path = wd, pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
all.F.ap.W <- array(NA, c(nyr, length(runnums)), dimnames = list(year = 1974:2015, run = runnums))

for (i in 1:length(runnums)) {
  
  result_file_name <- paste("BFTW2017_", runnums[i], "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  F.res <- as.matrix(Results[32:73, 2:17], nrow = nyr, ncol = nageW)
  F.res <- apply(F.res, c(1,2), as.numeric)
  
  for (j in 1:42) {
    
    all.F.ap.W[j, i] <- max(F.res[j, ])
    
  }
}


write.csv(cbind(all.F.ap.W, F.OM.ap.W), paste(wd, "/W_apicalF_data_converge.csv", sep = ""))





#### LAYOUTS  ####

## EM PERFORMANCE PLOTS W/ PERCENT RELATIVE BIAS ##
grid.arrange(W.R.plot, E.R.plot,
             W.baseR.PRB, E.baseR.PRB,
             W.SSB.plot, E.SSB.plot,
             W.baseSSB.PRB, E.baseSSB.PRB,
             W.F.plot, E.F.plot,
             W.F.PRB, E.F.PRB,
             nrow = 6, ncol = 2,
             heights=c(5,3,5,3,5,3))

jpeg("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Publishing/CJFAS - Bluefin Tuna Simulations/Figures/EMplots-2.jpeg",
     width = 1100, height = 1350, units = "px", quality = 100)
plot_grid(W.R.plot, E.R.plot,
          W.baseR.PRB, E.baseR.PRB,
          W.SSB.plot, E.SSB.plot,
          W.baseSSB.PRB, E.baseSSB.PRB,
          W.F.plot, E.F.plot,
          W.F.PRB, E.F.PRB,
          ncol = 2, nrow = 6, 
          align = "v", rel_heights = c(7,3,7,3,7,3))
dev.off()





#### DETERMINISTIC VS STOCHASTIC ####

# East SSB avg stochastic (converged) vs. deterministic from EM E3
SSB.converge <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 2/Converged/E_SSB_data_converge.csv", header = TRUE)
SSB.pop <- SSB.converge[,483]
SSB.stock <- SSB.converge[,484]
SSB.mean <- SSB.converge[,485]
SSB.det <- SSB.converge[,486]
SSB.vs <- data.frame(cbind(1974:2015,SSB.pop,SSB.stock,SSB.mean,SSB.det))
colnames(SSB.vs) <- c("V1","population", "stock", "stochastic mean", "deterministic")
SSB.vs_1 <- melt(SSB.vs, id.vars = "V1")
E.SSB.bias <- ggplot(SSB.vs_1, aes(x=V1)) +
  geom_line(aes(y=value, linetype=variable, color=variable), size=1.5) +
  scale_color_manual(values=c(2,2,1,1)) +
  scale_linetype_manual(values=c("solid","dotted","solid","dashed")) +
  labs(y="",x="",title="East") +
  coord_cartesian(ylim = c(0,1000000)) +
  theme_classic() +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 14,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 14),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman"),
        legend.key.width = unit(1.5,"cm"))

# SSB series, EM only
SSB.vs2 <- data.frame(cbind(1974:2015,SSB.mean,SSB.det))
colnames(SSB.vs2) <- c("V1", "stochastic", "deterministic")
SSB.vs_2 <- melt(SSB.vs2, id.vars = "V1")
E.SSB.bias <- ggplot(SSB.vs_2, aes(x=V1)) +
  geom_line(aes(y=value, linetype=variable), size=1) +
  labs(y="SSB
       (tonnes)",x="") +
  coord_cartesian(ylim = c(0,800000)) +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position="none") +
  scale_y_continuous(breaks = seq(0,800000,400000), label = scientific_format(digits = 2))


#relative bias
SSB.rbias <- (SSB.mean-SSB.det)/SSB.det*100
mean(SSB.rbias)
# Pull out NAA-X, R, SSB from all results
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 2/Converged")
NAA10 <- matrix(NA,nrow=42,ncol=1) 
NAA7 <- matrix(NA,nrow=42,ncol=1)
NAA5 <- matrix(NA,nrow=42,ncol=1)
NAA4 <- matrix(NA,nrow=42,ncol=1)
Rec <- matrix(NA,nrow=42,ncol=1)
SSB <- matrix(NA,nrow=42,ncol=1)
for (i in E500.conv) {
  result.file <- paste("BFTE2017_", i, "_RESULTS.R", sep="")
  result.data <- as.data.frame(read.table(file = result.file,fill = T, col.names = 1:max(count.fields(result.file))))
  # East NAA-10
  NAA10.f <- result.data[79:120,11]
  NAA10 <- cbind(NAA10, as.numeric(levels(NAA10.f))[NAA10.f])
  # East NAA-7
  NAA7.f <- result.data[79:120,8]
  NAA7 <- cbind(NAA7, as.numeric(levels(NAA7.f))[NAA7.f])
  # East NAA-5
  NAA5.f <- result.data[79:120,6]
  NAA5 <- cbind(NAA5, as.numeric(levels(NAA5.f))[NAA5.f])
  # East NAA-4
  NAA4.f <- result.data[79:120,5]
  NAA4 <- cbind(NAA4, as.numeric(levels(NAA4.f))[NAA4.f])
  # East R
  Rec.f <- result.data[79:120,2]
  Rec <- cbind(Rec,as.numeric(levels(Rec.f))[Rec.f])
  # East SSB
  SSB.f <- result.data[175:216,2]
  SSB <- cbind(SSB, as.numeric(levels(SSB.f))[SSB.f])
}  
NAA10.mean <- rowMeans(NAA10[,-1])
NAA7.mean <- rowMeans(NAA7[,-1])
NAA5.mean <- rowMeans(NAA5[,-1])
NAA4.mean <- rowMeans(NAA4[,-1])
Rec.mean <- rowMeans(Rec[,-1])
SSB.mean <- rowMeans(SSB[,-1])
# Pull out deterministic 
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East - 500 Sims - 2/Converged")
result.data <- as.data.frame(read.table(file = "BFTE2017_0_RESULTS.R",fill = T, col.names = 1:max(count.fields("BFTE2017_0_RESULTS.R"))))
NAA10.det.f <- result.data[79:120,11]
NAA10.det <- as.numeric(levels(NAA10.det.f))[NAA10.det.f]
NAA7.det.f <- result.data[79:120,8]
NAA7.det <- as.numeric(levels(NAA7.det.f))[NAA7.det.f]
NAA5.det.f <- result.data[79:120,6]
NAA5.det <- as.numeric(levels(NAA5.det.f))[NAA5.det.f]
NAA4.det.f <- result.data[79:120,5]
NAA4.det <- as.numeric(levels(NAA4.det.f))[NAA4.det.f]
Rec.det.f <- result.data[79:120,2]
Rec.det <- as.numeric(levels(Rec.det.f))[Rec.det.f]
SSB.det.f <- result.data[175:216,2]
SSB.det <- as.numeric(levels(SSB.det.f))[SSB.det.f]
# Make dfs
NAA10.df <- data.frame(cbind(1974:2015,NAA10.mean,NAA10.det))
NAA10_1 <- melt(NAA10.df, id.vars = "V1")
NAA7.df <- data.frame(cbind(1974:2015,NAA7.mean,NAA7.det))
NAA7_1 <- melt(NAA7.df, id.vars = "V1")
NAA5.df <- data.frame(cbind(1974:2015,NAA5.mean,NAA5.det))
NAA5_1 <- melt(NAA5.df, id.vars = "V1")
NAA4.df <- data.frame(cbind(1974:2015,NAA4.mean,NAA4.det))
NAA4_1 <- melt(NAA4.df, id.vars = "V1")
Rec.df <- data.frame(cbind(1974:2015,Rec.mean,Rec.det))
Rec_1 <- melt(Rec.df, id.vars = "V1")
# Plots
E.NAA10.bias <- ggplot(NAA10_1, aes(x=V1)) +
  geom_line(aes(y=value, linetype=variable), size=1) +
  labs(y="Age 10 abundance
       (numbers)",x="") +
  coord_cartesian(ylim = c(0,3000000)) +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position="None") +
  scale_y_continuous(label=scientific_format(digits = 2))
E.NAA7.bias <- ggplot(NAA7_1, aes(x=V1)) +
  geom_line(aes(y=value, linetype=variable), size=1.5) +
  labs(y="",x="") +
  coord_cartesian(ylim = c(0,800000)) +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position="None")
E.NAA5.bias <- ggplot(NAA5_1, aes(x=V1)) +
  geom_line(aes(y=value, linetype=variable), size=1) +
  labs(y="Age 5 abundance
       (numbers)",x="") +
  coord_cartesian(ylim = c(0,2000000)) +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position="None") +
  scale_y_continuous(breaks = seq(0,2000000,1000000), label=scientific_format(digits = 2))
E.NAA4.bias <- ggplot(NAA4_1, aes(x=V1)) +
  geom_line(aes(y=value, linetype=variable), size=1.5) +
  labs(y="",x="") +
  coord_cartesian(ylim = c(0,2000000)) +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position="None") +
  scale_y_continuous(breaks = seq(0,2000000,1000000), label=scientific_format(digits = 1))
E.Rec.bias <- ggplot(Rec_1, aes(x=V1)) +
  geom_line(aes(y=value, linetype=variable), size=1) +
  labs(y="Recruitment
       (numbers)",x="") +
  coord_cartesian(ylim = c(0,6000000)) +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position="None") +
  scale_y_continuous(breaks = c(0, 6000000, 3000000), label = scientific_format(digits = 2))
# relative bias
E.NAA10.rbias <- (NAA10.mean-NAA10.det)/NAA10.det*100
E.NAA7.rbias <- (NAA7.mean-NAA7.det)/NAA7.det*100
E.NAA5.rbias <- (NAA5.mean-NAA5.det)/NAA5.det*100
E.NAA4.rbias <- (NAA4.mean-NAA4.det)/NAA4.det*100
E.Rec.rbias <- (Rec.mean-Rec.det)/Rec.det*100
E.SSB.rbias <- (SSB.mean-SSB.det)/SSB.det*100



#boxplots (ages 10,7,4,1)
E.rel.bias.data <- data.frame(1974:2015,abs(E.NAA10.rbias),abs(E.NAA7.rbias),abs(E.NAA4.rbias),abs(E.Rec.rbias))
E.rel.bias.data_1 <- melt(E.rel.bias.data, id.vars = 1)
ggplot(E.rel.bias.data_1,aes(x=variable,y=value)) +
  geom_boxplot(fill="gray80") +
  labs(y="Relative difference (%)",x="") +
  theme_classic() +
  coord_cartesian(ylim = c(0,80)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 14),
        axis.text.x = element_text(family = "Times New Roman",
                                   face="bold",
                                   size=12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12)) +
  scale_x_discrete(labels = c("age 10", "age 7", "age 4", "age 1 (recruits)"))


ggplot(data=R.df_1,aes(x=variable, y=value)) +
  geom_boxplot(fill = "gray80") +
  coord_cartesian(ylim = c(-100,500)) +
  labs(y="Relative difference (%)",x="") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman")) +
  geom_hline(yintercept=0)

#boxplots (ages 10,5,1)
E.rel.bias.data2 <- data.frame(1974:2015,abs(E.NAA10.rbias),abs(E.NAA5.rbias),abs(E.Rec.rbias))
E.rel.bias.data_2 <- melt(E.rel.bias.data2, id.vars = 1)
ggplot(E.rel.bias.data_2,aes(x=variable,y=value)) +
  geom_boxplot(fill="gray80") +
  labs(y="Relative difference (%)",x="") +
  theme_classic() +
  coord_cartesian(ylim = c(0,80)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 14),
        axis.text.x = element_text(family = "Times New Roman",
                                   face="bold",
                                   size=12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12)) +
  scale_x_discrete(labels = c("age 10", "age 5", "age 1"))

# Plot line graphs next to boxplots, each row a different data: recruits, age 5, age 10, SSB #
#boxplot R
E.R.rel.bias.data <- data.frame(1974:2015,abs(E.Rec.rbias))
E.rel.bias.data_R <- melt(E.R.rel.bias.data, id.vars = 1)
E.R.boxplot <- ggplot(E.rel.bias.data_R,aes(x=variable,y=value)) +
  geom_boxplot(fill="gray80") +
  labs(y="Relative difference (%)",x="") +
  theme_classic() +
  coord_cartesian(ylim = c(0,80)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.ticks.x = element_blank())
#scale_x_discrete(labels = c("age 10", "age 7", "age 4", "age 1 (recruits)"))

#boxplot age 5
E.5.rel.bias.data <- data.frame(1974:2015,abs(E.NAA5.rbias))
E.rel.bias.data_5 <- melt(E.5.rel.bias.data, id.vars = 1)
E.NAA5.boxplot <- ggplot(E.rel.bias.data_5,aes(x=variable,y=value)) +
  geom_boxplot(fill="gray80") +
  labs(y="Relative difference (%)",x="") +
  theme_classic() +
  coord_cartesian(ylim = c(0,80)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.ticks.x = element_blank())

#boxplot age 10
E.10.rel.bias.data <- data.frame(1974:2015,abs(E.NAA10.rbias))
E.rel.bias.data_10 <- melt(E.10.rel.bias.data, id.vars = 1)
E.NAA10.boxplot <- ggplot(E.rel.bias.data_10,aes(x=variable,y=value)) +
  geom_boxplot(fill="gray80") +
  labs(y="Relative difference (%)",x="") +
  theme_classic() +
  coord_cartesian(ylim = c(0,80)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.ticks.x = element_blank())

#boxplot SSB
E.SSB.rel.bias.data <- data.frame(1974:2015,abs(E.SSB.rbias))
E.rel.bias.data_SSB <- melt(E.SSB.rel.bias.data, id.vars = 1)
E.SSB.boxplot <- ggplot(E.rel.bias.data_SSB,aes(x=variable,y=value)) +
  geom_boxplot(fill="gray80") +
  labs(y="Relative difference (%)",x="") +
  theme_classic() +
  coord_cartesian(ylim = c(0,80)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.ticks.x = element_blank())

#Layout
grid.arrange(E.Rec.bias, E.R.boxplot, 
             E.NAA5.bias, E.NAA5.boxplot, 
             E.NAA10.bias, E.NAA10.boxplot, 
             E.SSB.bias, E.SSB.boxplot,
             nrow=4, ncol=2, widths = c(5,2)) #layout_matrix = rbind(c(1,2))












#### Fcurrent/F0.1 ####


wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
nyr <- 42
stock <- 2 # switch east (1) vs. west (2)
if (stock == 1) #reference ages (from OM) and plus group age
{
  a.ref <- 4 #east
  A.ref <- 5 
  nage <- 10 
  alph <- "E"
} else { 
  a.ref <- 8 #west
  A.ref <-14 
  nage <- 16
  alph <- "W"
}



## Read in biological data ##

biolparm <- (as.matrix(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs/BFTBiolparm.csv"),header=T))
M <- array(biolparm[1:nage,2:3],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) 
waa <- array(biolparm[1:nage,4:5],c(nage,2),dimnames=list(age=1:nage,unit=1:2))



## Calculations ##

FF01 <- matrix(NA, nrow=3, ncol=1, dimnames=list(reference=c("Fcur", "F01", "Fcur/F01"), value=1))
FF01.bias <- matrix(NA, nrow=2, ncol=1, dimnames=list(bias=c("pop", "stock"), value=1))

setwd(wd)

## True F0.1 (from base case OM) ##
if (stock == 1) {  
  F01.OM.p <- 0.1606871 #east
  F01.OM.s <- 0.1064634 #east
} else {
  F01.OM.p <- 0.1141979 #west
  F01.OM.s <- 0.109491 #west
}

# True stock status (from base case OM)
F01.stat.true <- rep(NA,2)
if (stock == 1) {
  F01.stat.true[1] <- 0.119511074319948 #east pop
  F01.stat.true[2] <- 0.336578739314839 #east stock
} else {
  F01.stat.true[1] <- 0.293933398300163 #west pop
  F01.stat.true[2] <- 0.620620839  #west stock
}


for (i in runnums) 
{
  
  ## Read in Results files ##
  result.filename <- paste("BFT", alph, "2017_", i, "_RESULTS.R", sep="")
  
  result.file <- as.data.frame(read.table(file = result.filename,
                                          fill = T, col.names = 1:max(count.fields(
                                            result.filename
                                          ))))
  
  F.mat <- as.matrix(result.file[32:73, 2:(nage+1)], nrow=nyr, ncol=nage) #save F-at-age matrix
  F.mat <- apply(F.mat, c(1,2), as.numeric)
  
  ## Partial recruitment ##
  P.vpa <- array(NA, c(3,nage), dimnames=list(year=1:3,age=1:nage))
  for (y in (nrow(F.mat)-3):(nrow(F.mat)-1))
  {
    Ffull.vpa <- max(F.mat[y,])
    for (a in 1:nage)
    {
      P.vpa[y-38,a] <- F.mat[y,a]/Ffull.vpa
    }
  }
  
  # Average partial recruitment for each age over all reference years
  P.vpa.avg <- rep(NA,nage)
  for (a in 1:nage)
  {
    P.vpa.avg[a] <- mean(P.vpa[,a])
  }
  
  # Rescaled to 1
  P.vpa.fin <- rep(NA,nage)
  Ffull.vpa2 <- max(P.vpa.avg)
  for (a in 1:nage)
  {
    P.vpa.fin[a] <- P.vpa.avg[a]/Ffull.vpa2
  }
  
  
  ## Calculate F0.1 ##
  YPR <- ypr(age=seq(1,nage,1), wgt=waa[1:nage,stock], partial=P.vpa.fin, 
             M=M[1:nage,stock], plus=TRUE, oldest=nage, maxF=1.0, incrF=0.01, graph=FALSE)
  F01 <- YPR$Reference_Points[1,1]
  
  # Calculate F0.1 adjusted for the reference ages using the average partial recruitment for the reference ages
  F01.vpa <- F01 * mean(P.vpa.fin[a.ref:A.ref])
  
  
  ## Determine stock status ##
  
  # Calculate Fcurrent
  F.cur.vpa <- mean(F.mat[(nrow(F.mat)-3):(nrow(F.mat)-1),a.ref:A.ref])
  
  # Calculate Fcurrent/F0.1
  F01.status.vpa <- F.cur.vpa/F01.vpa
  
  FF01 <- cbind(FF01, c(F.cur.vpa, F01.vpa, F01.status.vpa))
  
  
  ## Calculate bias ##
  
  # Calculate bias in Fcurrent/F0.1 relative to "true" ratio from operating model
  F01.rel.bias.p <- (F01.status.vpa - as.numeric(F01.stat.true[1])) / as.numeric(F01.stat.true[1]) #from OM-P
  F01.rel.bias.s <- (F01.status.vpa - as.numeric(F01.stat.true[2])) / as.numeric(F01.stat.true[2]) #from OM-S
  
  FF01.bias <- cbind(FF01.bias, c(F01.rel.bias.p, F01.rel.bias.s))
  
}



## Save F0.1 results ##
colnames(FF01) <- c("x", runnums)
colnames(FF01.bias) <- c("x", runnums)
write.csv(FF01[,-1], "F01_results_converge.csv")
write.csv(FF01.bias[,-1], "F01_bias_converge.csv")


FF01.west <- FF01[,-1]
FF01.bias.west <- FF01.bias[,-1]
# FF01.east <- FF01[,-1]
# FF01.bias.east <- FF01.bias[,-1]



## Explore F0.1 results ##
sum(FF01.west[3,] < 1) / ncol(FF01.west)
sum(FF01.east[3,] < 1) / ncol(FF01.east)
summary(FF01.west[1,])
quantile(FF01.west[2,], 0.975)
summary(FF01.east[1,])
quantile(FF01.east[3,], 0.975)




## Plot F0.1 results ##

# Boxplots of Fcur/F0.1 for each stock
F01.data.w <- as.data.frame(FF01.west[3,])
F01.data.w <- cbind(rep("West", nrow(F01.data.w)), F01.data.w)
colnames(F01.data.w) <- c("stock", "ratio")
F01.data.e <- as.data.frame(FF01.east[3,])
F01.data.e <- cbind(rep("East", nrow(F01.data.e)), F01.data.e)
colnames(F01.data.e) <- c("stock", "ratio")
F01.data <- rbind(F01.data.w, F01.data.e)
true.df <- data.frame(c(0.119511074319948, 0.336578739314839, 0.293933398300163, 0.620620839)) #east pop, stock; west pop, stock
true.df2 <- cbind(c("east", "east", "west", "west"), c("pop", "stock", "pop", "stock"), true.df)
colnames(true.df2) <- c("stock", "view", "value")
F01.data$ratio <- as.numeric(as.character(F01.data$ratio))

West <- ggplot(data=subset(F01.data,stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F01.data,stock %in% c("West")), color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=0.293933398300163, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.620620839, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 4, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y="Fcurrent/F0.1", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   face = "bold",
                                   color = 1),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22))

East <- ggplot(data=subset(F01.data,stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F01.data,stock %in% c("East")), color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=0.119511074319948, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.336578739314839, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 4, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   face = "bold",
                                   color = 1),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())


# LAYOUT #

jpeg("C:/Users/mmorse1/Documents/Publishing/Revisions - Bluefin Tuna Simulations/ICES JMS Review/Figures/FvsF01.jpeg",
     width = 2000, height = 2000, units = "px", quality = 100, res = 300)
plot_grid(West, East, rel_widths = c(10,9))
dev.off()









#### END ####