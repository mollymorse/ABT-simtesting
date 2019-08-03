################################################################################################
############ BLUEFIN TUNA SIMULATIONS - CALCULATING REFERENCE POINTS (M.MORSE 2018) ############
################################################################################################

# DESCRIPTION
# This code is meant to calculate reference points on the VPA outputs of the simulations, i.e., 
# from existing Results files

# ** Contact M.Morse for questions


############################################ F0.1s #############################################



## Load packages ##

library(fishmethods)
library(grid)
library(cowplot)
library(ggplot2)
library(extrafont)


#### True F0.1s from OM ####

## Population ##
# read numbers for calculating true population fishing mortality array
biolparm <- as.matrix(read.csv("C:/Users/mmorse1/Documents/Simulations_lomov/R Code + Inputs/BFTBiolparm.csv"), header = T)
M        <- array(biolparm[1:nage,2:3],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) #annualized M
waa      <- array(biolparm[1:nage,4:5],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) #weight-at-age
naa      <- as.matrix(read.csv("C:/Users/mmorse1/Documents/Simulations_lomov/OM_output/naa.csv"), header = T)[, -1] %>%
  array(dim = c(42, 29, 4, 7, 2), dimnames = list(year = 1974:2015, age = 1:29, quarter = 1:4, zone = 1:7, unit = 1:2))
naa_2 <- array(NA, dim = c(42, 29, 4, 2), dimnames = list(year = 1974:2015, age = 1:29, quarter = 1:4, unit = 1:2))
for (y in 1:42)
  for (a in 1:29)
    for (q in 1:4)
      for (u in 1:2) {
        naa_2[y, a, q, u] <- sum(naa[y, a, q, 1:7, u])  #calculate total naa (sum over zones)
      }  

Fa.p <- array(NA, c(41, 28, 2), dimnames = list(year = 1974:2014, age = 1:28, unit = 1:2)) 
for (y in 1:41)
  for (a in 1:28)
    for (u in 1:2) {
      Fa.p[y, a, u] <- log(naa_2[y, a, 3, u] / naa_2[y + 1, a + 1, 3, u]) - M[a, u] #calculate population F at year, age
    }


# calculate partial recruitment - population
P_om_e <- array(NA, c(3, 10), dimnames = list(year = 1:3, age = 1:10))
P_om_w <- array(NA, c(3, 16), dimnames = list(year = 1:3, age = 1:16))

#east
for (y in (nrow(Fa.p) - 2):(nrow(Fa.p)))
  for (u in 1) {
    Ffull_om <- max(Fa.p[y, , u])
    for (a in 1:10) {
      P_om_e[y - 38, a] <- Fa.p[y, a, u]/Ffull_om
    }
  }

#west
for (y in (nrow(Fa.p) - 2):(nrow(Fa.p)))
  for (u in 2) {
    Ffull_om <- max(Fa.p[y, , u])
    for (a in 1:16) {
      P_om_w[y - 38, a] <- Fa.p[y, a, u]/Ffull_om
    }
  }


# average partial recruitment for each age over all reference years
P_om_avg_e <- rep(NA, 10)
P_om_avg_w <- rep(NA, 16)

#east
for (a in 1:10) {
  P_om_avg_e[a] <- mean(P_om_e[, a])
}

#west
for (a in 1:16) {
  P_om_avg_w[a] <- mean(P_om_w[, a])
}


# scaled to 1 (maximum partial recruitment)
P_om_fin_e <- rep(NA, 10)
P_om_fin_w <- rep(NA, 16)

#east
for (a in 1:10)
{
  P_om_fin_e[a] <- P_om_avg_e[a]/max(P_om_avg_e)
}

#west
for (a in 1:16)
{
  P_om_fin_w[a] <- P_om_avg_w[a]/max(P_om_avg_w)
}



## Stock ##
# read in true fishing mortality array - stock 
Fa.s <- as.matrix(read.csv("C:/Users/mmorse1/Documents/Simulations_lomov/OM_output/Fa.csv", header = T))[, -1] %>%
  array(dim = c(42, 29, 4, 7), dimnames = list(year = 1974:2015, age = 1:29, quarter = 1:4, zone = 1:7))
Fa.s2 <- array(NA, dim = c(42, 29, 2), dimnames = list(year = 1974:2015, age = 1:29, stock = 1:2))
for (y in 1:42)
  for (a in 1:29){
    Fa.s2[y, a, 1] <- sum(Fa.s[y, a, 3, 4:7])
    Fa.s2[y, a, 2] <- sum(Fa.s[y, a, 3, 1:3])
  }
    

# calculate partial recruitment - stock
P_oms_e <- array(NA, c(3, 10), dimnames = list(year = 1:3, age = 1:10))
P_oms_w <- array(NA, c(3, 16), dimnames = list(year = 1:3, age = 1:16))

#east
for (y in (nrow(Fa.s2) - 3):(nrow(Fa.s2) - 1))
  for (s in 1) {
    Ffull_om <- max(Fa.s2[y, , s])
    for (a in 1:10) {
      P_oms_e[y - 38, a] <- Fa.s2[y, a, s]/Ffull_om
    }
  }

#west
for (y in (nrow(Fa.s2) - 3):(nrow(Fa.s2) - 1))
  for (s in 2) {
    Ffull_om <- max(Fa.s2[y, , s])
    for (a in 1:16) {
      P_oms_w[y - 38, a] <- Fa.s2[y, a, s]/Ffull_om
    }
  }


# average partial recruitment for each age over all reference years - stock
P_oms_avg_e <- rep(NA, 10)
P_oms_avg_w <- rep(NA, 16)

#east
for (a in 1:10) {
  P_oms_avg_e[a] <- mean(P_oms_e[, a])
}

#west
for (a in 1:16) {
  P_oms_avg_w[a] <- mean(P_oms_w[, a])
}


# scaled to 1 (maximum partial recruitment) - stock
P_oms_fin_e <- rep(NA, 10)
P_oms_fin_w <- rep(NA, 16)

#east
for (a in 1:10)
{
  P_oms_fin_e[a] <- P_oms_avg_e[a]/max(P_oms_avg_e)
}

#west
for (a in 1:16)
{
  P_oms_fin_w[a] <- P_oms_avg_w[a]/max(P_oms_avg_w)
}


## Calculate true OM F0.1 ##
F01 <- array(NA, c(2, 2), dimnames = list(type = c("pop", "stock"), unit = c("east", "west")))

YPR <- ypr(age = seq(1, 10, 1), wgt = waa[1:10, 1], partial = P_om_fin_e, 
    M = M[1:10, 1], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
F01[1, 1] <- YPR$Reference_Points[1,1]

YPR <- ypr(age=seq(1, 16, 1), wgt = waa[1:16, 2], partial = P_om_fin_w, 
           M = M[1:16, 2], plus = FALSE, oldest = 16, maxF = 1.0, incrF = 0.01, graph = FALSE)
F01[1, 2] <- YPR$Reference_Points[1,1]















# Calculate F0.1 adjusted for the reference ages using the average partial recruitment for the reference ages
F01.vpa <- F01 * mean(P.vpa.fin[a.ref:A.ref])


## Determine stock status ##

# Calculate Fcurrent
F.cur.vpa <- mean(F.mat[(nrow(F.mat)-3):(nrow(F.mat)-1),a.ref:A.ref])

# Calculate Fcurrent/F0.1
F01.status.vpa <- F.cur.vpa/F01.vpa

FF01 <- cbind(FF01, c(F.cur.vpa, F01.vpa, F01.status.vpa))














#### Estimated F01s from VPA ####

## Define variables ##

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

West <- ggplot(data=subset(F01.data,stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_rect(fill = "palegreen2", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, alpha = 0.01) +
  geom_rect(fill = "indianred2", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, alpha = 0.01) +
  geom_boxplot(data=subset(F01.data,stock %in% c("West")), color="black", fill="slategray1") +
  geom_abline(intercept=0.293933398300163, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.620620839, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y="Fcurrent/F0.1", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))
East <- ggplot(data=subset(F01.data,stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_rect(fill = "palegreen2", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, alpha = 0.01) +
  geom_rect(fill = "indianred2", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, alpha = 0.01) +
  geom_boxplot(data=subset(F01.data,stock %in% c("East")), color="black", fill="slategray1") +
  geom_abline(intercept=0.119511074319948, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.336578739314839, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

plot_grid(West, East, rel_widths = c(10,9))


# Boxplots of bias in Fcur/F0.1 for each stock relative to both the OM population and stock views

temp <- cbind(c("population", "stock"), FF01.bias.east)
temp2 <- melt(temp, id.vars="V1") 
temp2 <- cbind(rep("east",nrow(temp2)), temp2)
colnames(temp2) <- c("Stock", "View", "Run", "Value")
temp3 <- cbind(c("population", "stock"), FF01.bias.west)
temp4 <- melt(temp3, id.vars="V1")
temp4 <- cbind(rep("west",nrow(temp4)), temp4)
colnames(temp4) <- c("Stock", "View", "Run", "Value")
temp5 <- rbind(temp2[-2:-1,], temp4[-2:-1,])

temp6 <- temp5[temp5$Stock=="east",]
temp6[,4] <- as.numeric(levels(temp6[,4]))[temp6[,4]]
temp7 <- temp5[temp5$Stock=="west",]
temp7[,4] <- as.numeric(levels(temp7[,4]))[temp7[,4]]

# east
ggplot(data=temp6, aes(x=View, y=Value)) +
  geom_boxplot(data=temp6, color="black", fill="gray80") +
  theme_classic() +
  labs(y="Relative bias in Fcurrent/F0.1", title = "East") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 17),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

# west
ggplot(data=temp7, aes(x=View, y=Value)) +
  geom_boxplot(data=temp7, color="black", fill="gray80") +
  theme_classic() +
  labs(y="Relative bias in Fcurrent/F0.1", title="West") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 17),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))



## Plot F01 results ##

F01.data.e <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/East/E_F01_results.csv", header = TRUE)
F01.data.e <- t(F01.data.e[-2,-1])
F01.data.e <- cbind(rep("East", nrow(F01.data.e)), F01.data.e)
F01.data.e <- data.frame(F01.data.e, stringsAsFactors = TRUE)
colnames(F01.data.e) <- c("stock", "ratio")
F01.data.e$ratio <- as.numeric(levels(F01.data.e$ratio))[F01.data.e$ratio]

F01.data.w <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/West/W_F01_results.csv", header = TRUE)
F01.data.w <- t(F01.data.w[-2,-1])
F01.data.w <- cbind(rep("West", nrow(F01.data.w)), F01.data.w)
F01.data.w <- data.frame(F01.data.w, stringsAsFactors = TRUE)
colnames(F01.data.w) <- c("stock", "ratio")
F01.data.w$ratio <- as.numeric(levels(F01.data.w$ratio))[F01.data.w$ratio]
F01.data <- rbind(F01.data.w, F01.data.e)

WestF01 <- ggplot(data=subset(F01.data, stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F01.data, stock %in% c("West")), color="black", fill="gray80") +
  geom_abline(intercept=0.293933398, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.620620839, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y="Fcurrent/F0.1", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

EastF01 <- ggplot(data=subset(F01.data,stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F01.data,stock %in% c("East")), color="black", fill="gray80") +
  geom_abline(intercept=0.119511074, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.336578739, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())


## Plot F30% results ##

F30.data.e <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/East/E_F30_results.csv", header = TRUE)
F30.data.e <- t(F30.data.e[-2,-1])
F30.data.e <- cbind(rep("East", nrow(F30.data.e)), F30.data.e)
F30.data.e <- data.frame(F30.data.e, stringsAsFactors = TRUE)
colnames(F30.data.e) <- c("stock", "ratio")
F30.data.e$ratio <- as.numeric(levels(F30.data.e$ratio))[F30.data.e$ratio]

F30.data.w <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/West/W_F30_results.csv", header = TRUE)
F30.data.w <- t(F30.data.w[-2,-1])
F30.data.w <- cbind(rep("West", nrow(F30.data.w)), F30.data.w)
F30.data.w <- data.frame(F30.data.w, stringsAsFactors = TRUE)
colnames(F30.data.w) <- c("stock", "ratio")
F30.data.w$ratio <- as.numeric(levels(F30.data.w$ratio))[F30.data.w$ratio]
F30.data <- rbind(F30.data.w, F30.data.e)

WestF30 <- ggplot(data=subset(F30.data, stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F30.data, stock %in% c("West")), color="black", fill="gray80") +
  geom_abline(intercept=0.286858491, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.580671883, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y="Fcurrent/F30%", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

EastF30 <- ggplot(data=subset(F30.data,stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F30.data,stock %in% c("East")), color="black", fill="gray80") +
  geom_abline(intercept=0.093669266, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.234599375, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

plot_grid(West, East, rel_widths = c(10,9))


## Plot F40% results ##

F40.data.e <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/East/E_F40_results.csv", header = TRUE)
F40.data.e <- t(F40.data.e[-2,-1])
F40.data.e <- cbind(rep("East", nrow(F40.data.e)), F40.data.e)
F40.data.e <- data.frame(F40.data.e, stringsAsFactors = TRUE)
colnames(F40.data.e) <- c("stock", "ratio")
F40.data.e$ratio <- as.numeric(levels(F40.data.e$ratio))[F40.data.e$ratio]

F40.data.w <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/West/W_F40_results.csv", header = TRUE)
F40.data.w <- t(F40.data.w[-2,-1])
F40.data.w <- cbind(rep("West", nrow(F40.data.w)), F40.data.w)
F40.data.w <- data.frame(F40.data.w, stringsAsFactors = TRUE)
colnames(F40.data.w) <- c("stock", "ratio")
F40.data.w$ratio <- as.numeric(levels(F40.data.w$ratio))[F40.data.w$ratio]
F40.data <- rbind(F40.data.w, F40.data.e)

WestF40 <- ggplot(data=subset(F40.data, stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F40.data, stock %in% c("West")), color="black", fill="gray80") +
  geom_abline(intercept=0.385412298, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.787508873, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y="Fcurrent/F40%", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

EastF40 <- ggplot(data=subset(F40.data,stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F40.data,stock %in% c("East")), color="black", fill="gray80") +
  geom_abline(intercept=0.127702643, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.327783599, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

## Plot Fmax results ##

Fmax.data.e <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/East/E_Fmax_results.csv", header = TRUE)
Fmax.data.e <- t(Fmax.data.e[-2,-1])
Fmax.data.e <- cbind(rep("East", nrow(Fmax.data.e)), Fmax.data.e)
Fmax.data.e <- data.frame(Fmax.data.e, stringsAsFactors = TRUE)
colnames(Fmax.data.e) <- c("stock", "ratio")
Fmax.data.e$ratio <- as.numeric(levels(Fmax.data.e$ratio))[Fmax.data.e$ratio]

Fmax.data.w <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/West/W_Fmax_results.csv", header = TRUE)
Fmax.data.w <- t(Fmax.data.w[-2,-1])
Fmax.data.w <- cbind(rep("West", nrow(Fmax.data.w)), Fmax.data.w)
Fmax.data.w <- data.frame(Fmax.data.w, stringsAsFactors = TRUE)
colnames(Fmax.data.w) <- c("stock", "ratio")
Fmax.data.w$ratio <- as.numeric(levels(Fmax.data.w$ratio))[Fmax.data.w$ratio]
Fmax.data <- rbind(Fmax.data.w, Fmax.data.e)

WestFmax <- ggplot(data=subset(Fmax.data, stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(Fmax.data, stock %in% c("West")), color="black", fill="gray80") +
  geom_abline(intercept=0.195764236, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.385463724, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y="Fcurrent/Fmax", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

EastFmax <- ggplot(data=subset(Fmax.data,stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(Fmax.data,stock %in% c("East")), color="black", fill="gray80") +
  geom_abline(intercept=0.079596243, slope=0, linetype=1, size=1) +
  geom_abline(intercept=0.220222417, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,3.5)) +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

## Plot SSB01 results ##

SSB01.data.e <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/East/E_SSB01_results.csv", header = TRUE)
SSB01.data.e <- t(SSB01.data.e[-2,-1])
SSB01.data.e <- cbind(rep("East", nrow(SSB01.data.e)), SSB01.data.e)
SSB01.data.e <- data.frame(SSB01.data.e, stringsAsFactors = TRUE)
colnames(SSB01.data.e) <- c("stock", "ratio")
SSB01.data.e$ratio <- as.numeric(levels(SSB01.data.e$ratio))[SSB01.data.e$ratio]

SSB01.data.w <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/West/W_SSB01_results.csv", header = TRUE)
SSB01.data.w <- t(SSB01.data.w[-2,-1])
SSB01.data.w <- cbind(rep("West", nrow(SSB01.data.w)), SSB01.data.w)
SSB01.data.w <- data.frame(SSB01.data.w, stringsAsFactors = TRUE)
colnames(SSB01.data.w) <- c("stock", "ratio")
SSB01.data.w$ratio <- as.numeric(levels(SSB01.data.w$ratio))[SSB01.data.w$ratio]
SSB01.data <- rbind(SSB01.data.w, SSB01.data.e)

WestSSB01 <- ggplot(data=subset(SSB01.data, stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(SSB01.data, stock %in% c("West")), color="black", fill="gray80") +
  geom_abline(intercept=1.439817273, slope=0, linetype=1, size=1) +
  geom_abline(intercept=5.157401233, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,10)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y="SSBcurrent/SSB0.1", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

EastSSB01 <- ggplot(data=subset(SSB01.data,stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(SSB01.data,stock %in% c("East")), color="black", fill="gray80") +
  geom_abline(intercept=2.032496893, slope=0, linetype=1, size=1) +
  geom_abline(intercept=1.751787589, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,10)) +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

## Plot SSBfmax results ##

SSBfmax.data.e <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/East/E_SSBfmax_results.csv", header = TRUE)
SSBfmax.data.e <- t(SSBfmax.data.e[-2,-1])
SSBfmax.data.e <- cbind(rep("East", nrow(SSBfmax.data.e)), SSBfmax.data.e)
SSBfmax.data.e <- data.frame(SSBfmax.data.e, stringsAsFactors = TRUE)
colnames(SSBfmax.data.e) <- c("stock", "ratio")
SSBfmax.data.e$ratio <- as.numeric(levels(SSBfmax.data.e$ratio))[SSBfmax.data.e$ratio]

SSBfmax.data.w <- read.csv("C:/Users/mmorse1/Documents/BFT-MSE/West/W_SSBfmax_results.csv", header = TRUE)
SSBfmax.data.w <- t(SSBfmax.data.w[-2,-1])
SSBfmax.data.w <- cbind(rep("West", nrow(SSBfmax.data.w)), SSBfmax.data.w)
SSBfmax.data.w <- data.frame(SSBfmax.data.w, stringsAsFactors = TRUE)
colnames(SSBfmax.data.w) <- c("stock", "ratio")
SSBfmax.data.w$ratio <- as.numeric(levels(SSBfmax.data.w$ratio))[SSBfmax.data.w$ratio]
SSBfmax.data <- rbind(SSBfmax.data.w, SSBfmax.data.e)

WestSSBfmax <- ggplot(data=subset(SSBfmax.data, stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(SSBfmax.data, stock %in% c("West")), color="black", fill="gray80") +
  geom_abline(intercept=2.378549906, slope=0, linetype=1, size=1) +
  geom_abline(intercept=8.923472479, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,10)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y="SSBcurrent/SSBfmax", x="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

EastSSBfmax <- ggplot(data=subset(SSBfmax.data,stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(SSBfmax.data,stock %in% c("East")), color="black", fill="gray80") +
  geom_abline(intercept=3.025364472, slope=0, linetype=1, size=1) +
  geom_abline(intercept=2.409602629, slope=0, linetype=2, size=1) +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,10)) +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  labs(y="", x="") +
  theme(axis.title.y = element_blank(),
        #axis.title.x = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 15),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

plot_grid(WestF01, EastF01, WestF30, EastF30, WestF40, EastF40,
          WestFmax, EastFmax, WestSSB01, EastSSB01, WestSSBfmax, EastSSBfmax#, 
          #rel_widths = c(10,9)
          )
