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
yrs <- 1974:2015
par(family = "serif", font.lab = 2)

#### Revised plots for eastern simulations ####

## 100 Runs ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 100 Sims - 1")
#setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_4/East")
nsims <- 100
# SSB #
BFTE.100.SSB <- read.csv("E_SSB_data.csv")
par(mar = c(5.1, 5.1, 4.1, 2.1))
matplot(head(yrs,1):tail(yrs,1), (BFTE.100.SSB[,-c(1,105)]/1000), type = "n", col = c(rep("gray65",nsims),1,1,1), 
                   lty = c(rep(1,nsims),2,3,1), lwd = c(rep(1,nsims),3,3,3), ylim=c(0, 1100), xlab = "", ylab = "SSB ('000 tonnes)", xaxs="i", yaxs="i",
                   las = 1, mgp = c(3,0.5,1), lab = c(4,4,7), yaxt = "n", xaxt = "n", cex.lab = 1.2)
axis(side=1, col = 0, mgp = c(3,0.3,1))
axis(side=2, col = 0, las = 1, at = c(200, 600, 1000), mgp = c(3,0.5,1))
box(which = "plot", col = "gray90")
grid(col = "gray90", lty = 1)
matplot(head(yrs,1):tail(yrs,1), (BFTE.100.SSB[,-c(1,105)]/1000), type = "l", col = c(rep("gray65",nsims),1,1,1), 
        lty = c(rep(1,nsims),2,3,1), lwd = c(rep(1,nsims),3,3,3), ylim=c(0, 1100), xlab = "", ylab = "SSB ('000 tonnes)", xaxs="i", yaxs="i",
        las = 1, mgp = c(3,0.5,1), lab = c(4,4,7), yaxt = "n", xaxt = "n", add = TRUE,  cex.lab = 1.2)
legend(x = 1975, y = 1050, legend = c("stochastic runs", "stochastic mean", "95% CI", "operating model population", "operating model stock"),
       col = c("gray65",1,1,1,1), lty = c(1,1,3,2,3), lwd = c(1,3,2,3,3), cex = 0.7)

# SSB CI #
BFTE.100.SSBCI <- read.csv("E_SSBCI_data.csv")
matplot(head(yrs,1):tail(yrs,1), (BFTE.100.SSBCI[,-c(1,7)]/1000), type = "l", col = c(1,1,1,1,1), lty = c(1,3,3,2,3),
        lwd = c(3,2,2,3,3), ylim=c(0, 1100), xlab = "", ylab = "SSB ('000 tonnes)", xaxs="i", yaxs = "i",
        add = TRUE,  las = 1, mgp = c(3,0.5,1), lab = c(4,4,7), yaxt = "n", xaxt = "n", cex.lab = 1.2)
legend(x = 1975, y = 1090000, legend = c("VPA simulations mean", "VPA simulations 95% CI", "VPA deterministic", "operating model population", "operating model stock"),
       col = c(rep(1,5)), lty = c(1,3,1,1,3), lwd = c(2,1,2,3,3), cex = 0.8)

# R #
BFTE.100.R <- read.csv("E_R_data.csv")
par(mar = c(5.1, 5.1, 4.1, 2.1))
matplot(head(yrs,1):tail(yrs,1), BFTE.100.R[,-1], type = "l", lty = 1, col = c(rep(brewer.pal(5,"Set3"),(nsims/5)),1,2,4),
        lwd = c(rep(1,nsims),3,2,2), ylim=c(0, 10000000), xlab = "Year", ylab = "Recruits (n)", main = "East VPA Testing - Recruitment", xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 9700000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model"),  col = c("indianred1",2,4,1),
       lty = 1, lwd = c(1,2,2,3), cex = 0.7)

# R CI #
BFTE.100.RCI <- read.csv("E_RCI_data.csv")
par(mar = c(5.1, 5.1, 4.1, 2.1))
matplot(head(yrs,1):tail(yrs,1), BFTE.100.RCI[,-1], type = "l", col = c(2,2,2,1,4), lty = c(1,3,3,1,1),
        lwd = c(2,1,1,3,2), ylim=c(0, 10000000), xlab = "Year", ylab = "Recruits (n)", main = "East", xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 9700000, legend = c("VPA simulations mean", "VPA simulations 95% CI", "VPA deterministic", "operating model"), 
       col = c(2,2,4,1), lty = c(1,3,1,1), lwd = c(2,1,2,3), cex = 0.8)

# NAA-9 #
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 100 Sims")
BFTE.100.NAA9 <- read.csv("E_NAA9_data.csv")
par(mar = c(5.1, 5.1, 4.1, 2.1))
matplot(head(yrs,1):tail(yrs,1), BFTE.100.NAA9[,-1], type = "l", col = c(rep(brewer.pal(5,"Set3"),(nsims/5)),1,1,2,4), lty = c(rep(1,nsims),1,3,1,1),
        lwd = c(rep(1,nsims),3,3,2,2), ylim=c(0, 800000), xlab = "Year", ylab = "Age-9 abundance", main = "East", xaxs="i", yaxs="i")
legend(x = 1975, y = 750000, legend = c("VPA simulations", "VPA simulations mean", "VPA deterministic", "operating model population", "operating model stock"),
       col = c(2,2,4,1,1), lty = c(1,1,1,1,3), lwd = c(1,2,2,3,3), cex = 0.8)

## 500 Runs ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims")
nsims <- 500
# SSB #
BFTE.500.SSB <- read.csv("E_SSB_data.csv")
BFTW.500.SSB.t <- t(BFTE.500.SSB[,2:501])
colnames(BFTE.500.SSB.t) <- 1974:2015
boxplot.matrix((BFTE.500.SSB.t/1000), use.cols = TRUE, ylim=c(0,1100), las = 1, col = "lightblue", ylab = "SSB (mt)",
               outpch = 21, outcex = .5, outcol = "black", outbg = "lightblue")
matplot(1974:2015, (BFTE.500.SSB[,502:503]/1000), type = "b", col = c(1,1), 
        lty = c(2,3), lwd = c(3,3), ylim=c(0, 1100), xlab = "", ylab = "SSB ('000 tonnes)", xaxs="i", yaxs="i",
        las = 1, mgp = c(3,0.5,1), lab = c(4,4,7), yaxt = "n", xaxt = "n", cex.lab = 1.2, add = TRUE)
axis(side=1, col = 0, mgp = c(3,0.3,1))
axis(side=2, col = 0, las = 1, at = c(200, 600, 1000), mgp = c(3,0.5,1))
box(which = "plot", col = "gray90")
grid(col = "gray90", lty = 1)

#### Revised plots for western simulations ####

## 100 Runs ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 100 Sims - 1")
nsims <- 100
# SSB #
BFTW.100.SSB <- read.csv("W_SSB_data.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.100.SSB[,-1], type = "l", col = c(rep(brewer.pal(5,"Set3"),(nsims/5)),1,1,2,4), lty = c(rep(1,nsims),1,3,1,1),
        lwd = c(rep(1,nsims),3,3,2,2), ylim=c(0, 150000), xlab = "Year", ylab = "SSB (mt)", main = "West VPA Testing - SSB",  xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 145000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model population", "operating model stock"),
       col = c(2,2,4,1,1), lty = c(1,1,1,1,3), lwd = c(1,2,2,3,3), cex = 0.7)

# SSB CI #
BFTW.100.SSBCI <- read.csv("W_SSBCI_data.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.100.SSBCI[,-1], type = "l", col = c(2,2,2,1,1,4), lty = c(1,3,3,1,3,1),
        lwd = c(2,1,1,3,3,2), ylim=c(0, 200000), xlab = "Year", ylab = "SSB (mt)", main = "West",xaxs="i", yaxs = "i")
legend(x = 1975, y = 190000, legend = c("VPA simulations mean", "VPA simulations 95% CI", "VPA deterministic", "operating model population", "operating model stock"),
       col = c(2,2,4,1,1), lty = c(1,3,1,1,3), lwd = c(2,1,2,3,3), cex = 0.8)

# R #
BFTW.100.R <- read.csv("W_R_data.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.100.R[,-1], type = "l", ylim=c(0, 4000000), col = c(rep(brewer.pal(5,"Set3"),(nsims/5)),1,2,4), lty = 1,
        lwd = c(rep(1,nsims),3,2,2), xlab = "Year", ylab = "Recruits (n)", main = "West", xaxs="i", yaxs = "i")
legend(x = 1975, y = 3900000, legend = c("VPA simulations", "VPA simulations mean", "VPA deterministic", "operating model"),
       col = c(2,2,4,1), lty = 1, lwd = c(1,2,2,3), cex = 0.8)

# R CI #
BFTW.100.RCI <- read.csv("W_RCI_data.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.100.RCI[,-1], type = "l", col = c(2,2,2,1,4), lty = c(1,3,3,1,1),
        lwd = c(2,1,1,3,2), ylim=c(0, 4000000), xlab = "Year", ylab = "Recruits (n)", main = "West", xaxs="i", yaxs = "i")
legend(x = 1975, y = 3900000, legend = c("VPA simulations mean", "VPA simulations 95% CI", "VPA deterministic", "operating model"),
       col = c(2,2,4,1), lty = c(1,3,1,1), lwd = c(2,1,2,3), cex = 0.8)

## 500 Runs ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims")
nsims <- 500
# SSB time series #
BFTW.500.SSB <- read.csv("W_SSB_data.csv")
par(mar = c(5.1, 5.1, 4.1, 2.1))
# matplot(head(yrs,1):tail(yrs,1), BFTW.500.SSB[,-1], type = "l", col = c(rep(brewer.pal(5,"Set3"),(nsims/5)),1,1,2,4), lty = c(rep(1,nsims),1,3,1,1),
#         lwd = c(rep(1,nsims),3,3,2,2), ylim=c(0, 200000), xlab = "Year", ylab = "SSB (mt)", main = "West",  xaxs="i", yaxs = "i",
#         las = 1, mgp = c(4,1,0))
matplot(head(yrs,1):tail(yrs,1), BFTW.500.SSB[,-1], type = "l", col = c(rep("orchid1",nsims),1,1,2,4), lty = c(rep(1,nsims),1,3,1,1),
        lwd = c(rep(1,nsims),3,3,2,2), ylim=c(0, 150000), xlab = "Year", ylab = "SSB (mt)", main = "West - 500 runs",  xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 145000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model population", "operating model stock"),
       col = c("orchid1",2,4,1,1), lty = c(1,1,1,1,3), lwd = c(1,2,2,3,3), cex = 0.8)

# SSB boxplots #
BFTW.500.SSB.sims <- t(BFTW.500.SSB[,2:501])
colnames(BFTW.500.SSB.sims) <- 1974:2015
boxplot.matrix(BFTW.500.SSB.sims, use.cols = TRUE, ylim=c(0,200000), las = 1, col = "lightblue", ylab = "SSB (mt)",
               outpch = 21, outcex = .5, outcol = "black", outbg = "lightblue")

# SSB CI time series #
BFTW.500.SSBCI <- read.csv("W_SSBCI_data.csv")
par(mar = c(5.1, 5.1, 4.1, 2.1))
matplot(head(yrs,1):tail(yrs,1), BFTW.500.SSBCI[,-1], type = "l", col = c(2,2,2,1,1,4), lty = c(1,3,3,1,3,1),
        lwd = c(2,1,1,3,3,2), ylim=c(0, 200000), xlab = "Year", ylab = "SSB (mt)", main = "West",xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 190000, legend = c("VPA simulations mean", "VPA simulations 95% CI", "VPA deterministic", "operating model population", "operating model stock"),
       col = c(2,2,4,1,1), lty = c(1,3,1,1,3), lwd = c(2,1,2,3,3), cex = 0.8)

# R #
BFTW.500.R <- read.csv("W_R_data.csv")
par(mar = c(5.5, 5.5, 4.1, 2.1))
# matplot(head(yrs,1):tail(yrs,1), BFTW.500.R[,-1], type = "l", ylim=c(0, 3000000), col = c(rep(brewer.pal(5,"Set3"),(nsims/5)),1,2,4), lty = 1,
#         lwd = c(rep(1,nsims),3,2,2), xlab = "Year", ylab = "Recruits (n)", main = "West", xaxs="i", yaxs = "i",
#         las = 1, mgp = c(4.5,1,0))
matplot(head(yrs,1):tail(yrs,1), BFTW.500.R[,-1], type = "l", ylim=c(0, 2500000), col = c(rep("orchid1",nsims),1,2,4), lty = 1,
        lwd = c(rep(1,nsims),3,2,2), xlab = "Year", ylab = "Recruits (n)", main = "West", xaxs="i", yaxs = "i",
        las = 1, mgp = c(4.5,1,0))
legend(x = 1975, y = 2400000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model"),
       col = c("orchid1",2,4,1), lty = 1, lwd = c(1,2,2,3), cex = 0.8)

# R CI #
BFTW.500.RCI <- read.csv("W_RCI_data.csv")
par(mar = c(5.1, 5.1, 4.1, 2.1))
matplot(head(yrs,1):tail(yrs,1), BFTW.500.RCI[,-1], type = "l", col = c(2,2,2,1,4), lty = c(1,3,3,1,1),
        lwd = c(2,1,1,3,2), ylim=c(0, 3000000), xlab = "Year", ylab = "Recruits (n)", main = "West", xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 2900000, legend = c("VPA simulations mean", "VPA simulations 95% CI", "VPA deterministic", "operating model"),
       col = c(2,2,4,1), lty = c(1,3,1,1), lwd = c(2,1,2,3), cex = 0.8)

## West SSB plot + pull out data ##
SSB.data <- matrix(NA, nrow=nyr, ncol=31)
for (i in 0:30) {
  pastewd <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - CAA CV=.86/Run", i, sep="")
  setwd(pastewd)
  result_file_name <- paste("BFTW2017_", i, "_RESULTS.R", sep="")
  Results <- as.data.frame(read.table(file = result_file_name,
                                      fill = T, col.names = 1:max(count.fields(
                                        result_file_name
                                      ))))
  
  # SSB from VPA
  SSB.res <- as.matrix(Results[175:216, 2], nrow = nyr, ncol = 1) # save SSB results
  SSB.data[,i+1] <- apply(SSB.res, c(1,2), as.numeric) # Convert SSB values to numeric
}
SSB.data <- cbind(SSB.data, rowMeans(SSB.data[,2:31]))
write.csv(SSB.data, file = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - CAA CV=.86/W_SSB_data.csv")
matplot(head(yrs,1):tail(yrs,1), SSB.data, type = "l", col = c(4,rep(brewer.pal(5,"Set3"),(30/5)),2), lty = 1,
        lwd = c(2,rep(1,30),2), ylim=c(0, 200000), xlab = "Year", ylab = "SSB (mt)", main = "West",  xaxs="i", yaxs = "i")
legend(x = 1975, y = 190000, legend = c("VPA simulations", "VPA simulations mean", "VPA deterministic"),
       col = c(2,2,4), lty = 1, lwd = c(1,2,2), cex = 0.8)

## West fix variance scaling pars to ICCAT (2017) ##
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - convergence settings stoch")
nsims <- 50
# SSB #
BFTW.50.SSB <- read.csv("W_SSB_data.csv")
par(mar = c(5.1, 5.1, 4.1, 2.1))
matplot(head(yrs,1):tail(yrs,1), BFTW.50.SSB[,-1], type = "l", col = c(rep("orchid1",nsims),1,1,2,4), lty = c(rep(1,nsims),1,3,1,1),
        lwd = c(rep(1,nsims),3,3,2,2), ylim=c(0, 150000), xlab = "Year", ylab = "SSB (mt)", main = "",  xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0), lab = c(4,4,7))
legend(x = 1975, y = 145000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model population", "operating model stock"),
       col = c("orchid1",2,4,1,1), lty = c(1,1,1,1,3), lwd = c(1,2,2,3,3), cex = 0.7)

# R #
BFTW.50.R <- read.csv("W_R_data.csv")
par(mar = c(6, 6, 4, 2))
matplot(head(yrs,1):tail(yrs,1), BFTW.50.R[,-1], type = "l", ylim=c(0, 2500000), col = c(rep("orchid1",nsims),1,2,4), lty = 1,
        lwd = c(rep(1,nsims),3,2,2), xlab = "Year", ylab = "Recruits (n)", main = "West", xaxs="i", yaxs = "i",
        las = 1, mgp = c(4.5,1,0), lab = c(4,4,7))
legend(x = 1975, y = 2400000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model"),
       col = c("orchid1",2,4,1), lty = 1, lwd = c(1,2,2,3), cex = 0.8)



#### Multipanel results plots ####

# mat <- matrix(c(1,2,3,
#                 4,5,6,
#                 7,8,9,
#                 10,11,12,
#                 13,14,15,
#                 16,17,18),
#               nrow=6,ncol=3,byrow=T)
# layout(mat=mat, widths = rep.int(2,ncol(mat)), heights=rep.int(2,nrow(mat)))
# layout.show(n=12)
jpeg(filename = "C:/Users/mmorse1/Desktop/plotpractice.jpg", width = 1000, height = 2400)
#par(mfrow=c(6,2))
layout(mat=matrix(c(1,2,3,4,
                    5,6,7,8,
                    9,10,11,12),
                  nrow=3, ncol=4, byrow=TRUE), 
       widths = c(0.8,0.8,2,2), heights=c(0.5,rep(1.7,2)))
plot(1,1,type="n",frame.plot = FALSE, axes = FALSE, xlab = "", ylab = "")
u <- par("usr")
text(1,u[4], labels = "Scenario", pos=1, cex=4, family="serif") #1st box "scenario"
plot(1,1,type="n",frame.plot = FALSE, axes = FALSE, xlab = "", ylab = "")
text(1,u[4], labels = "West", pos=1, cex=4, family="serif") #2nd box "west"
plot(1,1,type="n",frame.plot = FALSE, axes = FALSE, xlab = "", ylab = "")
text(1,u[4], labels = "East", pos=1, cex=4, family="serif") #3rd box "east#"
plot(1,1,type="n",frame.plot = FALSE, axes = FALSE, xlab = "", ylab = "")
text(1,1, labels = "1", pos=1, cex=4, family="serif") #4th box "1"
matplot(head(yrs,1):tail(yrs,1), BFTE.100.RCI[,-1], type = "l", col = c(2,2,2,1,4), lty = c(1,3,3,1,1), #plot
        lwd = c(2,1,1,3,2), ylim=c(0, 6000000), xlab = "", ylab = "", xaxs="i", yaxs = "i", las = 1)
matplot(head(yrs,1):tail(yrs,1), BFTW.100.RCI[,-1], type = "l", col = c(2,2,2,1,4), lty = c(1,3,3,1,1), #plot
        lwd = c(2,1,1,3,2), ylim=c(0, 2500000), xlab = "", ylab = "", xaxs="i", yaxs = "i", las = 1)
plot(1,1,type="n",frame.plot = FALSE, axes = FALSE, xlab = "", ylab = "")
text(1,1, labels = "2", pos=1, cex=4, family="serif") #"2"
dev.off()



#### BIAS PLOTS ####

## East ##

# base OM #
# R (revised EM) #
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 2/Converged")
E.R.biasdata <- as.data.frame(read.csv("E_R_bias_data_converge.csv", header = TRUE))
E.R.biasdata <- E.R.biasdata[1:38,2:482]
E.R.biasavg <- (rowMeans(E.R.biasdata[1:38,]))*100
E.R.biasavgdf <- as.data.frame(E.R.biasavg)
E.R.biasavgdf <- cbind(1974:2011,E.R.biasavgdf)
colnames(E.R.biasavgdf) <- c("years","vals")
#mytext <- "Base case"
#mygrob <- grid.text(mytext, x=1, y=.5, rot = 270)
E.baseR.PRB <- ggplot(E.R.biasavgdf, aes(years,vals)) +
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
                               size = 22),
    axis.text.x = element_text(family = "Times New Roman",
                               size = 22)) #+
  #annotation_custom(mygrob)
  #annotate(geom = "text", x=2012, y=-0.5, label = "Base case", family = "Times New Roman", angle = 270)

# SSB (revised EM) #
E.SSB.biasdata <- as.data.frame(read.csv("E_SSB_bias_data_converge_pop.csv", header = TRUE))
E.SSB.biasdata <- E.SSB.biasdata[1:42,2:482]
E.SSB.biasavg <- (rowMeans(E.SSB.biasdata))*100
E.SSB.biasavgdf <- as.data.frame(E.SSB.biasavg)
E.SSB.biasavgdf <- cbind(1974:2015,E.SSB.biasavgdf)
colnames(E.SSB.biasavgdf) <- c("years","vals")
E.baseSSB.PRB <- ggplot(E.SSB.biasavgdf, aes(years,vals)) +
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
                               size = 22),
    axis.text.x = element_text(family = "Times New Roman",
                               size = 22))

# R (default EM) # FOR MANUSCRIPT #
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged")
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

# SSB (default EM) # FOR MANUSCRIPT #
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
                               margin = margin(t = 5)))


# alt OM #
# R #
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - alt/Converged")
E.R.biasdata <- as.data.frame(read.csv("E_R_bias_data_converge.csv", header = TRUE))
E.R.biasdata <- E.R.biasdata[1:38,2:475]
E.R.biasavg.alt <- (rowMeans(E.R.biasdata))*100
E.R.biasavg.altdf <- as.data.frame(E.R.biasavg.alt)
E.R.biasavg.altdf <- cbind(1974:2011,E.R.biasavg.altdf)
colnames(E.R.biasavg.altdf) <- c("years","vals")
E.altR.PRB <- ggplot(E.R.biasavg.altdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-80,20,50),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2011,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-80,20)) +
  labs(y="", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 15),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13))


# SSB #
E.SSB.biasdata <- as.data.frame(read.csv("E_SSB_data_converge+popbias.csv", header = TRUE))
E.SSB.biasdata <- E.SSB.biasdata[1:42,2:475]
E.SSB.biasavg.alt <- (rowMeans(E.SSB.biasdata))*100
E.SSB.biasavg.altdf <- as.data.frame(E.SSB.biasavg.alt)
E.SSB.biasavg.altdf <- cbind(1974:2015,E.SSB.biasavg.altdf)
colnames(E.SSB.biasavg.altdf) <- c("years","vals")
E.altSSB.PRB <- ggplot(E.SSB.biasavg.altdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-100,0,50),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2015,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-100, 0)) +
  labs(y="", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 15),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13))

# self-test #
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


## West ##

# base OM #
# R (revised EM) #
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged")
W.R.biasdata <- as.data.frame(read.csv("W_R_bias_data_converge.csv", header = TRUE))
W.R.biasdata <- W.R.biasdata[1:38,2:406]
W.R.biasavg <- (rowMeans(W.R.biasdata))*100
# boxplot(E.R.biasavg, W.R.biasavg, names = c("East", "West"),
#         ylab = "Relative bias", main = "Recruitment", ylim = c(-3,3), las = 1)
# abline(a = 0, b = 0, lty = 3, lwd = 2)
# barplot(W.R.biasavg, names.arg = 1974:2011, col = "gray80", xaxs = "i", yaxs = "i", las = 1)
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
  labs(y="Percent
relative bias (%)", x="") +
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



# SSB (revised EM) # FOR MANUSCRIPT #
W.SSB.biasdata <- as.data.frame(read.csv("W_SSB_data_converge+popbias.csv", header = TRUE))
W.SSB.biasdata <- W.SSB.biasdata[1:42,2:406]
W.SSB.biasavg <- (rowMeans(W.SSB.biasdata))*100
# boxplot(W.SSB.biasavg, col = "light green", ylab = "Relative bias", main = "Spawning stock biomass")
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
  labs(y="Percent
relative bias (%)", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 24,
                                   margin = margin(r = 10)),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20,
                                   margin = margin(t = 5)))


# alt OM 3 #
# R #
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - alt/Converged")
W.R.biasdata <- as.data.frame(read.csv("W_R_bias_data_converge.csv", header = TRUE))
W.R.biasdata <- W.R.biasdata[1:38,2:323]
W.R.biasavg.alt <- (rowMeans(W.R.biasdata))*100
W.R.biasavg.altdf <- as.data.frame(W.R.biasavg.alt)
W.R.biasavg.altdf <- cbind(1974:2011,W.R.biasavg.altdf)
colnames(W.R.biasavg.altdf) <- c("years","vals")
W.altR.PRB <- ggplot(W.R.biasavg.altdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,700,350),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2011,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(0,700)) +
  labs(y="", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 11),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13))

# SSB #
W.SSB.biasdata <- as.data.frame(read.csv("W_SSB_data_converge+popbias.csv", header = TRUE))
W.SSB.biasdata <- W.SSB.biasdata[1:42,2:323]
W.SSB.biasavg.alt <- (rowMeans(W.SSB.biasdata))*100
W.SSB.biasavg.altdf <- as.data.frame(W.SSB.biasavg.alt)
W.SSB.biasavg.altdf <- cbind(1974:2015,W.SSB.biasavg.altdf)
colnames(W.SSB.biasavg.altdf) <- c("years","vals")
W.altSSB.PRB <- ggplot(W.SSB.biasavg.altdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,200,100),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2015,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(0,200)) +
  labs(y="", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 15),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13))

# arrange grids
grid.arrange(W.baseR.PRB,E.baseR.PRB,W.altR.PRB,E.altR.PRB,ncol=2)
grid.arrange(W.baseSSB.PRB,E.baseSSB.PRB,W.altSSB.PRB,E.altSSB.PRB,ncol=2)
# Recruitment
grid.arrange(W.R.plot, E.R.plot, W.baseR.PRB, E.baseR.PRB, W.R.plot.alt, E.R.plot.alt, W.altR.PRB, E.altR.PRB,
             nrow=4,ncol=2,heights=c(5,3,5,3))
plot_grid(W.R.plot, E.R.plot, W.baseR.PRB, E.baseR.PRB, W.R.plot.alt, E.R.plot.alt, W.altR.PRB, E.altR.PRB,
          ncol = 2, nrow =  4, align = "v", rel_heights = c(7,3,7,3))
# SSB
plot_grid(W.SSB.plot, E.SSB.plot, W.baseSSB.PRB, E.baseSSB.PRB, W.SSB.plot.alt, E.SSB.plot.alt, W.altSSB.PRB, E.altSSB.PRB,
          ncol = 2, nrow = 4, align = "v", rel_heights = c(7,3,7,3))
# SSB - self-test
plot_grid(E.SSB.plot, E.selfSSB.PRB, ncol = 1, nrow = 2, align = "v", rel_heights = c(7,3))



# Make data frames for building ggplot2 boxplots
# R #
R.df <- data.frame(1974:2011,W.R.biasavg*100,W.R.biasavg.alt*100,E.R.biasavg*100,E.R.biasavg.alt*100)
colnames(R.df) <- c("years", "W2", "W-Alt", "E1", "E-Alt")
R.df_1 <- melt(R.df, id.vars = "years")
ggplot(data=R.df_1,aes(x=variable, y=value)) +
  geom_boxplot(fill = c("gray80","white","gray80","white")) +
  coord_cartesian(ylim = c(-100,500)) +
  labs(y="Relative bias (%)",x="") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 14),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14,
                                   face = "bold",
                                   color = "black"),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        plot.title = element_text(family = "Times New Roman",
                                  size = 16,
                                  face = "bold")) +
  geom_hline(yintercept=0) +
  #scale_x_discrete(labels = c("W2 - base", "W1 - alternative", "E1 - base", "E1 - alternative"))
  scale_x_discrete(labels = c("                       West", "", "                        East", ""))
  
# SSB #
SSB.df <- data.frame(1974:2015,W.SSB.biasavg*100,W.SSB.biasavg.alt*100,E.SSB.biasavg*100,E.SSB.biasavg.alt*100)
colnames(SSB.df) <-  c("years", "W4", "W-Alt", "E1", "E-Alt")
SSB.df_1 <- melt(SSB.df, id.vars = "years")
ggplot(data=SSB.df_1,aes(x=variable, y=value)) +
  geom_boxplot(fill = c("gray80","white","gray80","white")) +
  coord_cartesian(ylim = c(-100,200)) +
  labs(y="Relative bias (%)",x="") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 14),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14,
                                   face = "bold",
                                   color = "black"),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12)) +
  geom_hline(yintercept = 0) +
  #scale_x_discrete(labels = c("W2 - base", "W1 - alternative", "E1 - base", "E1 - alternative"))
  scale_x_discrete(labels = c("                       West", "", "                        East", ""))


#### COMPARING OM to ICCAT ####
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/Misc")
#setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_4/Misc")
ICCAT <- read.csv("ICCATresults.csv")
#ICCAT <- read.csv("VPAresults_ICCATrevised_Fratiofixed.csv")

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
E.SSB <- cbind(1974:2015, OM.E.SSB.S.base, OM.E.SSB.P.base, OM.E.SSB.S.alt, OM.E.SSB.P.alt, ICCAT.E.SSB)
# par(mar = c(5.1, 5.1, 4.1, 2.1))
# OMaltE.plot <- matplot(head(yrs,1):tail(yrs,1), E.SSB, type = "l", col = c(2,4,2), lty = c(3,1,1), lwd = 3, ylim=c(0, 1100000), 
#         xlab = "Year", ylab = "SSB (mt)", main = "", xaxs="i", yaxs="i", las = 1, mgp = c(4,1,0))
# legend(x = 1975, y = 1070000, legend = c("operating model population", "operating model stock", "ICCAT 2017"),
#        col = c(2,2,4), lty = c(1,3,1), lwd = 3, cex = 0.8)

# GGPLOT - FOR MANUSCRIPT (w/ ALT OM)
E.SSB.df <- as.data.frame(E.SSB)
colnames(E.SSB.df) <- c("years","Base stock","Base population","Alt stock","Alt population","ICCAT")
E.SSB_1 <- melt(E.SSB.df, id.vars = "years")
E.SSB.OM.plot <- ggplot(data=E.SSB_1,aes(x=years,y=value,color=variable,linetype=variable)) +
  geom_line(aes(size =variable)) +
  geom_point(aes(shape = variable), size = 3) +
  theme_classic() +
  labs(y = "", x="", title="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  #scale_color_manual(values = c(2,2,4,4,1)) +
  scale_color_manual(values = c(1,1,"gray50","gray50",1)) +
  #scale_linetype_manual(values = c("dotted", "solid", "dotted", "solid", "solid")) +
  scale_linetype_manual(values = c( "dotted","solid","dotted", "solid", "solid")) +
  scale_shape_manual(values = c(32,32,32,32,32)) +
  scale_size_manual(values = c(1,1,1,1,2)) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,1000000,500000)) +
  coord_cartesian(ylim=c(0,1000000))

# GGPLOT - FOR MANUSCRIPT (w/o ALT OM)
ICCAT.E.SSB <- ICCAT[,2]
E.SSB <- cbind(1974:2015, OM.E.SSB.P.base, OM.E.SSB.S.base, ICCAT.E.SSB)
E.SSB.df <- as.data.frame(E.SSB)
colnames(E.SSB.df) <- c("years","Population view","Stock view","ICCAT")
E.SSB_1 <- melt(E.SSB.df, id.vars = "years")
E.SSB.OM.plot <- 
  ggplot(data=E.SSB_1,aes(x=years,y=value,linetype=variable)) +
  geom_line(aes(size = variable)) +
  theme_classic() +
  labs(y = "", x="", title="") +
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
  scale_linetype_manual(values = c("solid","dotted","solid")) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,1000000,500000)) +
  coord_cartesian(ylim=c(0,1000000))

# FOR CAPAM PRESENTATION
ICCAT.E.SSB <- ICCAT[,2]
E.SSB <- cbind(1974:2015, OM.E.SSB.S.base, OM.E.SSB.P.base, ICCAT.E.SSB)
E.SSB.df <- as.data.frame(E.SSB)
colnames(E.SSB.df) <- c("years","OM stock","OM population","ICCAT")
E.SSB_1 <- melt(E.SSB.df, id.vars = "years")
E.SSB.OM.plot <- 
  ggplot(data=E.SSB_1,aes(x=years,y=value,color=variable,linetype=variable)) +
  geom_line(size = 1) +
  theme_classic() +
  labs(y = "", x="", title="East") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 16,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 16,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14),
        legend.position = c(0.2, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman")) +
  scale_color_manual(values = c("darkorange3", "darkorange3", 1)) +
  scale_linetype_manual(values = c( "dotted","solid","solid")) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,1000000,500000)) +
  coord_cartesian(ylim=c(0,1000000))
  


# SSB - West (stock) #
# OM.W.SSB <- BFTW.100.SSB[,103] #stock
# OM.W.SSBP <- BFTW.100.SSB[,102] #population
# OM.W.SSB.alt <- T_Wssb[1:nyr,3] #stock Q3
# OM.W.SSBP.alt <- T_Pssb[1:nyr,3,2] #population Q3
# OM.W.SSB.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 1/W_SSBCI_data.csv", header = TRUE)[,6]
# OM.W.SSBP.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 1/W_SSBCI_data.csv", header = TRUE)[,5]
ICCAT.W.SSB <- ICCAT[,3]
W.SSB <- cbind(1974:2015, OM.W.SSB.S.base, OM.W.SSB.P.base, OM.W.SSB.S.alt, OM.W.SSB.P.alt, ICCAT.W.SSB)
# par(mar = c(5.1, 5.1, 4.1, 2.1))
# OMaltW.plot <- matplot(head(yrs,1):tail(yrs,1), W.SSB, type = "l", col = c(2,4,2), lty = c(3,1,1), lwd = 3, ylim=c(0, 120000), 
#                        xlab = "", ylab = "", main = "",  xaxs="i", yaxs = "i", las = 1, mgp = c(4,1,0), xaxt="n")
# legend(x = 1975, y = 115000, legend =  c("operating model population", "operating model stock", "ICCAT 2017"),
#        col = c(2,2,4), lty = c(1,3,1), lwd = 3, cex = 0.8)

# FOR MANUSCRIPT (w/ ALT OM)
W.SSB.df <- as.data.frame(W.SSB)
colnames(W.SSB.df) <- c("years","Base stock","Base population","Alt stock","Alt population","ICCAT")
W.SSB_1 <- melt(W.SSB.df, id.vars="years")
W.SSB.OM.plot <- ggplot(data=W.SSB_1,aes(x=years,y=value,color=variable,linetype=variable)) +
  geom_line(aes(size = variable)) +
  theme_classic() +
  labs(y = "SSB (tonnes)", x="", title="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position = "none",
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  scale_color_manual(values = c(1,1,"gray50","gray50",1)) +
  scale_linetype_manual(values = c("dotted", "solid", "dotted", "solid", "solid")) +
  scale_size_manual(values = c(1,1,1,1,2)) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,100000,50000)) +
  coord_cartesian(ylim=c(0,100000))

# FOR MANUSCRIPT (w/o ALT OM)
ICCAT.W.SSB <- ICCAT[,3]
W.SSB <- cbind(1974:2015,  OM.W.SSB.P.base, OM.W.SSB.S.base,ICCAT.W.SSB)
W.SSB.df <- as.data.frame(W.SSB)
colnames(W.SSB.df) <- c("years","Population view","Stock view","ICCAT")
W.SSB_1 <- melt(W.SSB.df, id.vars="years")
W.SSB.OM.plot <- 
  ggplot(data=W.SSB_1,aes(x=years,y=value,linetype=variable)) +
  geom_line(aes(size = variable)) +
  theme_classic() +
  labs(y = "SSB (tonnes)", x="", title="") +
  theme(plot.title = element_text(family ="Times New Roman",
                                  size = 24,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 24,
                                    face = "bold",
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        legend.position = c(0.2, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",
                                   size = 19),
        legend.key.width = unit(2, "cm")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid")) +
  scale_size_manual(values = c(1,1,2)) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,100000,50000)) +
  coord_cartesian(ylim=c(0,100000))

# FOR CAPAM PRESENTATION
ICCAT.W.SSB <- ICCAT[,3]
W.SSB <- cbind(1974:2015, OM.W.SSB.S.base, OM.W.SSB.P.base, ICCAT.W.SSB)
W.SSB.df <- as.data.frame(W.SSB)
colnames(W.SSB.df) <- c("years","OM stock","OM population","ICCAT")
W.SSB_1 <- melt(W.SSB.df, id.vars="years")
W.SSB.OM.plot <- ggplot(data=W.SSB_1,aes(x=years,y=value,color=variable,linetype=variable)) +
  geom_line(size = 1) +
  theme_classic() +
  labs(y = "SSB (tonnes)", x="", title="West") +
  theme(plot.title = element_text(family ="Times New Roman",
                                  size = 16,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 16,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14),
        legend.position = c(0.2, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman")) +
  scale_color_manual(values = c("slateblue3", "slateblue3", 1)) +
  scale_linetype_manual(values = c("dotted", "solid", "solid")) +
  scale_y_continuous(label=scientific_format(digits = 1), breaks = seq(0,100000,50000)) +
  coord_cartesian(ylim=c(0,100000))


# R - East #
#OM.E.R <- BFTE.100.R[,102]
# OM.E.R.alt <- naa[1:nyr,1,1,7,1]
# OM.E.R.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs/BFTstockassess.csv", header = TRUE)[1:42,2]
ICCAT.E.R <- ICCAT[,4]
E.R <- cbind(1974:2015, OM.E.R.base, OM.E.R.alt)
# par(mar = c(5.1, 5.1, 4.1, 2.1))
# matplot(head(yrs,1):tail(yrs,1), E.R, type = "l", lty = 1, col = c(2,4),
#         lwd = 3, ylim=c(0, 6000000), xlab = "Year", ylab = "Recruits (n)", main = "East", xaxs="i", yaxs = "i",
#         las = 1, mgp = c(4,1,0))
# legend(x = 1975, y = 5700000, legend = c("operating model scenario 2", "ICCAT 2017 & operating model scenario 1"),  col = c(2,4),
#        lty = 1, lwd = 3, cex = 0.6)
#ggplot
E.R.df <- as.data.frame(E.R)
colnames(E.R.df) <- c("years","Base/ICCAT","Alt")
E.R_1 <- melt(E.R.df, id.vars="years")

# FOR MANUSCRIPT (w/ ALT OM)
E.R.OM.plot <- ggplot(data=E.R_1,aes(x=years,y=value,color=variable)) +
  geom_line(size = 1) +
  #geom_point(aes(shape = variable), size = 4) +
  theme_classic() +
  labs(y = "", x="", title="East") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 14,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  scale_color_manual(values = c(1,"gray50")) +
  #scale_shape_manual(values = c(0,1)) +
  scale_y_continuous(label=scientific_format(digits = 2), breaks = seq(0,6000000,2000000)) +
  coord_cartesian(ylim=c(0,6000000))

# FOR MANUSCRIPT (w/o ALT OM)
E.R <- cbind(1974:2015, OM.E.R.base)
E.R.df <- as.data.frame(E.R)
colnames(E.R.df) <- c("years")
E.R_1 <- melt(E.R.df, id.vars="years")
E.R.OM.plot <- 
  ggplot(data=E.R_1,aes(x=years,y=value)) +
  geom_line(size = 1) +
  theme_classic() +
  labs(y = "", x="", title="East") +
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
                                   size = 22),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman")) +
  scale_y_continuous(label=scientific_format(digits = 2), breaks = seq(0,6000000,2000000)) +
  coord_cartesian(ylim=c(0,6000000))

#for CAPAM presentation
E.R <- cbind(1974:2015, OM.E.R.base)
E.R.df <- as.data.frame(E.R)
colnames(E.R.df) <- c("years")
E.R_1 <- melt(E.R.df, id.vars="years")
E.R.OM.plot <- ggplot(data=E.R_1,aes(x=years,y=value)) +
  geom_line(size = 1, color = "darkorange3") +
  #geom_point(aes(shape = variable), size = 4) +
  theme_classic() +
  labs(y = "", x="", title="Eastern Population") +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 16,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 16,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman")) +
  scale_y_continuous(label=scientific_format(digits = 2), breaks = seq(0,6000000,2000000)) +
  coord_cartesian(ylim=c(0,6000000))


# R - West #
# OM.W.R.alt <- naa[1:nyr,1,1,1,2]
# OM.W.R.base <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs/BFTstockassess.csv", header = TRUE)[1:42,3]
ICCAT.W.R <- ICCAT[,5]
W.R <- cbind(1974:2015,OM.W.R.base,OM.W.R.alt)
par(mar = c(5.1, 5.1, 4.1, 2.1))
matplot(head(yrs,1):tail(yrs,1), W.R, type = "l", lty = 1, col = c(2,4),
        lwd = 3, ylim=c(0, 700000), xlab = "Year", ylab = "Recruits (n)", main = "West", xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 670000, legend = c("operating model scenario 2", "ICCAT 2017 & operating model scenario 1"),  col = c(2,4),
       lty = 1, lwd = 3, cex = 0.6)
#ggplot
W.R.df <- as.data.frame(W.R)
colnames(W.R.df) <- c("years","Base/ICCAT","Alt")
W.R_1 <- melt(W.R.df, id.vars="years")

# FOR MANUSCRIPT (w/ ALT OM)
W.R.OM.plot <- ggplot(data=W.R_1,aes(x=years,y=value,color=variable)) +
  geom_line(size = 1) +
  #geom_point(aes(shape = variable), size = 4) +
  theme_classic() +
  labs(y = "Recruitment (numbers)", x="", title="West") +
  theme(plot.title = element_text(family="Times New Roman",
                                  size = 14,
                                  face = "bold",
                                  hjust = 0.5), 
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position = "none",
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  scale_color_manual(values = c(1,"gray50")) +
  scale_y_continuous(label=scientific_format(digits = 2)) +
  coord_cartesian(xlim=c(1974:2015))

# FOR MANUSCRIPT (w/o ALT OM)
ICCAT.W.R <- ICCAT[,5]
W.R <- cbind(1974:2015, OM.W.R.base)
W.R.df <- as.data.frame(W.R)
colnames(W.R.df) <- c("years", "Base/ICCAT")
W.R_1 <- melt(W.R.df, id.vars="years")
W.R.OM.plot <- 
  ggplot(data=W.R_1,aes(x=years,y=value)) +
  geom_line(size = 1) +
  theme_classic() +
  labs(y = "Recruitment (numbers)", x="", title="West") +
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
                                   size = 22),
        legend.position = "none",
        legend.title = element_blank()) +
  scale_y_continuous(label=scientific_format(digits = 2)) +
  coord_cartesian(xlim=c(1974:2015))

#for CAPAM presentation
ICCAT.W.R <- ICCAT[,5]
W.R <- cbind(1974:2015, OM.W.R.base)
W.R.df <- as.data.frame(W.R)
colnames(W.R.df) <- c("years", "Base/ICCAT")
W.R_1 <- melt(W.R.df, id.vars="years")
W.R.OM.plot <- ggplot(data=W.R_1,aes(x=years,y=value)) +
  geom_line(size = 1, color = "slateblue3") +
  #geom_point(aes(shape = variable), size = 4) +
  theme_classic() +
  labs(y = "Recruitment (numbers)", x="", title="Western Population") +
  theme(plot.title = element_text(family="Times New Roman",
                                  size = 16,
                                  face = "bold",
                                  hjust = 0.5), 
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 16,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14),
        legend.position = "none",
        legend.title = element_blank()) +
  scale_y_continuous(label=scientific_format(digits = 2)) +
  coord_cartesian(xlim=c(1974:2015))


#layouts
plot_grid(W.R.OM.plot, E.R.OM.plot, W.SSB.OM.plot, E.SSB.OM.plot, 
          ncol = 2, nrow = 2, align = "v", rel_widths = c(3,4))
grid.arrange(W.R.OM.plot, E.R.OM.plot, W.SSB.OM.plot, E.SSB.OM.plot,
             widths = c(3,4), layout_matrix = rbind(c(1,2),
                                                    c(3,4)))
grid.arrange(W.R.OM.plot, E.R.OM.plot, widths=c(3,4))
grid.arrange(W.R.OM.plot, E.R.OM.plot, ncol = 2, nrow = 1)
grid.arrange(W.SSB.OM.plot, E.SSB.OM.plot, widths=c(3,4))
grid.arrange(W.SSB.OM.plot, E.SSB.OM.plot, ncol = 2, nrow = 1)

#manuscript
jpeg("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Publishing/CJFAS - Bluefin Tuna Simulations/Figures/OMplots.jpeg",
     width = 1400, height = 1200, units = "px", quality = 100)
grid.arrange(W.R.OM.plot, E.R.OM.plot,
             W.SSB.OM.plot, E.SSB.OM.plot,
             ncol = 2, nrow = 2)
dev.off()



#### Comparing standard settings to reducing convergence settings ####
# SSB #
SSB.std <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - det ICCAT(2017) vs. convergence settings/Run0 - standard settings/W_SSB_VPA_resultsrun0.csv", header = TRUE)
SSB.conv <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - det ICCAT(2017) vs. convergence settings/Run0/W_SSB_VPA_resultsrun0.csv", header = TRUE)
SSB.compare <- cbind(SSB.std[,2], SSB.conv[,2])
matplot(head(yrs,1):tail(yrs,1), SSB.compare, type = "l", lty = 1, col = c("forestgreen","gray45"), lwd = 3, ylim = c(0,150000),
        xlab = "Year", ylab = "SSB (mt)", xaxs = "i", yaxs = "i", las = 1, mgp = c(4,1,0), lab = c(4,4,7))
legend(x=1975, y=145000, legend = c("standard settings", "convergence settings"), col = c("forestgreen","gray45"),
       lty = 1, lwd =3, cex = 0.7)
(SSB.compare[,1] - SSB.compare[,2])/SSB.compare[,2] *100
# R #
R.std <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - det ICCAT(2017) vs. convergence settings/Run0 - standard settings/W_R_VPA_resultsrun0.csv", header = TRUE)
R.conv <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - det ICCAT(2017) vs. convergence settings/Run0/W_R_VPA_resultsrun0.csv", header = TRUE)
R.compare <- cbind(R.std[,2], R.conv[,2])
matplot(head(yrs,1):tail(yrs,1), R.compare, type = "l", lty = 1, col = c("forestgreen","gray45"), lwd = 3, ylim = c(0,2500000),
        xlab = "Year", ylab = "Recruits (n)", xaxs = "i", yaxs = "i", las = 1, mgp = c(4,1,0), lab = c(4,4,7))
legend(x=1975, y=2400000, legend = c("standard settings", "convergence settings"), col = c("forestgreen","gray45"),
       lty = 1, lwd =3, cex = 0.7)


#### F-Ratios ####

# East stochastic sims #
Fratios <- matrix(NA, nrow = 102, ncol = 3)
for (r in 1:100) {
  path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 100 Sims/Run", r, "/BFTE2017_", r, ".e1", sep = "")
  par.est <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
  best.ests <- par.est[,2]
  best.ests.num <- as.character(best.ests)
  par.est.noD <- str_replace_all(best.ests.num, "D+", "E")
  par.est.noD.num <- as.numeric(par.est.noD)
  ncells <- c(10,17,32)
  Fratios[r,] <- par.est.noD.num[ncells]
}
hist(Fratios[,3])

# East deterministic sim #
detpath <- ("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 100 Sims/Run0/BFTE2017_0.e1")
detpar.est <- as.data.frame(read.table(file = detpath, fill = T, col.names = 1:max(count.fields(detpath))))
detbest.ests <- detpar.est[,2]
detbest.ests.num <- as.character(detbest.ests)
detpar.est.noD <- str_replace_all(detbest.ests.num, "D+", "E")
detpar.est.noD.num <- as.numeric(detpar.est.noD)
ncells <- c(10,17,32)
Fratios[101,] <- detpar.est.noD.num[ncells]

# East ICCAT #
ICCATpath <- ("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Data/ICCAT 2017 BFT Assessments/FINAL ICCAT 2017 EAST/bfte2017.e1")
ICCATpar.est <- as.data.frame(read.table(file = ICCATpath, fill = T, col.names = 1:max(count.fields(ICCATpath))))
ICCATbest.ests <- ICCATpar.est[,2]
ICCATbest.ests.num <- as.character(ICCATbest.ests)
ICCATpar.est.noD <- str_replace_all(ICCATbest.ests.num, "D+", "E")
ICCATpar.est.noD.num <- as.numeric(ICCATpar.est.noD)
ncells <- c(10,23,38)
Fratios[102,] <- ICCATpar.est.noD.num[ncells]

hist(log(Fratios[1:100,3]), main = "", xlab = "ln(Fratio3)", xaxs = "i", yaxs = "i")
abline(v=log(Fratios[101,3]), col = 2, lwd = 2)
abline(v=log(Fratios[102,3]), col = 4, lwd = 2)
legend(x=-1.4, y=17, legend = c("deterministic run", "ICCAT VPA"), col = c(2,4), lty = 1, cex = 0.8)


#### Catch-at-age distributions ####

# East CAA #
# take catch-at-age from random year (here, 1990) across all simulations (nsims=100)
CAA.dist <- matrix(NA, nrow = 101, ncol = 10)
series <- c(1:100, 0)
for (r in series) {
  if (r==0) {  #deterministic run
    path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 100 Sims/Run", r, "/BFTE2017_", r, ".d1", sep = "")
    data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
    CAA.dist[r+1,] <- as.matrix(data.file[23,2:11])  #catch for year 1990, all ages
    matplot(1:10, CAA.dist[r+1,], type = "l", col = 2, add = TRUE)
  } else if (r==1) {  #run 1
    path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 100 Sims/Run", r, "/BFTE2017_", r, ".d1", sep = "")
    data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
    CAA.dist[r+1,] <- as.matrix(data.file[23,2:11])  #catch for year 1990, all ages
    matplot(1:10, CAA.dist[r+1,], type = "l")
  } else {  #runs 2-100
    path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 100 Sims/Run", r, "/BFTE2017_", r, ".d1", sep = "")
    data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
    CAA.dist[r+1,] <- as.matrix(data.file[23,2:11])  #catch for year 1990, all ages
    matplot(1:10, CAA.dist[r+1,], type = "l", add = TRUE)
  }
}
for (r in 1:10) {
  hist(log(as.numeric(CAA.dist[2:51,r])))
  abline(v=log(as.numeric(CAA.dist[1,r])), col = 2)
}
# CAA.list <- list(as.numeric(CAA.dist[2:51,1]), as.numeric(CAA.dist[2:51,2]), as.numeric(CAA.dist[2:51,3]), as.numeric(CAA.dist[2:51,4]), as.numeric(CAA.dist[2:51,5]),
#                  as.numeric(CAA.dist[2:51,6]), as.numeric(CAA.dist[2:51,7]), as.numeric(CAA.dist[2:51,8]), as.numeric(CAA.dist[2:51,9]), as.numeric(CAA.dist[2:51,10]))
CAA.list <- list(as.numeric(CAA.dist[2:101,1]), as.numeric(CAA.dist[2:101,2]), as.numeric(CAA.dist[2:101,3]), as.numeric(CAA.dist[2:101,4]), as.numeric(CAA.dist[2:101,5]),
                 as.numeric(CAA.dist[2:101,6]), as.numeric(CAA.dist[2:101,7]), as.numeric(CAA.dist[2:101,8]), as.numeric(CAA.dist[2:101,9]), as.numeric(CAA.dist[2:101,10]))
# CAA.list <- list(log(as.numeric(CAA.dist[2:101,1])), log(as.numeric(CAA.dist[2:101,2])), log(as.numeric(CAA.dist[2:101,3])), log(as.numeric(CAA.dist[2:101,4])), log(as.numeric(CAA.dist[2:101,5])),
#                  log(as.numeric(CAA.dist[2:101,6])), log(as.numeric(CAA.dist[2:101,7])), log(as.numeric(CAA.dist[2:101,8])), log(as.numeric(CAA.dist[2:101,9])), log(as.numeric(CAA.dist[2:101,10])))

CAA.boxplot <- boxplot(x=CAA.list, ylim = c(0,300000), xlab = "Age", ylab = "Catch-at-age")
CAA.dist.0 <- as.numeric(CAA.dist[1,])
lines(x = c(1:10), y = CAA.dist.0, col = 4, lwd = 3) #plot deterministic 
CAA.means <- lapply(CAA.list, mean)
CAA.means <- unlist(CAA.means)
points(x= c(1:10), y=CAA.means, col = 2, pch = 16) #plot CAA means
legend('topleft', legend=c("stochastic means (bias-corrected)", "deterministic"), col=c(2,4), lty=c(NA,1), pch=c(16,NA), bg="white")


# East CAA-9 #
# take catch-at-age 9 for all years across all simulations (nsims=100)
CAA9.dist <- matrix(NA, nrow = 51, ncol = 42)
for (r in 0:50) {
  path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/Run", r, "/BFTE2017_", r, ".d1", sep = "")
  data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
  CAA9.dist[r+1,] <- t(as.matrix(data.file[7:48,10]))
}

CAA9.list <- split(CAA9.dist[2:51,], rep(1:ncol(CAA9.dist), each = nrow(CAA9.dist)-1))
names(CAA9.list) <- c(1974:2015)
CAA9.boxplot <- boxplot(x=CAA9.list, ylim=c(0,40000), xlab = "year", ylab = "Catch-at-age 9")
lines(x=c(1:42),y=CAA9.dist[1,], col = 4, lwd = 3) #plot deterministic
CAA9.means <- lapply(CAA9.list, mean)
CAA9.means <- unlist(CAA9.means)
points(x = c(1:42), y = CAA9.means, col = 2, pch = 16)
legend('topleft', legend=c("stochastic means (bias-corrected)", "deterministic"), col=c(2,4), lty=c(NA,1), pch=c(16,NA), bg="white")

# log-transform
CAA9.dist.ln <- matrix(NA, nrow = 101, ncol = 41)
for (r in 0:100) {
  path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 100 Sims/Run", r, "/BFTE2017_", r, ".d1", sep = "")
  data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
  CAA9.dist.ln[r+1,] <- t(as.matrix(log(data.file[8:48,10])))
}
CAA9.list.ln <- split(CAA9.dist.ln[2:101,], rep(1:ncol(CAA9.dist.ln), each = nrow(CAA9.dist.ln)-1))
names(CAA9.list.ln) <- c(1975:2015)
CAA9.ln.boxplot <- boxplot(x=CAA9.list.ln, ylim=c(5,12), xlab = "year", ylab = "Catch-at-age 9")
lines(x=c(1:41),y=CAA9.dist.ln[1,], col = 2, lwd = 3)


# East PCAA-9 (JPN_LL_EastMed) #
PCAA6.dist <- matrix(NA, nrow = 51, ncol = 35)
for (r in 0:50) {
  path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/Run", r, "/BFTE2017_", r, ".d1", sep = "")
  data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
  PCAA6.dist[r+1,] <- t(as.matrix(data.file[253:287,8]))
}
class(PCAA6.dist) <- "numeric"
PCAA6.list <- split(PCAA6.dist[2:51,], rep(1:ncol(PCAA6.dist), each=nrow(PCAA6.dist)-1))
names(PCAA6.list) <- c(1975:2009)
PCAA6.boxplot <- boxplot(x=PCAA6.list, ylim = c(0,40000), xlab = "year", ylab="Partial catch-at-age 6")
lines(x=c(1:35), y=PCAA6.dist[1,], col=4, pch=16, lwd=2)
PCAA6.means <- lapply(PCAA6.list, mean)
PCAA6.means <- unlist(PCAA6.means)
points(x=c(1:35), y=PCAA6.means, col=2, pch=16)
legend('topleft', legend=c("stochastic means (bias-corrected)", "deterministic"), col=c(2,4), lty=c(NA,1), pch=c(16,NA), bg="white")


# East CPUE (JPN_LL_EastMed) #
CPUE.dist <- matrix(NA, nrow=51, ncol=35)
for (r in 0:50) {
  path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/Run", r, "/BFTE2017_", r, ".d1", sep = "")
  data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
  CPUE.dist[r+1,] <- t(as.matrix(data.file[96:130,3]))
}
CPUE.list <- split(CPUE.dist[2:51,], rep(1:ncol(CPUE.dist), each=nrow(CPUE.dist)-1))
names(CPUE.list) <- c(1975:2009)
CPUE.boxplot <- boxplot(x=CPUE.list, ylim = c(0,7), xlab = "year", ylab = "CPUE", main = "JPN_LL_EastMed")
lines(x=c(1:35), y=CPUE.dist[1,], col=4, lwd=2)
CPUE.means <- lapply(CPUE.list, mean)
CPUE.means <- unlist(CPUE.means)
points(x=c(1:35), y=CPUE.means, col=2, pch=16)
legend('topleft', legend=c("stochastic means", "deterministic"), col=c(2,4), lty=c(NA,1), pch=c(16,NA), bg="white")


# West CAA #
# take catch-at-age from random year (here, 1990) across all simulations (nsims=100)
CAA.dist <- matrix(NA, nrow = 101, ncol = 16)
series <- c(1:100, 0)
for (r in series) {
  if (r==0) {  #deterministic run
    path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 100 Sims/Run", r, "/BFTW2017_", r, ".d1", sep = "")
    data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
    CAA.dist[r+1,] <- as.matrix(data.file[23,2:17])  #catch for year 1990, all ages
    matplot(1:16, CAA.dist[r+1,], type = "l", col = 2, add = TRUE)
  } else if (r==1) {  #run 1
    path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 100 Sims/Run", r, "/BFTW2017_", r, ".d1", sep = "")
    data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
    CAA.dist[r+1,] <- as.matrix(data.file[23,2:17])  #catch for year 1990, all ages
    matplot(1:16, CAA.dist[r+1,], type = "l")
  } else {  #runs 2-100
    path <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 100 Sims/Run", r, "/BFTW2017_", r, ".d1", sep = "")
    data.file <- as.data.frame(read.table(file = path, fill = T, col.names = 1:max(count.fields(path))))
    CAA.dist[r+1,] <- as.matrix(data.file[23,2:17])  #catch for year 1990, all ages
    matplot(1:16, CAA.dist[r+1,], type = "l", add = TRUE)
  }
}
# for (r in 1:10) {
#   hist(log(as.numeric(CAA.dist[2:51,r])))
#   abline(v=log(as.numeric(CAA.dist[1,r])), col = 2)
# }
CAA.list <- list(as.numeric(CAA.dist[2:101,1]), as.numeric(CAA.dist[2:101,2]), as.numeric(CAA.dist[2:101,3]), as.numeric(CAA.dist[2:101,4]), as.numeric(CAA.dist[2:101,5]),
                 as.numeric(CAA.dist[2:101,6]), as.numeric(CAA.dist[2:101,7]), as.numeric(CAA.dist[2:101,8]), as.numeric(CAA.dist[2:101,9]), as.numeric(CAA.dist[2:101,10]),
                 as.numeric(CAA.dist[2:101,11]), as.numeric(CAA.dist[2:101,12]), as.numeric(CAA.dist[2:101,13]), as.numeric(CAA.dist[2:101,14]), as.numeric(CAA.dist[2:101,15]), 
                 as.numeric(CAA.dist[2:101,16]))
# CAA.list <- list(log(as.numeric(CAA.dist[2:101,1])), log(as.numeric(CAA.dist[2:101,2])), log(as.numeric(CAA.dist[2:101,3])), log(as.numeric(CAA.dist[2:101,4])), log(as.numeric(CAA.dist[2:101,5])),
#                  log(as.numeric(CAA.dist[2:101,6])), log(as.numeric(CAA.dist[2:101,7])), log(as.numeric(CAA.dist[2:101,8])), log(as.numeric(CAA.dist[2:101,9])), log(as.numeric(CAA.dist[2:101,10])))

CAA.boxplot <- boxplot(x=CAA.list, ylim = c(0,40000), xlab = "Age", ylab = "Catch-at-age")
CAA.dist.0 <- as.numeric(CAA.dist[1,])
lines(x = c(1:16), y = CAA.dist.0, col = 4, lwd = 3) #plot deterministic 
CAA.means <- lapply(CAA.list, mean)
CAA.means <- unlist(CAA.means)
points(x= c(1:16), y=CAA.means, col = 2, pch = 16) #plot CAA means
legend('topleft', legend=c("stochastic means (bias-corrected)", "deterministic"), col=c(2,4), lty=c(NA,1), pch=c(16,NA), bg="white")




#### Spawning fraction ####

W.old <- c(rep(0,5),0.001, 0.007, 0.039, 0.186, 0.563, 0.879, 0.976, 0.996, 0.999, 1, 1)
W.young <- c(0, 0, 0.25, 0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
plot(W.young, type = "l", lty = 1, lwd = 4, col = 1, xlab = "Age", ylab = "Spawning fraction", xaxt = "n", las = 1, family = "serif")
axis(side = 1, at=seq(0,16,2), labels=seq(0,16,2), family = "serif")
lines(W.old, lty = 3, lwd = 4, col = "gray50")
par(family="serif")
legend(x=11, y=0.3, legend = c("West (young) 
& East", "West (old)"), col = c(1, "gray50"), lty = c(1,3), lwd = 3,
       cex = 0.8, seg.len = 3)
#ggplot (colored)
spfr <- cbind(1:16,W.young,W.old)
spfr.df <- as.data.frame(spfr)
spfr_1 <- melt(spfr.df, id.vars="V1")
ggplot(spfr_1, aes(x=V1,y=value,color=variable)) +
  geom_line(size=1.5) +
  theme_classic() +
  labs(y="Spawning fraction", x="Age") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.title.x = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position = c(0.85,0.35),
        legend.text = element_text(family="Times New Roman",
                                   size=13)) +
  scale_x_continuous(breaks=seq(2, 16, 2)) +
  scale_color_manual(name="",
                     labels=c("West young", "West old"),
                     values = c("steelblue3","firebrick2"))
#ggplot (grayscale)
spfr <- cbind(1:16,W.young,W.old)
spfr.df <- as.data.frame(spfr)
spfr_1 <- melt(spfr.df, id.vars="V1")
ggplot(spfr_1, aes(x=V1,y=value,linetype=variable)) +
  geom_line(size=1.5) +
  theme_classic() +
  labs(y="Spawning fraction", x="Age") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.title.x = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        #legend.position = c(0.85,0.35),
        legend.position = "none",
        #legend.text = element_text(family="Times New Roman",
        #                           size=13),
        legend.key.width = unit(1.5,"cm")) +
  scale_x_continuous(breaks=seq(2, 16, 2)) +
  scale_y_continuous(breaks=seq(0,1,.5)) +
  scale_linetype_manual(name="",
                     labels=c("West young", "West old"),
                     values = c("dashed","solid"))


#### VPA-2BOX Convergence ####

# West #
nsims <- 500
notconverge <- c()
folder <- paste("West - 500 Sims - 2")
for (n in 0:nsims) {
  wd <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/Run", n, sep = "")
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
    results.to <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/Converged/BFTW2017_", n, "_RESULTS.R", sep = "")##Create "Converged" folder## 
    file.copy(results.from, results.to)  #copy converged results files to a new directory
  } else {  #i.e., convergence criteria not met
    notconverge <- append(notconverge,n)  #save nsim to remove from SSB, R results files
  }
}
length(notconverge)  #gives number of runs that DID NOT converge
100-(length(notconverge))/nsims*100  #gives % run that DID converge
notconverge
SSB.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/W_SSB_data.csv", sep = "")
SSB.result <- read.csv(SSB.flnm, header = TRUE)  #read in all SSB results
SSB.result <- SSB.result[1:42,-1]  #remove 1st col (years)
SSB.converge <- SSB.result[,-notconverge]  #remove SSB results from runs that didn't converge
SSB.write <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/Converged/W_SSB_data_converge.csv", sep = "")
write.csv(SSB.converge, SSB.write)  #write a new SSB results file only converged runs

SSB.bias.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/W_SSB_bias_data.csv", sep = "")
SSB.bias <- read.csv(SSB.bias.flnm, header = TRUE)  #read in all SSB results
SSB.bias <- SSB.bias[1:42,-1]  #remove 1st col (years)
SSB.bias.converge <- SSB.bias[,-notconverge]  #remove SSB results from runs that didn't converge
SSB.bias.write <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/Converged/W_SSB_bias_data_converge.csv", sep = "")
write.csv(SSB.bias.converge, SSB.bias.write)  #write a new SSB results file only converged runs

R.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/W_R_data.csv", sep = "")
R.result <- read.csv(R.flnm, header = TRUE)  #read in all R results
R.result <- R.result[1:42,-1]  #remove 1st col (years)
R.converge <- R.result[,-notconverge]  #remove R results from runs that didn't converge
R.write <- paste( "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/Converged/W_R_data_converge.csv", sep = "")
write.csv(R.converge, R.write)

R.bias.flnm <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/W_R_bias_data.csv", sep = "")
R.bias <- read.csv(R.bias.flnm, header = TRUE)  #read in all R results
R.bias <- R.bias[1:42,-1]  #remove 1st col (years)
R.bias.converge <- R.bias[,-notconverge]  #remove R results from runs that didn't converge
R.bias.write <- paste( "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/Converged/W_R_bias_data_converge.csv", sep = "")
write.csv(R.bias.converge, R.bias.write)

BFTW.100.SSB <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 100 Sims/W_SSB_data_converge.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.100.SSB[,-1], type = "l", col = c(rep(brewer.pal(5,"Set3"),(nsims/5)),1,1,2,4), lty = c(rep(1,nsims),1,3,1,1),
        lwd = c(rep(1,nsims),3,3,2,2), ylim=c(0, 150000), xlab = "Year", ylab = "SSB (mt)", main = "West VPA Testing - SSB",  xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 145000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model population", "operating model stock"),
       col = c(2,2,4,1,1), lty = c(1,1,1,1,3), lwd = c(1,2,2,3,3), cex = 0.7)
BFTW.500.SSB <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims/Converged/W_SSB_data_converge.csv")
matplot(head(yrs,1):tail(yrs,1), BFTW.500.SSB[,-1], type = "l", col = c(rep("orchid1",(500-length(notconverge))),1,1,2,4), lty = c(rep(1,(500-length(notconverge))),1,3,1,1),
        lwd = c(rep(1,(500-length(notconverge))),3,3,2,2), ylim=c(0, 150000), xlab = "Year", ylab = "SSB (mt)", main = "",  xaxs="i", yaxs = "i",
        las = 1, mgp = c(4,1,0))
legend(x = 1975, y = 145000, legend = c("stochastic runs", "stochastic mean", "deterministic run", "operating model population", "operating model stock"),
       col = c("orchid1",2,4,1,1), lty = c(1,1,1,1,3), lwd = c(1,2,2,3,3), cex = 0.7)
BFTW.500.R <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims/Converged/W_R_data_converge.csv")
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
  wd <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/Run", n, sep = "")
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
    results.to <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/", folder, "/Converged/BFTE2017_", n, "_RESULTS.R", sep = "")
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
SSB <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged/E_SSB_data_converge.csv")
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
SSB <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged/E_SSB_data_converge.csv")
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


# Compare parameter estimates between different values of CAA sigma (0 to 0.7) #
# Terminal F
termFs <- matrix(NA, nrow = 9, ncol = 1)
Fratios <- matrix(NA, nrow = 3, ncol = 1)
firstder <- matrix(NA, nrow = 18, ncol = 1)
for (i in c(1:50)[-notconverge]) {
  wd <- paste("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 50 Sims - CAAsigma0.6/Run", i, sep = "")
  setwd(wd)
  #from parameter estimates file, look at terminal Fs and F-ratios
  parest.filename <- paste("BFTE2017_", i, ".e1", sep="")
  parest.file <- as.data.frame(read.table(file = parest.filename,
                                          fill = T, col.names = 1:max(count.fields(
                                            parest.filename
                                          ))))
  parest.mat <- as.matrix(parest.file)
  parest.sub <- sub("D", "E", parest.mat)
  parest.num <- apply(parest.sub, c(1,2), as.numeric)
  termFs <- cbind(termFs, parest.num[1:9,2]) #[1:9,2] #look at all 9
  Fratios <- cbind(Fratios, parest.num[c(10,17,32),2]) #look at rows 1, 8, 23
  #from VPA-2BOX log file, look at 1st derivative test central values
  log.file <- as.data.frame(read.table(file = "VPA-2BOX.LOG",
                                       fill = T, col.names = 1:max(count.fields(
                                         "VPA-2BOX.LOG"
                                       ))))
  log.mat <- as.matrix(log.file)
  log.sub <- sub("D", "E", log.mat)
  firstder.dat <- log.sub[which(log.sub[,1]=="FIRST"):which(log.sub[,1]=="HESSIAN"),]
  firstder.num <- apply(firstder.dat, c(1,2), as.numeric)
  firstder <- cbind(firstder, firstder.num[5:22,4]) #look at all 18
  
}

termFs.0.6 <- termFs
Fratios.0.6 <- Fratios
firstder.0.6 <- firstder

rowMeans(termFs[,-1])
summary(termFs.0.3[3,-1])
summary(termFs.0.86[3,-1])

rowMeans(Fratios[,-1])
summary(Fratios.0.3[3,-1])
summary(Fratios.0.86[3,-1])

rowMeans(firstder[,-1])
summary(firstder.0.3[12,-1])
summary(firstder.0.86[12,-1])

# F-ratios boxplots 
Fratios1 <- rbind(Fratios.0.0[3,-1], Fratios.0.3[3,-1], Fratios.0.6[3,-1], Fratios.0.86[3,-1])
Fratios1 <- cbind(c(0.0, 0.3, 0.6, 0.86), Fratios1)
Fratios1 <- as.data.frame(Fratios1)
colnames(Fratios1) <- c("sigma", 1:49)
Fratios1_ <- melt(Fratios1, id.vars = "sigma")
allLevels <- levels(factor(c(Fratios1_$sigma)))
Fratios1_$sigma <- factor(Fratios1_$sigma, levels=(allLevels))
Fratio1 <- ggplot(data=Fratios1_) +
  geom_boxplot(data=Fratios1_, aes(x=sigma, y=value), fill = "gray80", outlier.shape = 16, outlier.size=2) +
  stat_summary(fun.y=mean,geom="point",aes(x=sigma, y=value), size=4,color=1, shape=18) +
  labs(y="", x="Standard deviation") +
  coord_flip() +
  annotate("text", x=1, y=37, label="(a)", size=7, family = "Times New Roman") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 18),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 18),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 16),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 16),
        plot.margin = unit(c(0,0,0,0), "cm"))

Fratio2 <- ggplot(data=Fratios1_) +
  geom_boxplot(data=Fratios1_, aes(x=sigma, y=value), fill = "gray80", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun.y=mean,geom="point",aes(x=sigma, y=value), size=4,color=1, shape=18) +
  labs(y="F-ratio", x="") +
  coord_flip() +
  annotate("text", x=1, y=10.5, label="(b)", size=7, family = "Times New Roman") +
  theme(
    #axis.title.y = element_text(family = "Times New Roman",
    #                                face = "bold",
    #                                size = 16),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 18),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 16),
    #    axis.text.y = element_text(family = "Times New Roman",
    #                               size = 14)
    axis.text.y = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"))

Fratio3 <- ggplot(data=Fratios1_) +
  geom_boxplot(data=Fratios1_, aes(x=sigma, y=value), fill = "gray80", outlier.shape = 16, outlier.size=2) +
  stat_summary(fun.y=mean,geom="point",aes(x=sigma, y=value), size=4,color=1, shape=18) +
  labs(y="", x="") +
  coord_flip() +
  annotate("text", x=1, y=35.5, label="(c)", size=7, family = "Times New Roman") +
  theme(
    #axis.title.y = element_text(family = "Times New Roman",
    #                                face = "bold",
    #                                size = 16),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 18),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 16),
    #    axis.text.y = element_text(family = "Times New Roman",
    #                               size = 14)
    axis.text.y = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm"))

plot_grid(Fratio1, Fratio2, Fratio3, ncol = 3, nrow = 1, align = "v")




#### ESTIMATION MODEL PLOTS ####

## Boxplots ##

# East #
# R #
# base #
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
# run the following 3 lines to import "Times New Roman" and other fonts (must have extrafont package installed)
# font_import()
# y
# loadfonts(device = "win")
#boxplot (colored)
E.R.plot <- ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1,aes(x=years, y=value), color="steelblue4",fill="slategray1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  #labs(y="",x="") +
  labs(y="", x="", title="East") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,8000000,4000000)) +
  coord_cartesian(ylim = c(0,8000000)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 16,
                                  hjust = 0.5),
        #axis.title.y = element_text(family = "Times New Roman",
        #                            face = "bold",
        #                            size = 16),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14))
#boxplot (grayscale)
E.R.plot <- ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,8000000,4000000)) +
  coord_cartesian(ylim = c(0,8000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

# BOXPLOT FOR MANUSCRIPT
E.R.plot <- 
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

#violinplot
# ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
#   geom_violin(data=E.R.sims_1,aes(x=years, y=value),fill="gray80") + 
#   stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5) +
#   labs(y="Recruitment (numbers)",x="") +
#   theme_classic() +
#   scale_x_discrete(breaks = seq(1974,2015,10)) +
#   coord_cartesian(ylim = c(0,10000000))

# alt OM #
E.R.dataalt <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - alt/Converged/E_R_data_converge.csv", header = TRUE)
E.R.dataalt[1:38,1] <- years
E.R.simsalt <- as.data.frame(E.R.dataalt[1:38,1:475]) #create data frame of simulations data
colnames(E.R.simsalt) <- c("years",1:474)
E.R.omalt <- as.data.frame(E.R.dataalt[1:38,c(1,476)]) #create data frame of operating model data
colnames(E.R.omalt) <- c("years","OM")
E.R.sims_1alt <- melt(E.R.simsalt, id.vars = "years") #melt sims data
allLevels2 <- levels(factor(c(E.R.sims_1alt$years,E.R.omalt$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
E.R.sims_1alt$years <- factor(E.R.sims_1alt$years,levels=(allLevels2))
E.R.omalt$years <- factor(E.R.omalt$years,levels=(allLevels2))
#boxplot (colored)
E.R.plot.alt <- ggplot(data=E.R.omalt, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1alt,aes(x=years, y=value), color="steelblue4",fill="slategray1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,4000000,2000000)) +
  coord_cartesian(ylim = c(0,4000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))
#boxplot (grayscale)
E.R.plot.alt <- ggplot(data=E.R.omalt, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1alt,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,4000000,2000000)) +
  coord_cartesian(ylim = c(0,4000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))


# SSB #
# base #
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
#boxplot (color)
E.SSB.plot <- ggplot(data=E.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1,aes(x=years, y=value), color="steelblue4",fill="slategray1") +
  geom_line(data=E.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="firebrick3") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  labs(y="",x="", title = "East") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1500000,500000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,1500000)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 16,
                                  hjust = 0.5),
    #axis.title.y = element_text(family = "Times New Roman",
    #                               face = "bold",
    #                              size = 15),
    axis.text.x = element_text(family = "Times New Roman",
                               size = 14),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 14))
# boxplot (grayscale)
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

# BOXPLOT FOR MANUSCRIPT
E.SSB.plot <- 
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

# self-test #
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


# alt OM #
E.SSB.dataalt <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - alt/Converged/E_SSB_data_converge.csv", header = TRUE)
E.SSB.dataalt[,1] <- years
E.SSB.simsalt <- as.data.frame(E.SSB.dataalt[1:42,1:475]) #create data frame of simulations data
colnames(E.SSB.simsalt) <- c("years",1:474)
E.SSB.ompalt <- as.data.frame(E.SSB.dataalt[,c(1,476)]) #create data frame of operating model data
colnames(E.SSB.ompalt) <- c("years","OMP")
E.SSB.omsalt <- as.data.frame(E.SSB.dataalt[,c(1,477)])
colnames(E.SSB.omsalt) <- c("years","OMS")
E.SSB.sims_1alt <- melt(E.SSB.simsalt, id.vars = "years") #melt sims data
allLevels4 <- levels(factor(c(E.SSB.sims_1alt$years,E.SSB.ompalt$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
E.SSB.sims_1alt$years <- factor(E.SSB.sims_1alt$years,levels=(allLevels4))
E.SSB.ompalt$years <- factor(E.SSB.ompalt$years,levels=(allLevels4))
E.SSB.omsalt$years <- factor(E.SSB.omsalt$years,levels=(allLevels4))
#boxplot (colored)
E.SSB.plot.alt <- ggplot(data=E.SSB.ompalt, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1alt,aes(x=years, y=value), color="steelblue4",fill="slategray1") + 
  geom_line(data=E.SSB.omsalt,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="firebrick3") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1000000,500000)) +
  coord_cartesian(ylim = c(0,1000000)) +
  theme(#axis.title.y = element_text(family = "Times New Roman",
        #                            face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 12),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))
#boxplot (grayscale)
E.SSB.plot.alt <- ggplot(data=E.SSB.ompalt, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1alt,aes(x=years, y=value), color="black",fill="gray80") + 
  geom_line(data=E.SSB.omsalt,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1000000,500000)) +
  coord_cartesian(ylim = c(0,1000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

# West #
# R #
# base #
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
# boxplot (colored)
W.R.plot <- ggplot(data=W.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="steelblue4",fill="slategray1") + 
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=1.5, color="firebrick3") +
  #labs(y="",x="") +
  labs(y="Recruitment (numbers)", x="", title = "West") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  coord_cartesian(ylim = c(0,2000000)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 16,
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 16),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14),
        #axis.text.x = element_blank(),    
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14)) +
  scale_y_continuous(breaks = seq(0,2000000,1000000), label=scientific_format(digits = 2))

# boxplot (grayscale)
W.R.plot <- ggplot(data=W.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  coord_cartesian(ylim = c(0,2000000)) +
  theme(
    #axis.text.x = element_text(family = "Times New Roman",
    #                           size = 12),
    axis.text.x = element_blank(),    
    axis.text.y = element_text(family = "Times New Roman",
                               size = 13)) +
  scale_y_continuous(breaks = seq(0,2000000,1000000), label=scientific_format(digits = 2))

# BOXPLOT FOR MANUSCRIPT
W.R.plot <- 
  ggplot(data=W.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y = "Recruitment (numbers)", x = "", title = "West") +
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

# alt OM #
W.R.data1 <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - alt/Converged/W_R_data_converge.csv", header = TRUE)
W.R.data1[1:38,1] <- years
W.R.sims1 <- as.data.frame(W.R.data1[1:38,1:323]) #create data frame of simulations data
colnames(W.R.sims1) <- c("years",1:100)
W.R.om1 <- as.data.frame(W.R.data1[1:38,c(1,324)]) #create data frame of operating model data
colnames(W.R.om1) <- c("years","OM")
W.R.sims_1 <- melt(W.R.sims1, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(W.R.sims_1$years,W.R.om1$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
W.R.sims_1$years <- factor(W.R.sims_1$years,levels=(allLevels))
W.R.om1$years <- factor(W.R.om1$years,levels=(allLevels))
# boxplot(colored)
W.R.plot.alt <- ggplot(data=W.R.om1, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="steelblue4",fill="slategray1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  coord_cartesian(ylim = c(0,1.5e+06)) +
  theme(axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13)) +
  scale_y_continuous(label=scientific_format(digits = 2))
#boxplot(grayscale)
W.R.plot.alt <- ggplot(data=W.R.om1, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=W.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  coord_cartesian(ylim = c(0,1.5e+06)) +
  theme(
    #axis.text.x = element_text(family = "Times New Roman",
    #                               size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 13)) +
  scale_y_continuous(label=scientific_format(digits = 2))

# SSB #
# base #
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
#boxplot (colored)
W.SSB.plot <- ggplot(data=W.SSB.omp, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1,aes(x=years, y=value), color="steelblue4",fill="slategray1") +
  geom_line(data=W.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="firebrick3") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  labs(y = "SSB (tonnes)", x="", title = "West") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,200000,100000)) +
  coord_cartesian(ylim = c(0,200000)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 16,
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 16),
        axis.text.x = element_text(family = "Times New Roman",
                                  size = 14),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14))
#boxplot (grayscale)
W.SSB.plot <- ggplot(data=W.SSB.omp, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1,aes(x=years, y=value), color="black",fill="gray80") +
  geom_line(data=W.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,200000,100000)) +
  coord_cartesian(ylim = c(0,200000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 13))

# BOXPLOT FOR MANUSCRIPT
W.SSB.plot <- 
  ggplot(data=W.SSB.omp, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1,aes(x=years, y=value), color="black",fill="gray80") +
  geom_line(data=W.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y = "SSB (tonnes)", x = "") +
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

# alt OM #
W.SSB.dataalt <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - alt/Converged/W_SSB_data_converge.csv", header = TRUE)
W.SSB.dataalt[,1] <- years
W.SSB.simsalt <- as.data.frame(W.SSB.dataalt[1:42,1:323]) #create data frame of simulations data
colnames(W.SSB.simsalt) <- c("years",1:322)
W.SSB.ompalt <- as.data.frame(W.SSB.dataalt[,c(1,324)]) #create data frame of operating model data
colnames(W.SSB.ompalt) <- c("years","OMP")
W.SSB.omsalt <- as.data.frame(W.SSB.dataalt[,c(1,325)])
colnames(W.SSB.omsalt) <- c("years","OMS")
W.SSB.sims_1alt <- melt(W.SSB.simsalt, id.vars = "years") #melt sims data
allLevels4 <- levels(factor(c(W.SSB.sims_1alt$years,W.SSB.ompalt$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
W.SSB.sims_1alt$years <- factor(W.SSB.sims_1alt$years,levels=(allLevels4))
W.SSB.ompalt$years <- factor(W.SSB.ompalt$years,levels=(allLevels4))
W.SSB.omsalt$years <- factor(W.SSB.omsalt$years,levels=(allLevels4))
#boxplot (colored)
W.SSB.plot.alt <- ggplot(data=W.SSB.ompalt, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1alt,aes(x=years, y=value), color="steelblue4",fill="slategray1") + 
  geom_line(data=W.SSB.omsalt,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="firebrick3") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,200000,100000)) +
  coord_cartesian(ylim = c(0,200000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 13),
        axis.text.y = element_text(family = "Times New Roman", 
                                   size = 13))
#boxplot (grayscale)
W.SSB.plot.alt <- ggplot(data=W.SSB.ompalt, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data=W.SSB.sims_1alt,aes(x=years, y=value), color="black",fill="gray80") + 
  geom_line(data=W.SSB.omsalt,aes(x=factor(years),y=OMS,group=1),linetype="longdash",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,200000,100000)) +
  coord_cartesian(ylim = c(0,200000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold"),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", 
                                   size = 13))




#layouts
grid.arrange(W.R.plot, E.R.plot,  W.R.plot.alt, E.R.plot.alt) # Recruitment
grid.arrange(W.R.plot, E.R.plot, nrow = 1, ncol = 2) # Recruitment (no alt OM)

grid.arrange(W.SSB.plot, E.SSB.plot, W.SSB.plot.alt, E.SSB.plot.alt) # SSB
grid.arrange(W.SSB.plot, E.SSB.plot, nrow = 1, ncol = 2) # SSB (no alt OM)

grid.arrange(E.R.plot, E.SSB.plot, ncol = 2)

# MANUSCRIPT LAYOUT w/ RELATIVE BIAS
grid.arrange(W.R.plot, E.R.plot,
             W.baseR.PRB, E.baseR.PRB,
             W.SSB.plot, E.SSB.plot,
             W.baseSSB.PRB, E.baseSSB.PRB,
             nrow = 4, ncol = 2,
             heights=c(5,3,5,3))

jpeg("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Publishing/CJFAS - Bluefin Tuna Simulations/Figures/EMplots.jpeg",
     width = 1100, height = 1200, units = "px", quality = 100)
plot_grid(W.R.plot, E.R.plot,
          W.baseR.PRB, E.baseR.PRB,
          W.SSB.plot, E.SSB.plot,
          W.baseSSB.PRB, E.baseSSB.PRB,
          ncol = 2, nrow = 4, align = "v", rel_heights = c(7,3,7,3))
dev.off()



#### Deterministic vs. Stochastic ####

# East SSB avg stochastic (converged) vs. deterministic from EM E3
SSB.converge <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 2/Converged/E_SSB_data_converge.csv", header = TRUE)
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
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 2/Converged")
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
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 2/Converged")
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


# West SSB " " " from EM W4
SSB.converge <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 100 Sims - 2/Converged/W_SSB_data_converge.csv")
SSB.pop <- SSB.converge[,89]
SSB.stock <- SSB.converge[,90]
SSB.mean <- SSB.converge[,91]
SSB.det <- SSB.converge[,92]
SSB.vs <- data.frame(cbind(1974:2015,SSB.pop,SSB.stock,SSB.mean,SSB.det))
SSB.vs_1 <- melt(SSB.vs, id.vars = "V1")
W.SSB.bias <- ggplot(SSB.vs_1, aes(x=V1)) +
  geom_line(aes(y=value, linetype=variable,color=variable), size=1.5) +
  labs(y="SSB (tonnes)",x="",title="West") +
  coord_cartesian(ylim = c(0,100000)) +
  scale_color_manual(values=c(2,2,1,1)) +
  scale_linetype_manual(values=c("solid","dotted","solid","dashed")) +
  theme_classic() +
  theme(plot.title = element_text(family = "Times New Roman",
                                  size = 14,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size=14),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        legend.position="None")
#relative bias
SSB.rbias <- (SSB.mean-SSB.det)/SSB.det*100
mean(SSB.rbias)

#layouts
grid.arrange(W.SSB.bias, E.SSB.bias, nrow=1, widths = c(5,7), layout_matrix = rbind(c(1,2))) # SSB
grid.arrange(E.NAA10.bias,E.NAA7.bias,E.NAA4.bias,E.Rec.bias)

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

plot_grid(W.SSB.plot, E.SSB.plot, W.baseSSB.PRB, E.baseSSB.PRB, W.SSB.plot.alt, E.SSB.plot.alt, W.altSSB.PRB, E.altSSB.PRB,
          ncol = 2, nrow = 4, align = "v", rel_heights = c(7,3,7,3))


#### Operating Model Plots ####

# Fs #
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/OM_Base_Output")
Fdata <- as.data.frame(read.csv("F_grouped.csv", header = TRUE))
Fdata_1 <- melt(Fdata, id.vars = "Year")
# Operating Model
F.OM.plot <- ggplot(data=Fdata_1[1:84,], aes(x=Year,y=value,color=variable)) +
  geom_line(size = 1.5) +
  theme_classic() +
  coord_cartesian(ylim = c(0,0.25)) +
  labs(y = "Fishing mortality rate", x="", title="Operating Model") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        title = element_text(family = "Times New Roman",
                             size = 14,
                             face = "bold"),
        legend.position = "none") +
  scale_color_manual(name="",
                     labels=c("West", "East"),
                     values = c("palegreen3","steelblue3")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

# ICCAT (2017) VPA Estimates
F.ICCAT.plot <- ggplot(data=Fdata_1[85:168,], aes(x=Year,y=value,color=variable)) +
  geom_line(size = 1.5) +
  theme_classic() +
  coord_cartesian(ylim = c(0,0.25)) +
  labs(y = "Fishing mortality rate", x="", title="ICCAT (2017) VPA Estimates") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        title = element_text(family = "Times New Roman",
                             size = 14,
                             face = "bold"),
        legend.position = c(0.8,0.9),
        legend.text = element_text(family = "Times New Roman",
                                   size = 12)) +
  scale_color_manual(name="",
                     labels=c("West", "East"),
                     values = c("palegreen3","steelblue3")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))
  

plot_grid(F.ICCAT.plot, F.OM.plot,ncol = 1, nrow = 2, align = "v")

# OM Fs weighted by abundance
# West stock area
F.weight.w <- array(NA, c(42,29,3), dimnames = list(year=1974:2015, age=1:29, zone=1:3))
for (y in 1:42)
  for (a in 1:29)
    for (z in 1:3)
    {
      F.weight.w[y,a,z] <- sum(naa[y,a,3,z,1:2]) * Fannual[y,z,a]
    }
F.weight.avg.w <- rep(NA,42)
for (y in 1:42)
{
  F.weight.avg.w[y] <- sum(F.weight.w[y,1:29,1:3]) / sum(naa[y,1:29,3,1:3,1:2])
}
# East stock area
F.weight.e <- array(NA, c(42,29,4), dimnames = list(year=1974:2015, age=1:29, zone=4:7))
for (y in 1:42)
  for (a in 1:29)
    for (z in 1:4)
    {
      F.weight.e[y,a,z] <- sum(naa[y,a,3,z+3,1:2]) * Fannual[y,z+3,a]
    }
F.weight.avg.e <- rep(NA,42)
for (y in 1:42)
{
  F.weight.avg.e[y] <- sum(F.weight.e[y,1:29,1:4]) / sum(naa[y,1:29,3,4:7,1:2])
}

# ICCAT Fs weighted by abundance
# Western stock
ICCATwest <- as.data.frame(read.table(file = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Data/ICCAT 2017 BFT Assessments/FINAL ICCAT 2017 WEST/high maturity/BFTW2017.R01",
                                     fill = T, col.names = 1:max(count.fields(
                                       "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Data/ICCAT 2017 BFT Assessments/FINAL ICCAT 2017 WEST/high maturity/BFTW2017.R01"
                                     ))))
ICCATwest.F <- as.matrix(ICCATwest[32:73,2:17])
ICCATwest.F <- apply(ICCATwest.F, c(1,2), as.numeric)
ICCATwest.N <- as.matrix(ICCATwest[79:120,2:17])
ICCATwest.N <- apply(ICCATwest.N, c(1,2), as.numeric)
ICCATwest.F.weight <- matrix(NA, nrow=42, ncol=16)
for (y in 1:42)
  for (a in 1:16)
  {
    ICCATwest.F.weight[y,a] <- ICCATwest.F[y,a] * ICCATwest.N[y,a]
  }
ICCATwest.F.weight.avg <- rep(NA,42)
for (y in 1:42)
{
  ICCATwest.F.weight.avg[y] <- sum(ICCATwest.F.weight[y,1:16]) / sum(ICCATwest.N[y,1:16])
}
# Eastern stock
ICCATeast <- as.data.frame(read.table(file = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Data/ICCAT 2017 BFT Assessments/FINAL ICCAT 2017 EAST/MINUS0.R",
                                      fill = T, col.names = 1:max(count.fields(
                                        "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Data/ICCAT 2017 BFT Assessments/FINAL ICCAT 2017 EAST/MINUS0.R"
                                      ))))
ICCATeast.F <- as.matrix(ICCATeast[38:79,2:11])
ICCATeast.F <- apply(ICCATeast.F, c(1,2), as.numeric)
ICCATeast.N <- as.matrix(ICCATeast[91:132,2:11])
ICCATeast.N <- apply(ICCATeast.N, c(1,2), as.numeric)
ICCATeast.F.weight <- matrix(NA, nrow=42, ncol=10)
for (y in 1:42)
  for (a in 1:10)
  {
    ICCATeast.F.weight[y,a] <- ICCATeast.F[y,a] * ICCATeast.N[y,a]
  }
ICCATeast.F.weight.avg <- rep(NA,42)
for (y in 1:42)
{
  ICCATeast.F.weight.avg[y] <- sum(ICCATeast.F.weight[y,1:10]) / sum(ICCATeast.N[y,1:10])
}

# Fs by zone and quarter #
Fmeans <- array(NA,c(nyr,4,7),dimnames = list(years=1974:2015,quarters=1:4,zones=1:7))
for (y in 1:nyr)
  for (q in 1:4)
    for (z in 1:7)
    {
      Fmeans[y,q,z] <- mean(Fa[y,1:29,q,z])
    }
FmeansQ1 <- as.data.frame(melt(Fmeans[,1,], id.vars = "years"))
# Q1
levels1 <- levels(factor(FmeansQ1$years))  #change the x vars in both datasets to factors that contain all the levels of both vars
FmeansQ1$years <- factor(FmeansQ1$years,levels=(levels1))
levels2 <- levels(factor(FmeansQ1$zones))
FmeansQ1$zones <- factor(FmeansQ1$zones,levels=(levels2))
F.Q1plot <- ggplot(data=FmeansQ1, aes(x=years,y=value,color=zones)) +
  geom_line(size = 1.5) +
  theme_classic() +
  #coord_cartesian(ylim = c(0,0.15)) +
  labs(y = "Fishing mortality rate", x="", title="Quarter 1") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        title = element_text(family = "Times New Roman",
                             size = 14,
                             face = "bold"),
        legend.position = "none") +
  scale_color_manual(name="",
                     #labels=c("West", "East"),
                     values = c("palegreen3","steelblue3","steelblue1","royalblue4","seagreen","limegreen","greenyellow")) #+
  #scale_y_discrete(expand = c(0,0)) +
  #scale_x_discrete(expand = c(0,0))
ggplot(data=FmeansQ1, aes(x=years)) +
  geom_line(data=FmeansQ1,aes(y=value))


# Recruits by population #
Recruits.plotdat <- as.data.frame(cbind(1974:2015,naa[,1,1,1,2], naa[,1,1,7,1]))
Recruits.plotdat_1 <- melt(Recruits.plotdat, id.vars = "V1")
ggplot(data=Recruits.plotdat_1, aes(x=V1,y=value,color=variable)) +
  geom_line(size = 1.5) +
  theme_classic() +
  #coord_cartesian(ylim = c(0,0.25)) +
  labs(y = "Recruits", x="", title="") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    size = 14,
                                    face = "bold"),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 12),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 12),
        title = element_text(family = "Times New Roman",
                             size = 14,
                             face = "bold"),
        legend.position = c(.1,.95),
        legend.text = element_text(family = "Times New Roman",
                                   size = 12)) +
  scale_color_manual(name="",
                     labels=c("West", "East"),
                     values = c("palegreen3","steelblue3")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

# Abundance #
#quarter 3, by zone, year (sum over ages, units)
naa.plotdat <- array(NA,c(nyr,7),dimnames = list(years=1974:2015,zones=1:7))
for (y in 1:nyr)
  for (z in 1:7)
  {
    naa.plotdat[y,z] <- sum(naa[y,1:29,3,z,1:2])
  }
#by quarter, zone, year (sum over ages, units)
naa.plotdatQ <- array(NA,c(nyr,4,7),dimnames = list(years=1974:2015,quarters=1:4,zones=1:7))
for (y in 1:nyr)
  for (q in 1:4)
    for (z in 1:7)
    {
      naa.plotdatQ[y,q,z] <- sum(naa[y,1:29,q,z,1:2])
    }



#### Self-test Plots ####

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
E.selfSSB.plot <- ggplot(data=E.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1,aes(x=years, y=value), color="black",fill="gray80") +
  geom_line(data=E.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="black") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="SSB (tonnes)",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1500000,500000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,1500000)) +
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
# boxplot (colored)
E.SSB.plot <- ggplot(data=E.SSB.omp, aes(x=factor(years),y=OMP)) +
  geom_boxplot(data=E.SSB.sims_1,aes(x=years, y=value), color="steelblue4",fill="slategray1") +
  geom_line(data=E.SSB.oms,aes(x=factor(years),y=OMS,group=1),linetype="dashed",size=1.5,color="firebrick3") +
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  labs(y="SSB (tonnes)",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,1500000,500000),label=scientific_format(digits = 2)) +
  coord_cartesian(ylim = c(0,1500000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                   face = "bold",
                                  size = 16),
    axis.text.x = element_text(family = "Times New Roman",
                               size = 14),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Times New Roman",
                               size = 14))

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
  labs(y="Percent
relative bias (%)", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 24,
                                   margin = margin(r = 28)),
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
E.selfR.plot <- ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="Recruitment (numbers)",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,8000000,4000000)) +
  coord_cartesian(ylim = c(0,8000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22))
# boxplot (colored)
E.selfR.plot <- ggplot(data=E.R.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.R.sims_1,aes(x=years, y=value), color="steelblue4",fill="slategray1") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="firebrick3") +
  labs(y="Recruitment (numbers)",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2011,10)) +
  scale_y_continuous(breaks = seq(0,8000000,4000000)) +
  coord_cartesian(ylim = c(0,8000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 16),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14))
# R bias barplot
E.R.biasdata <- as.data.frame(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged/E_R_bias_data_converge.csv", header = TRUE))
E.R.biasdata <- E.R.biasdata[1:38,2:493]
E.R.biasavg <- (rowMeans(E.R.biasdata[1:38,]))*100
E.R.biasavgdf <- as.data.frame(E.R.biasavg)
E.R.biasavgdf <- cbind(1974:2011,E.R.biasavgdf)
colnames(E.R.biasavgdf) <- c("years","vals")
#mytext <- "Base case"
#mygrob <- grid.text(mytext, x=1, y=.5, rot = 270)
E.selfR.PRB <- ggplot(E.R.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(-30,50,40),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2011,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(-30,50)) +
  labs(y="Percent
relative bias (%)", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 24,
                                   margin = margin(r = 10)),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   margin = margin(t = 10)))

plot_grid(E.selfR.plot, E.selfR.PRB, ncol = 1, nrow = 2, align = "v", rel_heights = c(7,3))
grid.arrange(E.selfR.plot, E.SSB.plot)
plot_grid(E.selfR.plot, E.SSB.plot, nrow  = 2, ncol = 1, align = "v")
jpeg("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Publishing/CJFAS - Bluefin Tuna Simulations/Figures/selftestplots.jpeg",
     width = 1400, height = 800, units = "px", quality = 100)
grid.arrange(E.selfR.plot, E.selfSSB.plot,
             E.selfR.PRB, E.selfSSB.PRB,
             ncol = 2,
             heights = c(7,3))
dev.off()


# Different abundances at age (adapt code below)
convergedruns <- c(1:500)[-notconverge]
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged")
for (a in 10) {
  NAA.dat <- matrix(NA, nrow=nyr, ncol=1)
  for (i in convergedruns) {
    result_file_name <- paste("BFTE2017_", i, "_RESULTS.R", sep="")
    Results <- as.data.frame(read.table(file = result_file_name,
                                        fill = T, col.names = 1:max(count.fields(
                                          result_file_name
                                        ))))
    
    # NAA from VPA
    NAA.res <- as.matrix(Results[79:120, a+1], nrow = nyr, ncol = 1) # save NAA results (change col # for different age groups)
    NAA.dat <- cbind(NAA.dat,apply(NAA.res, c(1,2), as.numeric)) # Convert NAA values to numeric
  }
  NAA.avg <- rowMeans(NAA.dat[,-1])
  NAA.OM <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/OM Output/NAA.csv", header = TRUE)
  # for 10+ group
  NAA10pl.OM <- rowSums(NAA.OM[,11:30])
  NAA.dat <- cbind(NAA.dat, NAA.avg, NAA10pl.OM)
  yaxis <- paste("Age ", a, " abundance", sep = "")
  par(mar = c(5.1, 5.1, 4.1, 2.1))
  matplot(head(yrs,1):tail(yrs,1), NAA.dat[,-1], type = "l", col = c(rep(brewer.pal(4,"Set3"),123),2,1), lty = 1,
          lwd = c(rep(1,492),3,3), ylim=c(0, 5000000), xlab = "Year", ylab = yaxis, main = "East", xaxs="i", yaxs="i")
}
legend(x = 1975, y = 8000000, legend = c("VPA simulations", "VPA simulations mean", "operating model"),
       col = c(2,2,1), lty = 1, lwd = c(1,3,3), cex = 0.8)
write.csv(NAA.dat[,-1], "E_NAA10_data_converged.csv")

# Age 10 boxplots
E.NAA10.data <- as.data.frame(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_selftest/East/Converged/E_NAA10_data_converged.csv", header = TRUE))
years <- matrix(1974:2015,nrow = 42,ncol=1)
E.NAA10.data[1:42,1] <- years
E.NAA10.sims <- as.data.frame(E.NAA10.data[1:42,1:493]) #create data frame of simulations data
colnames(E.NAA10.sims) <- c("years",1:492)
E.NAA10.om <- as.data.frame(E.NAA10.data[1:42,c(1,495)]) #create data frame of operating model data
colnames(E.NAA10.om) <- c("years","OM")
E.NAA10.sims_1 <- melt(E.NAA10.sims, id.vars = "years") #melt sims data
allLevels <- levels(factor(c(E.NAA10.sims_1$years,E.NAA10.om$years)))  #change the x vars in both datasets to factors that contain all the levels of both vars
E.NAA10.sims_1$years <- factor(E.NAA10.sims_1$years,levels=(allLevels))
E.NAA10.om$years <- factor(E.NAA10.om$years,levels=(allLevels))
E.NAA10.plot <- ggplot(data=E.NAA10.om, aes(x=factor(years), y=OM)) +
  geom_boxplot(data=E.NAA10.sims_1,aes(x=years, y=value), color="black",fill="gray80") + 
  stat_summary(fun.y=mean,geom="line",aes(group=1), size=1.5,color="black") +
  labs(y="",x="") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  scale_y_continuous(breaks = seq(0,5000000,2000000)) +
  coord_cartesian(ylim = c(0,5000000)) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 15),
        #axis.text.x = element_text(family = "Times New Roman",
        #                           size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14))

# Age 10 barplots
NAA.dat2 <- NAA.dat[,2:493]
NAA10.rel.bias <- matrix(NA, nrow=nyr, ncol=492)
for (n in 1:492) {
  for (y in 1:nyr) {
    NAA10.rel.bias[y,n] <- (NAA.dat2[y,n] - NAA10pl.OM[y])/NAA10pl.OM[y]
  }
}
E.NAA10.biasavg <- (rowMeans(NAA10.rel.bias))*100
E.NAA10.biasavgdf <- as.data.frame(E.NAA10.biasavg)
E.NAA10.biasavgdf <- cbind(1974:2015,E.NAA10.biasavgdf)
colnames(E.NAA10.biasavgdf) <- c("years","vals")
E.selfNAA10.PRB <- ggplot(E.NAA10.biasavgdf, aes(years,vals)) +
  geom_bar(stat="identity", fill="gray70") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,60,30),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1974,2015,10),expand = c(0,0)) +
  coord_cartesian(ylim = c(0,60)) +
  labs(y="", x="") +
  theme(axis.title.y= element_text(family = "Times New Roman",
                                   face = "bold",
                                   size = 15),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 14),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 14))

plot_grid(E.NAA10.plot, E.selfNAA10.PRB, ncol = 1, nrow = 2, align = "v", rel_heights = c(7,3))



#### Selectivities (East VPAs) ####

# Pull out selectivities for each index modeled for each run of the VPA simulations
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
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
Selectivity <- (as.matrix(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs/Selectivity.csv"), header=TRUE))  
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


#### Selectivities (West VPAs) ####

# Pull out selectivities for each index modeled for each run of the VPA simulations
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
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
Selectivity <- (as.matrix(read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs/Selectivity.csv"), header=TRUE))  
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


CAN.GSL.Acoustic.dat <- rbind(matrix(0, nrow = 7, ncol = ncol(CAN.GSL.Acoustic)), CAN.GSL.Acoustic)
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
colnames(US.RR.145.sims) <- c("age", 1:405)
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
colnames(US.RR.66.114.sims) <- c("age", 1:405)
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
colnames(US.RR.115.144.sims) <- c("age", 1:405)
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
colnames(US.RR.195.sims) <- c("age", 1:405)
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
colnames(JLL.AREA.2.WEST.sims) <- c("age", 1:405)
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
colnames(JLL.AREA.2.RECENT.sims) <- c("age", 1:405)
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
colnames(LARVAL.ZERO.INFLATED.sims) <- c("age", 1:405)
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
colnames(GOM.PLL.1.6.sims) <- c("age", 1:405)
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
colnames(JLL.GOM.sims) <- c("age", 1:405)
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


#### F-at-age Comparisons ####

# Compare deterministic F-at-age matrix to the OM
#East VPA Det
F.det.E <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 2/Run0/E_F_VPA_resultsrun0.csv",
                    header = TRUE)
#East OM (end at age 10)
F.OM.E <- array(NA, c(42,10),dimnames=list(year=1974:2015,age=1:10))
for (y in 1:42)
  for (a in 1:10) {
    F.OM.E[y,a] <- sum(Fa[y,a,1:4,4:7])
  }

#East OM (age 10 as plus group)
F.OM.E2 <- array(NA, c(42,29),dimnames=list(year=1974:2015,age=1:29))
for (y in 1:42)
  for (a in 1:29) {
    F.OM.E2[y, a] <- sum(Fa[y, a, 1:4, 4:7])
  }

F.OM.E2pl <- array(NA, c(42,1), dimnames = list(year = 1974:2015, age=10))
for (y in 1:42) {
  F.OM.E2pl[y] <- sum(F.OM.E2[y, 10:29])
}
F.OM.E2new <- cbind(F.OM.E2[, 1:9], F.OM.E2pl)

F.OM.ap <- matrix(NA, nrow = 42, ncol = 1)
for (i in 1:42) {
  F.OM.ap[i, ] <- max(F.OM.E[i, ])
}

F.OM.ap2 <- matrix(NA, nrow = 42, ncol = 1)
for (i in 1:42) {
  F.OM.ap2[i, ] <- max(F.OM.E2new[i, ])
}

matplot(1974:2015, F.det.E[,-1], type = "l", ylim = c(0, 1))
matplot(1974:2015, F.OM.E, type = "l", ylim = c(0, 1))


#West VPA Det
F.det.W <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Run0/W_F_VPA_resultsrun0.csv",
                    header = TRUE)
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


matplot(1974:2015, F.det.W[,-1], type = "l", ylim = c(0,0.6))
matplot(1974:2015, F.OM.W, type = "l", ylim = c(0,0.6))

# Compare averages of the stochastic F-at-age matrices to the OM
#East
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
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
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
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
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East - 500 Sims - 1/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
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
wd <- "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged" #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path="C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West - 500 Sims - 2/Converged", pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters (make sure to switch the folder)
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
