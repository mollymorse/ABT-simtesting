################################################################################################
############ BLUEFIN TUNA SIMULATIONS - CALCULATING REFERENCE POINTS (M.MORSE 2018) ############
################################################################################################

# DESCRIPTION
# This code is meant to calculate reference points on the VPA outputs of the simulations, i.e., 
# from existing Results files

# ** Contact M.Morse for questions: mollymorse@ucsb.edu


############################################ F/F01 for 2012-2014 #############################################



## Load packages ##

library(fishmethods)
library(grid)
library(cowplot)
library(ggplot2)
library(extrafont)
library(dplyr)


## Settings ##

dir_scen  <- "Simulations_2" #directory name for the scenario (e.g., base, low movement, self-test)
dir_om    <- "OM_Base_Output"         #directory name for the OM outputs



#### >> True F0.1s from OM - Cross-tests ####

## Population ##
# read in parameters
nage     <- 29
biolparm <- as.matrix(read.csv(paste0("C:/Users/mmorse1/Documents/", dir_scen, "/R Code + Inputs/BFTBiolparm.csv")), header = T)
M        <- array(biolparm[1:nage,2:3],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) #annualized M
waa      <- array(biolparm[1:nage,4:5],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) #weight-at-age (t)
naa      <- as.matrix(read.csv(paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/naa.csv")), header = T)[, -1] %>%
  array(dim = c(42, 29, 4, 7, 2), dimnames = list(year = 1974:2015, age = 1:29, quarter = 1:4, zone = 1:7, unit = 1:2))
naa_2 <- array(NA, dim = c(42, 29, 2), dimnames = list(year = 1974:2015, age = 1:29, unit = 1:2))
for (y in 1:42)
  for (a in 1:29)
    for (u in 1:2) {
      naa_2[y, a, u] <- sum(naa[y, a, 1, 1:7, u])  #calculate quarter 1 population naa (sum over zones)
      # naa_2[y, a, u] <- sum(naa[y, a, 1:4, 1:7, u])  #calculate annual (summed over quarters) population naa (sum over zones) - INCORRECT, DO NOT USE
    }  

Fa.p <- array(NA, c(41, 28, 2), dimnames = list(year = 1974:2014, age = 1:28, unit = 1:2)) 
for (y in 1:41)
  for (a in 1:28)
    for (u in 1:2) {
      Fa.p[y, a, u] <- log(naa_2[y, a, u] / naa_2[y + 1, a + 1, u]) - M[a, u] #calculate population F at year, age
    }


# calculate partial recruitment - population (years Y-3 to Y-1, where Y is 2015)
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
Fa.s <- as.matrix(read.csv(paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/Fa.csv"), header = T))[, -1] %>%
  array(dim = c(42, 29, 4, 7), dimnames = list(year = 1974:2015, age = 1:29, quarter = 1:4, zone = 1:7))
Fa.s2 <- array(NA, dim = c(42, 29, 2), dimnames = list(year = 1974:2015, age = 1:29, stock = 1:2))
for (y in 1:42)
  for (a in 1:29){
    # Fa.s2[y, a, 1] <- sum(Fa.s[y, a, 3, 4:7])  #3rd quarter because that's when the majority of F occurs in ABT fishery
    # Fa.s2[y, a, 2] <- sum(Fa.s[y, a, 3, 1:3])  #3rd quarter because that's when the majority of F occurs in ABT fishery
    Fa.s2[y, a, 1] <- sum(Fa.s[y, a, 1:4, 4:7])  #annual (summed over quarters) 
    Fa.s2[y, a, 2] <- sum(Fa.s[y, a, 1:4, 1:3])  #annual (summed over quarters) 
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
F01_om <- array(NA, c(2, 2), dimnames = list(type = c("pop", "stock"), unit = c("east", "west")))

#east pop
YPR <- ypr(age = seq(1, 10, 1), wgt = waa[1:10, 1], partial = P_om_fin_e, 
           M = M[1:10, 1], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
F01_om[1, 1] <- YPR$Reference_Points[1,1]

#west pop
YPR <- ypr(age = seq(1, 16, 1), wgt = waa[1:16, 2], partial = P_om_fin_w, 
           M = M[1:16, 2], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
F01_om[1, 2] <- YPR$Reference_Points[1,1]

#east stock
YPR <- ypr(age = seq(1, 10, 1), wgt = waa[1:10, 1], partial = P_oms_fin_e, 
           M = M[1:10, 1], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
F01_om[2, 1] <- YPR$Reference_Points[1,1]

#west stock
YPR <- ypr(age = seq(1, 16, 1), wgt = waa[1:16, 2], partial = P_oms_fin_w, 
           M = M[1:16, 2], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
F01_om[2, 2] <- YPR$Reference_Points[1,1]


# calculate reference ages
for (y in 39:41) {
  
  # Isolate ages where partial recruitment is greater than or equal to 0.8
  ref.east   <- which(P_om_fin_e  >= 0.8)  #east population
  ref.west   <- which(P_om_fin_w  >= 0.8)  #west population
  ref.east_s <- which(P_oms_fin_e >= 0.8)  #east stock
  ref.west_s <- which(P_oms_fin_w >= 0.8)  #west stock
  
}

# adjust F0.1 for the reference ages 
# using the average partial recruitment for the reference ages
F01_om[1, 1] <- F01_om[1, 1] * mean(P_om_fin_e[ref.east])
F01_om[1, 2] <- F01_om[1, 2] * mean(P_om_fin_w[ref.west])
F01_om[2, 1] <- F01_om[2, 1] * mean(P_oms_fin_e[ref.east_s])
F01_om[2, 2] <- F01_om[2, 2] * mean(P_oms_fin_w[ref.west_s])



## Determine stock status ##

# Calculate Fcurrent
F_cur_e   <- mean(Fa.p[39:41,  ref.east,   1])
F_cur_w   <- mean(Fa.p[39:41,  ref.west,   2])
F_cur_s_e <- mean(Fa.s2[39:41, ref.east_s, 1])
F_cur_s_w <- mean(Fa.s2[39:41, ref.west_s, 2])

# Calculate Fcurrent/F0.1
Expl_status_om <- array(NA, c(2, 2), dimnames = list(type = c("pop", "stock"), unit = c("east", "west")))

Expl_status_om[1, 1] <- F_cur_e/F01_om[1, 1]
Expl_status_om[1, 2] <- F_cur_w/F01_om[1, 2]
Expl_status_om[2, 1] <- F_cur_s_e/F01_om[2, 1]
Expl_status_om[2, 2] <- F_cur_s_w/F01_om[2, 2]


# Save true OM values (Fcur, F0.1, Fcur/F0.1)
write.csv((matrix(c(F_cur_e, F_cur_w, F_cur_s_e, F_cur_s_w,
         F01_om[1, 1], F01_om[1, 2], F01_om[2, 1], F01_om[2, 2],
         Expl_status_om[1, 1], Expl_status_om[1, 2], Expl_status_om[2, 1], Expl_status_om[2, 2]),
       nrow = 3,
       ncol = 4,
       byrow = TRUE,
       dimnames = list(metric = c("Fcur", "F01", "Fcur/F01"),
                       group  = c("e-pop", "w-pop", "e-stock", "w-stock")))),
       paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F01_results_om.csv"))







#### >> True F0.1s from OM - Self-test ####

# read in parameters
nage     <- 29
biolparm <- as.matrix(read.csv(paste0("C:/Users/mmorse1/Documents/", dir_scen, "/R Code + Inputs/BFTBiolparm.csv")), header = T)
M        <- array(biolparm[1:nage,2:3],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) #annualized M
waa      <- array(biolparm[1:nage,4:5],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) #weight-at-age
Fa       <- as.matrix(read.csv(paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/Fa.csv")))
Fa       <- array(Fa[, -1], c(42, 29, 2), dimnames = list(year = 1974:2015, age = 1:29, unit = 1:2))


# calculate partial recruitment - stock
P_om_e_self <- array(NA, c(3, 10), dimnames = list(year = 1:3, age = 1:10))
P_om_w_self <- array(NA, c(3, 16), dimnames = list(year = 1:3, age = 1:16))

#east
for (y in (nrow(Fa) - 3):(nrow(Fa) - 1))
  for (s in 1) {
    Ffull_om <- max(Fa[y, , s])
    for (a in 1:10) {
      P_om_e_self[y - 38, a] <- Fa[y, a, s]/Ffull_om
    }
  }

#west
for (y in (nrow(Fa) - 3):(nrow(Fa) - 1))
  for (s in 2) {
    Ffull_om <- max(Fa[y, , s])
    for (a in 1:16) {
      P_om_w_self[y - 38, a] <- Fa[y, a, s]/Ffull_om
    }
  }


# average partial recruitment for each age over all reference years - stock
P_om_avg_e_self <- rep(NA, 10)
P_om_avg_w_self <- rep(NA, 16)

#east
for (a in 1:10) {
  P_om_avg_e_self[a] <- mean(P_om_e_self[, a])
}

#west
for (a in 1:16) {
  P_om_avg_w_self[a] <- mean(P_om_w_self[, a])
}


# scaled to 1 (maximum partial recruitment) - stock
P_om_fin_e_self <- rep(NA, 10)
P_om_fin_w_self <- rep(NA, 16)

#east
for (a in 1:10)
{
  P_om_fin_e_self[a] <- P_om_avg_e_self[a]/max(P_om_avg_e_self)
}

#west
for (a in 1:16)
{
  P_om_fin_w_self[a] <- P_om_avg_w_self[a]/max(P_om_avg_w_self)
}


## Calculate true OM F0.1 ##
F01_om <- array(NA, c(1, 2), dimnames = list(type = c("value"), unit = c("east", "west")))

#east
YPR <- ypr(age = seq(1, 10, 1), wgt = waa[1:10, 1], partial = P_om_fin_e_self, 
           M = M[1:10, 1], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
F01_om[1, 1] <- YPR$Reference_Points[1,1]

#west
YPR <- ypr(age = seq(1, 16, 1), wgt = waa[1:16, 2], partial = P_om_fin_w_self, 
           M = M[1:16, 2], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
F01_om[1, 2] <- YPR$Reference_Points[1,1]

# calculate reference ages
for (y in 39:41) {
  
  # Isolate ages where partial recruitment is greater than or equal to 0.8
  ref.east <- which(P_om_fin_e_self >= 0.8)  #east
  ref.west <- which(P_om_fin_w_self >= 0.8)  #west
  
}

# adjust F0.1 for the reference ages 
# using the average partial recruitment for the reference ages
F01_om[1, 1] <- F01_om[1, 1] * mean(P_om_fin_e_self[ref.east])
F01_om[1, 2] <- F01_om[1, 2] * mean(P_om_fin_w_self[ref.west])


## Determine stock status ##

# Calculate Fcurrent
F_cur_e   <- mean(Fa[39:41, ref.east, 1])
F_cur_w   <- mean(Fa[39:41, ref.west, 2])

# Calculate Fcurrent/F0.1
Expl_status_om <- array(NA, c(1, 2), dimnames = list(type = c("value"), unit = c("east", "west")))

Expl_status_om[1, 1] <- F_cur_e/F01_om[1, 1]
Expl_status_om[1, 2] <- F_cur_w/F01_om[1, 2]


# Save true OM values (Fcur, F0.1, Fcur/F0.1)
write.csv((matrix(c(F_cur_e, F_cur_w,
                    F01_om[1, 1], F01_om[1, 2],
                    Expl_status_om[1, 1], Expl_status_om[1, 2]),
                  nrow = 3,
                  ncol = 2,
                  byrow = TRUE,
                  dimnames = list(metric = c("Fcur", "F01", "Fcur/F01"),
                                  group  = c("e-pop", "w-pop")))),
          paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F01_results_om.csv"))








#### >> Estimated F0.1s from VPA ####

## Define variables ##

dir_stock <- "West - 500 Sims - 2"            #directory name for the stock; for estimation model calcs
stock     <- 2                                #for estimation model calcs; east (1) vs. west (2) 
wd        <- paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_stock, "/Converged") #switch folder
filenums  <- gsub("[A-z \\.\\(\\)]", "", 
                  list.files(path = paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_stock, "/Converged"), pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
nyr <- 42
if (stock == 1) #reference ages (from OM) and plus group age
{
  #old (from original base case) - new (low movement) use OM ref ages calculated above
  # a.ref <- 4 #east
  # A.ref <- 5 
  #new - uses all ages P >= 0.8, not a range
  a.ref <- ref.east
  nage <- 10 
  alph <- "E"
} else { 
  #old (from original base case) - new (low movement) use OM ref ages calculated above
  # a.ref <- 8 #west
  # A.ref <- 14 
  #new - uses all ages P >= 0.8, not a range
  a.ref <- ref.west
  nage <- 16
  alph <- "W"
}


## Calculations ##

FF01 <- matrix(NA, nrow=3, ncol=1, dimnames=list(reference=c("Fcur", "F01", "Fcur/F01"), value=1))
FF01.bias <- matrix(NA, nrow=2, ncol=1, dimnames=list(bias=c("pop", "stock"), value=1))

setwd(wd)

## True F0.1 ##
if (stock == 1) {
  F01.OM.p <- F01_om[1, 1] #east pop
  F01.OM.s <- F01_om[2, 1] #east stock
} else {
  F01.OM.p <- F01_om[1, 2] #west pop
  F01.OM.s <- F01_om[2, 2] #west stock
}

## True stock status (from low movement OM) ##
F01.stat.true <- rep(NA,2)
if (stock == 1) {
  F01.stat.true[1] <- Expl_status_om[1, 1] #east pop
  F01.stat.true[2] <- Expl_status_om[2, 1] #east stock
} else {
  F01.stat.true[1] <- Expl_status_om[1, 2] #west pop
  F01.stat.true[2] <- Expl_status_om[2, 2]  #west stock
}

# NOTE: in the low movement scenario, ypr function got stuck on east runnums 15, 202, 207 so those were skipped. (runnums[-c(12, 187, 192)])
# This will take about 15 sec
for (i in runnums) {
# for (i in runnums[-c(12, 187, 192)]) { #low move

  ## Read in Results files ##
  result.filename <- paste("BFT", alph, "2017_", i, "_RESULTS.R", sep="")
  
  result.file <- as.data.frame(read.table(file = result.filename,
                                       fill = T, col.names = 1:max(count.fields(
                                         result.filename
                                       ))))
  
  F.mat <- as.matrix(result.file[32:73, 2:(nage+1)], nrow=nyr, ncol=nage) #save F-at-age matrix
  F.mat <- apply(F.mat, c(1,2), as.numeric)
  
  ## Partial recruitment ##
  P.vpa <- array(NA, c(3, nage), dimnames = list(year = 1:3, age = 1:nage))
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
  
  P.vpa.fin <- round(P.vpa.fin, 4)
  
  ## Calculate F0.1 ##
  YPR <- ypr(age = seq(1, nage, 1), wgt = waa[1:nage, stock], partial = P.vpa.fin, 
             M = M[1:nage, stock], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE) #changed to plus = FALSE for low move scenario
  F01 <- YPR$Reference_Points[1, 1]

  # Calculate F0.1 adjusted for the reference ages using the average partial recruitment for the reference ages
  F01.vpa <- F01 * mean(P.vpa.fin[a.ref])

  
  ## Determine stock status ##
  
  # Calculate Fcurrent
  F.cur.vpa <- mean(F.mat[(nrow(F.mat)-3):(nrow(F.mat)-1),a.ref])
  
  # Calculate Fcurrent/F0.1
  F01.status.vpa <- F.cur.vpa/F01.vpa

  FF01 <- cbind(FF01, c(F.cur.vpa, F01.vpa, F01.status.vpa))
  
  
  ## Calculate bias ##
  
  # Calculate bias in Fcurrent/F0.1 relative to "true" ratio from operating model
  # F01.rel.bias.p <- (F01.status.vpa - as.numeric(F01.stat.true[1])) / as.numeric(F01.stat.true[1]) #from OM-P
  # F01.rel.bias.s <- (F01.status.vpa - as.numeric(F01.stat.true[2])) / as.numeric(F01.stat.true[2]) #from OM-S
  
  # FF01.bias <- cbind(FF01.bias, c(F01.rel.bias.p, F01.rel.bias.s))
  
}



## Save F0.1 results ##
# colnames(FF01) <- c("x", runnums[-c(12, 187, 192)]) #low move 
# colnames(FF01.bias) <- c("x", runnums[-c(12, 187, 192)]) #low move east
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



#### >> Plots ####

## Plot F0.1 results ##

# Read in existing Fcur/F01 results
FF01.west.base <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/West - 500 Sims - 2/Converged/F01_results_converge.csv")
FF01.east.base <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/East - 500 Sims - 1/Converged/F01_results_converge.csv")
FF01.west.low  <- read.csv("C:/Users/mmorse1/Documents/Simulations_lomov/West/Converged/F01_results_converge.csv")
FF01.east.low  <- read.csv("C:/Users/mmorse1/Documents/Simulations_lomov/East/Converged/F01_results_converge.csv")
FF01.west.self <- read.csv("C:/Users/mmorse1/Documents/Simulations_selftest/West/Converged/F01_results_converge.csv")
FF01.east.self <- read.csv("C:/Users/mmorse1/Documents/Simulations_selftest/East/Converged/F01_results_converge.csv")


# Boxplots of Fcur/F0.1 for each stock
F01.data.w.base <- as.data.frame(FF01.west.base[3, -1])
F01.data.w.base <- t(rbind(rep("West", ncol(F01.data.w.base)), rep("Cross-test", ncol(F01.data.w.base)), F01.data.w.base))
colnames(F01.data.w.base) <- c("stock", "scenario", "ratio")

F01.data.e.base <- as.data.frame(FF01.east.base[3, -1])
F01.data.e.base <- t(rbind(rep("East", ncol(F01.data.e.base)), rep("Cross-test", ncol(F01.data.e.base)), F01.data.e.base))
colnames(F01.data.e.base) <- c("stock", "scenario", "ratio")

F01.data.w.low <- as.data.frame(FF01.west.low[3, -1])
F01.data.w.low <- t(rbind(rep("West", ncol(F01.data.w.low)), rep("Low-move", ncol(F01.data.w.low)), F01.data.w.low))
colnames(F01.data.w.low) <- c("stock", "scenario", "ratio")

F01.data.e.low <- as.data.frame(FF01.east.low[3, -1])
F01.data.e.low <- t(rbind(rep("East", ncol(F01.data.e.low)), rep("Low-move", ncol(F01.data.e.low)), F01.data.e.low))
colnames(F01.data.e.low) <- c("stock", "scenario", "ratio")

F01.data.w.self <- as.data.frame(FF01.west.self[3, -1])
F01.data.w.self <- t(rbind(rep("West", ncol(F01.data.w.self)), rep("Self-test", ncol(F01.data.w.self)), F01.data.w.self))
colnames(F01.data.w.self) <- c("stock", "scenario", "ratio")

F01.data.e.self <- as.data.frame(FF01.east.self[3, -1])
F01.data.e.self <- t(rbind(rep("East", ncol(F01.data.e.self)), rep("Self-test", ncol(F01.data.e.self)), F01.data.e.self))
colnames(F01.data.e.self) <- c("stock", "scenario", "ratio")

F01.data <- rbind(F01.data.w.base, F01.data.e.base,
                  F01.data.w.low,  F01.data.e.low,
                  F01.data.w.self, F01.data.e.self) %>%
  as.data.frame()
F01.data$ratio <- as.numeric(as.character(F01.data$ratio))


ggplot(data = F01.data, aes(x = factor(scenario), y = ratio)) +
  geom_boxplot(data = F01.data, aes(fill = factor(stock)), outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0, 2, 1), labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,2)) +
  labs(y="Fcurrent/F0.1", x="") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) 


## New plots (for 12/4/19 ICES submission) - VERSION 1 ##

w.cross <- ggplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Cross-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Cross-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  geom_abline(intercept = 0.167727284245676, slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = 0.414965843264025, slope=0, linetype=2, size=1) + #true stock
  coord_cartesian(ylim = c(0,1.5)) +
  labs(y="Fcurrent/F0.1", x="") +
  theme(plot.margin = unit(c(0,0,0,.2), "cm")) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   face = "bold",
                                   color = 1),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22))

e.cross <- ggplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Cross-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Cross-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  geom_abline(intercept = 0.0206818108502776, slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = 0.115470838578193, slope=0, linetype=2, size=1) + #true stock
  coord_cartesian(ylim = c(0,1.5))  +
  labs(y="", x="") +
  theme(plot.margin = unit(c(0,.2,0,-.1), "cm")) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   face = "bold",
                                   color = 1),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

w.self  <- ggplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Self-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Self-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = 1, slope=0, linetype=3, size=1) +
  geom_abline(intercept = 0.414283741188939, slope=0, linetype=1, size=1) + #true
  coord_cartesian(ylim = c(0,1.5)) +
  labs(y="Fcurrent/F0.1", x="") +
  theme(plot.margin = unit(c(0,0,0,.2), "cm")) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   face = "bold",
                                   color = 1),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22))

e.self  <- ggplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Self-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Self-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = 1, slope=0, linetype=3, size=1) +
  geom_abline(intercept = 0.115735775896511, slope=0, linetype=1, size=1) + #true
  coord_cartesian(ylim = c(0,1.5))  +
  labs(y="", x="") +
  theme(plot.margin = unit(c(0,.2,0,-.1), "cm")) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   face = "bold",
                                   color = 1),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

p1 <- plot_grid(w.cross, e.cross,
          ncol = 2, nrow = 1, rel_widths = c(10, 8))
p2 <- plot_grid(w.self, e.self,
                ncol = 2, nrow = 1, rel_widths = c(10, 8))

jpeg("C:/Users/mmorse1/Documents/Publishing/Revisions - Bluefin Tuna Simulations/ICES JMS Review/Revisions for 12-4-19/FcurF01.jpeg",
     width = 4000, height = 1750, units = "px", quality = 100, res = 300)
plot_grid(p1, p2,
          ncol = 2, nrow = 1, 
          labels = "AUTO", label_fontfamily = "Times New Roman", label_size = 22)
dev.off()


w.low  <- ggplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Low-move")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Low-move")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  geom_abline(intercept = 0.150184015876974, slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = 0.414965843264025, slope=0, linetype=2, size=1) + #true stock
  coord_cartesian(ylim = c(0,2)) +
  labs(y="Fcurrent/F0.1", x="") +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   face = "bold",
                                   color = 1),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 22))

e.low  <- ggplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Low-move")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Low-move")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  geom_abline(intercept = 0.0145333333333333, slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = 0.115470838578193, slope=0, linetype=2, size=1) + #true stock
  coord_cartesian(ylim = c(0,2))  +
  labs(y="", x="") +
  theme(plot.margin = unit(c(0,0,0,-.1), "cm")) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 22,
                                   face = "bold",
                                   color = 1),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

jpeg("C:/Users/mmorse1/Documents/Publishing/Revisions - Bluefin Tuna Simulations/ICES JMS Review/Revisions for 12-4-19/FcurF01_lowmov.jpeg",
     width = 2000, height = 2000, units = "px", quality = 100, res = 300)
plot_grid(w.low, e.low, rel_widths = c(10,8))
dev.off()


## NEW plots (for 12/4/19 ICES submission) - VERSION 2 ##

grob1 <- grid.text("E", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
w.cross.2 <-
  ggplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Cross-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Cross-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1.5) +
  geom_abline(intercept = 0.167727284245676, slope=0, linetype=1, size=1.5) + #true population
  geom_abline(intercept = 0.414965843264025, slope=0, linetype=2, size=1.5) + #true stock
  coord_cartesian(ylim = c(0,1.5), clip = "off") +
  labs(y="Fcurrent/F0.1", x="", title = "") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10))) +
  annotation_custom(grob1)

grob2 <- grid.text("F", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
e.cross.2 <-
  ggplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Cross-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Cross-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1.5) +
  geom_abline(intercept = 0.0206818108502776, slope=0, linetype=1, size=1.5) + #true population
  geom_abline(intercept = 0.115470838578193, slope=0, linetype=2, size=1.5) + #true stock
  coord_cartesian(ylim = c(0,1.5), clip = "off")  +
  labs(y="", x="", title = " ") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10))) +
  annotation_custom(grob2)

grob3 <- grid.text("E", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
w.self.2  <- ggplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Self-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Self-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = 1, slope=0, linetype=3, size=1.5) +
  geom_abline(intercept = 0.414283741188939, slope=0, linetype=1, size=1.5) + #true
  coord_cartesian(ylim = c(0,1.5), clip = "off") +
  labs(y="Fcurrent/F0.1", x="", title = " ") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10))) +
  annotation_custom(grob3)

grob4 <- grid.text("F", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
e.self.2  <- ggplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Self-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Self-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = 1, slope=0, linetype=3, size=1.5) +
  geom_abline(intercept = 0.115735775896511, slope=0, linetype=1, size=1.5) + #true
  coord_cartesian(ylim = c(0,1.5), clip = "off")  +
  labs(y="", x="", title = " ") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10))) +
  annotation_custom(grob4)

grob5 <- grid.text("E", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
w.low.2  <- ggplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Low-move")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Low-move")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1.5) +
  geom_abline(intercept = 0.150184015876974, slope=0, linetype=1, size=1.5) + #true population
  geom_abline(intercept = 0.414965843264025, slope=0, linetype=2, size=1.5) + #true stock
  coord_cartesian(ylim = c(0,2), clip = "off") +
  theme_classic() +
  labs(y="Fcurrent/F0.1", x="", title = " ") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10))) +
  annotation_custom(grob5)

grob6 <- grid.text("F", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
e.low.2  <- ggplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Low-move")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("East") & scenario %in% c("Low-move")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1.5) +
  geom_abline(intercept = 0.0145333333333333, slope=0, linetype=1, size=1.5) + #true population
  geom_abline(intercept = 0.115470838578193, slope=0, linetype=2, size=1.5) + #true stock
  coord_cartesian(ylim = c(0,2), clip = "off")  +
  theme_classic() +
  labs(y="", x="", title = " ") +
  theme(axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10))) +
  annotation_custom(grob6)







## See old plots below ##

# F01.data.w <- as.data.frame(FF01.west[3,])
# F01.data.w <- cbind(rep("West", nrow(F01.data.w)), F01.data.w)
# colnames(F01.data.w) <- c("stock", "ratio")
# F01.data.e <- as.data.frame(FF01.east[3,])
# F01.data.e <- cbind(rep("East", nrow(F01.data.e)), F01.data.e)
# colnames(F01.data.e) <- c("stock", "ratio")
# F01.data <- rbind(F01.data.w, F01.data.e)
# true.df <- data.frame(c(0.119511074319948, 0.336578739314839, 0.293933398300163, 0.620620839)) #east pop, stock; west pop, stock
# true.df2 <- cbind(c("east", "east", "west", "west"), c("pop", "stock", "pop", "stock"), true.df)
# colnames(true.df2) <- c("stock", "view", "value")

West.base <- ggplot(data=subset(F01.data.base, stock %in% c("West")), aes(x=stock, y=ratio)) +
  # geom_rect(fill = "palegreen2", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, alpha = 0.01) +
  # geom_rect(fill = "indianred2", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, alpha = 0.01) +
  geom_boxplot(data=subset(F01.data.base, stock %in% c("West")), color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = Expl_status_om[1, 2], slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = Expl_status_om[2, 2], slope=0, linetype=2, size=1) + #true stock
  geom_abline(intercept=1, slope=0, linetype=3, size=1) +
  theme_classic() +
  scale_y_discrete(breaks = seq(0, 4, 1), labels = scales::number_format(accuracy = 0.1)) +
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

East.base <- ggplot(data=subset(F01.data.base, stock %in% c("East")), aes(x=stock, y=ratio)) +
  # geom_rect(fill = "palegreen2", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, alpha = 0.01) +
  # geom_rect(fill = "indianred2", xmin = -Inf, xmax = Inf, ymin = 1, ymax = Inf, alpha = 0.01) +
  geom_boxplot(data=subset(F01.data.base, stock %in% c("East")), color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = Expl_status_om[1, 1], slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = Expl_status_om[2, 1], slope=0, linetype=2, size=1) + #true stock
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

West.low <- ggplot(data=subset(F01.data.low, stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F01.data.low, stock %in% c("West")), color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = Expl_status_om[1, 2], slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = Expl_status_om[2, 2], slope=0, linetype=2, size=1) + #true stock
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

East.low <- ggplot(data=subset(F01.data.low, stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F01.data.low, stock %in% c("East")), color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = Expl_status_om[1, 1], slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = Expl_status_om[2, 1], slope=0, linetype=2, size=1) + #true stock
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

West.self <- ggplot(data=subset(F01.data.self, stock %in% c("West")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F01.data.self, stock %in% c("West")), color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = Expl_status_om[1, 2], slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = Expl_status_om[2, 2], slope=0, linetype=2, size=1) + #true stock
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

East.self <- ggplot(data=subset(F01.data.self, stock %in% c("East")), aes(x=stock, y=ratio)) +
  geom_boxplot(data=subset(F01.data.self, stock %in% c("East")), color="lightblue4", fill="lightblue1") +
  geom_abline(intercept = Expl_status_om[1, 1], slope=0, linetype=1, size=1) + #true population
  geom_abline(intercept = Expl_status_om[2, 1], slope=0, linetype=2, size=1) + #true stock
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

jpeg("C:/Users/mmorse1/Documents/Publishing/Revisions - Bluefin Tuna Simulations/ICES JMS Review/Revisions for 12-4-19.jpeg",
     width = 2000, height = 1000, units = "px", quality = 100, res = 300)
# plot_grid(West.base, East.base, West.low, East.low, West.self, East.self, rel_widths = c(10,9,9,9,9,9))
plot_grid(West.base, East.base, West.low, East.low, rel_widths = c(10,9,9,9))
dev.off()



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




























#### ///// ####

###################################### F/F01 for full time series ######################################



## Settings ##

dir_scen  <- "Simulations_2"        #directory name for the scenario (e.g., base, low movement, self-test)
dir_om    <- "OM_Base_Output"            #directory name for the OM outputs



#### >> True F0.1s from OM - Cross-tests ####

## Population ##
# read in parameters
nage     <- 29
biolparm <- as.matrix(read.csv(paste0("C:/Users/mmorse1/Documents/", dir_scen, "/R Code + Inputs/BFTBiolparm.csv")), header = T)
M        <- array(biolparm[1:nage,2:3],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) #annualized M
waa      <- array(biolparm[1:nage,4:5],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) #weight-at-age
naa      <- as.matrix(read.csv(paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/naa.csv")), header = T)[, -1] %>%
  array(dim = c(42, 29, 4, 7, 2), dimnames = list(year = 1974:2015, age = 1:29, quarter = 1:4, zone = 1:7, unit = 1:2))
naa_2 <- array(NA, dim = c(42, 29, 2), dimnames = list(year = 1974:2015, age = 1:29, unit = 1:2))
for (y in 1:42)
  for (a in 1:29)
    for (u in 1:2) {
      naa_2[y, a, u] <- sum(naa[y, a, 1, 1:7, u])  #calculate quarter 1 population naa (sum over zones)
    }  

Fa.p <- array(NA, c(41, 28, 2), dimnames = list(year = 1974:2014, age = 1:28, unit = 1:2)) 
for (y in 1:41)
  for (a in 1:28)
    for (u in 1:2) {
      Fa.p[y, a, u] <- log(naa_2[y, a, u] / naa_2[y + 1, a + 1, u]) - M[a, u] #calculate population F at year, age
    }



# calculate partial recruitment - population (years Y-2 to Y)
P_om_fin_e <- array(NA, c(39, 10), dimnames = list(iter = 1:39, age = 1:10))
P_om_fin_w <- array(NA, c(39, 16), dimnames = list(iter = 1:39, age = 1:16))

for (i in 1:39) {
  
  P_om_e <- array(NA, c(3, 10), dimnames = list(year = 1:3, age = 1:10))
  P_om_w <- array(NA, c(3, 16), dimnames = list(year = 1:3, age = 1:16))
  
    for (u in 1) {
      Ffull_om <- max(Fa.p[i, , u])
      for (a in 1:10) {
        P_om_e[1, a] <- Fa.p[i, a, u]/Ffull_om
      }
      Ffull_om <- max(Fa.p[(i+1), , u])
      for (a in 1:10) {
        P_om_e[2, a] <- Fa.p[(i+1), a, u]/Ffull_om
      }
      Ffull_om <- max(Fa.p[(i+2), , u])
      for (a in 1:10) {
        P_om_e[3, a] <- Fa.p[(i+2), a, u]/Ffull_om
      }
    }
    for (u in 2) {
      Ffull_om <- max(Fa.p[i, , u])
      for (a in 1:16) {
        P_om_w[1, a] <- Fa.p[i, a, u]/Ffull_om
      }
      Ffull_om <- max(Fa.p[(i+1), , u])
      for (a in 1:16) {
        P_om_w[2, a] <- Fa.p[(i+1), a, u]/Ffull_om
      }
      Ffull_om <- max(Fa.p[(i+2), , u])
      for (a in 1:16) {
        P_om_w[3, a] <- Fa.p[(i+2), a, u]/Ffull_om
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
  #east
  for (a in 1:10)
  {
    P_om_fin_e[i, a] <- P_om_avg_e[a]/max(P_om_avg_e)
  }
  
  #west
  for (a in 1:16)
  {
    P_om_fin_w[i, a] <- P_om_avg_w[a]/max(P_om_avg_w)
  }
  
}





## Stock ##
# read in true fishing mortality array - stock 
Fa.s <- as.matrix(read.csv(paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/Fa.csv"), header = T))[, -1] %>%
  array(dim = c(42, 29, 4, 7), dimnames = list(year = 1974:2015, age = 1:29, quarter = 1:4, zone = 1:7))
Fa.s2 <- array(NA, dim = c(42, 29, 2), dimnames = list(year = 1974:2015, age = 1:29, stock = 1:2))
for (y in 1:42)
  for (a in 1:29){
    Fa.s2[y, a, 1] <- sum(Fa.s[y, a, 1:4, 4:7])  #annual F (summed over quarters) 
    Fa.s2[y, a, 2] <- sum(Fa.s[y, a, 1:4, 1:3])  #annual F (summed over quarters) 
  }


# calculate partial recruitment - stock (years Y-2 to Y)
P_oms_fin_e <- array(NA, c(40, 10), dimnames = list(iter = 1:40, age = 1:10))
P_oms_fin_w <- array(NA, c(40, 16), dimnames = list(iter = 1:40, age = 1:16))

for (i in 1:40) {

  P_oms_e <- array(NA, c(3, 10), dimnames = list(year = 1:3, age = 1:10))
  P_oms_w <- array(NA, c(3, 16), dimnames = list(year = 1:3, age = 1:16))
  
  for (s in 1) {
    Ffull_om <- max(Fa.s2[i, , s])
    for (a in 1:10) {
      P_oms_e[1, a] <- Fa.s2[i, a, s]/Ffull_om
    }
    Ffull_om <- max(Fa.s2[(i+1), , s])
    for (a in 1:10) {
      P_oms_e[2, a] <- Fa.s2[(i+1), a, s]/Ffull_om
    }
    Ffull_om <- max(Fa.s2[(i+2), , s])
    for (a in 1:10) {
      P_oms_e[3, a] <- Fa.s2[(i+2), a, s]/Ffull_om
    }
  }
  for (s in 2) {
    Ffull_om <- max(Fa.s2[i, , s])
    for (a in 1:16) {
      P_oms_w[1, a] <- Fa.s2[i, a, s]/Ffull_om
    }
    Ffull_om <- max(Fa.s2[(i+1), , s])
    for (a in 1:16) {
      P_oms_w[2, a] <- Fa.s2[(i+1), a, s]/Ffull_om
    }
    Ffull_om <- max(Fa.s2[(i+2), , s])
    for (a in 1:16) {
      P_oms_w[3, a] <- Fa.s2[(i+2), a, s]/Ffull_om
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
  #east
  for (a in 1:10)
  {
    P_oms_fin_e[i, a] <- P_oms_avg_e[a]/max(P_oms_avg_e)
  }
  
  #west
  for (a in 1:16)
  {
    P_oms_fin_w[i, a] <- P_oms_avg_w[a]/max(P_oms_avg_w)
  }
  
}  




## Calculate true OM F0.1 ##
F01_omp <- array(NA, c(39, 2), dimnames = list(iter = 1:39, population = c("east", "west")))
F01_oms <- array(NA, c(40, 2), dimnames = list(iter = 1:40, stock = c("east", "west")))

for (i in 1:39) {
  #east pop
  YPR <- ypr(age = seq(1, 10, 1), wgt = waa[1:10, 1], partial = P_om_fin_e[i, ], 
             M = M[1:10, 1], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
  F01_omp[i, 1] <- YPR$Reference_Points[1,1]
  
  #west pop
  YPR <- ypr(age = seq(1, 16, 1), wgt = waa[1:16, 2], partial = P_om_fin_w[i, ], 
             M = M[1:16, 2], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
  F01_omp[i, 2] <- YPR$Reference_Points[1,1]
}

for (i in 1:40) {
  #east stock
  YPR <- ypr(age = seq(1, 10, 1), wgt = waa[1:10, 1], partial = P_oms_fin_e[i, ], 
             M = M[1:10, 1], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
  F01_oms[i, 1] <- YPR$Reference_Points[1,1]
  
  #west stock
  YPR <- ypr(age = seq(1, 16, 1), wgt = waa[1:16, 2], partial = P_oms_fin_w[i, ], 
             M = M[1:16, 2], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE)
  F01_oms[i, 2] <- YPR$Reference_Points[1,1]
}


## Derive reference ages (where partial R is > or = 0.8) ##

# ref.pop <- array(NA, c(39, 2), dimnames = list(iter = 1:39, population = c("east", "west")))
# ref.stk <- array(NA, c(40, 2), dimnames = list(iter = 1:40, stock      = c("east", "west")))
ref.east <- vector(mode = "list")
ref.west <- vector(mode = "list")
ref.east.s <- vector(mode = "list")
ref.west.s <- vector(mode = "list")

for (i in 1:39) {
  # ref.pop[i, 1] <- which(P_om_fin_e[i, ]  >= 0.8)  #east population
  # ref.pop[i, 2] <- which(P_om_fin_w[i, ]  >= 0.8)  #west population
  ref.east[[i]] <- which(P_om_fin_e[i, ]  >= 0.8)  #east population
  ref.west[[i]] <- which(P_om_fin_w[i, ]  >= 0.8)  #east population

}

for (i in 1:40) {
  # ref.stk[i, 1] <- which(P_oms_fin_e[i, ] >= 0.8)  #east stock
  # ref.stk[i, 2] <- which(P_oms_fin_w[i, ] >= 0.8)  #west stock
  ref.east.s[[i]] <- which(P_oms_fin_e[i, ]  >= 0.8)  #east stock
  ref.west.s[[i]] <- which(P_oms_fin_w[i, ]  >= 0.8)  #east stock
}


## Adjust F0.1 for the reference ages ##
# using the average partial recruitment for the reference ages
for (i in 1:39) {
  F01_omp[i, 1] <- F01_omp[i, 1] * mean(P_om_fin_e[i, ref.east[[i]]])
  F01_omp[i, 2] <- F01_omp[i, 2] * mean(P_om_fin_w[i, ref.west[[i]]])
}

for (i in 1:40) {
  F01_oms[i, 1] <- F01_oms[i, 1] * mean(P_oms_fin_e[i, ref.east.s[[i]]])
  F01_oms[i, 2] <- F01_oms[i, 2] * mean(P_oms_fin_w[i, ref.west.s[[i]]])
}




## Determine stock status ##

# Calculate Fy (average over reference ages) for each year
F_p <- array(NA, c(39, 2), dimnames = list(year = 1976:2014, unit = c("east", "west"))) 
F_s <- array(NA, c(40, 2), dimnames = list(year = 1976:2015, unit = c("east", "west")))

for (i in 1:39) {
  F_p[i, 1] <- mean(Fa.p[(i+2), ref.east[[i]], 1]) #east population
  F_p[i, 2] <- mean(Fa.p[(i+2), ref.west[[i]], 2]) #west population
  }

for (i in 1:40) {
  F_s[i, 1] <- mean(Fa.s2[(i+2), ref.east.s[[i]], 1]) #east stock
  F_s[i, 2] <- mean(Fa.s2[(i+2), ref.west.s[[i]], 2]) #west stock
}



# Calculate Fy/F0.1
Expl_status_om_p <- array(NA, c(39, 2), dimnames = list(year = 1976:2014, unit = c("east", "west")))
Expl_status_om_s <- array(NA, c(40, 2), dimnames = list(year = 1976:2015, unit = c("east", "west")))

for (i in 1:39) {
  Expl_status_om_p[i, 1] <- F_p[i, 1] / F01_omp[i, 1] #east population
  Expl_status_om_p[i, 2] <- F_p[i, 2] / F01_omp[i, 2] #west population
}
for (i in 1:40) {
  Expl_status_om_s[i, 1] <- F_s[i, 1] / F01_oms[i, 1] #east stock
  Expl_status_om_s[i, 2] <- F_s[i, 2] / F01_oms[i, 2] #west stock
}


# Save true OM values (Fy, F0.1, Fy/F0.1)
write.csv(F_p, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F_p_om.csv"))
write.csv(F_s, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F_s_om.csv"))
write.csv(F01_omp, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F01_omp.csv"))
write.csv(F01_oms, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F01_oms.csv"))
write.csv(Expl_status_om_p, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F_F01_p_om.csv"))
write.csv(Expl_status_om_s, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F_F01_s_om.csv"))







#### >> Estimated F0.1s from VPA ####


## Define variables ##
dir_stock <- "East - 500 Sims - 1"  #directory name for the stock; for estimation model calcs only
stock     <- 1                      #for estimation model calcs only; east (1) vs. west (2) 
wd        <- paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_stock, "/Converged") #switch folder
setwd(wd)
filenums  <- gsub("[A-z \\.\\(\\)]", "", 
                  list.files(path = paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_stock, "/Converged"), pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters
runnums   <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
nyr       <- 42

if (stock == 1) #reference ages (from OM) and plus group age
{
  a.ref <- ref.east
  nage <- 10 
  alph <- "E"
} else { 
  a.ref <- ref.west
  nage <- 16
  alph <- "W"
}


## Calculations ##

FF01 <- array(NA, c(length(runnums), 40, 3), dimnames=list(realization = runnums, year = 1976:2015, reference = c("Fcur", "F01", "Fcur/F01")))


# NOTE: in the low movement scenario, ypr function got stuck on east runnums 15, 202, 207 so those were skipped. (runnums[-c(12, 187, 192)])
for (i in runnums) {
# for (i in runnums[-c(207, 261)]) { #base move
  # for (i in runnums[-c(12, 187, 192)]) { #low move
  
  ## Read in Results files ##
  result.filename <- paste("BFT", alph, "2017_", i, "_RESULTS.R", sep="")
  
  result.file <- as.data.frame(read.table(file = result.filename,
                                          fill = T, col.names = 1:max(count.fields(
                                            result.filename
                                          ))))
  
  F.mat <- as.matrix(result.file[32:73, 2:(nage+1)], nrow=nyr, ncol=nage) #save F-at-age matrix
  F.mat <- apply(F.mat, c(1,2), as.numeric)
  
  ## Partial recruitment for each set of 3 years ##
  
  P.vpa.fin <- array(NA, c(40, nage), dimnames = list(iter = 1:40, age = 1:nage))
  F01.vpa <- rep(NA, 40)
  F.cur.vpa <- rep(NA, 40)
  F01.status.vpa <- rep(NA, 40)
  
  for (y in 1:40) {
    
    P.vpa <- array(NA, c(3, nage), dimnames = list(year = 1:3, age = 1:nage))
    
    Ffull.vpa <- max(F.mat[y, ])
    for (a in 1:nage) {
      P.vpa[1, a] <- F.mat[y, a]/Ffull.vpa
    } 
    
    Ffull.vpa <- max(F.mat[(y+1), ])
    for (a in 1:nage) {
      P.vpa[2, a] <- F.mat[(y+1), a]/Ffull.vpa
    }   
    
    Ffull.vpa <- max(F.mat[(y+2), ])
    for (a in 1:nage) {
      P.vpa[3, a] <- F.mat[(y+2), a]/Ffull.vpa
    }
    
    
    # Average partial recruitment for each age over the 3 years
    P.vpa.avg <- rep(NA, nage)
    for (a in 1:nage) {
      P.vpa.avg[a] <- mean(P.vpa[, a])
    }
    
    
    # Rescaled to 1
    P.vpa.fin <- rep(NA, nage)
    Ffull.vpa2 <- max(P.vpa.avg)
    for (a in 1:nage) {
      P.vpa.fin[a] <- P.vpa.avg[a]/Ffull.vpa2
    }
    
    
    # for (a in 1:nage) {
    #   if (P.vpa.fin[a] == 0) {
    #     P.vpa.fin[a] <- 0.00001
    #   }
    # }
    
    
    ## Calculate F0.1 ##
    YPR <- ypr(age = seq(1, nage, 1), wgt = waa[1:nage, stock], partial = P.vpa.fin, 
               M = M[1:nage, stock], plus = FALSE, maxF = 1.0, incrF = 0.01, graph = FALSE) #changed to plus = FALSE for low move scenario
    F01 <- YPR$Reference_Points[1, 1]
    
    # Calculate F0.1 adjusted for the reference ages using the average partial recruitment for the reference ages
    if (y <= 39) {
      F01.vpa[y] <- F01 * mean(P.vpa.fin[a.ref[[y]]]) # Calculate F0.1 adjusted for the reference ages using the average partial recruitment for the reference ages
      F.cur.vpa[y] <- mean(F.mat[(y+2), a.ref[[y]]])  # Calculate Fcurrent 
    } else {
      F01.vpa[y] <- F01 * mean(P.vpa.fin[a.ref[[y-1]]]) # Calculate F0.1 adjusted for the reference ages using the average partial recruitment for the reference ages (using reference ages from 2014, the last year calculated for the true population)
      F.cur.vpa[y] <- mean(F.mat[(y+2), a.ref[[y-1]]])  # Calculate Fcurrent (using reference ages from 2014, the last year calculated for the true population)
      }
  
    
    ## Determine stock status ##
    
    # Calculate Fcurrent/F01
    F01.status.vpa[y] <- F.cur.vpa[y] / F01.vpa[y]
    
    FF01[which(runnums==i), y, 1] <- F.cur.vpa[y]
    FF01[which(runnums==i), y, 2] <- F01.vpa[y]
    FF01[which(runnums==i), y, 3] <- F01.status.vpa[y]

  }

}



## Save F0.1 results ##
save(FF01, file = "F01_results_allyrs.rdata")
write.csv(FF01, "F01_results_allyrs.csv") #large file takes a while to save














#### >> Plots ####

## Read in F/F01 results @@
FF01.w  <- load("C:/Users/mmorse1/Documents/Simulations_2/West - 500 Sims - 2/Converged/F01_results_allyrs.rdata")
FF01.w  <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/West - 500 Sims - 2/Converged/F01_results_allyrs.csv", header = T)
FF01.w1 <- array(FF01.west, c(405, 40, 3), dimnames = list(realization = 1:405, year = 1976:2015, reference = c("Fcur", "F01", "Fcur/F01")))

FF01.e  <- load("C:/Users/mmorse1/Documents/Simulations_2/East - 500 Sims - 1/Converged/F01_results_allyrs.rdata")
FF01.e  <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/East - 500 Sims - 1/Converged/F01_results_allyrs.csv", header = T)
FF01.e1 <- array(FF01.east, c(XXX, 40, 3), dimnames = list(realization = 1:XXX, year = 1976:2015, reference = c("Fcur", "F01", "Fcur/F01")))

FF01.pop <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/OM_Base_Output/F_F01_p_om.csv", header = T)
FF01.stk <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/OM_Base_Output/F_F01_s_om.csv", header = T)



## West boxplots ##
FF01.w2 <- as.data.frame(t(FF01.w1[, , 3]))     #take only F/F01 values, transpose, and convert to data frame
years   <- matrix(1974:2015, nrow = 42, ncol=1) #create column of year values
FF01.w2[, 1] <- years
FF01.w2.1 <- melt(FF01.w2, id.vars = "years")

FF01.omp <- as.data.frame(FF01.pop[, 2]) #create data frame of operating model data
colnames(FF01.omp) <- c("years", "OMP")

FF01.oms <- as.data.frame(FF01.stk[, 2]) #create data frame of operating model data
colnames(FF01.oms) <- c("years", "OMS")

allLevels <- levels(factor(c(FF01.w2.1$years, FF01.omp$years, FF01.oms$years)))  #change the x vars to factors
FF01.w2.1$years <- factor(FF01.w2.1$years,levels=(allLevels))
FF01.omp$years  <- factor(FF01.omp$years,levels=(allLevels))
FF01.oms$years  <- factor(FF01.oms$years,levels=(allLevels))


grob1 <- grid.text("E", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
w.all <-
  ggplot(data = FF01.omp, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data = FF01.w2.1, aes(x=years, y=value), color="lightblue4", fill="lightblue1", outlier.shape = NA) +
  geom_line(data = FF01.oms, aes(x=factor(years), y=OMS, group=1), linetype="dashed", size=1.5, color="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=1.5, color="black") +
  coord_cartesian(ylim = c(0, 1.5), clip = "off") +
  labs(y="",x="", title = " ") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        #axis.title.y = element_text(family = "Times New Roman",
        #                               face = "bold",
        #                              size = 14),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  annotation_custom(grob1)




## East boxplots ##
FF01.e2 <- as.data.frame(t(FF01.e1[, , 3]))     #take only F/F01 values, transpose, and convert to data frame
years   <- matrix(1974:2015, nrow = 42, ncol=1) #create column of year values
FF01.e2[, 1] <- years
FF01.e2.1 <- melt(FF01.e2, id.vars = "years")

FF01.omp <- as.data.frame(FF01.pop[, 1]) #create data frame of operating model data
colnames(FF01.omp) <- c("years", "OMP")

FF01.oms <- as.data.frame(FF01.stk[, 1]) #create data frame of operating model data
colnames(FF01.oms) <- c("years", "OMS")

allLevels <- levels(factor(c(FF01.e2.1$years, FF01.omp$years, FF01.oms$years)))  #change the x vars to factors
FF01.e2.1$years <- factor(FF01.e2.1$years,levels=(allLevels))
FF01.omp$years  <- factor(FF01.omp$years,levels=(allLevels))
FF01.oms$years  <- factor(FF01.oms$years,levels=(allLevels))

grob2 <- grid.text("F", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
e.all <-
  ggplot(data = FF01.omp, aes(x=factor(years), y=OMP)) +
  geom_boxplot(data = FF01.e2.1, aes(x=years, y=value), color="lightblue4", fill="lightblue1", outlier.shape = NA) +
  geom_line(data = FF01.oms, aes(x=factor(years), y=OMS, group=1), linetype="dashed", size=1.5, color="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=1.5, color="black") +
  coord_cartesian(ylim = c(0, 1.5), clip = "off") +
  labs(y="",x="", title = " ") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1974,2015,10)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        #axis.title.y = element_text(family = "Times New Roman",
        #                               face = "bold",
        #                              size = 14),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  annotation_custom(grob2)







#### ///// ####

#### END #####


