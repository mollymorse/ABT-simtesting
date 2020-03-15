################################################################################################
############ BLUEFIN TUNA SIMULATIONS - CALCULATING REFERENCE POINTS (M.MORSE 2018) ############
##################################### V2 update 3-12-2020 ######################################
################################################################################################

# DESCRIPTION
# This code is meant to calculate reference points on the VPA outputs of the simulations, i.e., 
# from existing Results files
# using code for calculating YPR (as opposed to using ypr() function from fishmethods package)

# ** Contact M.Morse for questions: mollymorse@ucsb.edu


############################################ F/F01 for 2012-2014 #############################################



## Load packages ##

library(grid)
library(cowplot)
library(ggplot2)
library(extrafont)
library(dplyr)
library(reshape2)

## Build functions ##

# YPR function
#par  = partial recruitment at age vector
#Fvar = F for which to calculate the YPR
#ages = number of age classes
#stk  = stock ("E" or "W")
ypr_fun <- function(par, Fvar, ages, stk) {
  
  if (stk == "E") {
    mort <- M[1:ages, 1]   #east natural mortality
    wt   <- waa[1:ages, 1] #east weight-at-age
  } else {
    mort <- M[1:ages, 2]   #west natural mortality
    wt   <- waa[1:ages, 2] #west weight-at-age
  }
  
  NAA <- rep(NA, ages)
  CAT <- rep(NA, ages)
  NAA[1] <- 1000
  
  for (a in 2:ages) {
    
    NAA[a] <- NAA[a - 1] * exp(-((Fvar * par[a - 1]) + mort[a - 1]))
    
  }
  
  for (a in 1:ages) {
    
    CAT[a] <- NAA[a] * (1 - exp(-((Fvar * par[a]) + mort[a]))) * ((Fvar * par[a]) / ((Fvar * par[a]) + mort[a]))
    
  }  
  
  YLD <- sum(CAT * wt)
  
  YPR <- YLD / NAA[1]
  
  return(YPR)
  
}

# F01 function
#ypr_par = vector of YPR values
#F_par   = vector of range of F values
F01_fun <- function(ypr_par, F_par) {
  
  slope <- rep(NA, (length(F_par) - 1))
  diffs <- rep(NA, length(slope) - 1)
  
  for (i in 1:(length(F_par) - 1)) {
    slope[i] <- (ypr_par[i+1] - ypr_par[i]) / (F_par[i+1] - F_par[i]) #calculate slopes for a range of Fs (YPRs)
  }
  
  for (i in 2:length(slope)) {
    diffs[i - 1] <- (slope[1] * 0.1) - slope[i] #calculate the difference between 1/10 the slope at the origin and all the other slopes
  }
  
  tmp <- min(abs(diffs[diffs<=0]), diffs[diffs>=0]) #calculate the smallest difference (i.e., closest to 1/10 slope at the origin)
  return(F_par[which(abs(diffs) == tmp)]) #identify and return the F01
  
}


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


# East pop
YPR_vec <- vector()
F_seq   <- seq(0, 1.0, 0.01)
for (i in F_seq) {
  Fs <- i
  YPR_vec <- append(YPR_vec, ypr_fun(P_om_fin_e, Fs, 10, "E")) #calculate YPRs for a range of Fs
}

F01_om[1, 1] <- F01_fun(YPR_vec, F_seq)


# West pop
YPR_vec <- vector()
F_seq   <- seq(0, 1.0, 0.01)
for (i in F_seq) {
  Fs <- i
  YPR_vec <- append(YPR_vec, ypr_fun(P_om_fin_w, Fs, 16, "W")) #calculate YPRs for a range of Fs
}

F01_om[1, 2] <- F01_fun(YPR_vec, F_seq)


# East stock
YPR_vec <- vector()
F_seq   <- seq(0, 1.0, 0.01)
for (i in F_seq) {
  Fs <- i
  YPR_vec <- append(YPR_vec, ypr_fun(P_oms_fin_e, Fs, 10, "E")) #calculate YPRs for a range of Fs
}

F01_om[2, 1] <- F01_fun(YPR_vec, F_seq)


# West stock
YPR_vec <- vector()
F_seq   <- seq(0, 1.0, 0.01)
for (i in F_seq) {
  Fs <- i
  YPR_vec <- append(YPR_vec, ypr_fun(P_oms_fin_w, Fs, 16, "W")) #calculate YPRs for a range of Fs
}

F01_om[2, 2] <- F01_fun(YPR_vec, F_seq)



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
       paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F01_results_om_v3.csv"))








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

# East stock
YPR_vec <- vector()
F_seq   <- seq(0, 1.0, 0.01)
for (i in F_seq) {
  Fs <- i
  YPR_vec <- append(YPR_vec, ypr_fun(P_om_fin_e_self, Fs, 10, "E")) #calculate YPRs for a range of Fs
}

F01_om[1, 1] <- F01_fun(YPR_vec, F_seq)


# West stock
YPR_vec <- vector()
F_seq   <- seq(0, 1.0, 0.01)
for (i in F_seq) {
  Fs <- i
  YPR_vec <- append(YPR_vec, ypr_fun(P_om_fin_w_self, Fs, 16, "W")) #calculate YPRs for a range of Fs
}

F01_om[1, 2] <- F01_fun(YPR_vec, F_seq)



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

dir_stock <- "West - 500 Sims - 2"  #directory name for the stock; for estimation model calcs
stock     <- 2                      #for estimation model calcs; east (1) vs. west (2) 
wd <- paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_stock, "/Converged") #switch folder
filenums <- gsub("[A-z \\.\\(\\)]", "", 
                 list.files(path = paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_stock, "/Converged"), pattern="\\.R$")) #create a list of Results filenames, removing non-numeric characters
runnums <- sort(as.numeric(sub(pattern="2017", replacement="", filenums))) # the ID numbers of runs that converged
nyr <- 42
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

FF01 <- matrix(NA, nrow=3, ncol=1, dimnames=list(reference=c("Fcur", "F01", "Fcur/F01"), value=1))

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


# This will take about 15 sec
for (i in runnums) {

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
  YPR_vec <- vector()
  F_seq   <- seq(0, 1.0, 0.01)
  for (i in F_seq) {
    Fs <- i
    YPR_vec <- append(YPR_vec, ypr_fun(P.vpa.fin, Fs, nage, alph)) #calculate YPRs for a range of Fs
  }

  F01 <- F01_fun(YPR_vec, F_seq)
  
  
  # Calculate F0.1 adjusted for the reference ages using the average partial recruitment for the reference ages
  F01.vpa <- F01 * mean(P.vpa.fin[a.ref])

  
  ## Determine stock status ##
  
  # Calculate Fcurrent
  F.cur.vpa <- mean(F.mat[(nrow(F.mat)-3):(nrow(F.mat)-1),a.ref])
  
  # Calculate Fcurrent/F0.1
  F01.status.vpa <- F.cur.vpa/F01.vpa

  FF01 <- cbind(FF01, c(F.cur.vpa, F01.vpa, F01.status.vpa))
  
}



## Save F0.1 results ##
colnames(FF01) <- c("x", runnums)
write.csv(FF01[,-1], "F01_results_converge_v2.csv")





#### >> Plots ####

## Plot F0.1 results ##

# Read in existing Fcur/F01 results
FF01.west.base <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/West - 500 Sims - 2/Converged/F01_results_converge_v2.csv")
FF01.east.base <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/East - 500 Sims - 1/Converged/F01_results_converge_v2.csv")
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





## NEW plots (for 4/6/20 ICES submission) ##

grob1 <- grid.text("E", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
w.cross.2 <-
  ggplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Cross-test")), aes(x = stock, y = ratio)) +
  geom_boxplot(data = subset(F01.data, stock %in% c("West") & scenario %in% c("Cross-test")), outlier.shape = NA, color="lightblue4", fill="lightblue1") +
  geom_abline(intercept=1, slope=0, linetype=3, size=1.5) +
  geom_abline(intercept = Expl_status_om[1, 2], slope=0, linetype=1, size=1.5) + #true population
  geom_abline(intercept = Expl_status_om[2, 2], slope=0, linetype=2, size=1.5) + #true stock
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
  geom_abline(intercept = Expl_status_om[1, 1], slope=0, linetype=1, size=1.5) + #true population
  geom_abline(intercept = Expl_status_om[2, 1], slope=0, linetype=2, size=1.5) + #true stock
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








#### >> Calculate Bias ####

## West
w.bias.1 <- array(NA, c(2, 2), dimnames = list(unit = c("population", "stock"), type = c("absolute", "relative")))

med <- median(as.numeric(FF01.west.base[3, -1])) #median of all realizations

w.bias.1[1, 1] <- med - Expl_status_om[1, 2]                  #absolute bias with true population values
w.bias.1[1, 2] <- w.bias.1[1, 1] / Expl_status_om[1, 2] * 100 #relative bias with true population values
w.bias.1[2, 1] <- med - Expl_status_om[2, 2]                  #absolute bias with true stock values
w.bias.1[2, 2] <- w.bias.1[2, 1] / Expl_status_om[2, 2] * 100 #relative bias with true stock values


## East
e.bias.1 <- array(NA, c(2, 2), dimnames = list(unit = c("population", "stock"), type = c("absolute", "relative")))

med <- median(as.numeric(FF01.east.base[3, -1])) #median of all realizations

e.bias.1[1, 1] <- med - Expl_status_om[1, 1]                  #absolute bias with true population values
e.bias.1[1, 2] <- e.bias.1[1, 1] / Expl_status_om[1, 1] * 100 #relative bias with true population values
e.bias.1[2, 1] <- med - Expl_status_om[2, 1]                  #absolute bias with true stock values
e.bias.1[2, 2] <- e.bias.1[2, 1] / Expl_status_om[2, 1] * 100 #relative bias with true stock values















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
  # East pop
  YPR_vec <- vector()
  F_seq   <- seq(0, 1.0, 0.01)
  for (j in F_seq) {
    Fs <- j
    YPR_vec <- append(YPR_vec, ypr_fun(P_om_fin_e[i, ], Fs, 10, "E")) #calculate YPRs for a range of Fs
  }
  F01_omp[i, 1] <- F01_fun(YPR_vec, F_seq)
  
  # West pop
  YPR_vec <- vector()
  F_seq   <- seq(0, 1.0, 0.01)
  for (j in F_seq) {
    Fs <- j
    YPR_vec <- append(YPR_vec, ypr_fun(P_om_fin_w[i, ], Fs, 16, "W")) #calculate YPRs for a range of Fs
  }
  F01_omp[i, 2] <- F01_fun(YPR_vec, F_seq)
}

for (i in 1:40) {
  # East stock
  YPR_vec <- vector()
  F_seq   <- seq(0, 1.0, 0.01)
  for (j in F_seq) {
    Fs <- j
    YPR_vec <- append(YPR_vec, ypr_fun(P_oms_fin_e[i, ], Fs, 10, "E")) #calculate YPRs for a range of Fs
  }
  F01_oms[i, 1] <- F01_fun(YPR_vec, F_seq)
  
  # West stock
  YPR_vec <- vector()
  F_seq   <- seq(0, 1.0, 0.01)
  for (j in F_seq) {
    Fs <- j
    YPR_vec <- append(YPR_vec, ypr_fun(P_oms_fin_w[i, ], Fs, 16, "W")) #calculate YPRs for a range of Fs
  }
  F01_oms[i, 2] <- F01_fun(YPR_vec, F_seq)

}





## Derive reference ages (where partial R is > or = 0.8) ##

ref.east <- vector(mode = "list")
ref.west <- vector(mode = "list")
ref.east.s <- vector(mode = "list")
ref.west.s <- vector(mode = "list")

for (i in 1:39) {
  ref.east[[i]] <- which(P_om_fin_e[i, ]  >= 0.8)  #east population
  ref.west[[i]] <- which(P_om_fin_w[i, ]  >= 0.8)  #east population

}

for (i in 1:40) {
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
write.csv(F_p, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F_p_om_v2.csv"))
write.csv(F_s, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F_s_om_v2.csv"))
write.csv(F01_omp, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F01_omp_v2.csv"))
write.csv(F01_oms, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F01_oms_v2.csv"))
write.csv(Expl_status_om_p, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F_F01_p_om_v2.csv"))
write.csv(Expl_status_om_s, paste0("C:/Users/mmorse1/Documents/", dir_scen, "/", dir_om, "/F_F01_s_om_v2.csv"))







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

for (i in runnums) {
  
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

    
    ## Calculate F0.1 ##
    YPR_vec <- vector()
    F_seq   <- seq(0, 1.0, 0.01)
    for (j in F_seq) {
      Fs <- j
      YPR_vec <- append(YPR_vec, ypr_fun(P.vpa.fin, Fs, nage, alph)) #calculate YPRs for a range of Fs
    }
    F01 <- F01_fun(YPR_vec, F_seq)
    
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
saveRDS(FF01, file = "F01_allyrs_v2.rds")












#### >> Plots ####

## Read in F/F01 results ##
FF01.w <- readRDS(file = "C:/Users/mmorse1/Documents/Simulations_2/West - 500 Sims - 2/Converged/F01_allyrs_v2.rds") #load rds object
FF01.e <- readRDS(file = "C:/Users/mmorse1/Documents/Simulations_2/East - 500 Sims - 1/Converged/F01_allyrs_v2.rds") #load rds object

FF01.pop <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/OM_Base_Output/F_F01_p_om_v2.csv", header = T)
FF01.stk <- read.csv("C:/Users/mmorse1/Documents/Simulations_2/OM_Base_Output/F_F01_s_om_v2.csv", header = T)



## West boxplots ##
FF01.w1 <- as.data.frame(t(FF01.w[, , 3]))     #take only F/F01 values, transpose, and convert to data frame
years   <- matrix(1976:2015, nrow = 40, ncol=1) #create column of year values
FF01.w1 <- cbind(years, FF01.w1)
FF01.w2 <- melt(FF01.w1, id.vars = "years")

FF01.omp <- as.data.frame(FF01.pop[, c(1, 3)]) #create data frame of operating model data
colnames(FF01.omp) <- c("years", "OMP")

FF01.oms <- as.data.frame(FF01.stk[, c(1, 3)]) #create data frame of operating model data
colnames(FF01.oms) <- c("years", "OMS")

allLevels <- levels(factor(c(FF01.w2$years, FF01.omp$years, FF01.oms$years)))  #change the x vars to factors
FF01.w2$years <- factor(FF01.w2$years,levels=(allLevels))
FF01.omp$years  <- factor(FF01.omp$years,levels=(allLevels))
FF01.oms$years  <- factor(FF01.oms$years,levels=(allLevels))


grob1 <- grid.text("E", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
w.all <-
  ggplot(data = FF01.omp, aes(x=factor(years), y=OMP)) +
  geom_abline(intercept=1, slope=0, linetype=1, size=1) +
  geom_boxplot(data = FF01.w2, aes(x=years, y=value), color="lightblue4", fill="lightblue1", outlier.shape = NA) +
  geom_line(data = FF01.oms, aes(x=factor(years), y=OMS, group=1), linetype="dashed", size=1.5, color="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=1.5, color="black") +
  coord_cartesian(ylim = c(0, 4), clip = "off") +
  labs(y="Fcurrent/F0.1",x="", title = " ") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1976,2015,10)) +
  theme(plot.title = element_text(family = "Times New Roman",
                                  face = "bold",
                                  size = 24,
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "bold",
                                    size = 24,
                                    margin = margin(r = 10)),
        axis.text.x = element_text(family = "Times New Roman",
                                   size = 20),
        axis.text.y = element_text(family = "Times New Roman",
                                   size = 20)) +
  annotation_custom(grob1)



## East boxplots ##
FF01.e1 <- as.data.frame(t(FF01.e[, , 3]))     #take only F/F01 values, transpose, and convert to data frame
years   <- matrix(1976:2015, nrow = 40, ncol=1) #create column of year values
FF01.e1 <- cbind(years, FF01.e1)
FF01.e2 <- melt(FF01.e1, id.vars = "years")

FF01.omp <- as.data.frame(FF01.pop[, c(1, 2)]) #create data frame of operating model data
colnames(FF01.omp) <- c("years", "OMP")

FF01.oms <- as.data.frame(FF01.stk[, c(1, 2)]) #create data frame of operating model data
colnames(FF01.oms) <- c("years", "OMS")

allLevels <- levels(factor(c(FF01.e2$years, FF01.omp$years, FF01.oms$years)))  #change the x vars to factors
FF01.e2$years <- factor(FF01.e2$years,levels=(allLevels))
FF01.omp$years  <- factor(FF01.omp$years,levels=(allLevels))
FF01.oms$years  <- factor(FF01.oms$years,levels=(allLevels))

grob2 <- grid.text("F", x = unit(-0.15, "npc"), y = unit(1.05, "npc"), gp = gpar(col = 1, fontfamily = "Times New Roman", cex = 2))
e.all <-
  ggplot(data = FF01.omp, aes(x=factor(years), y=OMP)) +
  geom_abline(intercept=1, slope=0, linetype=1, size=1) +
  geom_boxplot(data = FF01.e2, aes(x=years, y=value), color="lightblue4", fill="lightblue1", outlier.shape = NA) +
  geom_line(data = FF01.oms, aes(x=factor(years), y=OMS, group=1), linetype="dashed", size=1.5, color="black") +
  stat_summary(fun.y=mean, geom="line", aes(group=1), size=1.5, color="black") +
  coord_cartesian(ylim = c(0, 4.3), clip = "off") +
  labs(y="",x="", title = " ") +
  theme_classic() +
  scale_x_discrete(breaks = seq(1976,2015,10)) +
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






#### >> Calculate Bias ####

## West
w.bias <- array(NA, c(nrow(FF01.w1), 2, 2), dimnames = list(year = 1976:2015, unit = c("population", "stock"), type = c("absolute", "relative")))
for (i in 1:nrow(FF01.w1)) {
  med <- median(as.numeric(FF01.w1[i, -1]))                        #annual median of all realizations (take out 1st col which is years)
  
  if (i <= 39) {
    w.bias[i, 1, 1] <- med - FF01.pop[i, 3]              #absolute bias with true population values
    w.bias[i, 1, 2] <- w.bias[i, 1, 1] / FF01.pop[i, 3] * 100 #relative bias with true population values
    w.bias[i, 2, 1] <- med - FF01.stk[i, 3]              #absolute bias with true stock values
    w.bias[i, 2, 2] <- w.bias[i, 2, 1] / FF01.stk[i, 3] * 100 #relative bias with true stock values
  } else {
    w.bias[i, 2, 1] <- med - FF01.stk[i, 3]              #absolute bias with true stock values
    w.bias[i, 2, 2] <- w.bias[i, 2, 1] / FF01.stk[i, 3] * 100 #relative bias with true stock values
  }
  
}

median(w.bias[, 1, 1], na.rm = T) #population absolute bias
median(w.bias[, 1, 2], na.rm = T) #population relative bias
median(w.bias[, 2, 1], na.rm = T) #stock absolute bias
median(w.bias[, 2, 2], na.rm = T) #stock relative bias



## East
e.bias <- array(NA, c(nrow(FF01.e1), 2, 2), dimnames = list(year = 1976:2015, unit = c("population", "stock"), type = c("absolute", "relative")))
for (i in 1:nrow(FF01.e1)) {
  med <- median(as.numeric(FF01.e1[i, -1]))                        #annual median of all realizations (take out 1st col which is years)
  
  if (i <= 39) {
    e.bias[i, 1, 1] <- med - FF01.pop[i, 2]              #absolute bias with true population values
    e.bias[i, 1, 2] <- e.bias[i, 1, 1] / FF01.pop[i, 2] * 100 #relative bias with true population values
    e.bias[i, 2, 1] <- med - FF01.stk[i, 2]              #absolute bias with true stock values
    e.bias[i, 2, 2] <- e.bias[i, 2, 1] / FF01.stk[i, 2] * 100 #relative bias with true stock values
  } else {
    e.bias[i, 2, 1] <- med - FF01.stk[i, 2]              #absolute bias with true stock values
    e.bias[i, 2, 2] <- e.bias[i, 2, 1] / FF01.stk[i, 2] * 100 #relative bias with true stock values
  }
  
}

median(e.bias[, 1, 1], na.rm = T) #population absolute bias
median(e.bias[, 1, 2], na.rm = T) #population relative bias
median(e.bias[, 2, 1], na.rm = T) #stock absolute bias
median(e.bias[, 2, 2], na.rm = T) #stock relative bias



#### ///// ####

#### END #####


