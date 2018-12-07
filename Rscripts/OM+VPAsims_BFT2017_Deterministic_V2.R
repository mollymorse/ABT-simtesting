#####################################################################################################
## BLUEFIN TUNA OPERATING MODEL FOR GENERATING PSEUDODATA (L.KERR 2017, modified by M. MORSE 2018) ##
#####################################################################################################

#Conditioned on 2017 Atlantic bluefin tuna stock assessment VPA

#Set the operating model working directory
setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs")


############################ DEFINE MATRICES ##############################

# Eastern pop. u=1, Western pop. u=2, spawning quarter=1, spawning zones=1&7

nyr <- 42  #number of years in OM + VPA
nage <- 29  #number of ages in OM
nageE <- 10  #number of ages in OM (east)
nageW <- 16  #number of ages in OM (west)
ngr <- 27  #number of gear types (fleets) in OM
ngrE <- 10  #number of fleets in east
ngrW <- 17  #number of fleets in west

# NAA matrix for for east, west populations
naa <- array(NA,c(nyr,nage,4,7,2),dimnames=list(year=1974:2015,age=1:nage,quarter=1:4,zones=1:7,unit=1:2))

# Total SSB for east, west populations and stocks
Pssb <-  array(NA,c(nyr,4,7,2),dimnames=list(year=1974:2015,quarter=1:4,zones=1:7,unit=1:2)) #population SSB by zone
T_Pssb <- array(NA,c(nyr,4,2),dimnames=list(year=1974:2015,quarter=1:4,unit=1:2)) #population SSB all zones
T_Essb <- array(NA,c(nyr,4),dimnames=list(year=1974:2015,quarter=1:4)) #east stock SSB
T_Wssb <- array(NA,c(nyr,4),dimnames=list(year=1974:2015,quarter=1:4)) #west stock SSB

# CAA matrix for east, west populations and stocks
Pcaa <- array(NA,c(nyr,nage,4,7,2),dimnames=list(year=1974:2015,age=1:nage,quarter=1:4,zones=1:7,unit=1:2)) #population CAA
Ecaa <- array(NA,c(nyr,nage),dimnames=list(year=1974:2015,age=1:nage)) #east stock CAA
Wcaa <- array(NA,c(nyr,nage),dimnames=list(year=1974:2015,age=1:nage)) #west stock CAA

# Partial CAA for east, west stocks
Epcaa <- array(NA,c(nyr,nage,ngrE),dimnames=list(year=1974:2015,age=1:nage,gear=1:ngrE)) #east stock PCAA
Wpcaa <- array(NA,c(nyr,nage,ngrW),dimnames=list(year=1974:2015,age=1:nage,gear=1:ngrW)) #west stock PCAA

# Yield for east, west populations and stocks
Pyield <- array(NA,c(nyr,4,7,2),dimnames=list(year=1974:2015,quarter=1:4,zones=1:7,unit=1:2)) #population yield by zone
T_Pyield <- array(NA,c(nyr,4,2),dimnames=list(year=1974:2015,quarter=1:4,unit=1:2)) #population yield all zones
T_Eyield <- array(NA,c(nyr),dimnames=list(year=1974:2015)) #east stock yield
T_Wyield <- array(NA,c(nyr),dimnames=list(year=1974:2015)) #west stock yield

# Fishing Mortality matrix
Fa <- array(NA,c(nyr,nage,4,7),dimnames=list(year=1974:2015,age=1:nage,quarter=1:4,zones=1:7))

# Exploitation at age matrix
eaa <- array(NA,c(nyr,nage,4,7),dimnames=list(year=1974:2015,age=1:nage,quarter=1:4,zones=1:7))

# Partial exploitation at age matrix
peaa <- array(NA,c(nyr,nage,ngr),dimnames=list(year=1974:2015,age=1:nage,gear=1:ngr)) #exploitation by fleet for all stocks
Epeaa <- array(NA,c(nyr,nage,ngrE),dimnames=list(year=1974:2015,age=1:nage,gear=1:ngrE)) #exploitation by fleet within east stock
Wpeaa <- array(NA,c(nyr,nage,ngrW),dimnames=list(year=1974:2015,age=1:nage,gear=1:ngrW)) #exploitation by fleet within west stock


####################### CONDITIONING OF MODEL #######################

# Read in biological/stock assessment data
biolparm <- (as.matrix(read.csv("BFTBiolparm.csv"),header=T))  
BFTstockassess <- (as.matrix(read.csv("BFTstockassess.csv"),header=T))  
move_matrix <- (as.matrix(read.csv("MoveMatrix.csv"),header=T))  
FishMort <- (as.matrix(read.csv("F.csv"),header=T))  
Selectivity <- (as.matrix(read.csv("Selectivity.csv"),header=T))  
catchability <- (as.matrix(read.csv("Catchability.csv"),header=T))
effort <- (as.matrix(read.csv("Effort.csv"),header=T))

# Conditioning intial year abundance-at-age based on stock assessment output 
naa[1,1:29,1,7,1] <- array(BFTstockassess[1:nage,5],c(nage,1),dimnames=list(age=1:nage,unit=1)) #east 1974 abundance
naa[1,1:29,1,1,2] <- array(BFTstockassess[1:nage,6],c(nage,1),dimnames=list(age=1:nage,unit=2)) #west 1974 abundance

# Movement matrix
C <- array(move_matrix[1:7,2:1624],c(7,7,4,nage,2),dimnames=list(zone=1:7,zone=1:7,quarter=1:4,age=1:nage,unit=1:2))

# Natural Mortality-at-age 
M <- array(biolparm[1:nage,2:3],c(nage,2),dimnames=list(age=1:nage,unit=1:2)) 
M <- M/4  #natural mortality by seasonal quarter

# Weight-at-age 
waa <- array(biolparm[1:nage,4:5],c(nage,2),dimnames=list(age=1:nage,unit=1:2))

# Maturity-at-age  
maa <- array(biolparm[1:nage,6:7],c(nage,2),dimnames=list(age=1:nage,unit=1:2))

# Recruits for years 1974-2015
R <- array(BFTstockassess[1:nyr,2:3],c(nyr,2),dimnames=list(year=1974:2015,unit=1:2)) 

# Gear Selectivity
Sa <- (array(Selectivity[2:30,2:28],c(nage,ngr),dimnames=list(age=1:nage,gear=1:ngr))) 

# Fleet Catchability
Q <- array(catchability[2,2:28],c(ngr),dimnames=list(gear=1:ngr)) 

# Effort
E <- apply((array(effort[2:43,2:28],c(nyr,ngr),dimnames=list(year=1974:2015,gear=1:ngr))),c(1,2),as.numeric)

# Fishing Mortality: Annual
Fannual <- array(FishMort[1:294,3:31],c(nyr,7,nage),dimnames=list(year=1974:2015,zones=1:7,age=1:nage)) 

# Fishing Mortality: Quarterly proportions
P <- array(BFTstockassess[1:4,8:14],c(4,7),dimnames=list(quarter=1:4,zone=1:7)) 

# Quarterly F at age
for (y in 1:nyr)
  for (a in 1:nage)
    for (q in 1:4)
      for (z in 1:7)       
      {
        Fa[y,a,q,z]<-Fannual[y,z,a]*P[q,z]
      }    

# Exploition at age, calculated based on Baranov's catch equation
for (y in 1:nyr)
  for (a in 1:nage)
    for (q in 1:4)
      for (z in 1:7)
        for (u in 1:2)
        {
          eaa[y,a,q,z]<-((Fa[y,a,q,z])/(Fa[y,a,q,z]+M[a,u]))*(1-exp(-(Fa[y,a,q,z]+M[a,u]))) #eaa doesnt acct for 2 units in M vectors. Should be summing over them? Ask Lisa
        }
#Partial exploitation at age and fleet
for (y in 1:nyr)
  for (a in 1:29)
    for (g in 1:ngr)
      for (u in 1:2)
      {
        peaa[y,a,g]<-((E[y,g]*Q[g]*Sa[a,g])/((E[y,g]*Q[g]*Sa[a,g])+M[a,u]))*(1-exp(-((E[y,g]*Q[g]*Sa[a,g])+M[a,u]))) 
      }
#Stock specific partial exploitation rates
#peaa[is.na(eaa)] <- 0
Wpeaa<-peaa[1:nyr,1:29,1:17]
Epeaa<-peaa[1:nyr,1:29,18:27]

# Abundances at age in year 1 non-spawning zones set equal to zero
for (y in 1)
{  
  for (q in 1)
    for (a in 1:nage)
      for (u in 1:2) 
      {
        naa[y,a,q,1:6,1] <- 0
        naa[y,a,q,2:7,2] <- 0
      }
  
  #Model check:  
  #naa[1,1:29,1,1:7,1]
  #naa[1,1:29,1,1:7,2]
  
  # Calculate abundances at age in year 1, quarter 2-4
  for (q in 2:4)
    for (a in 1:nage)
      for (g in 1:ngr)
        for (z in 1:7)
          for (u in 1:2)
          {
            naa[y,a,q,1,u] <- (naa[y,a,q-1,1,u]*C[1,1,q,a,u]+ naa[y,a,q-1,2,u]*C[2,1,q,a,u]+ naa[y,a,q-1,3,u]*C[3,1,q,a,u]+ naa[y,a,q-1,4,u]*C[4,1,q,a,u]+ naa[y,a,q-1,5,u]*C[5,1,q,a,u]+ naa[y,a,q-1,6,u]*C[6,1,q,a,u]+ naa[y,a,q-1,7,u]*C[7,1,q,a,u])* exp(-((sum(Fa[y,a,q,1]))+ M[a,u]))
            naa[y,a,q,2,u] <- (naa[y,a,q-1,2,u]*C[2,2,q,a,u]+ naa[y,a,q-1,1,u]*C[1,2,q,a,u]+ naa[y,a,q-1,3,u]*C[3,2,q,a,u]+ naa[y,a,q-1,4,u]*C[4,2,q,a,u]+ naa[y,a,q-1,5,u]*C[5,2,q,a,u]+ naa[y,a,q-1,6,u]*C[6,2,q,a,u]+ naa[y,a,q-1,7,u]*C[7,2,q,a,u])* exp(-((sum(Fa[y,a,q,2]))+ M[a,u]))
            naa[y,a,q,3,u] <- (naa[y,a,q-1,3,u]*C[3,3,q,a,u]+ naa[y,a,q-1,1,u]*C[1,3,q,a,u]+ naa[y,a,q-1,2,u]*C[2,3,q,a,u]+ naa[y,a,q-1,4,u]*C[4,3,q,a,u]+ naa[y,a,q-1,5,u]*C[5,3,q,a,u]+ naa[y,a,q-1,6,u]*C[6,3,q,a,u]+ naa[y,a,q-1,7,u]*C[7,3,q,a,u])* exp(-((sum(Fa[y,a,q,3]))+ M[a,u]))
            naa[y,a,q,4,u] <- (naa[y,a,q-1,4,u]*C[4,4,q,a,u]+ naa[y,a,q-1,1,u]*C[1,4,q,a,u]+ naa[y,a,q-1,2,u]*C[2,4,q,a,u]+ naa[y,a,q-1,3,u]*C[3,4,q,a,u]+ naa[y,a,q-1,5,u]*C[5,4,q,a,u]+ naa[y,a,q-1,6,u]*C[6,4,q,a,u]+ naa[y,a,q-1,7,u]*C[7,4,q,a,u])* exp(-((sum(Fa[y,a,q,4]))+ M[a,u]))  
            naa[y,a,q,5,u] <- (naa[y,a,q-1,5,u]*C[5,5,q,a,u]+ naa[y,a,q-1,1,u]*C[1,5,q,a,u]+ naa[y,a,q-1,2,u]*C[2,5,q,a,u]+ naa[y,a,q-1,3,u]*C[3,5,q,a,u]+ naa[y,a,q-1,4,u]*C[4,5,q,a,u]+ naa[y,a,q-1,6,u]*C[6,5,q,a,u]+ naa[y,a,q-1,7,u]*C[7,5,q,a,u])* exp(-((sum(Fa[y,a,q,5]))+ M[a,u]))  
            naa[y,a,q,6,u] <- (naa[y,a,q-1,6,u]*C[6,6,q,a,u]+ naa[y,a,q-1,1,u]*C[1,6,q,a,u]+ naa[y,a,q-1,2,u]*C[2,6,q,a,u]+ naa[y,a,q-1,3,u]*C[3,6,q,a,u]+ naa[y,a,q-1,4,u]*C[4,6,q,a,u]+ naa[y,a,q-1,5,u]*C[5,6,q,a,u]+ naa[y,a,q-1,7,u]*C[7,6,q,a,u])* exp(-((sum(Fa[y,a,q,6]))+ M[a,u])) 
            naa[y,a,q,7,u] <- (naa[y,a,q-1,7,u]*C[7,7,q,a,u]+ naa[y,a,q-1,1,u]*C[1,7,q,a,u]+ naa[y,a,q-1,2,u]*C[2,7,q,a,u]+ naa[y,a,q-1,3,u]*C[3,7,q,a,u]+ naa[y,a,q-1,4,u]*C[4,7,q,a,u]+ naa[y,a,q-1,5,u]*C[5,7,q,a,u]+ naa[y,a,q-1,6,u]*C[6,7,q,a,u])* exp(-((sum(Fa[y,a,q,7]))+ M[a,u])) 
          }
  
  #Model check:  
  #naa[1,1:29,1:4,1:7,2]
  #naa[1,1:29,1:4,1:7,1]
  
  # Calculating spawning stock biomass in year 1
  for (a in 1:nage)
    for (z in 1:7)
      for (q in 1:4)
        for (g in 1:ngr)
          for (u in 1:2)
          {
            Pssb[y,q,z,u]  <-sum(naa[y,1:nage,q,z,u]*waa[1:nage,u]*maa[1:nage,u])
            Pyield[y,q,z,u]<-sum(naa[y,1:nage,q,z,u]*waa[1:nage,u]*eaa[y,1:nage,q,z]) 
            Pcaa[y,a,q,z,u]<-sum(naa[y,a,q,z,u]*eaa[y,a,q,z]) 
          }
  
  # Model check: 
  #Pssb[1,1:4,1:7,1]
  #Pssb[1,1:4,1:7,2]
  
  # Calculating PCAA in year 1
  for (a in 1:nage)
  {
    for (g in 1:ngrW)
    {
      Wpcaa[y,a,g]<-sum(naa[y,a,1:4,1:3,1:2]*Wpeaa[y,a,g]) #western partial catch-at-age
    }
    for (g in 1:ngrE)
    {
      Epcaa[y,a,g]<-sum(naa[y,a,1:4,4:7,1:2]*Epeaa[y,a,g]) #eastern partial catch-at-age
    }
    Wcaa[y,a]<-sum(Pcaa[y,a,1:4,1:3,1:2]) #western stock catch-at-age
    Ecaa[y,a]<-sum(Pcaa[y,a,1:4,4:7,1:2]) #eastern stock catch-at-age
  }
  
  for (q in 1:4)
    for (u in 1:2) 
    {
      T_Pssb[y,q,u] <-sum(Pssb[y,q,1:7,u]) #total bluefin SSB by unit (population)
      T_Wssb[y,q] <-sum(Pssb[y,q,1:3,1:2]) #total western stock SSB
      T_Essb[y,q]  <-sum(Pssb[y,q,4:7,1:2]) #total eastern stock SSB
      T_Pyield[y,q,u]<-sum(Pyield[y,q,1:7,u]) #total bluefin yield by unit (population)
      T_Wyield[y]<-sum(Pyield[y,1:4,1:3,1:2]) #total western stock yield
      T_Eyield[y]<-sum(Pyield[y,1:4,4:7,1:2]) #total eastern stock yield
    }
}  


###################### DETERMINISTIC POP DYNAMICS #######################

for(y in 2:nyr)
{
  # Input time-series of recruits
  for (q in 1)
  {
    naa[y,1,q,7,1] <-R[y,1]
    naa[y,1,q,1:6,1] <-0
    naa[y,1,q,1,2]<-R[y,2]
    naa[y,1,q,2:7,2] <-0
  }
  
  # Fill in naa matrix and calculate SSB
  for (q in 1)
    for (a in 2:nage)
      for (u in 1:2)
      {
        naa[y,a,q,1,u] <- (naa[y-1,a-1,4,1,u]*C[1,1,q,a,u]+ naa[y-1,a-1,4,2,u]*C[2,1,q,a,u]+ naa[y-1,a-1,4,3,u]*C[3,1,q,a,u]+ naa[y-1,a-1,4,4,u]*C[4,1,q,a,u]+ naa[y-1,a-1,4,5,u]*C[5,1,q,a,u]+ naa[y-1,a-1,4,6,u]*C[6,1,q,a,u]+ naa[y-1,a-1,4,7,u]*C[7,1,q,a,u])* exp(-(sum(Fa[y,a,q,1])+ M[a-1,u]))
        naa[y,a,q,2,u] <- (naa[y-1,a-1,4,2,u]*C[2,2,q,a,u]+ naa[y-1,a-1,4,1,u]*C[1,2,q,a,u]+ naa[y-1,a-1,4,3,u]*C[3,2,q,a,u]+ naa[y-1,a-1,4,4,u]*C[4,2,q,a,u]+ naa[y-1,a-1,4,5,u]*C[5,2,q,a,u]+ naa[y-1,a-1,4,6,u]*C[6,2,q,a,u]+ naa[y-1,a-1,4,7,u]*C[7,2,q,a,u])* exp(-(sum(Fa[y,a,q,2])+ M[a-1,u])) 
        naa[y,a,q,3,u] <- (naa[y-1,a-1,4,3,u]*C[3,3,q,a,u]+ naa[y-1,a-1,4,1,u]*C[1,3,q,a,u]+ naa[y-1,a-1,4,2,u]*C[2,3,q,a,u]+ naa[y-1,a-1,4,4,u]*C[4,3,q,a,u]+ naa[y-1,a-1,4,5,u]*C[5,3,q,a,u]+ naa[y-1,a-1,4,6,u]*C[6,3,q,a,u]+ naa[y-1,a-1,4,7,u]*C[7,3,q,a,u])* exp(-(sum(Fa[y,a,q,3])+ M[a-1,u]))
        naa[y,a,q,4,u] <- (naa[y-1,a-1,4,4,u]*C[4,4,q,a,u]+ naa[y-1,a-1,4,1,u]*C[1,4,q,a,u]+ naa[y-1,a-1,4,2,u]*C[2,4,q,a,u]+ naa[y-1,a-1,4,3,u]*C[3,4,q,a,u]+ naa[y-1,a-1,4,5,u]*C[5,4,q,a,u]+ naa[y-1,a-1,4,6,u]*C[6,4,q,a,u]+ naa[y-1,a-1,4,7,u]*C[7,4,q,a,u])* exp(-(sum(Fa[y,a,q,4])+ M[a-1,u]))
        naa[y,a,q,5,u] <- (naa[y-1,a-1,4,5,u]*C[5,5,q,a,u]+ naa[y-1,a-1,4,1,u]*C[1,5,q,a,u]+ naa[y-1,a-1,4,2,u]*C[2,5,q,a,u]+ naa[y-1,a-1,4,3,u]*C[3,5,q,a,u]+ naa[y-1,a-1,4,4,u]*C[4,5,q,a,u]+ naa[y-1,a-1,4,6,u]*C[6,5,q,a,u]+ naa[y-1,a-1,4,7,u]*C[7,5,q,a,u])* exp(-(sum(Fa[y,a,q,5])+ M[a-1,u]))  
        naa[y,a,q,6,u] <- (naa[y-1,a-1,4,6,u]*C[6,6,q,a,u]+ naa[y-1,a-1,4,1,u]*C[1,6,q,a,u]+ naa[y-1,a-1,4,2,u]*C[2,6,q,a,u]+ naa[y-1,a-1,4,3,u]*C[3,6,q,a,u]+ naa[y-1,a-1,4,4,u]*C[4,6,q,a,u]+ naa[y-1,a-1,4,5,u]*C[5,6,q,a,u]+ naa[y-1,a-1,4,7,u]*C[7,6,q,a,u])* exp(-(sum(Fa[y,a,q,6])+ M[a-1,u]))  
        naa[y,a,q,7,u] <- (naa[y-1,a-1,4,7,u]*C[7,7,q,a,u]+ naa[y-1,a-1,4,1,u]*C[1,7,q,a,u]+ naa[y-1,a-1,4,2,u]*C[2,7,q,a,u]+ naa[y-1,a-1,4,3,u]*C[3,7,q,a,u]+ naa[y-1,a-1,4,4,u]*C[4,7,q,a,u]+ naa[y-1,a-1,4,5,u]*C[5,7,q,a,u]+ naa[y-1,a-1,4,6,u]*C[6,7,q,a,u])* exp(-(sum(Fa[y,a,q,7])+ M[a-1,u]))  
      }
  
  for (q in 2:4)
    for (a in 1:29)
      for (u in 1:2)
      {
        naa[y,a,q,1,u] <- (naa[y,a,q-1,1,u]*C[1,1,q,a,u]+ naa[y,a,q-1,2,u]*C[2,1,q,a,u]+ naa[y,a,q-1,3,u]*C[3,1,q,a,u]+ naa[y,a,q-1,4,u]*C[4,1,q,a,u]+ naa[y,a,q-1,5,u]*C[5,1,q,a,u]+ naa[y,a,q-1,6,u]*C[6,1,q,a,u]+ naa[y,a,q-1,7,u]*C[7,1,q,a,u])* exp(-((sum(Fa[y,a,q,1]))+ M[a,u]))
        naa[y,a,q,2,u] <- (naa[y,a,q-1,2,u]*C[2,2,q,a,u]+ naa[y,a,q-1,1,u]*C[1,2,q,a,u]+ naa[y,a,q-1,3,u]*C[3,2,q,a,u]+ naa[y,a,q-1,4,u]*C[4,2,q,a,u]+ naa[y,a,q-1,5,u]*C[5,2,q,a,u]+ naa[y,a,q-1,6,u]*C[6,2,q,a,u]+ naa[y,a,q-1,7,u]*C[7,2,q,a,u])* exp(-((sum(Fa[y,a,q,2]))+ M[a,u]))
        naa[y,a,q,3,u] <- (naa[y,a,q-1,3,u]*C[3,3,q,a,u]+ naa[y,a,q-1,1,u]*C[1,3,q,a,u]+ naa[y,a,q-1,2,u]*C[2,3,q,a,u]+ naa[y,a,q-1,4,u]*C[4,3,q,a,u]+ naa[y,a,q-1,5,u]*C[5,3,q,a,u]+ naa[y,a,q-1,6,u]*C[6,3,q,a,u]+ naa[y,a,q-1,7,u]*C[7,3,q,a,u])* exp(-((sum(Fa[y,a,q,3]))+ M[a,u]))
        naa[y,a,q,4,u] <- (naa[y,a,q-1,4,u]*C[4,4,q,a,u]+ naa[y,a,q-1,1,u]*C[1,4,q,a,u]+ naa[y,a,q-1,2,u]*C[2,4,q,a,u]+ naa[y,a,q-1,3,u]*C[3,4,q,a,u]+ naa[y,a,q-1,5,u]*C[5,4,q,a,u]+ naa[y,a,q-1,6,u]*C[6,4,q,a,u]+ naa[y,a,q-1,7,u]*C[7,4,q,a,u])* exp(-((sum(Fa[y,a,q,4]))+ M[a,u]))  
        naa[y,a,q,5,u] <- (naa[y,a,q-1,5,u]*C[5,5,q,a,u]+ naa[y,a,q-1,1,u]*C[1,5,q,a,u]+ naa[y,a,q-1,2,u]*C[2,5,q,a,u]+ naa[y,a,q-1,3,u]*C[3,5,q,a,u]+ naa[y,a,q-1,4,u]*C[4,5,q,a,u]+ naa[y,a,q-1,6,u]*C[6,5,q,a,u]+ naa[y,a,q-1,7,u]*C[7,5,q,a,u])* exp(-((sum(Fa[y,a,q,5]))+ M[a,u]))  
        naa[y,a,q,6,u] <- (naa[y,a,q-1,6,u]*C[6,6,q,a,u]+ naa[y,a,q-1,1,u]*C[1,6,q,a,u]+ naa[y,a,q-1,2,u]*C[2,6,q,a,u]+ naa[y,a,q-1,3,u]*C[3,6,q,a,u]+ naa[y,a,q-1,4,u]*C[4,6,q,a,u]+ naa[y,a,q-1,5,u]*C[5,6,q,a,u]+ naa[y,a,q-1,7,u]*C[7,6,q,a,u])* exp(-((sum(Fa[y,a,q,6]))+ M[a,u])) 
        naa[y,a,q,7,u] <- (naa[y,a,q-1,7,u]*C[7,7,q,a,u]+ naa[y,a,q-1,1,u]*C[1,7,q,a,u]+ naa[y,a,q-1,2,u]*C[2,7,q,a,u]+ naa[y,a,q-1,3,u]*C[3,7,q,a,u]+ naa[y,a,q-1,4,u]*C[4,7,q,a,u]+ naa[y,a,q-1,5,u]*C[5,7,q,a,u]+ naa[y,a,q-1,6,u]*C[6,7,q,a,u])* exp(-((sum(Fa[y,a,q,7]))+ M[a,u])) 
      }
  
  # Calculating spawning stock biomass and yield in year 1
  for (z in 1:7)
    for (q in 1:4)
      for (a in 1:nage)
        for (g in 1:ngr)
          for (u in 1:2)
          {
            Pssb[y,q,z,u]<-  sum(naa[y,1:nage,q,z,u]*waa[1:nage,u]*maa[1:nage,u])
            Pyield[y,q,z,u] <-sum(naa[y,1:nage,q,z,u]*waa[1:nage,2]*eaa[y,1:nage,q,z]) 
            Pcaa[y,a,q,z,u]<-sum(naa[y,a,q,z,u]*eaa[y,a,q,z]) 
          }
  
  for (a in 1:nage)
  {
    for (g in 1:ngrW)
    {
      Wpcaa[y,a,g]<-sum(naa[y,a,1:4,1:3,1:2]*Wpeaa[y,a,g]) #western partial catch-at-age
    }
    for (g in 1:ngrE)
    {
      Epcaa[y,a,g]<-sum(naa[y,a,1:4,4:7,1:2]*Epeaa[y,a,g]) #eastern partial catch-at-age
    }
    Wcaa[y,a]<-sum(Pcaa[y,a,1:4,1:3,1:2]) #western stock catch-at-age
    Ecaa[y,a]<-sum(Pcaa[y,a,1:4,4:7,1:2]) #eastern stock catch-at-age
  }
  # Model check
  # Wpcaa[42,1:29,1:17]
  # Wcaa[42,1:29]
  # Ecaa[42,1:29]
  for (q in 1:4)
    for (u in 1:2) 
    {
      T_Pssb[y,q,u] <-sum(Pssb[y,q,1:7,u]) #total bluefin SSB by unit (population)
      T_Wssb[y,q] <-sum(Pssb[y,q,1:3,1:2]) #total western stock SSB
      T_Essb[y,q]  <-sum(Pssb[y,q,4:7,1:2]) #total eastern stock SSB
      T_Pyield[y,q,u]<-sum(Pyield[y,q,1:7,u]) #total bluefin yield by unit (population)
      T_Wyield[y]<-sum(Pyield[y,1:4,1:3,1:2]) #total western stock yield
      T_Eyield[y]<-sum(Pyield[y,1:4,4:7,1:2])  #total eastern stock yield
    }
  
} # close parentheses years 2:nyr


######################## OPERATING MODEL PLOTS ##############################

plot(1974:2015, naa[1:nyr,1,1,1,2], ylim=c(0, 3000000), pch=19, type='b', ylab="Recruits (n)",  xlab="Years", main="Western Stock Recruits")
plot(1974:2015, naa[1:nyr,1,1,7,1], ylim=c(0,12000000), pch=19, type='b', ylab="Recruits (n)",  xlab="Years", main="Eastern Stock Recruits")
plot(1974:2015, T_Essb[1:nyr,1], pch=19, type='b',  ylim=c(0, 1000000), ylab="SSB (mT)", xlab="Year", main="Eastern Stock SSB")
plot(1974:2015, T_Wssb[1:nyr,1], pch=19, type='b', ylim=c(0,150000), ylab="SSB (mT)", xlab="Year", main="Western Stock SSB")
plot(1974:2015, T_Eyield[1:nyr], pch=19, type='b', ylim=c(0,40000), ylab="Yield (mT)", xlab="Year", main="Eastern Stock Yield")
plot(1974:2015, T_Wyield[1:nyr], pch=19, type='b', ylim=c(0,15000), ylab="Yield (mT)", xlab="Year", main="Western Stock Yield")
matplot(Wcaa, ylim=c(0,80000), type='l',ylab="W CAA ", xlab="Year", main="Western Stock CAA")
matplot(Ecaa, ylim=c(0,400000), type='l',ylab="E CAA", xlab="Year", main="Eastern Stock CAA")


#############################################################################
############### BLUEFIN TUNA VPA SIMULATIONS (M.MORSE 2017) #################
#############################################################################

########################### OBSERVATION MODEL ###############################

# Function for generating CPUES
CPUE.FUN <- function(east, cv.file) {
  
  if(east==TRUE) {
    print("East indices generated!")
    setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East")
    
    # Calculate Q1 & Q3 abundance for east stock
    Naa.E.Q3 <- array(NA, c(nyr,nage), dimnames = list(year = 1974:2015, age = 1:nage))
    Naa.E.Q1 <- array(NA, c(nyr,nage), dimnames = list(year = 1974:2015, age = 1:nage))
    for(y in 1:nyr)
      for(a in 1:nage)
      {
        Naa.E.Q1[y,a] <- sum(naa[y,a,1,4:7,1:2]) #sum abundance-at-age for quarter 1, zones 4-7, both populations (exploitable by eastern fisheries)
        Naa.E.Q3[y,a] <- sum(naa[y,a,3,4:7,1:2]) #sum abundance-at-age for quarter 3, zones 4-7, both populations (exploitable by eastern fisheries)
      }
    Naa.E.Q1.plus <- rowSums(Naa.E.Q1[,nageE:nage]) #calculate Q1 plus group by summing abundance-at-ages 10-29
    Naa.E.Q1 <- cbind(Naa.E.Q1[,1:(nageE-1)],Naa.E.Q1.plus) 
    colnames(Naa.E.Q1)[nageE] <- nageE    
    Naa.E.Q3.plus <- rowSums(Naa.E.Q3[,nageE:nage])  #calculate Q3 plus group by summing abundance-at-ages 10-29
    Naa.E.Q3 <- cbind(Naa.E.Q3[,1:(nageE-1)], Naa.E.Q3.plus)
    colnames(Naa.E.Q3)[nageE] <- nageE
    
    # transpose WAA and convert mt to kg
    Waa.temp <- t(waa[1:nageE,1]) * 1000
    
    Years <- rep(yrs,length(E.indices)) #years column for CPUEs
    IndexNum <- 1:length(E.indices)  #number of index IDs
    Indices <- matrix(NA,nrow=length(yrs)*length(E.indices),ncol=5)  #matrix for filling in CPUE information
    colnames(Indices) <- c("ID", "Year", "Value", "CV", "Name")
    Indices[,2] <- Years
    
    # Calculating and filling in index values
    Sa.E <- Sa[1:10,18:27]  #east selectivities
    Sa.E.t <- t(Sa.E)
    for (x in 1:ngrE)
    {
      Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),5] <- c(rep(E.indices[x],length(yrs)))  #fill index names for each year
      Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),1] <- rep(IndexNum[x],nyr)  #fill index ID numbers for each year
      Mult <- rep(NA,nyr)
      if (x==1 | x==2 | x==8 | x==9) {  #indices measured as abundance using Q1 abundance (spawning zone)
        for (y in 1:nyr) {
          Mult[y] <- sum(Sa.E.t[x,]*Naa.E.Q1[y,]) * Q[(x+17)]
        }   
        Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),3] <- Mult
      }
      if (x==3 | x==4 | x==5) {  #indices measured as abundance using Q3 abundance (non-spawning zones)
        for (y in 1:nyr) {
          Mult[y] <- sum(Sa.E.t[x,]*Naa.E.Q3[y,]) * Q[(x+17)]
        }
        Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),3] <- Mult
      } 
      if (x==10) {  #indices measured as biomass using Q1 abundance (spawning zone)
        for (y in 1:nyr) {
          Mult[y] <- sum(Sa.E.t[x,]*Naa.E.Q1[y,]*Waa.temp) * Q[(x+17)]
        }
        Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),3] <- Mult
      }
      if (x==6 | x==7) {  #indices measured as biomass using Q3 abundance (non-spawning zones)
        for (y in 1:nyr) {
          Mult[y] <- sum(Sa.E.t[x,]*Naa.E.Q3[y,]*Waa.temp) * Q[(x+17)]
        }
        Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),3] <- Mult
      }
    }
    
    # Read in CVs and input to indices data matrix
    CVs <- as.matrix(read.csv(cv.file), header = TRUE)
    Indices[,4] <- CVs[1:420,1]
    
    # CPUES/CVs not used (insert -999)
    not.used <- c(1:7,39:80,85,121:142,163:204,244:285,294:320,325:371,376,379:405,411:416)  #currently need to respecify this by hand
    Indices.file <- Indices[-not.used,]
    write.csv(Indices.file, file = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/Ecpues.csv")
    
  } else {
    print("West indices generated!")
    setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West")
    
    # Calculate Q1&Q3 abundance for west stock
    Naa.W.Q1 <- array(NA, c(nyr,nage), dimnames = list(year = 1974:2015, age = 1:nage))
    Naa.W.Q3 <- array(NA, c(nyr,nage), dimnames = list(year = 1974:2015, age = 1:nage))
    for(y in 1:nyr)
      for(a in 1:nage)
      {
        Naa.W.Q1[y,a] <- sum(naa[y,a,1,1:3,1:2]) #sum abundance-at-age for quarter 1, zones 1-3, both populations (exploitable by western fisheries)
        Naa.W.Q3[y,a] <- sum(naa[y,a,3,1:3,1:2]) #sum abundance-at-age for quarter 3, zones 1-3, both populations (exploitable by western fisheries)
      }
    Naa.W.Q1.plus <- rowSums(Naa.W.Q1[,nageW:nage])   #calculate Q1 plus group by summing abundance-at-ages 16-29
    Naa.W.Q1 <- cbind(Naa.W.Q1[,1:(nageW-1)],Naa.W.Q1.plus)
    colnames(Naa.W.Q1)[nageW] <- nageW
    Naa.W.Q3.plus <- rowSums(Naa.W.Q3[,nageW:nage])   #calculate Q3 plus group by summing abundance-at-ages 16-29
    Naa.W.Q3 <- cbind(Naa.W.Q3[,1:(nageW-1)],Naa.W.Q3.plus)
    colnames(Naa.W.Q3)[nageW] <- nageW
    
    # transpose WAA and convert mt to kg
    Waa.temp <- t(waa[1:nageW,2]) * 1000
    
    Years <- rep(yrs,length(W.indices)) #years column for CPUEs
    IndexNum <- 1:length(W.indices)  #number of index IDs
    Indices <- matrix(NA,nrow=length(yrs)*length(W.indices),ncol=5)  #matrix for filling in CPUE information
    colnames(Indices) <- c("ID", "Year", "Value", "CV", "Name")
    Indices[,2] <- Years
    
    # Calculating and filling in index values
    Sa.W <- Sa[1:16,1:ngrW]  #west selectivities
    Sa.W.t <- t(Sa.W)
    for (x in 1:ngrW) {
      Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),5] <- c(rep(W.indices[x],length(yrs)))  #fill index names for each year
      Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),1] <- rep(IndexNum[x],nyr)  #fill index ID numbers for each year
      Mult <- rep(NA,nyr)
      if (x==14 | x==15) {  #indices measured as abundance using Q1 abundance (spawning zone)
        for (y in 1:nyr) {
          Mult[y] <- sum(Sa.W.t[x,]*Naa.W.Q1[y,]) * Q[x]
        }
        Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),3] <- Mult
      }
      if (x==1 | x==2 | x==3 | x==4 | x==5 | x==6 | x==7 | x==8 | x==9 | x==10 | x==11 | x==12 | x==17) {  #indices measured as abundance using Q3 abundance (non-spawning zones)
        for (y in 1:nyr) {
          Mult[y] <- sum(Sa.W.t[x,]*Naa.W.Q3[y,]) * Q[x]
        }
        Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),3] <- Mult
      }
      if (x==13) {  #indices measured as biomass using Q1 abundance (spawning zone)
        for (y in 1:nyr) {
          Mult[y] <- sum(Sa.W.t[x,]*Naa.W.Q1[y,]*Waa.temp) * Q[x]
        }
        Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),3] <- Mult
      }
      if (x==16) {  #indices measured as biomass using Q3 abundance (non-spawning zones)
        for (y in 1:nyr) {
          Mult[y] <- sum(Sa.W.t[x,]*Naa.W.Q3[y,]*Waa.temp) * Q[x]
        }
        Indices[(1+(nyr*(x-1))):(nyr+(nyr*(x-1))),3] <- Mult
      }
    }
    
    # Read in CVs and input to Wcpues matrix
    CVs <- as.matrix(read.csv(cv.file), header = TRUE) # read in file that uses the average CV of all CVs for a given index (from the 2017 VPA) as the CVs for each new index
    Indices[,4] <- CVs[421:1134,1]
    
    # Insert -999 in the place of yearly CPUE/CV values not used 
    not.used <- c(1:10,43:62,85:90,95,104:145,169:187,211:261,272:355,379:380,415:507,510:511,516,547:564,597:630,639:708)
    Indices.file <- Indices[-not.used,]
    write.csv(Indices.file, file = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West/Wcpues.csv")
  }
}

# Function to generate PCAAs & CAAs
CAA.FUN <- function(east) {
  
  if (east==TRUE) {
    print("East PCAA & CAA generated!")
    setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East")
    # Format Epcaa into matrix w/ plus group
    Epcaa.det <- aperm(a = Epcaa, perm = c("year", "gear", "age"))  #transpose Epcaa matrix to create matrix for storing new deterministic PCAAs
    dim(Epcaa.det) <- c((nyr*ngrE), nage)  #rename dimensions of new Epcaa matrix
    colnames(Epcaa.det) <- c(1:nage)  #rename columns for ages 1-10+
    # Epcaa.det <- Epcaa.det * exp((0.863^2)/2)  #bias correction ##SWITCH ON/OFF##
    Epcaa.det.plus <- rowSums(Epcaa.det[,nageE:nage])  #age-10+ group
    # Epcaa.det.plus <- Epcaa.det.plus * exp((0.863^2)/2)  #bias correction ##SWITCH ON/OFF##
    Years <- rep(yrs,length(E.indices))  #years column
    IndexNums <- rep(NA, times=(nyr*ngrE))  #PCAAs/indices numbers
    IndexNum <- 1:length(E.indices)  #number of index IDs
    for (x in 1:ngrE) {
      IndexNums[(1+(nyr*(x-1))):(nyr+(nyr*(x-1)))] <- rep(IndexNum[x],nyr)  #fill index ID numbers for each year
    }
    Epcaa.det.mat <- cbind(IndexNums, Years, Epcaa.det[, 1:(nageE-1)], Epcaa.det.plus)  #new matrix of deterministic PCAAs (ages 1-10+) and index/year specifications
    colnames(Epcaa.det.mat)[nageE+2] <- nageE
    # In 2017 stock assessment, the WMED_LARV survey index (ID #10) used maturity vector as PCAA
    maa.tran <- t(maa[1:nageE,1])
    WMEDLARV <- matrix(rep(maa.tran, nyr), nrow = nyr, ncol = nageE, byrow = TRUE)
    Epcaa.det.mat[Epcaa.det.mat[,1]==10,3:12] <- WMEDLARV
    # Remove rows that did not have PCAA entries in the 2017 stock assessment (assumes 1s for all values not entered--see VPA-2BOX manual v 3.01 p 41)
    pcaa.not.used <- c(1:7,39:80,85,121:142,163:204,244:285,295:405)
    Epcaa.det.mat <- Epcaa.det.mat[-pcaa.not.used,]
    write.csv(Epcaa.det.mat, file = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/Epcaa.csv")  #write .csv file for deterministic Epcaa data
    
    # Format Ecaa into plus groups
    Ecaa.plus <- rowSums(Ecaa[,nageE:nage])  #calculate plus group from Ecaa from operating model
    Ecaa.10 <- cbind(Ecaa[, 1:(nageE-1)], Ecaa.plus)  #create new matrix of Ecaa ages 1-10+
    colnames(Ecaa.10)[nageE] <- nageE  #rename column "Ecaa.plus" as "10"
    # Ecaa.10 <- Ecaa.10 * exp((0.863^2)/2)  #bias correction ##SWITCH ON/OFF##
    write.csv(Ecaa.10, file = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/Ecaa.csv")  #write .csv file for deterministic Ecaa data (1-10+)
    
  } else { 
    print("West PCAA & CAA generated!")
    setwd("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West")
    # Format Wpcaa into matrix w/ plus group
    Wpcaa.det <- aperm(a = Wpcaa, perm = c("year", "gear", "age"))  #transpose Wpcaa matrix to create matrix for storing new deterministic pcaas
    dim(Wpcaa.det) <- c((nyr*ngrW), nage)  #rename dimensions of new Wpcaa matrix
    colnames(Wpcaa.det) <- c(1:nage)  #rename columns for ages
    Wpcaa.det.plus <- rowSums(Wpcaa.det[,nageW:nage])  #age-16+ group
    Years <- rep(yrs,length(W.indices)) #years column
    IndexNums <- rep(NA, times=(nyr*ngrW))  #PCAAs/indices numbers
    IndexNum <- 1:length(W.indices)  #number of index IDs
    for (x in 1:ngrW) {
      IndexNums[(1+(nyr*(x-1))):(nyr+(nyr*(x-1)))] <- rep(IndexNum[x],nyr)  #fill index ID numbers for each year
    }
    Wpcaa.det.mat <- cbind(IndexNums, Years, Wpcaa.det[, 1:(nageW-1)], Wpcaa.det.plus)   #new matrix of deterministic PCAAs (ages 1-16+) and index/year specifications
    colnames(Wpcaa.det.mat)[nageW+2] <- nageW
    write.csv(Wpcaa.det.mat, file = "Wpcaa.csv")  #write .csv file for deterministic Wpcaa data
    
    # Format Wcaa into plus groups
    Wcaa.plus <- rowSums(Wcaa[, nageW:nage])  #calculate plus group from Wcaa from operating model
    Wcaa.16 <- cbind(Wcaa[, 1:(nageW-1)], Wcaa.plus)  #create new matrix of Wcaa ages 1-16+
    colnames(Wcaa.16)[nageW] <- nageW  #rename column "Wcaa.plus" as "16"
    write.csv(Wcaa.16, file = "Wcaa.csv")  #write .csv file for deterministic Wcaa data (1-16+)
  }
}


#######################  CALCULATE HARVEST RATIO  ###########################

HR.FUN <- function(stock.is) {
  
  if (stock.is=="East") {
    # East Population Harvest Ratio (=number of fish caught/exploitable abundance)
    print("East operating model harvest ratio calculated!")
    # Calculate Z (total mortality)
    naa.plus <- rep(NA, nyr)
    naa.E <- array(NA,c(nyr,(nageE-1)),dimnames=list(year=head(yrs,1):tail(yrs,1),age=1:(nageE-1)))
    for (y in 1:nyr) {
      naa.plus[y] <- sum(naa[y,(nageE:nage),1,1:7,1])  #plus group for NAA (east POPULATION)
      for (a in 1:(nageE-1)) {
        naa.E[y,a] <- sum(naa[y,a,1,1:7,1])  #sum over quarter 1, zones 1-7, unit 1 (east POPULATION)
      }
    }
    # for (y in 1:nyr) {
    #   naa.plus[y] <- sum(naa[y,(nageE:nage),1,4:7,1:2])  #plus group for NAA (east STOCK)
    #   for (a in 1:(nageE-1)) {
    #     naa.E[y,a] <- sum(naa[y,a,1,4:7,1:2])  #sum over quarter 1, zones 4-7, units 1-2 (east STOCK)
    #   }
    # }
    naa.E <- cbind(naa.E, naa.plus)  #new NAA matrix with 10+ group
    Z <- matrix(NA, nrow = (nyr-1), ncol = (nageE-1))
    for (y in 1:(nyr-1))
      for (a in 1:(nageE-1)) {
        Z[y,a] <- log(naa.E[y,a]/naa.E[y+1,a+1])  #Z calculation
      }
    
    # Calculate F (=Z-M)
    Fatage <- matrix(NA, nrow = (nyr-1), ncol = (nageE-1))  #fishing mortality
    for (y in 1:(nyr-1))
      for (a in 1:(nageE-1)) {
        Fatage[y,a] <- Z[y,a] - M[a,1]  #PROBLEM WITH LAST AGE (LARGE NEGATIVE)--MAYBE DON'T USE PLUS GROUPS?
      }
    
    # Calculate selectivity, S
    Select <- matrix(NA, nrow = (nyr-1), ncol = (nageE-1))
    for (y in 1:(nyr-1))
      for (a in 1:(nageE-1)) {
        Select[y,a] <- Fatage[y,a]/max(Fatage[y,])
      }
    
    # Calculate harvest ratio using CAA from observation model and NAA from operating model output
    Ecaa.HR <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/Ecaa.csv", header = TRUE)
    Ecaa.HR <- Ecaa.HR[,-1]
    HR.OM <- rep(NA, nyr-1)
    for (y in 1:(nyr-1)) {
      HR.OM[y] <- sum(Ecaa.HR[y,(1:nageE-1)])/sum(naa.E[y,(1:nageE-1)]*Select[y,(1:nageE-1)])
    }  
    assign("HR.OM", HR.OM, .GlobalEnv)
    
  } else {
    
    # West Population Harvest Ratio  (=number of fish caught/exploitable abundance)
    print("West operating model harvest ratio calculated!")
    
    # Calculate Z (total mortality)
    naa.plus <- rep(NA, nyr)
    naa.W <- array(NA,c(nyr,(nageW-1)),dimnames=list(year=head(yrs,1):tail(yrs,1),age=1:(nageW-1)))
    for (y in 1:nyr) {
      naa.plus[y] <- sum(naa[y,(nageW:nage),1,1:7,2])  #plus group for NAA (west POPULATION)
      for (a in 1:(nageW-1)) {
        naa.W[y,a] <- sum(naa[y,a,1,1:7,2])  #sum over quarter 1, zones 1-7, unit 2 (west POPULATION)
      }
    }
    # for (y in 1:nyr) {
    #   naa.plus[y] <- sum(naa[y,(nageW:nage),1,1:3,1:2])  #plus group for NAA (west STOCK)
    #   for (a in 1:(nageW-1)) {
    #     naa.W[y,a] <- sum(naa[y,a,1,1:3,1:2])  #sum over quarter 1, zones 1-3, units 1-2 (west STOCK)
    #   }
    # }
    naa.W <- cbind(naa.W, naa.plus)  #new NAA matrix with 16+ group
    Z <- matrix(NA, nrow = (nyr-1), ncol = (nageW-1))
    for (y in 1:(nyr-1))
      for (a in 1:(nageW-1)) {
        Z[y,a] <- log(naa.W[y,a]/naa.W[y+1,a+1])  #Z calculation
      }
    
    # Calculate F (=Z-M)
    Fatage <- matrix(NA, nrow = (nyr-1), ncol = (nageW-1))  #fishing mortality
    for (y in 1:(nyr-1))
      for (a in 1:(nageW-1)) {
        Fatage[y,a] <- Z[y,a] - M[a,1]  #PROBLEM WITH LAST AGE (LARGE NEGATIVE)--MAYBE DON'T USE PLUS GROUPS?
      }
    
    # Calculate selectivity, S
    Select <- matrix(NA, nrow = (nyr-1), ncol = (nageW-1))
    for (y in 1:(nyr-1))
      for (a in 1:(nageW-1)) {
        Select[y,a] <- Fatage[y,a]/max(Fatage[y,])
      }
    
    # Calculate harvest ratio using CAA from observation model and NAA from operating model output
    Wcaa.HR <- read.csv("C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West/Wcaa.csv")
    Wcaa.HR <- Wcaa.HR[,-1]
    HR.OM <- rep(NA, nyr-1)
    for (y in 1:(nyr-1)) {
      HR.OM[y] <- sum(Wcaa.HR[y,(1:nageW-1)])/sum(naa.W[y,(1:nageW-1)]*Select[y,(1:nageW-1)])
    }  
    assign("HR.OM", HR.OM, .GlobalEnv)
  }
}


##########################  WRITE CONTROL FILE  ###############################

# Function to format control file
write_c1VPA <- function(dir_out                            = dirRun,
                        name_run                           = name_here,
                        years                              = years_1,
                        run_num                            = run_number,
                        ## MODEL TYPE OPTIONS
                        number_zone                        = 1, ## NUMBER OF ZONES (1 OR 2)
                        model_type                         = 1, ## MODEL_TYPE (1=DIFFUSION, 2=OVERLAP)
                        ### TAGGING DATA SWITCH
                        tagging_data_switch                = 0,     ## (0=do not use tagging data, 1=use tagging data)
                        weighting_factor                   = 1.0,   ## for modifying importance of tagging data in objective function
                        tag_timing_factor                  = c(1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1), # pDF OF TAG DATA,DOWNWEIGHTING DIVISOR, TIME OF YEAR WHEN FISHING SEASON BEGINS, DURATION OF FISHING SEASON-- (<=0) DO NOT USE TAGGING DATA
                        ### SEARCH ALGORITHM CONTROLS
                        random_seed                        = r_seed,## RANDOM NUMBER SEED
                        max_amoeba                         = 100,   ## MAXIMUM NUMBER OF AMOEBA SIMPLEX SEARCH RESTARTS
                        number_restart                     = 5,    	## NUMBER OF CONSECUTIVE RESTARTS THAT MUST VARY BY LESS THAN 1% TO STOP SEARCH  
                        PDEV                               = 0.4,		## PDEV (standard deviation controlling vertices for Initial simplex of each restart)
                        ### INDEX WEIGHTING CONTROLS
                        scale                              = 1,     ## SCALE (DIVIDE INDEX VALUES BY THEIR MEAN)- ANY VALUE > 0 = YES
                        index_weighting                    = 1.0,   ## INDEX WEIGHTING:(0)INPUT CV's, (+)DEFAULT CV, (-)DEFAULT STD. DEV., (999)MLE
                        multiplicative_variance            = 0,     ## (0) MULTIPLICATIVE VARIANCE SCALING FACTOR or (1) ADDITIVE VARIANCE SCALING FACTOR 
                        ### CONSTRAINT ON VulnerabilitY (PARTIAL RECRUITMENT) 
                        penalty                            = 3,		  ## apply this penalty to the last N years (SET N = 0 TO IGNORE)
                        sd_severity                        = .5,	  ## |  standard deviation controlling the severity of the penalty
                        first_age_affected                 = 1,	        ## |  |  first age affected
                        last_age_affected                  = 15,	    ## |  |  |  last age affected
                        ### CONSTRAINTS ON RECRUITMENT 
                        penalty_recruitment_nyr            = c(2,0.5),	## apply this penalty to the last N years (SET N = 0 TO IGNORE)
                        penalty_recruitment_2stock         = c(0,.1,1),	## |  standard deviation controlling the severity of the penalty and ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
                        ### CONSTRAINT ON SPAWNER-RECRUIT RELATIONSHIP
                        pdf_SR                             = 0,			## PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE
                        fit_SR_year_start                  = 1971,	## |  first and last years to use in fitting (in terms of recruits)
                        fit_SR_year_end                    = 1996,	## |  first and last years to use in fitting (in terms of recruits)
                        ### PARAMETER ESTIMATION OPTIONS
                        option_parameter                   = 1,	    ## OPTION TO USE (1) F'S OR (2) N'S AS TERMINAL YEAR PARAMETERS
                        estimate_q                         = -1,	  ## ESTIMATE Q IN (+) SEARCH or (<0) by concentrated MLE's
                        ### BOOTSTRAP ANALYSES 
                        number_bootstraps                  = nb_boot,		## Number of bootstraps to run (negative value = do a parametric bootstrap)
                        stine_correction                   = 1,		  ## |   Use Stine correction to inflate bootstrap residuals (0=NO)
                        file_type_bootstraps               = 1,	    ## |   |   File type (0 = ASC-II, 1=Binary)
                        # RETROSPECTIVE ANALYSES (CANNOT DO RETROSPECTIVE ANALYSES AND BOOTSTRAPS AT SAME TIME)
                        number_year_retrospective_analyses = nb_retro)

{
  
  control_file_name                <- paste(dir_out, "/Run", run_num, "/", name_run, run_num,".c1", sep="")
  control_file                     <<- paste(name_run, run_num, ".c1", sep = "") # make control_file a global variable to be called later in call to VPA-2BOX
  run_title                        <- paste(name_run, years[1], "-", years[2], " Run ", run_num, sep = "")
  data_file_name                   <- paste(name_run, run_num, ".d1", sep="")
  parameter_file_name              <- paste(name_run, run_num, ".p1", sep="")
  result_file_name                 <<- paste(name_run, run_num, "_RESULTS.R", sep="")
  parameter_estimate_file_name     <- paste(name_run, run_num, ".e1", sep="")
  f_spreadsheet_results            <- paste(name_run, run_num, ".spd", sep="")
  tagging_file_name                <- 'none'
  
  cat("#-----------------------------------------------------------------------------
#--               CONTROL FILE FOR PROGRAM VPA-2BOX, Version 3.0           ---
#-----------------------------------------------------------------------------
#  INSTRUCTIONS: the control options are entered in the order specified.
#                Additional comment lines may be inserted anywhere in this 
#                file provided they are preceded by a # symbol in the FIRST 
#                column, otherwise the line is perceived as free-format data.
#-----------------------------------------------------------------------------
# Input file generated automatically with the write_c1VPA R function on ", date(), 
"\n#-----------------------------------------------------------------------------
# TITLES AND FILE NAMES (MUST BE PLACED WITHIN SINGLE QUOTES)
#-----------------------------------------------------------------------------
#|--------must be 50 characters or less----------|\n",
paste("\'",run_title,"\'", sep=""), "\n",
paste("\'",data_file_name,"\'", sep=""), "\n",
paste("\'",parameter_file_name,"\'", sep=""),"\n",
paste("\'",result_file_name,"\'", sep=""), "\n",
paste("\'",parameter_estimate_file_name,"\'", sep=""), "\n",
paste("\'",f_spreadsheet_results,"\'", sep=""),"\n",
paste("\'",tagging_file_name,"\'", sep=""),"\n",
"#-----------------------------------------------------------------------------
# MODEL TYPE OPTIONS
#-----------------------------------------------------------------------------\n",
number_zone,"\n",
model_type, "\n",
"#-----------------------------------------------------------------------------
# TAGGING DATA SWITCH
#-----------------------------------------------------------------------------
# tagging data switch (0=do not use tagging data, 1=use tagging data)
# |  weighting factor for modifying importance of tagging data in objective function
# |  |     tag timing factors
# |  |     |    \n",
tagging_data_switch," ",  #(0=do not use tagging data, 1=use tagging data)
weighting_factor," ",  	# for modifying importance of tagging data in objective function
paste(tag_timing_factor,"",sep=" "), # PDF OF TAG DATA,DOWNWEIGHTING DIVISOR, TIME OF YEAR WHEN FISHING SEASON BEGINS, DURATION OF FISHING SEASON-- (<=0) DO NOT USE TAGGING DATA      
"\n#-----------------------------------------------------------------------------
# SEARCH ALGORITHM CONTROLS
#-----------------------------------------------------------------------------\n",
random_seed,"\n",     ## RANDOM NUMBER SEED
max_amoeba,"\n",   	## MAXIMUM NUMBER OF AMOEBA SIMPLEX SEARCH RESTARTS
number_restart,"\n",    	## NUMBER OF CONSECUTIVE RESTARTS THAT MUST VARY BY LESS THAN 1% TO STOP SEARCH  
PDEV,"\n",		## PDEV (standard deviation controlling vertices for Initial simplex of each restart)
"#-----------------------------------------------------------------------------
# INDEX WEIGHTING CONTROLS
#-----------------------------------------------------------------------------\n",
scale,"\n",        	## SCALE (DIVIDE INDEX VALUES BY THEIR MEAN)- ANY VALUE > 0 = YES
index_weighting,"\n",    	## INDEX WEIGHTING:(0)INPUT CV's, (+)DEFAULT CV, (-)DEFAULT STD. DEV., (999)MLE
multiplicative_variance,"\n",    ## (0) MULTIPLICATIVE VARIANCE SCALING FACTOR or (1) ADDITIVE VARIANCE SCALING FACTOR 
"#-----------------------------------------------------------------------------
# CONSTRAINT ON VULNERABILITY (PARTIAL RECRUITMENT) 
#-----------------------------------------------------------------------------
# apply this penalty to the last N years (SET N = 0 TO IGNORE)
# |  standard deviation controlling the severity of the penalty
# |  |  first age affected   
# |  |  |  last age affected
# |  |  |  | \n",
penalty, " ",  	# apply this penalty to the last N years (SET N = 0 TO IGNORE)
sd_severity, " ",	# |  standard deviation controlling the severity of the penalty
first_age_affected, " ",	# |  |  first age affected
last_age_affected, "\n",	# |  |  |  last age affected
"#-----------------------------------------------------------------------------
# CONSTRAINTS ON RECRUITMENT 
#-----------------------------------------------------------------------------
# apply this penalty to the last N years (SET N = 0 TO IGNORE)
# |  standard deviation controlling the severity of the penalty\n",
paste(penalty_recruitment_nyr,"",sep=" "),"links the recruitments in the last n years","\n",
paste(penalty_recruitment_2stock,"",sep=" "),"links the recruitments of the two stocks","\n",
"#        |
#        ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
#-----------------------------------------------------------------------------
# CONSTRAINT ON SPAWNER-RECRUIT RELATIONSHIP
#-----------------------------------------------------------------------------
# PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE
# |  first and last years to use in fitting (in terms of recruits)
# |  |\n",
pdf_SR," ",  		# PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE
fit_SR_year_start," ",	# |  first and last years to use in fitting (in terms of recruits)
fit_SR_year_end,"\n",		# |  first and last years to use in fitting (in terms of recruits)
"#               (note: check the parameter file to make sure you are estimating the S/R 
#                           parameters when pdf not 0, or not estimating them when pdf=0))
#-----------------------------------------------------------------------------
# PARAMETER ESTIMATION OPTIONS
#----------------------------------------------------------------------------\n",
option_parameter,"\n",  # OPTION TO USE (1) F'S OR (2) N'S AS TERMINAL YEAR PARAMETERS
estimate_q,"\n",	# ESTIMATE Q IN (+) SEARCH or (<0) by concentrated MLE's
"#-----------------------------------------------------------------------------
# BOOTSTRAP ANALYSES 
#-----------------------------------------------------------------------------
# Number of bootstraps to run (negative value = do a parametric bootstrap)
# |   Use Stine correction to inflate bootstrap residuals (0=NO)
# |   |   File type (0 = ASC-II, 1=Binary)
# |   |   |\n",
number_bootstraps," ",  	# Number of bootstraps to run (negative value = do a parametric bootstrap)
stine_correction," ",		# |   Use Stine correction to inflate bootstrap residuals (0=NO)
file_type_bootstraps,"\n",	# |   |   File type (0 = ASC-II, 1=Binary)
"#-----------------------------------------------------------------------------
# RETROSPECTIVE ANALYSES (CANNOT DO RETROSPECTIVE ANALYSES AND BOOTSTRAPS AT SAME TIME)
#-----------------------------------------------------------------------------\n",
number_year_retrospective_analyses,"\n",
"@@EOF@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@",
sep="", file=control_file_name) # Prints the above objects to this file
  
  print(paste("File ", control_file_name, ' has been created!!!', sep=""))
}

# Function to create control file
CTRLFILE.FUN <- function(east, run.num) {
  if(east==TRUE) {
    # Runs function to create EAST control file
    write_c1VPA(            dir_out                            = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East", # directory for saving control file (output of function)
                            name_run                           = "BFTE2017_",
                            years                              = c(head(yrs,1), tail(yrs,1)),
                            run_num                            = run.num,
                            ## MODEL TYPE OPTIONS
                            number_zone                        = 1, ## NUMBER OF ZONES (1 OR 2)
                            model_type                         = 1, ## MODEL_TYPE (1=DIFFUSION, 2=OVERLAP)
                            ### TAGGING DATA SWITCH
                            tagging_data_switch                = 0,   ## (0=do not use tagging data, 1=use tagging data) - not using tagging data
                            weighting_factor                   = 1,   ## for modifying importance of tagging data in objective function - Not applicable
                            tag_timing_factor                  = c(1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1), # pDF OF TAG DATA,DOWNWEIGHTING DIVISOR, TIME OF YEAR WHEN FISHING SEASON BEGINS, DURATION OF FISHING SEASON-- (<=0) DO NOT USE TAGGING DATA - Not applicable
                            ### SEARCH ALGORITHM CONTROLS
                            random_seed                        = -911, ## RANDOM NUMBER SEED
                            max_amoeba                         = 100,   ## MAXIMUM NUMBER OF AMOEBA SIMPLEX SEARCH RESTARTS
                            number_restart                     = 5,    	## NUMBER OF CONSECUTIVE RESTARTS THAT MUST VARY BY LESS THAN 1% TO STOP SEARCH  
                            PDEV                               = 0.4,		## PDEV (standard deviation controlling vertices for Initial simplex of each restart)
                            ### INDEX WEIGHTING CONTROLS
                            scale                              = 1,     ## SCALE (DIVIDE INDEX VALUES BY THEIR MEAN)- ANY VALUE > 0 = YES
                            index_weighting                    = 1,     ## INDEX WEIGHTING:(0)INPUT CV's, (+)DEFAULT CV, (-)DEFAULT STD. DEV., (999)MLE
                            multiplicative_variance            = 0,     ## (0) MULTIPLICATIVE VARIANCE SCALING FACTOR or (1) ADDITIVE VARIANCE SCALING FACTOR 
                            ### CONSTRAINT ON VULNERABILITY (PARTIAL RECRUITMENT) 
                            penalty                            = 3,		  ## apply this penalty to the last N years (SET N = 0 TO IGNORE)
                            sd_severity                        = 0.4,	  ## |  standard deviation controlling the severity of the penalty
                            first_age_affected                 = 1,     ## |  |  first age affected; formulas here didn't seem to work; type numbers instead
                            last_age_affected                  = 9,	    ## |  |  |  last age affected; formulas here didn't seem to work; type numbers instead
                            ### CONSTRAINTS ON RECRUITMENT 
                            penalty_recruitment_nyr            = c(0, 0.5),	    ## apply this penalty to the last N years (SET N = 0 TO IGNORE)
                            penalty_recruitment_2stock         = c(0, 0.1, 1),	## |  standard deviation controlling the severity of the penalty and ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
                            ### CONSTRAINT ON SPAWNER-RECRUIT RELATIONSHIP
                            pdf_SR                             = 0,			## PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE
                            fit_SR_year_start                  = 1971,	## |  first and last years to use in fitting (in terms of recruits)
                            fit_SR_year_end                    = 1996,	## |  first and last years to use in fitting (in terms of recruits)
                            ### PARAMETER ESTIMATION OPTIONS
                            option_parameter                   = 1,	    ## OPTION TO USE (1) F'S OR (2) N'S AS TERMINAL YEAR PARAMETERS
                            estimate_q                         = -1,	  ## ESTIMATE Q IN (+) SEARCH or (<0) by concentrated MLE's
                            ### BOOTSTRAP ANALYSES 
                            number_bootstraps                  = 0,		## Number of bootstraps to run (negative value = do a parametric bootstrap)
                            stine_correction                   = 1,		  ## |   Use Stine correction to inflate bootstrap residuals (0=NO)
                            file_type_bootstraps               = 1,	    ## |   |   File type (0 = ASC-II, 1=Binary)
                            # RETROSPECTIVE ANALYSES (CANNOT DO RETROSPECTIVE ANALYSES AND BOOTSTRAPS AT SAME TIME)
                            number_year_retrospective_analyses = 0)
  } else {
    
    # Runs function to create WEST control file
    write_c1VPA(            dir_out                            = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West", # directory for saving control file (output of function)
                            name_run                           = "BFTW2017_",
                            years                              = c(head(yrs,1), tail(yrs,1)),
                            run_num                            = run.num,
                            ## MODEL TYPE OPTIONS
                            number_zone                        = 1, ## NUMBER OF ZONES (1 OR 2)
                            model_type                         = 1, ## MODEL_TYPE (1=DIFFUSION, 2=OVERLAP)
                            ### TAGGING DATA SWITCH
                            tagging_data_switch                = 0,     ## (0=do not use tagging data, 1=use tagging data) - not using tagging data
                            weighting_factor                   = 1.0,   ## for modifying importance of tagging data in objective function - Not applicable
                            tag_timing_factor                  = c(0, 0), # PDF OF TAG DATA,DOWNWEIGHTING DIVISOR, TIME OF YEAR WHEN FISHING SEASON BEGINS, DURATION OF FISHING SEASON-- (<=0) DO NOT USE TAGGING DATA - Not applicable
                            ### SEARCH ALGORITHM CONTROLS
                            random_seed                        = -50, ## RANDOM NUMBER SEED
                            max_amoeba                         = 600,   ## MAXIMUM NUMBER OF AMOEBA SIMPLEX SEARCH RESTARTS
                            number_restart                     = 10,    	## NUMBER OF CONSECUTIVE RESTARTS THAT MUST VARY BY LESS THAN 1% TO STOP SEARCH  
                            PDEV                               = 0.4,		## PDEV (standard deviation controlling vertices for Initial simplex of each restart)
                            ### INDEX WEIGHTING CONTROLS
                            scale                              = 1,     ## SCALE (DIVIDE INDEX VALUES BY THEIR MEAN)- ANY VALUE > 0 = YES
                            index_weighting                    = 0,   ## INDEX WEIGHTING:(0)INPUT CV's, (+)DEFAULT CV, (-)DEFAULT STD. DEV., (999)MLE
                            multiplicative_variance            = 1,     ## (0) MULTIPLICATIVE VARIANCE SCALING FACTOR or (1) ADDITIVE VARIANCE SCALING FACTOR 
                            ### CONSTRAINT ON VulnerabilitY (PARTIAL RECRUITMENT) 
                            penalty                            = 3,		  ## apply this penalty to the last N years (SET N = 0 TO IGNORE)
                            sd_severity                        = .5,	  ## |  standard deviation controlling the severity of the penalty
                            first_age_affected                 = 1,	        ## |  |  first age affected; formulas here didn't seem to work; type numbers instead
                            last_age_affected                  = 15,	    ## |  |  |  last age affected; formulas here didn't seem to work; type numbers instead
                            ### CONSTRAINTS ON RECRUITMENT 
                            penalty_recruitment_nyr            = c(0, .1),	## apply this penalty to the last N years (SET N = 0 TO IGNORE)
                            penalty_recruitment_2stock         = c(0, .1, 1),	## |  standard deviation controlling the severity of the penalty and ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
                            ### CONSTRAINT ON SPAWNER-RECRUIT RELATIONSHIP
                            pdf_SR                             = 0,			## PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE
                            fit_SR_year_start                  = 1974,	## |  first and last years to use in fitting (in terms of recruits)
                            fit_SR_year_end                    = 1998,	## |  first and last years to use in fitting (in terms of recruits)
                            ### PARAMETER ESTIMATION OPTIONS
                            option_parameter                   = 1,   ## OPTION TO USE (1) F'S OR (2) N'S AS TERMINAL YEAR PARAMETERS
                            estimate_q                         = 1,	  ## ESTIMATE Q IN (+) SEARCH or (<0) by concentrated MLE's
                            ### BOOTSTRAP ANALYSES 
                            number_bootstraps                  = 0,		## Number of bootstraps to run (negative value = do a parametric bootstrap)
                            stine_correction                   = 1,		## |   Use Stine correction to inflate bootstrap residuals (0=NO)
                            file_type_bootstraps               = 1,	  ## |   |   File type (0 = ASC-II, 1=Binary)
                            # RETROSPECTIVE ANALYSES (CANNOT DO RETROSPECTIVE ANALYSES AND BOOTSTRAPS AT SAME TIME)
                            number_year_retrospective_analyses = 0)
  }
}


##########################  WRITE DATA FILE  ##################################

# Function to format data file
write_d1VPA <- function(dir_data                  = dirData,
                        dir_out                   = dirRun,
                        name_run                  = name_run_1,
                        run_num                   = run_num_1,
                        years                     = years_1,
                        age_minus                 = 0,
                        age_plus                  = 10,
                        cpue_number               = cpue_number_1,
                        spawning_season           = 6,
                        maturity_at_age           = c(0,0,0,0.5,rep(1,25))[first_age:last_age], # enter first age:last age in brackets
                        stock_title               = 'BFTW',
                        pdf_catch                 = 0,
                        sigma_catch               = 0.1,
                        file_caa                  = caa_file_name,
                        cpue_to_use               = cpue_to_use_1,
                        file_cpue_spec            = file_cpue_spec_1,
                        file_cpue                 = file_cpue_1,
                        file_pcaa                 = pcaa_file_name,
                        file_waa                  = file_waa_1,
                        fec1                      = waa,
                        scaling_parameter         = 1,
                        year_max_cpue             = year_max_cpue_1
)
{
  setwd(dir_data)
  file                <- paste(name_run, run_num, ".d1", sep="")
  filename            <- paste(dir_out,"Run", run_num, "/", file, sep="")
  first_age           <- sum(age_minus==0)+age_minus
  last_age            <- age_plus
  expanded_group      <- age_plus
  caa                 <- read.csv(file_caa, header = TRUE)
  year_min_cpue       <- rep(years[1], (cpue_number))
  year_max_cpue       <- rep(years[2], (cpue_number))
  cpue_spec           <- as.data.frame(read.csv(file_cpue_spec, header = TRUE))
  cpue_use            <- cpue_spec$cpue_use_1[1:cpue_number]
  cpue_units          <- cpue_spec$cpue_units_1[1:cpue_number]
  cpue_vulnerability  <- cpue_spec$cpue_vulnerability_1[1:cpue_number]
  cpue_timing         <- cpue_spec$cpue_timing_1[1:cpue_number]
  cpue_first_age      <- cpue_spec$cpue_first_age_1[1:cpue_number]
  cpue_last_age       <- cpue_spec$cpue_last_age_1[1:cpue_number]
  cpue_title          <- cpue_spec$cpue_title_1[1:cpue_number]
  cpues               <- as.data.frame(read.csv(file_cpue, header = TRUE))
  partial_catch       <- as.data.frame(read.csv(file_pcaa, header = TRUE))
  faa                 <- as.data.frame(read.csv(file_waa, header = TRUE))
  fec1                <- rep((fec1[(1:age_plus)]),length(years[1]:years[2]))
  fec                 <- matrix(fec1, nrow = length(years[1]:years[2]), 
                                ncol = age_plus, byrow = TRUE)
  fec                 <- fec*1000
  
cat("##############################################################################
#  DATA FILE FOR PROGRAM VPA-2BOX, Version 3.01
#   The data and specifications are entered in the order indicated 
#      by the existing comments. Additional comments must be preceded by a # symbol
#      in the first column, otherwise the line is perceived as free format input.
##############################################################################
#  Data file generated automatically from the write_d1VPA R function on ", date(),"
##############################################################################
# DATA FILE FOR RUN ", run_num, "\n", years,"\n",first_age, last_age, age_plus, expanded_group,
"\n##############################################################################
# BEGIN INPUT FOR ZONE/STOCK 1
##############################################################################\n",
cpue_number,"\n",spawning_season,"\n",maturity_at_age,
"\n# 50 CHARACTER TITLE WITHIN SINGLE QUOTES   ----->]  PDF OF CATCH 
# |                                                    |       SIGMA CATCH
", paste('\'',stock_title,'\'', sep=""), pdf_catch, sigma_catch,
"\n#============================================================================== 
# NOW ENTER THE CATCH-AT-AGE DATA. ROW=YEAR, COLUMN=AGE
#============================================================================== 
#   YEAR", sum(age_minus==0)+age_minus,"-",age_plus,"+ <--AGE\n", sep=" ", file=filename)
  
CAA=caa[which(caa[,1]>=years[1] & caa[,1]<=years[2]),]/scaling_parameter
write.table(round(CAA), file=filename, append=TRUE, eol="\n", row.names=FALSE, col.names=FALSE, quote = FALSE, sep='\t')

cat("-1
#============================================================================== 
# NOW ENTER IN THE ABUNDANCE INDEX SPECIFICATIONS
#============================================================================== 
# INDEX PDF (0= do not use,1=lognormal, 2=normal)
# |     |       UNITS (1 = numbers, 2 = biomass)
# |     |       |       VULNERABILITY (1=fixed, 2=frac.catches, 3=part. catches, 4=Butt. & Gero.
# |     |       |       |    TIMING (-1=average, +integer = number of months elapased}
# |     |       |       |    |      FIRST AGE LAST AGE  TITLE (IN SINGLE QUOTES)\n",
sep=" ", file=filename, append=T)
# cpue_last_age[cpue_last_age>7] <- age_plus
# if (age_plus<10) {cpue_first_age[3] <- age_plus} else {cpue_first_age[3] <- age_plus}
cpue_table <- cbind(1:cpue_number,
                    cpue_use,
                    cpue_units,
                    cpue_vulnerability,
                    cpue_timing,
                    cpue_first_age,
                    cpue_last_age,
                    paste("\'",cpue_title,"\'",sep="")
  )
cpue_table <- cpue_table[cpue_to_use,]
write.table(cpue_table, file=filename,sep = "\t", append=TRUE,eol="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)
cat("-1 
#============================================================================== 
# NOW ENTER IN THE INDICES OF ABUNDANCE
#============================================================================== 
#ID   YEAR      VALUE   CV (or STD ERROR)   INDEX NAME\n", sep=" ", file=filename, append=T)
  cpues11 <- cpues
  cpues <- cpues[which(cpues[,2] %in% cpue_to_use),] #where #ID matches cpues used, selects cpues' rows with that location
  cpues_final <- cpues[1,]
  for (i in 1:cpue_number){
    cpues_temp <- cpues[which(cpues[,2]==i & cpues[,3]>=year_min_cpue[i] & cpues[,3]<=year_max_cpue[i] ),] #selects cpues where the row has the #ID and years we're looking for
    cpues_final <- rbind(cpues_final,cpues_temp)
  }
  cpues <- cpues_final[-1,-1]
  cpues[,3] <- round(cpues[,3],3)
  write.table(cpues, file=filename,sep="\t", append=TRUE,eol="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)
cat("-1                                       
#============================================================================== 
# NOW ENTER IN THE VULNERABILITIES OR PARTIAL CATCHES FOR THE INDICES OF ABUNDANCE
#============================================================================== 
#INDEX YEAR   AGE ",sum(age_minus==0)+age_minus,"-",age_plus,"\n", sep=" ", file=filename, append=T)
  un                  <- unique(partial_catch[,2]) #stores unique #ID in PCAA df
  partial_catch_final <- partial_catch[1,] #stores top row of PCAA df
  
  for (i in 1:length(un)){ #for the number of unique #ID
    partial_catch_tmp   <- partial_catch[which(partial_catch[,2]==un[i] & 
                                                 partial_catch[,3]>=year_min_cpue[un[i]] &
                                                 partial_catch[,3]<=year_max_cpue[un[i]]),]
    partial_catch_final <- rbind(partial_catch_final,partial_catch_tmp)
  }
  partial_catch <- partial_catch_final[-1,-1]
  write.table(round(partial_catch,digits=2), file=filename,sep="\t", append=TRUE,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE)
  cat("-1 
#============================================================================== 
# NOW ENTER IN THE WEIGHTS AT AGE FOR THE INDICES OF ABUNDANCE (row=year, col=age)          
#============================================================================== 
#Index year  ages ",sum(age_minus==0)+age_minus,"-",age_plus,"\n", sep=" ", file=filename, append=T)
  # cpues <- cpues11
  # if (sum(is.na(match(cpue_to_use,which(cpue_units==2))))>0){ # if there are at least one of matching ID# of indices in use with location of indices in biomass units 
  #   iwh       <- cpue_to_use[!is.na(match(cpue_to_use,which(cpue_units==2)))] # then store these indices ID#s in iwh
  # } else {iwh <- which(cpue_units==2)} # otherwise store the position of indices in biomass
  # 
  # waa_tmp   <- NULL # generates empty object waa_tmp
  # #print(iwh)
  # for (i in 1:length(iwh)){ # for every number from 1 to the length of iwh (the # of indices in biomass units)
  #   # print(iwh[i])
  #   #  print(cpues)
  #   #print(cpues$V1)
  #   #print(which(as.numeric(cpues$V1)==iwh[i]))
  #   yearstart <- min(cpues[,3][which(as.numeric(cpues[,2])==iwh[i])]) #if the number in col 1 (#ID) of cpues equals the number in iwh, call that location from col 2 (year) and the earliest year for that #ID
  #   yearend   <- min(max(faa[,2]),max(cpues[,3][which(as.numeric(cpues[,2])==iwh[i]&cpues[,4]!=-999)]),years[2]) #save the earliest year b/w the max year in faa list, max year in cpues (this #ID), and the years vector
  #   waa_tmp <- rbind(waa_tmp,cbind(iwh[i], faa[which(faa[,2]%in%(yearstart:yearend)),2:(dim(faa)[2])])) # create a matrix binding the number of each iwh, range of years, and intersection b/w chosen years in faa and the 2nd dimension in faa (age?)
  # }
  waa_tmp <- cbind(faa[,1], faa[,2], round(faa[,3:ncol(faa)],3))
  
  #write.table(round(waa_tmp,3), file=filename,sep="\t", append=TRUE,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE)write.table(round(waa_tmp,3), file=filename,sep="\t", append=TRUE,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE)
  write.table(waa_tmp, file=filename, sep="\t", append=TRUE, eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE)
cat("-1
#============================================================================== 
# NOW ENTER IN THE FECUNDITY AT AGE FOR THE SPAWNING STOCK BIOMASS (row=year, col=age)       
#==============================================================================\n", sep=" ", file=filename, append=T)
  FEC <- cbind((years[1]:years[2]), fec)
  FEC <- FEC[which(FEC[,1]>=years[1] & FEC[,1]<=years[2]),]
  write.table(round(FEC,3), file=filename,sep="\t", append=TRUE,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE)
  cat("-1", sep=" ", file=filename, append=TRUE)
  print(paste("File ",filename,' has been created!!!', sep=""))
}

# Function to create data file
DATAFILE.FUN <- function(east, run.num, file.caaE, file.cpue.specE, file.cpueE, file.pcaaE, file.waaE,
                         file.caaW, file.cpue.specW, file.cpueW, file.pcaaW, file.waaW) {
  if(east==TRUE) {
    # Runs function to create data file (East)
    write_d1VPA(            dir_data                  = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/", # directory where data are found?
                            dir_out                   = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/", # directory for saving data file (output of function)
                            name_run                  = 'BFTE2017_',
                            run_num                   = run.num,
                            years                     = c(head(yrs,1), tail(yrs,1)),
                            age_minus                 = 0,
                            age_plus                  = nageE,
                            cpue_number               = ngrE,
                            spawning_season           = 6,
                            maturity_at_age           = paste(maa[1:nageE,1]),
                            stock_title               = 'BFTE',
                            pdf_catch                 = 0, # only used if bootstrapping (specify in control file)
                            sigma_catch               = 0.1, # only used if bootstrapping
                            file_caa                  = file.caaE, # reads in the CAA data from an external file (file_caa)
                            cpue_to_use               = c(1:ngrE), # specify which CPUEs to use (vector)
                            file_cpue_spec            = file.cpue.specE,
                            file_cpue                 = file.cpueE, # reads in CPUE data (from observation model)
                            file_pcaa                 = file.pcaaE, # reads in PCAA data (from observation model)
                            file_waa                  = file.waaE, # reads in WAA data for indices of abundance
                            fec1                      = waa[,1], # vector from OM
                            scaling_parameter         = 1,
                            year_max_cpue             = 2015)
  } else {
    # Runs function to create data file (West)
    write_d1VPA(            dir_data                  = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West/", # directory where data are found?
                            dir_out                   = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West/", # directory for saving data file (output of function)
                            name_run                  = 'BFTW2017_',
                            run_num                   = run.num,
                            years                     = c(head(yrs,1), tail(yrs,1)),
                            age_minus                 = 0,
                            age_plus                  = nageW,
                            cpue_number               = ngrW,
                            spawning_season           = 6,
                            maturity_at_age           = paste(maa[1:nageW,2]),
                            stock_title               = 'BFTW',
                            pdf_catch                 = 0, # only used if bootstrapping (specify in control file)
                            sigma_catch               = 0.1, # only used if bootstrapping
                            file_caa                  = file.caaW, # reads in the CAA data from an external file (file_caa)
                            cpue_to_use               = c(1:ngrW), # specify which CPUEs to use (vector of numbers?)
                            file_cpue_spec            = file.cpue.specW, # reads in abundance index specifications (from external file)
                            file_cpue                 = file.cpueW, # reads in CPUE data (from observation model)
                            file_pcaa                 = file.pcaaW, # reads in PCAA data (from observation model)
                            file_waa                  = file.waaW, # reads in WAA data for indices of abundance
                            fec1                      = waa[,2], # vector from OM
                            scaling_parameter         = 1,
                            year_max_cpue             = 2015)
  }
}


########################  WRITE PARAMETER FILE  ###########################

# Function to format parameter file
write_p1VPA <- function(dir_out                              = dirRun,
                        name_run                             = name_of_run_1,
                        run_num                              = run_num_1,
                        q_toggle                             = "east",
                        age_plus                             = 10,
                        age_minus                            = 0,
                        #  TERMINAL F PARAMETERS
                        min_bound_terminal_F_vector          = 0.1,
                        min_bound_terminal_F_vector_exponent = 6.0,
                        terminal_F_vector                    = 0.2,
                        max_bound_terminal_F_vector          = 0.5,
                        indicator                            = 1.0,
                        standard_deviation_prior             = 0.01,
                        #  F-RATIO PARAMETERS             
                        periods_duration                     = c(5,rep(c(1,4),6),1,3,1,4),
                        min_bound_terminal_F_vector_final    = c(rep(0.01, 17)),
                        min_bound_fratio_vector_exponent     = rep(0,7), 
                        terminal_F_vector_final              = c(0.7,rep(1,16)),
                        max_bound_terminal_F_vector_final    = c(rep(10,17)),
                        max_bound_fratio_vector_exponent     = rep(1,7),
                        indicator_final                      = c(0,rep(c(3,-0.1),8)),
                        standard_deviation_prior_final       = c(rep(c(0.3,0.5),8),0.3),
                        # reference_age_final                  = f_ratio_list$reference_age_final,
                        # NATURAL MORTALITY PARAMETERS
                        min_bound_terminal_M_vector          = rep(0.0, 16),
                        terminal_M_vector                    = c(0.65,0.51,0.42,0.34,0.30,0.27,0.25,0.24,0.21,0.20,0.20,0.19,0.19,0.19,0.18,0.18),
                        max_bound_terminal_M_vector          = rep(1.0, 16),
                        indicator_M                          = rep(0.0, 16),
                        standard_deviation_prior_M           = rep(0.1, 16),
                        # reference_age_M                      = seq((f_ratio_list$reference_age_final[1]+sum(f_ratio_list$periods_duration)),(f_ratio_list$reference_age_final[1]+sum(f_ratio_list$periods_duration))+age_plus-sum(age_minus==0)-age_minus),
                        # MIXING PARAMETERS: one parameter (set of specifications) for each age
                        mixing_parameter_1                   = 16,
                        min_bound_mixing_parameter           = 0,
                        best_mixing_parameter                = 0,
                        max_bound_mixing_parameter           = 0.1,
                        indicator_mixing_parameter           = 0,
                        standard_deviation_prior_parameter   = 0.1,
                        # reference_age_parameter              = (tail(reference_age_M,1)+1),
                        # STOCK-RECRUITMENT PARAMETERS
                        min_bound_SR                         = c(0.0, 0.0, 0.0, 0.0, 0.0),
                        SR                                   = c(0.2507, 0.1660, 0.1580, 0.1, 0.1),
                        D_best_SR                            = c(6,5,0,1,1),
                        max_bound_SR                         = c(0.1, 0.1, 0.9, 0.2, 0.2),
                        D_max_SR                             = c(21,21,0,1,1),
                        indicator_SR                         = c(0,0,0,0,0),
                        standard_deviation_prior_SR          = c(0.4,0,0,0,0),
                        # reference_age_parameter_SR           = seq((reference_age_parameter+age_plus-sum(age_minus==0)-age_minus+1),(reference_age_parameter+age_plus-sum(age_minus==0)-age_minus+1+4)),
                        # VARIANCE SCALING PARAMETER
                        cpue_number_p                        = cpue_number_1,
                        min_bound_VAR                        = rep(0.0,cpue_number_p),
                        VAR                                  = rep(0.1,cpue_number_p),
                        D_best_VAR                           = rep(1,cpue_number_p),
                        max_bound_VAR                        = rep(0.1,cpue_number_p),
                        D_max_VAR                            = rep(21,cpue_number_p),
                        indicator_VAR                        = c(1,rep(-0.1,(cpue_number_p-1))),
                        standard_deviation_prior_VAR         = rep(0.4,cpue_number_p),
                        # reference_age_parameter_VAR          = seq((tail(reference_age_parameter_SR,1)+1),(tail(reference_age_parameter_SR,1)+cpue_number_p))
                        # CATCHABILITY PARAMETERS
                        CAN_HL_dur, CAN_HL_min, CAN_HL_min_D, CAN_HL_best, CAN_HL_best_D,  CAN_HL_max, CAN_HL_indicator, CAN_HL_sd,
                        CAN_GSL_Acoustic_dur, CAN_GSL_Acoustic_min, CAN_GSL_Acoustic_min_D, CAN_GSL_Acoustic_best, CAN_GSL_Acoustic_best_D, CAN_GSL_Acoustic_max, CAN_GSL_Acoustic_indicator, CAN_GSL_Acoustic_sd,
                        US_RR_145_dur, US_RR_145_min, US_RR_145_min_D, US_RR_145_best, US_RR_145_best_D, US_RR_145_max, US_RR_145_indicator, US_RR_145_sd,
                        US_RR_66_114_dur, US_RR_66_114_min, US_RR_66_114_min_D, US_RR_66_114_best, US_RR_66_114_best_D, US_RR_66_114_max, US_RR_66_114_indicator, US_RR_66_114_sd,
                        US_RR_115_144_dur, US_RR_115_144_min, US_RR_115_144_min_D,US_RR_115_144_best, US_RR_115_144_best_D, US_RR_115_144_max, US_RR_115_144_indicator, US_RR_115_144_sd,
                        US_RR_145_177_dur, US_RR_145_177_min, US_RR_145_177_min_D, US_RR_145_177_best, US_RR_145_177_best_D, US_RR_145_177_max, US_RR_145_177_indicator, US_RR_145_177_sd,
                        US_RR_195_dur, US_RR_195_min, US_RR_195_min_D, US_RR_195_best, US_RR_195_best_D, US_RR_195_max, US_RR_195_indicator, US_RR_195_sd,
                        US_RR_195_COMB_dur, US_RR_195_COMB_min, US_RR_195_COMB_min_D, US_RR_195_COMB_best, US_RR_195_COMB_best_D, US_RR_195_COMB_max, US_RR_195_COMB_indicator, US_RR_195_COMB_sd,
                        US_RR_177_dur, US_RR_177_min, US_RR_177_min_D, US_RR_177_best, US_RR_177_best_D, US_RR_177_max, US_RR_177_indicator, US_RR_177_sd,
                        JLL_AREA_2_WEST_dur, JLL_AREA_2_WEST_min, JLL_AREA_2_WEST_min_D,JLL_AREA_2_WEST_best, JLL_AREA_2_WEST_best_D, JLL_AREA_2_WEST_max, JLL_AREA_2_WEST_indicator, JLL_AREA_2_WEST_sd,
                        JLL_AREA_3_dur, JLL_AREA_3_min, JLL_AREA_3_min_D, JLL_AREA_3_best, JLL_AREA_3_best_D, JLL_AREA_3_max, JLL_AREA_3_indicator, JLL_AREA_3_sd,
                        JLL_AREAS_17_18_dur, JLL_AREAS_17_18_min, JLL_AREAS_17_18_min_D, JLL_AREAS_17_18_best, JLL_AREAS_17_18_best_D, JLL_AREAS_17_18_max,JLL_AREAS_17_18_indicator, JLL_AREAS_17_18_sd,
                        LARVAL_ZERO_INFLATED_dur, LARVAL_ZERO_INFLATED_min ,LARVAL_ZERO_INFLATED_min_D, LARVAL_ZERO_INFLATED_best, LARVAL_ZERO_INFLATED_best_D, LARVAL_ZERO_INFLATED_max, LARVAL_ZERO_INFLATED_indicator, LARVAL_ZERO_INFLATED_sd,
                        GOM_PLL_1_6_dur, GOM_PLL_1_6_min, GOM_PLL_1_6_min_D, GOM_PLL_1_6_best, GOM_PLL_1_6_best_D, GOM_PLL_1_6_max, GOM_PLL_1_6_indicator, GOM_PLL_1_6_sd,
                        JLL_GOM_dur, JLL_GOM_min, JLL_GOM_min_D, JLL_GOM_best, JLL_GOM_best_D, JLL_GOM_max, JLL_GOM_indicator, JLL_GOM_sd,
                        TAGGING_dur, TAGGING_min, TAGGING_min_D, TAGGING_best, TAGGING_best_D, TAGGING_max, TAGGING_indicator, TAGGING_sd,
                        JLL_AREA_2_RECENT_dur, JLL_AREA_2_RECENT_min, JLL_AREA_2_RECENT_min_D,JLL_AREA_2_RECENT_best, JLL_AREA_2_RECENT_best_D, JLL_AREA_2_RECENT_max, JLL_AREA_2_RECENT_indicator, JLL_AREA_2_RECENT_sd
) {
  parameter_file_name                      <- paste(dir_out, "Run", run_num, "/", name_run, run_num, ".p1", sep="")
  reference_age                            <- seq((sum(age_minus==0)+age_minus), (age_plus-1))
  
cat("# PARAMETER FILE FOR PROGRAM VPA_2BOX, Version 3.0
## Parameter file generated automatically with the write_p1VPA R function on ", date(),
"\n#-----------------------------------------------------------------------------
#        The specifications are entered in the order indicated 
#        by the existing comments. Additional comments must be preceded by a number symbol
#        in the first column, otherwise the line is perceived as free format input.
#
#      	Each parameter in the model must have its own specification lines unless a dollar 
#      	symbol is placed in the first column followed by an integer value (n), which 
#      	tells the program that the next n parameters abide by the same specifications.
#
#      	The format of each specification line is as follows
#
#      	column 1
#      	|   number of parameters to which these specifications apply
#      	|   |    lower bound
#      	|   |    |       best estimate (prior expectation)
#      	|   |    |       |       upper bound
#      	|   |    |       |       |       method of estimation
#      	|   |    |       |       |       |      standard deviation of prior 
#      	$   5    0       1.2     2.0     1      0.1
#
#	The methods of estimation include:
#	0  	set equal to the value given for the best estimate (a fixed constant)
#	1	estimate in the usual frequentist (non-Bayesian) sense 
#	2(0.1)	estimate as a random deviation from the previous parameter
#	3(0.2)	estimate as a random deviation from the previous constant or type 1 parameter
#	4(0.3)	estimate as random deviation from the best estimate.
#	-0.1  	set equal to the value of the closest previous estimated parameter
#	-n  	set equal to the value of the nth parameter in the list (estimated or not)
#-----------------------------------------------------------------------------
#=============================================================================
# TERMINAL F PARAMETERS: (lower bound, best estimate, upper bound, indicator, reference age)
#   	Note 1: the method indicator for the terminal F parameters is unique in that if it is
#   	zero but the best estimate is set to a value < 9, then the 'best estimate'
#   	is taken to be the vulnerability relative to the reference age in the last 
#   	(fifth) column.  Otherwise these parameters are treated the same as the
#    	others below and the fifth column is the standard deviation of the prior.
# 	Note 2: the last age is represented by an F-ratio parameter (below), so the number
#     	of entries here should be 1 fewer than the number of ages 
#-----------------------------------------------------------------------------\n",
sep="", file=parameter_file_name)
  
  terminal_Fs_final <- cbind(paste(
    "  ", sprintf('%.4f',min_bound_terminal_F_vector),"D-0", min_bound_terminal_F_vector_exponent,
    "   ", sprintf('%.4f',terminal_F_vector), "D+00",
    "   ", sprintf('%.4f',max_bound_terminal_F_vector),"D+01",
    "      ",sprintf('%.1f',indicator),
    "   ", sprintf('%.4f',standard_deviation_prior),"D+01",
    #"     ",sprintf('%.0f',reference_age),
    sep=""))  
  write.table(terminal_Fs_final,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE,sep='',append=TRUE)
  
cat("#=============================================================================
# F-RATIO PARAMETERS F{oldest}/F{oldest-1} one parameter (set of specifications) for each year
#-----------------------------------------------------------------------------\n",
      sep="", file=parameter_file_name, append=T)
  
  terminal_Fs_final <- cbind(paste(
    "$ ",sprintf('%.0f',periods_duration),
    "  ", sprintf('%.4f',min_bound_terminal_F_vector_final),"D-0", min_bound_fratio_vector_exponent,
    "   ", sprintf('%.4f',terminal_F_vector_final), "D+00",
    "   ", sprintf('%.4f',max_bound_terminal_F_vector_final),"D+0", max_bound_fratio_vector_exponent,
    "      ",sprintf('%.1f',indicator_final),
    "   ", sprintf('%.4f',standard_deviation_prior_final),"D+00",
    # "    ",sprintf('%.0f',reference_age_final),
    sep=""))
  write.table(terminal_Fs_final,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
  
cat("#=============================================================================
# NATURAL MORTALITY PARAMETERS: one parameter (set of specifications) for each age
#-----------------------------------------------------------------------------\n", 
      sep="", file=parameter_file_name, append=T)
  
  terminal_Ms <- cbind(paste(
    "  ",sprintf('%.4f',min_bound_terminal_M_vector),"D+00",
    "   ", sprintf('%.4f',terminal_M_vector), "D+00",
    "   ", sprintf('%.4f',max_bound_terminal_M_vector),"D+00",
    "      ",sprintf('%.1f',indicator_M),
    "   ", sprintf('%.4f',standard_deviation_prior_M),"D+00",
    #"    ",sprintf('%.0f',reference_age),
    sep=""))
  write.table(terminal_Ms,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
cat("#=============================================================================
# MIXING PARAMETERS: one parameter (set of specifications) for each age
#-----------------------------------------------------------------------------\n",
      sep="", file=parameter_file_name, append=T)
  
  mixing_parameter <- cbind(paste(
    "$ ",sprintf('%.0f',mixing_parameter_1),
    "  ", sprintf('%.4f',min_bound_mixing_parameter),"D+00",
    "   ", sprintf('%.4f',best_mixing_parameter), "D+00",
    "   ", sprintf('%.4f',max_bound_mixing_parameter),"D+00",
    "      ",sprintf('%.1f',indicator_mixing_parameter),
    "   ", sprintf('%.4f',standard_deviation_prior_parameter),"D+00",
    # "    ",sprintf('%.0f',reference_age_parameter),
    sep=""))
  write.table(mixing_parameter,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
cat("#=============================================================================
# STOCK-RECRUITMENT PARAMETERS: five parameters so 5 sets of specifications
#-----------------------------------------------------------------------------\n",
      sep="", file=parameter_file_name, append=T)
  
  SR_parameter <- cbind(paste(
    "  ", sprintf('%.4f',min_bound_SR),"D+00",
    "   ", sprintf('%.4f',SR),
    paste("D+",sprintf('%.2i',D_best_SR),sep=""),
    "   ", sprintf('%.4f',max_bound_SR),
    paste("D+",sprintf('%.2i',D_max_SR),sep=""),
    "      ",sprintf('%.1f',indicator_SR),"   ",
    sprintf('%.4f',standard_deviation_prior_SR),"D+00",
    # "    ",sprintf('%.0f',reference_age_parameter_SR),
    sep=""))
  write.table(SR_parameter,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
  cat("#=============================================================================
# VARIANCE SCALING PARAMETER (lower bound, best estimate, upper bound, indicator, std. dev.) 
#   this parameter scales the input variance up or down as desired
#   In principal, if you estimate this you should obtain more accurate estimates of the
#   magnitude of the parameter variances-- all other things being equal.
#-----------------------------------------------------------------------------\n",
      sep="", file=parameter_file_name, append=T)
  
  VAR_parameter <- cbind(paste(
    "  ", sprintf('%.4f',min_bound_VAR),"D+00",
    "   ", sprintf('%.4f',VAR),
    paste("D+",sprintf('%.2i',D_best_VAR),sep=""),
    "   ", sprintf('%.4f',max_bound_VAR),
    paste("D+",sprintf('%.2i',D_max_VAR),sep=""),
    "      ",sprintf('%.1f',indicator_VAR),
    "   ", sprintf('%.4f',standard_deviation_prior_VAR),"D+00",
    # "    ",sprintf('%.0f',reference_age_parameter_VAR),
    sep=""))
  write.table(VAR_parameter,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
  if (q_toggle=="west") {  
    cat("#=============================================================================
# CATCHABILITY PARAMETERS
#-----------------------------------------------------------------------------\n",
        sep="", file=parameter_file_name, append=TRUE)
    
    cat("#1_CAN_HL \n",sep="",file=parameter_file_name,append=TRUE)
    CAN_HL_q <- cbind(paste(
      "$ ",sprintf('%.0f',CAN_HL_dur),
      "  ",sprintf('%.0f',CAN_HL_min),"D-",CAN_HL_min_D,
      "  ",sprintf('%.0f',CAN_HL_best),"D-",CAN_HL_best_D,
      "  ",sprintf('%.0f',CAN_HL_max),
      "  ",sprintf('%.1f',CAN_HL_indicator),
      "  ",sprintf('%.1f',CAN_HL_sd),
      sep=""))
    write.table(CAN_HL_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#2_CAN_GSL_Acoustic \n",sep="",file=parameter_file_name,append=TRUE)
    CAN_GSL_Acoustic_q <- cbind(paste(
      "$ ",sprintf('%.0f',CAN_GSL_Acoustic_dur),
      "  ",sprintf('%.0f',CAN_GSL_Acoustic_min),"D-",CAN_GSL_Acoustic_min_D,
      "  ",sprintf('%.0f',CAN_GSL_Acoustic_best),"D-",CAN_GSL_Acoustic_best_D,
      "  ",sprintf('%.0f',CAN_GSL_Acoustic_max),
      "  ",sprintf('%.1f',CAN_GSL_Acoustic_indicator),
      "  ",sprintf('%.1f',CAN_GSL_Acoustic_sd),
      sep=""))
    write.table(CAN_GSL_Acoustic_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#3_US_RR_<145 \n",sep="",file=parameter_file_name,append=TRUE)
    US_RR_145_q <- cbind(paste(
      "$ ",sprintf('%.0f',US_RR_145_dur),
      "  ",sprintf('%.0f',US_RR_145_min),"D-",US_RR_145_min_D,
      "  ",sprintf('%.0f',US_RR_145_best),"D-",US_RR_145_best_D,
      "  ",sprintf('%.0f',US_RR_145_max),
      "  ",sprintf('%.1f',US_RR_145_indicator),
      "  ",sprintf('%.1f',US_RR_145_sd),
      sep=""))
    write.table(US_RR_145_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#4_US_RR_66_114 \n",sep="",file=parameter_file_name,append=TRUE)
    US_RR_66_114_q <- cbind(paste(
      "$ ",sprintf('%.0f',US_RR_66_114_dur),
      "  ",sprintf('%.0f',US_RR_66_114_min),"D-",US_RR_66_114_min_D,
      "  ",sprintf('%.0f',US_RR_66_114_best),"D-",US_RR_66_114_best_D,
      "  ",sprintf('%.0f',US_RR_66_114_max),
      "  ",sprintf('%.1f',US_RR_66_114_indicator),
      "  ",sprintf('%.1f',US_RR_66_114_sd),
      sep=""))
    write.table(US_RR_66_114_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#5_US_RR_115_144 \n",sep="",file=parameter_file_name,append=TRUE)
    US_RR_115_144_q <- cbind(paste(
      "$ ",sprintf('%.0f',US_RR_115_144_dur),
      "  ",sprintf('%.0f',US_RR_115_144_min),"D-",US_RR_115_144_min_D,
      "  ",sprintf('%.0f',US_RR_115_144_best),"D-",US_RR_115_144_best_D,
      "  ",sprintf('%.0f',US_RR_115_144_max),
      "  ",sprintf('%.1f',US_RR_115_144_indicator),
      "  ",sprintf('%.1f',US_RR_115_144_sd),
      sep=""))
    write.table(US_RR_115_144_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#6_US_RR_145_177 \n",sep="",file=parameter_file_name,append=TRUE)
    US_RR_145_177_q <- cbind(paste(
      "$ ",sprintf('%.0f',US_RR_145_177_dur),
      "  ",sprintf('%.0f',US_RR_145_177_min),"D-",US_RR_145_177_min_D,
      "  ",sprintf('%.0f',US_RR_145_177_best),"D-",US_RR_145_177_best_D,
      "  ",sprintf('%.0f',US_RR_145_177_max),
      "  ",sprintf('%.1f',US_RR_145_177_indicator),
      "  ",sprintf('%.1f',US_RR_145_177_sd),
      sep=""))
    write.table(US_RR_145_177_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#7_US_RR>195 \n",sep="",file=parameter_file_name,append=TRUE)
    US_RR_195_q <- cbind(paste(
      "$ ",sprintf('%.0f',US_RR_195_dur),
      "  ",sprintf('%.0f',US_RR_195_min),"D-",US_RR_195_min_D,
      "  ",sprintf('%.0f',US_RR_195_best),"D-",US_RR_195_best_D,
      "  ",sprintf('%.0f',US_RR_195_max),
      "  ",sprintf('%.1f',US_RR_195_indicator),
      "  ",sprintf('%.1f',US_RR_195_sd),
      sep=""))
    write.table(US_RR_195_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#8_US_RR>195_COMB \n",sep="",file=parameter_file_name,append=TRUE)
    US_RR_195_COMB_q <- cbind(paste(
      "$ ",sprintf('%.0f',US_RR_195_COMB_dur),
      "  ",sprintf('%.0f',US_RR_195_COMB_min),"D-",US_RR_195_COMB_min_D,
      "  ",sprintf('%.0f',US_RR_195_COMB_best),"D-",US_RR_195_COMB_best_D,
      "  ",sprintf('%.0f',US_RR_195_COMB_max),
      "  ",sprintf('%.1f',US_RR_195_COMB_indicator),
      "  ",sprintf('%.1f',US_RR_195_COMB_sd),
      sep=""))
    write.table(US_RR_195_COMB_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#9_US_RR>177 \n",sep="",file=parameter_file_name,append=TRUE)
    US_RR_177_q <- cbind(paste(
      "$ ",sprintf('%.0f',US_RR_177_dur),
      "  ",sprintf('%.0f',US_RR_177_min),"D-",US_RR_177_min_D,
      "  ",sprintf('%.0f',US_RR_177_best),"D-",US_RR_177_best_D,
      "  ",sprintf('%.0f',US_RR_177_max),
      "  ",sprintf('%.1f',US_RR_177_indicator),
      "  ",sprintf('%.1f',US_RR_177_sd),
      sep=""))
    write.table(US_RR_177_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#10_JLL_AREA_2_(WEST) \n",sep="",file=parameter_file_name,append=TRUE)
    JLL_AREA_2_WEST_q <- cbind(paste(
      "$ ",sprintf('%.0f',JLL_AREA_2_WEST_dur),
      "  ",sprintf('%.0f',JLL_AREA_2_WEST_min),"D-",JLL_AREA_2_WEST_min_D,
      "  ",sprintf('%.0f',JLL_AREA_2_WEST_best),"D-",JLL_AREA_2_WEST_best_D,
      "  ",sprintf('%.0f',JLL_AREA_2_WEST_max),
      "  ",sprintf('%.1f',JLL_AREA_2_WEST_indicator),
      "  ",sprintf('%.1f',JLL_AREA_2_WEST_sd),
      sep=""))
    write.table(JLL_AREA_2_WEST_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#11_JLL_AREA_3_(31+32) \n",sep="",file=parameter_file_name,append=TRUE)
    JLL_AREA_3_q <- cbind(paste(
      "$ ",sprintf('%.0f',JLL_AREA_3_dur),
      "  ",sprintf('%.0f',JLL_AREA_3_min),"D-",JLL_AREA_3_min_D,
      "  ",sprintf('%.0f',JLL_AREA_3_best),"D-",JLL_AREA_3_best_D,
      "  ",sprintf('%.0f',JLL_AREA_3_max),
      "  ",sprintf('%.1f',JLL_AREA_3_indicator),
      "  ",sprintf('%.1f',JLL_AREA_3_sd),
      sep=""))
    write.table(JLL_AREA_3_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#12_JLL_AREAS_17+18 \n",sep="",file=parameter_file_name,append=TRUE)
    JLL_AREAS_17_18_q <- cbind(paste(
      "$ ",sprintf('%.0f',JLL_AREAS_17_18_dur),
      "  ",sprintf('%.0f',JLL_AREAS_17_18_min),"D-",JLL_AREAS_17_18_min_D,
      "  ",sprintf('%.0f',JLL_AREAS_17_18_best),"D-",JLL_AREAS_17_18_best_D,
      "  ",sprintf('%.0f',JLL_AREAS_17_18_max),
      "  ",sprintf('%.1f',JLL_AREAS_17_18_indicator),
      "  ",sprintf('%.1f',JLL_AREAS_17_18_sd),
      sep=""))
    write.table(JLL_AREAS_17_18_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#13_LARVAL_ZERO_INFLATED \n",sep="",file=parameter_file_name,append=TRUE)
    LARVAL_ZERO_INFLATED_q <- cbind(paste(
      "$ ",sprintf('%.0f',LARVAL_ZERO_INFLATED_dur),
      "  ",sprintf('%.0f',LARVAL_ZERO_INFLATED_min),"D-",LARVAL_ZERO_INFLATED_min_D,
      "  ",sprintf('%.0f',LARVAL_ZERO_INFLATED_best),"D-",LARVAL_ZERO_INFLATED_best_D,
      "  ",sprintf('%.0f',LARVAL_ZERO_INFLATED_max),
      "  ",sprintf('%.1f',LARVAL_ZERO_INFLATED_indicator),
      "  ",sprintf('%.1f',LARVAL_ZERO_INFLATED_sd),
      sep=""))
    write.table(LARVAL_ZERO_INFLATED_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#14_GOM_PLL_1-6 \n",sep="",file=parameter_file_name,append=TRUE)
    GOM_PLL_1_6_q <- cbind(paste(
      "$ ",sprintf('%.0f',GOM_PLL_1_6_dur),
      "  ",sprintf('%.0f',GOM_PLL_1_6_min),"D-",GOM_PLL_1_6_min_D,
      "  ",sprintf('%.0f',GOM_PLL_1_6_best),"D-",GOM_PLL_1_6_best_D,
      "  ",sprintf('%.0f',GOM_PLL_1_6_max),
      "  ",sprintf('%.1f',GOM_PLL_1_6_indicator),
      "  ",sprintf('%.1f',GOM_PLL_1_6_sd),
      sep=""))
    write.table(GOM_PLL_1_6_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#15_JLL_GOM \n",sep="",file=parameter_file_name,append=TRUE)
    JLL_GOM_q <- cbind(paste(
      "$ ",sprintf('%.0f',JLL_GOM_dur),
      "  ",sprintf('%.0f',JLL_GOM_min),"D-",JLL_GOM_min_D,
      "  ",sprintf('%.0f',JLL_GOM_best),"D-",JLL_GOM_best_D,
      "  ",sprintf('%.0f',JLL_GOM_max),
      "  ",sprintf('%.1f',JLL_GOM_indicator),
      "  ",sprintf('%.1f',JLL_GOM_sd),
      sep=""))
    write.table(JLL_GOM_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#16_TAGGING \n",sep="",file=parameter_file_name,append=TRUE)
    TAGGING_q <- cbind(paste(
      "$ ",sprintf('%.0f',TAGGING_dur),
      "  ",sprintf('%.0f',TAGGING_min),"D-",TAGGING_min_D,
      "  ",sprintf('%.0f',TAGGING_best),"D-",TAGGING_best_D,
      "  ",sprintf('%.0f',TAGGING_max),
      "  ",sprintf('%.1f',TAGGING_indicator),
      "  ",sprintf('%.1f',TAGGING_sd),
      sep=""))
    write.table(TAGGING_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    cat("#17_JLL_AREA_2_RECENT \n",sep="",file=parameter_file_name,append=TRUE)
    JLL_AREA_2_RECENT_q <- cbind(paste(
      "$ ",sprintf('%.0f',JLL_AREA_2_RECENT_dur),
      "  ",sprintf('%.0f',JLL_AREA_2_RECENT_min),"D-",JLL_AREA_2_RECENT_min_D,
      "  ",sprintf('%.0f',JLL_AREA_2_RECENT_best),"D-",JLL_AREA_2_RECENT_best_D,
      "  ",sprintf('%.0f',JLL_AREA_2_RECENT_max),
      "  ",sprintf('%.1f',JLL_AREA_2_RECENT_indicator),
      "  ",sprintf('%.1f',JLL_AREA_2_RECENT_sd),
      sep=""))
    write.table(JLL_AREA_2_RECENT_q,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)
    
    cat("@ END PARAMETER INPUT",sep="", file=parameter_file_name, append=TRUE)
  } else {
    cat("@ END PARAMETER INPUT",sep="", file=parameter_file_name, append=TRUE)
  }
  print(paste("File ",parameter_file_name,' has been created!!!', sep=""))
  
}


# Function to create parameter file
PARAMFILE.FUN <- function(east, run.num) {
  
  if(east==TRUE) {
    # Runs function to create parameter file (East)
    write_p1VPA(dir_out                              = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/",
                name_run                             = "BFTE2017_",
                run_num                              = run.num,
                q_toggle                             = "east",
                age_plus                             = nageE,
                age_minus                            = 0,
                # TERMINAL F PARAMETERS
                min_bound_terminal_F_vector          = rep(0.1,9),
                min_bound_terminal_F_vector_exponent = rep(6,9),
                terminal_F_vector                    = rep(0.2,9),
                max_bound_terminal_F_vector          = rep(0.5,9),
                indicator                            = 1.0,
                standard_deviation_prior             = 0.01,
                # F-RATIO PARAMETERS
                periods_duration                     = c(1,6,1,14,1,11,8),
                min_bound_terminal_F_vector_final    = rep(0.01,7),
                min_bound_fratio_vector_exponent     = rep(1,7), 
                terminal_F_vector_final              = c(rep(1.25,6),1),
                max_bound_terminal_F_vector_final    = rep(4,7),
                max_bound_fratio_vector_exponent     = rep(1,7),
                indicator_final                      = c(1,-0.1,1,-0.1,1,-0.1,0),
                standard_deviation_prior_final       = rep(0.3,7),
                # reference_age_final                  = f_ratio_list$reference_age_final,
                # NATURAL MORTALITY PARAMETERS
                min_bound_terminal_M_vector          = rep(0, 10),
                #terminal_M_vector                    = c(0.65,0.51,0.42,0.34,0.30,0.27,0.25,0.24,0.21,0.20),
                terminal_M_vector                    = as.numeric(paste(M[1:nageE,1]*4)),
                max_bound_terminal_M_vector          = rep(1.0, 10),
                indicator_M                          = rep(0.0, 10),
                standard_deviation_prior_M           = rep(0.1, 10),
                # reference_age_M                      = seq((f_ratio_list$reference_age_final[1]+sum(f_ratio_list$periods_duration)),(f_ratio_list$reference_age_final[1]+sum(f_ratio_list$periods_duration))+age_plus-sum(age_minus==0)-age_minus),
                # MIXING PARAMETERS: one parameter (set of specifications) for each age
                mixing_parameter_1                   = 10,
                min_bound_mixing_parameter           = 0,
                best_mixing_parameter                = 0,
                max_bound_mixing_parameter           = 1.0,
                indicator_mixing_parameter           = 0,
                standard_deviation_prior_parameter   = 0.1,
                # reference_age_parameter              = (tail(reference_age_M,1)+1),
                #  STOCK-RECRUITMENT PARAMETERS
                min_bound_SR                         = rep(0,5),
                SR                                   = c(0.2507,0.1660,0.1580,0.1000,0.1000),
                D_best_SR                            = c(6,5,0,1,1),
                max_bound_SR                         = c(0.1000,0.1000,0.9000,0.2000,0.2000),
                D_max_SR                             = c(21,21,0,1,1),
                indicator_SR                         = c(0,0,0,0,0),
                standard_deviation_prior_SR          = c(0.4,0,0,0,0),
                # reference_age_parameter_SR           = seq((reference_age_parameter+age_plus-sum(age_minus==0)-age_minus+1),(reference_age_parameter+age_plus-sum(age_minus==0)-age_minus+1+4)),
                # VARIANCE SCALING PARAMETER
                # cpue_number_p                        = cpue_number_1,
                min_bound_VAR                        = c(rep(0.0,8),0.01,0.0),
                VAR                                  = rep(0.1,10),
                D_best_VAR                           = rep(1,10),
                max_bound_VAR                        = rep(0.1,10),
                D_max_VAR                            = rep(4,10),
                indicator_VAR                        = c(1,-0.1,1,-0.1,-0.1,1,-0.1,1,1,1),
                standard_deviation_prior_VAR         = rep(0.4,10)
                # reference_age_parameter_VAR          = seq((tail(reference_age_parameter_SR,1)+1),(tail(reference_age_parameter_SR,1)+cpue_number_p))
    )
  } else {
    # Runs function to create parameter file (West)
    write_p1VPA(dir_out                              = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West/",
                name_run                             = "BFTW2017_",
                run_num                              = run.num,
                q_toggle                             = "west",
                age_plus                             = nageW,
                age_minus                            = 0,
                # TERMINAL F PARAMETERS
                # min_bound_terminal_F_vector          = rep(1,15),  #ICCAT default
                # min_bound_terminal_F_vector_exponent = rep(4,15),  #ICCAT default
                min_bound_terminal_F_vector          = rep(0.1,15),  #revised EM
                min_bound_terminal_F_vector_exponent = rep(6,15),    #revised EM
                terminal_F_vector                    = 0.41,
                max_bound_terminal_F_vector          = 0.4,
                indicator                            = 1.0,
                standard_deviation_prior             = 0.01,
                #  F-RATIO PARAMETERS
                periods_duration                     = c(1,41),
                min_bound_terminal_F_vector_final    = rep(0,2),
                min_bound_fratio_vector_exponent     = rep(0,2), 
                terminal_F_vector_final              = rep(1,2),
                max_bound_terminal_F_vector_final    = rep(4,2),
                max_bound_fratio_vector_exponent     = rep(0,2),
                indicator_final                      = rep(0,2),
                standard_deviation_prior_final       = rep(0.2,2),
                # reference_age_final                  = f_ratio_list$reference_age_final,
                #  NATURAL MORTALITY PARAMETERS
                min_bound_terminal_M_vector          = rep(0,16),
                #terminal_M_vector                    = c(0.65,0.51,0.42,0.34,0.30,0.27,0.25,0.24,0.21,0.20,0.20,0.19,0.19,0.19,0.18,0.18),
                terminal_M_vector                    = as.numeric(paste(M[1:nageW,2]*4)),
                max_bound_terminal_M_vector          = rep(1.0,16),
                indicator_M                          = rep(0,16),
                standard_deviation_prior_M           = rep(0.1,16),
                # reference_age_M                      = seq((f_ratio_list$reference_age_final[1]+sum(f_ratio_list$periods_duration)),(f_ratio_list$reference_age_final[1]+sum(f_ratio_list$periods_duration))+age_plus-sum(age_minus==0)-age_minus),
                # MIXING PARAMETERS: one parameter (set of specifications) for each age
                mixing_parameter_1                   = 16,
                min_bound_mixing_parameter           = 0,
                best_mixing_parameter                = 0,
                max_bound_mixing_parameter           = 1.0,
                indicator_mixing_parameter           = 0,
                standard_deviation_prior_parameter   = 0.1,
                # reference_age_parameter              = (tail(reference_age_M,1)+1),
                # STOCK-RECRUITMENT PARAMETERS
                min_bound_SR                         = rep(0,5),
                SR                                   = c(220982.5,16441.44,0.000,0.5,10),
                D_best_SR                            = c(0,0,0,0,0),
                max_bound_SR                         = c(1,1,0.9,1,1000),
                D_max_SR                             = c(20,20,0,0,0),
                indicator_SR                         = c(0,0,0,0,0),
                standard_deviation_prior_SR          = c(0.4,0,0,0,0),
                # reference_age_parameter_SR           = seq((reference_age_parameter+age_plus-sum(age_minus==0)-age_minus+1),(reference_age_parameter+age_plus-sum(age_minus==0)-age_minus+1+4)),
                #  VARIANCE SCALING PARAMETER
                # cpue_number_p                        = cpue_number_1,
                min_bound_VAR                        = rep(0.0,17),
                VAR                                  = c(0.4,0,rep(0.4,10),0,rep(0.4,4)),
                D_best_VAR                           = rep(0,17),
                max_bound_VAR                        = rep(4,17),
                D_max_VAR                            = rep(0,17),
                indicator_VAR                        = c(0,0,1,-0.1,-0.1,-0.1,1,-0.1,-0.1,1,-0.1,-0.1,0,1,-104,0,-104),
                standard_deviation_prior_VAR         = rep(0.2,17),
                # reference_age_parameter_VAR          = seq((tail(reference_age_parameter_SR,1)+1),(tail(reference_age_parameter_SR,1)+cpue_number_p))
                # CATCHABILITY PARAMETERS (west only in 2017)
                CAN_HL_dur = c(10,1,31), CAN_HL_min = rep(1,3), CAN_HL_min_D = rep(20,3), CAN_HL_best = rep(1,3), CAN_HL_best_D = rep(4,3),  CAN_HL_max = rep(1,3), CAN_HL_indicator = c(0,0,-0.1), CAN_HL_sd = rep(0.2,3),
                CAN_GSL_Acoustic_dur = c(20,1,21), CAN_GSL_Acoustic_min = rep(1,3), CAN_GSL_Acoustic_min_D = rep(20,3), CAN_GSL_Acoustic_best = rep(1,3), CAN_GSL_Acoustic_best_D = rep(4,3), CAN_GSL_Acoustic_max = rep(1,3), CAN_GSL_Acoustic_indicator = c(0,1,-0.1), CAN_GSL_Acoustic_sd = rep(0.2,3),
                US_RR_145_dur = c(1,41), US_RR_145_min = rep(1,2), US_RR_145_min_D = rep(20,2), US_RR_145_best = rep(1,2), US_RR_145_best_D = rep(4,2), US_RR_145_max = rep(1,2), US_RR_145_indicator = c(1,-0.1), US_RR_145_sd = rep(0.2,2),
                US_RR_66_114_dur = c(1,41), US_RR_66_114_min = rep(1,2), US_RR_66_114_min_D = rep(20,2), US_RR_66_114_best = rep(1,2), US_RR_66_114_best_D = rep(4,2), US_RR_66_114_max = rep(1,2), US_RR_66_114_indicator = c(1,-0.1), US_RR_66_114_sd = rep(0.2,2),
                US_RR_115_144_dur = c(1,41), US_RR_115_144_min = rep(1,2), US_RR_115_144_min_D = rep(20,2), US_RR_115_144_best = rep(1,2), US_RR_115_144_best_D = rep(4,2), US_RR_115_144_max = rep(1,2), US_RR_115_144_indicator = c(1,-0.1), US_RR_115_144_sd = rep(0.2,2),
                US_RR_145_177_dur = 42, US_RR_145_177_min = 1, US_RR_145_177_min_D = 20, US_RR_145_177_best = 1, US_RR_145_177_best_D = 4, US_RR_145_177_max = 1, US_RR_145_177_indicator = 0, US_RR_145_177_sd = 0.2,
                US_RR_195_dur = c(1,41), US_RR_195_min = rep(1,2), US_RR_195_min_D = rep(20,2), US_RR_195_best = rep(1,2), US_RR_195_best_D = rep(4,2), US_RR_195_max = rep(1,2), US_RR_195_indicator = c(1,-0.1), US_RR_195_sd = rep(0.2,2),
                US_RR_195_COMB_dur = 42, US_RR_195_COMB_min = 1, US_RR_195_COMB_min_D = 20, US_RR_195_COMB_best = 1, US_RR_195_COMB_best_D = 4, US_RR_195_COMB_max = 1, US_RR_195_COMB_indicator = 0, US_RR_195_COMB_sd = 0.2,
                US_RR_177_dur = c(19,1,22), US_RR_177_min = rep(1,3), US_RR_177_min_D = rep(20,3), US_RR_177_best = rep(1,3), US_RR_177_best_D = rep(4,3), US_RR_177_max = rep(1,3), US_RR_177_indicator = c(0,0,-0.1), US_RR_177_sd = rep(0.2,3),
                JLL_AREA_2_WEST_dur = c(2,1,33,6), JLL_AREA_2_WEST_min = rep(1,4), JLL_AREA_2_WEST_min_D = rep(20,4), JLL_AREA_2_WEST_best = rep(1,4), JLL_AREA_2_WEST_best_D = rep(4,4), JLL_AREA_2_WEST_max = rep(1,4), JLL_AREA_2_WEST_indicator = c(0,1,-0.1,0), JLL_AREA_2_WEST_sd = rep(0.2,4),
                JLL_AREA_3_dur = 42, JLL_AREA_3_min = 1, JLL_AREA_3_min_D = 20, JLL_AREA_3_best = 1, JLL_AREA_3_best_D = 4, JLL_AREA_3_max = 1, JLL_AREA_3_indicator = 0, JLL_AREA_3_sd = 0.2,
                JLL_AREAS_17_18_dur = 42, JLL_AREAS_17_18_min = 1, JLL_AREAS_17_18_min_D = 20, JLL_AREAS_17_18_best = 1, JLL_AREAS_17_18_best_D = 4, JLL_AREAS_17_18_max = 1, JLL_AREAS_17_18_indicator = 0, JLL_AREAS_17_18_sd = 0.2,
                LARVAL_ZERO_INFLATED_dur = c(1,41), LARVAL_ZERO_INFLATED_min = rep(1,2), LARVAL_ZERO_INFLATED_min_D = rep(20,2), LARVAL_ZERO_INFLATED_best = rep(1,2), LARVAL_ZERO_INFLATED_best_D = rep(4,2), LARVAL_ZERO_INFLATED_max = rep(1,2), LARVAL_ZERO_INFLATED_indicator = c(1,-0.1), LARVAL_ZERO_INFLATED_sd = rep(0.2,2),
                GOM_PLL_1_6_dur = c(1,41), GOM_PLL_1_6_min = rep(1,2), GOM_PLL_1_6_min_D = rep(20,2), GOM_PLL_1_6_best = rep(1,2), GOM_PLL_1_6_best_D = rep(4,2), GOM_PLL_1_6_max = rep(1,2), GOM_PLL_1_6_indicator = c(1,-0.1), GOM_PLL_1_6_sd = rep(0.2,2),
                JLL_GOM_dur = c(1,41), JLL_GOM_min = rep(1,2), JLL_GOM_min_D = rep(20,2), JLL_GOM_best = rep(1,2), JLL_GOM_best_D = rep(4,2), JLL_GOM_max = rep(1,2), JLL_GOM_indicator= c(1,-0.1), JLL_GOM_sd = rep(0.2,2),
                TAGGING_du = c(1,41), TAGGING_min = rep(1,2), TAGGING_min_D = rep(20,2), TAGGING_best = rep(1,2), TAGGING_best_D = rep(4,2), TAGGING_max = rep(1,2), TAGGING_indicator= c(0,-0.1), TAGGING_sd = rep(0.2,2),
                JLL_AREA_2_RECENT_dur = c(36,1,5), JLL_AREA_2_RECENT_min = rep(1,3), JLL_AREA_2_RECENT_min_D = rep(20,3), JLL_AREA_2_RECENT_best = rep(1,3), JLL_AREA_2_RECENT_best_D = rep(4,3), JLL_AREA_2_RECENT_max = rep(1,3), JLL_AREA_2_RECENT_indicator = c(0,1,3), JLL_AREA_2_RECENT_sd = rep(0.2,3)
    )
  }
}


############################## RUN VPA-2BOX ################################

# Function to run VPA-2BOX
VPA2BOX.FUN <- function(working.dir, ctrl.file) {
  
  # set working directory (will change for each bootstrap iteration)
  setwd(working.dir)
  
  ctrl <- ctrl.file # control file name from Control File function
  
  # specify system command (name of VPA-2BOX executable + name of contol file, which
  # will change for each bootstrap iteration)
  cmd <- (paste("vpa-2box.exe", ctrl, sep = " "))
  
  # shell function to run command; waits for command to finish before
  # continuing further in code
  shell(cmd, wait = TRUE)
}


######################## RETURN RESULTS OF VPA-2BOX ############################

# Creates function for reading in VPA-2BOX results files. Arguments: east = TRUE (east)
# or FALSE (west); run.num = run number; t = run number (for loop); directoryE = directory
# for saving East run files; directoryW = directory for saving West run files.
RESULTS.FUN <- function(east, run.num, t, directoryE, directoryW) {
  
  # name of storage object will change for each bootstrap iteration:
  # pull out fishing mortality rate matrix
  if(east==TRUE) {
    
    print("Saved eastern stock VPA results!")
    Results <<- as.data.frame(read.table(file = result_file_name,
                                         fill = T, col.names = 1:max(count.fields(
                                           result_file_name
                                         ))))
    
    # NAA from VPA
    NAA.res <- as.matrix(Results[79:121,2:11], nrow = (nyr+1), ncol = nageE)
    NAA.res <- apply(NAA.res, c(1,2), as.numeric)
    assign("NAA.res", NAA.res, .GlobalEnv)
    NAA.filenm.1 <- paste(directoryE, run.num, "/E_NAA_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(NAA.res, file = NAA.filenm.1)
    # Plot N-at-age 9
    NAA9.filenm <- paste(directoryE, run.num, "/E_NAA9_VPA_plotrun", run.num, ".jpg", sep = "")
    jpeg(filename = NAA9.filenm, width = 480, height = 500)
    plot(head(yrs,1):tail(yrs,1), NAA.res[1:42,9], type = "l", main = "Eastern Age-9 Abundance", xlab = "year",
         ylab = "abundance (numbers)", xaxs="i", yaxs = "i")
    dev.off()
    
    # SSB from VPA
    SSB.res <- as.matrix(Results[175:216, 2], nrow = nyr, ncol = 1) # Save SSB results
    SSB.res <- apply(SSB.res, c(1,2), as.numeric) # Convert SSB values to numeric
    assign("SSB.res", SSB.res, .GlobalEnv) # Save SSB results to global environment
    SSB.filenm.1 <- paste(directoryE, run.num, "/E_SSB_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(SSB.res, file = SSB.filenm.1)
    SSB.filenm.2 <- paste(directoryE, run.num, "/E_SSB_VPA_plotrun", run.num, ".jpg", sep = "")
    jpeg(filename = SSB.filenm.2, width = 480, height = 500)
    plot(head(yrs,1):tail(yrs,1), SSB.res, ylim = c(0,800000), pch=19, type="b", main = "Eastern Stock SSB (VPA)", 
         xlab = "year", ylab = "SSB (mt)", xaxs="i", yaxs = "i")
    dev.off()
    
    # Calculate SSB relative bias (VPA results vs. OM).
    SSB.OM <- as.matrix(T_Essb[1:nyr,1], nrow = nyr, ncol = 1) #eastern stock SSB
    SSB.rel.bias <- matrix(NA, nrow = nyr, ncol = 1)
    for (i in 1:nyr) {
      SSB.rel.bias[i,1] <- (SSB.res[i,1] - SSB.OM[i,1])/SSB.OM[i,1]
    }
    assign("SSB.rel.bias", SSB.rel.bias, .GlobalEnv)
    
    # R from VPA
    R.res <- as.matrix(Results[175:216, 3], nrow = nyr, ncol = 1)  # save R results
    R.res <- apply(R.res, c(1,2), as.numeric)
    assign("R.res", R.res, .GlobalEnv)
    R.filenm.1 <- paste(directoryE, run.num, "/E_R_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(R.res, file = R.filenm.1)
    R.filenm.2 <- paste(directoryE, run.num, "/E_R_VPA_plotrun", run.num, ".jpg", sep = "")
    jpeg(filename = R.filenm.2, width = 480, height = 500)
    plot(head(yrs,1):tail(yrs,1), R.res, ylim = c(0,15000000), pch = 19, type = "b", ylab = "Recruits (n)",
         xlab = "Years", main = "Eastern Stock Recruits (VPA)", xaxs="i", yaxs = "i")
    dev.off()
    # Calculate R relative bias (VPA results vs. OM).
    R.OM <- as.matrix(naa[1:nyr,1,1,7,1], nrow = nyr, ncol = 1)
    R.rel.bias <- matrix(NA, nrow = nyr, ncol = 1)
    for (i in 1:nyr) {
      R.rel.bias[i,1] <- (R.res[i,1] - R.OM[i,1])/R.OM[i,1]
    }
    assign("R.rel.bias", R.rel.bias, .GlobalEnv)
    
    # Pull out F results from VPA and save data & plot to specified Run directory
    F.res <- as.matrix(Results[32:73, 2:11], nrow = nyr, ncol = nageE)  # fishing mortality results - will need to change these dimensions if number of years or ages changes
    F.res <- apply(F.res, c(1,2), as.numeric)
    assign("F.res", F.res, .GlobalEnv) # save F results to global environment
    F.filenm.1 <- paste(directoryE, run.num, "/E_F_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(F.res, file = F.filenm.1)
    F.filenm.2 <- paste(directoryE, run.num, "/E_F_VPA_plotrun", run.num, ".jpg", sep = "")
    jpeg(filename = F.filenm.2, width = 480, height = 500)
    matplot(head(yrs,1):tail(yrs,1), F.res[1:nyr,1:nageE], ylim = c(0,0.6), type = "l", ylab = "Fishing mortality rate",
            xlab = "Year", main = "Eastern Stock F (VPA)", xaxs="i", yaxs = "i")
    dev.off()
    
    # Harvest ratio
    Sel <- matrix(NA, nrow = nyr, ncol = nageE)  #selectivity matrix
    for (y in 1:nyr)
      for (a in 1:nageE) {
        Sel[y,a] <- F.res[y,a]/max(F.res[y,])  #calculate selectivity from F-at-age
      }
    Cat <- as.matrix(Results[127:168,2:11], nrow = nyr, ncol = nageE)  #catch data
    Cat <- apply(Cat, c(1,2), as.numeric)
    Hratio <- rep(NA, nyr)
    for (y in 1:nyr) {
      Hratio[y] <- sum(Cat[y,1:nageE])/sum(NAA.res[y,1:nageE]*Sel[y,1:nageE])  #calculate harvest ratio 
    }
    assign("Hratio", Hratio, .GlobalEnv)
    HR.filenm.1 <- paste(directoryE, run.num, "/E_HR_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(Hratio, file = HR.filenm.1)
    HR.filenm.2 <- paste(directoryE, run.num, "/E_HR_VPA_resultsplot", run.num, ".jpg", sep = "")
    jpeg(filename = HR.filenm.2, width = 480, height = 500)
    plot(head(yrs,1):tail(yrs,1), Hratio, ylim = c(0,3), pch = 19, type = "b", ylab = "Harvest ratio", 
         xlab = "Years", main = "Eastern Harvest Ratio", xaxs="i", yaxs = "i")
    dev.off()
    
    # Save output statistics
    obj.fun <- Results[8,5] # total objective function
    assign("obj.fun", obj.fun, .GlobalEnv)
    chisq <- Results[15,4] # chi-square discrepancy
    assign("chisq", chisq, .GlobalEnv)
    logLikelihood <- Results[16,3] # Loglikelihood 
    assign("logLikelihood", logLikelihood, .GlobalEnv)
    deviance <- Results[16,5] # deviance
    assign("deviance", deviance, .GlobalEnv)
    
  } else {
    
    print("Saved western stock VPA results!")
    Results <<- as.data.frame(read.table(file = result_file_name,
                                         fill = T, col.names = 1:max(count.fields(
                                           result_file_name
                                         ))))
    
    # NAA from VPA
    NAA.res <- as.matrix(Results[79:121, 2:17], nrow = (nyr+1), ncol = nageW)
    NAA.res <- apply(NAA.res, c(1,2), as.numeric)
    assign("NAA.res", NAA.res, .GlobalEnv)
    NAA.filenm.1 <- paste(directoryW, run.num, "/W_NAA_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(NAA.res, file = NAA.filenm.1)
    
    # R from VPA
    R.res <- as.matrix(Results[175:216, 3], nrow = nyr, ncol = 1) # save R results
    R.res <- apply(R.res, c(1,2), as.numeric)
    assign("R.res", R.res, .GlobalEnv)
    R.filenm.1 <- paste(directoryW, run.num, "/W_R_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(R.res, file = R.filenm.1)
    R.filenm.2 <- paste(directoryW, run.num, "/W_R_VPA_plotrun", run.num, ".jpg", sep = "")
    jpeg(filename = R.filenm.2, width = 480, height = 500)
    plot(head(yrs,1):tail(yrs,1), R.res, ylim=c(0, 6000000), pch=19, type='b', ylab="Recruits (n)",  
         xlab="Years", main="Western Stock Recruits", xaxs="i", yaxs = "i")
    dev.off()
    # Calculate R relative bias (VPA results vs. OM)
    R.OM <- as.matrix(naa[1:nyr,1,1,1,2], nrow = nyr, ncol = 1)
    R.rel.bias <- matrix(NA, nrow = nyr, ncol = 1)
    for (i in 1:nyr) {
      R.rel.bias[i,1] <- (R.res[i,1] - R.OM[i,1])/R.OM[i,1]
    }
    assign("R.rel.bias", R.rel.bias, .GlobalEnv)
    
    # SSB from VPA
    SSB.res <- as.matrix(Results[175:216, 2], nrow = nyr, ncol = 1) # save SSB results
    SSB.res <- apply(SSB.res, c(1,2), as.numeric) # Convert SSB values to numeric
    assign("SSB.res", SSB.res, .GlobalEnv) # Save SSB results to global environment
    SSB.filenm.1 <- paste(directoryW, run.num, "/W_SSB_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(SSB.res, file = SSB.filenm.1)
    SSB.filenm.2 <- paste(directoryW, run.num, "/W_SSB_VPA_plotrun", run.num, ".jpg", sep = "")
    jpeg(filename = SSB.filenm.2, width = 480, height = 500)
    plot(head(yrs,1):tail(yrs,1), SSB.res, ylim = c(0,250000), pch=19, type="b", main = "Western Stock SSB (VPA)", 
         xlab = "year", ylab = "SSB (mt)", xaxs="i", yaxs = "i")
    dev.off()
    # Calculate SSB relative bias (VPA results vs. OM: new-old/old)
    SSB.OM <- as.matrix(T_Wssb[1:nyr,1], nrow = nyr, ncol = 1) #western stock SSB
    SSB.rel.bias <- matrix(NA, nrow = nyr, ncol = 1)
    for (i in 1:nyr) {
      SSB.rel.bias[i,1] <- (SSB.res[i,1] - SSB.OM[i,1])/SSB.OM[i,1]
    }
    assign("SSB.rel.bias", SSB.rel.bias, .GlobalEnv)
    
    # F from VPA
    F.res <- as.matrix(Results[32:73, 2:17], nrow = nyr, ncol = nageW) # fishing mortality results - will need to change these dimensions if number of years or ages changes
    F.res <- apply(F.res, c(1,2), as.numeric)
    assign("F.res", F.res, .GlobalEnv) # save F results to global environment
    F.filenm.1 <- paste(directoryW, run.num, "/W_F_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(F.res, file = F.filenm.1)
    F.filenm.2 <- paste(directoryW, run.num, "/W_F_VPA_plotrun", run.num, ".jpg", sep = "")
    jpeg(filename = F.filenm.2, width = 480, height = 500)
    matplot(head(yrs,1):tail(yrs,1), F.res[1:nyr,1:nageW], ylim = c(0,0.6), type = "l", ylab = "Fishing mortality rate", 
            xlab = "Year", main = "Western Stock F (VPA)", xaxs="i", yaxs = "i")
    dev.off()
    
    # Harvest ratio
    Sel <- matrix(NA, nrow = nyr, ncol = nageW)  #selectivity matrix
    for (y in 1:nyr)
      for (a in 1:nageW) {
        Sel[y,a] <- F.res[y,a]/max(F.res[y,])  #calculate selectivity from F-at-age
      }
    Cat <- as.matrix(Results[127:168,2:17], nrow = nyr, ncol = nageW)  #catch data
    Cat <- apply(Cat, c(1,2), as.numeric)
    Hratio <- rep(NA,nyr)
    for (y in 1:nyr) {
      Hratio[y] <- sum(Cat[y,1:nageW])/sum(NAA.res[y,1:nageW]*Sel[y,1:nageW])  #calculate harvest ratio 
    }
    assign("Hratio", Hratio, .GlobalEnv)
    HR.filenm.1 <- paste(directoryW, run.num, "/W_HR_VPA_resultsrun", run.num, ".csv", sep = "")
    write.csv(Hratio, file = HR.filenm.1)
    HR.filenm.2 <- paste(directoryW, run.num, "/W_HR_VPA_resultsplot", run.num, ".jpg", sep = "")
    jpeg(filename = HR.filenm.2, width = 480, height = 500)
    plot(head(yrs,1):tail(yrs,1), Hratio, ylim = c(0,3), pch = 19, type = "b", ylab = "Harvest ratio", 
         xlab = "Years", main = "Western Harvest Ratio", xaxs="i", yaxs = "i")
    dev.off()
    
    # Save output statistics
    obj.fun <- Results[8,5] # total objective function
    assign("obj.fun", obj.fun, .GlobalEnv)
    chisq <- Results[15,4] # chi-square discrepancy
    assign("chisq", chisq, .GlobalEnv)
    logLikelihood <- Results[16,3]
    assign("logLikelihood", logLikelihood, .GlobalEnv)
    deviance <- Results[16,5]
    assign("deviance", deviance, .GlobalEnv)
  }
}


########################### RUNNING SIMULATIONS #############################

# Create directories ("Run #X") for storing files for each simulation within 
# each East & West directory. Number of directories is equal to the number of 
# realizations. Run 0 is intended as the deterministic run; Runs 1-100+ are 
# realizations of pseudodata with measurement error. (uncomment to use)
# stock.set = "East"
# dir = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/"
# for (r in 0:50) {
#   path <- paste(dir, stock.set, "/Run", r, sep = "")
#   print(path)
#   dir.create(path = path, showWarnings = TRUE)
# }

# Copy VPA-2BOX executable file to all directories created above. 
# MAKE SURE YOU'RE USING THE LEGITIMATE, MOST RECENT VERSION DIRECTLY FROM 
# MATT LAURETTA OR CLAY PORCH AND NOT A CORRUPTED COPY. (uncomment to use)
# for (r in 0:50){
#   from.path <- paste(dir, "Misc/vpa-2box.exe", sep = "")
#   print(from.path)
#   to.path <- paste(dir, stock.set, "/Run", r, sep="")
#   print(to.path)
#   file.copy(from = from.path, to = to.path)
# }

# SIMS.FUN: Creates function for running VPA simulated realizations. This function encompasses
# sub-functions (created further down in script file) used to generate 
# pseudodata, add error, generate input files, run VPA-2BOX, and read in results. 
# Make adjustments to arguments of sub-functions.
SIMS.FUN <- function(decide, error, run.num1, directory) {
  
  CPUE.FUN(east = decide, cv.file = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/R Code + Inputs/CVs.csv")
  
  CAA.FUN(east = decide)
  
  CTRLFILE.FUN(east = decide, run.num = run.num1)
  
  #reads in CAA files that DO NOT HAVE observation error (deterministic)
  DATAFILE.FUN(east = decide, run.num = run.num1, file.caaE = "Ecaa.csv", file.cpue.specE = 'AbunIndexSpec_E.csv',
               file.cpueE = 'Ecpues.csv', file.pcaaE = 'Epcaa.csv', file.waaE = 'Ewaa.csv',
               file.caaW = 'Wcaa.csv', file.cpue.specW = 'AbunIndexSpec_W.csv',
               file.cpueW = 'Wcpues.csv', file.pcaaW = 'Wpcaa.csv', file.waaW = 'Wwaa.csv')
  
  PARAMFILE.FUN(east = decide, run.num = run.num1)
  
  working.dir1 <- paste(directory, "Run", run.num1, sep="")
  
  VPA2BOX.FUN(working.dir = working.dir1,
              ctrl.file = control_file)
  
  RESULTS.FUN(east = decide, run.num = run.num1,
              directoryE = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/East/Run",
              directoryW = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/West/Run")
  
}

# MASTER.FUN: Run X# simulated realizations using SIMS.FUN and saves & plots results.
# "stock.set" argument decides which stock, east (TRUE) or west (FALSE). Need to set SIMS.FUN arguments: 
# decide = TRUE (east) or FALSE (west); error = TRUE (add error) or FALSE (don't add error [deterministic]); 
# run.num1 = run number determined by for loop (nsims); directory = which stock directory we're currently running VPA-2BOX for.

MASTER.FUN <- function(nsims, yrs, stock.set, dir) {
  
  # Input information regarding fleets for running simulations:
  E.indices <<- c("MOR_SP_TP", "MOR_POR_TP", "JPN_LL_EastMed", "JPN_LL1_NEA", "JPN_LL2_NEA",
                  "SP_BB1", "SP_BB2", "FR_AER1", "FR_AER2", "WMED_LARV")  #specify the names of the indices
  W.indices <<- c("CAN_Combined_RR", "CAN_GSL_Acoustic", "US_RR<145", "US_RR_66_114", "US_RR_115_144",
                  "US_RR_145_177", "US_RR>195", "US_RR>195_COMB", "US_RR>177", "JLL_AREA_2_(WEST)", 
                  "JLL_AREA_3_(31+32)", "JLL_AREAS_17+18", "LARVAL_ZERO_INFLATED", "GOM_PLL_1-6",
                  "JLL_GOM", "TAGGING", "JLL_AREA_2_RECENT")
  E.ind.units <<- c(rep("N",5),"B", "B", "N", "N", "B")  #specify which units (Numbers or Biomass) the indices (above) are tuned to (in VPA data file)
  W.ind.units <<- c(rep("N",12),"B",rep("N",4))
  E.ind.units.2 <<- cbind(E.indices,E.ind.units)  #table of index names and their units
  W.ind.units.2 <<- cbind(W.indices,W.ind.units)
  yrs <<- yrs
  
  if (stock.set=="East") {
    decide.input <- TRUE
  } else if (stock.set=="West") {
    decide.input <- FALSE
  } else {
    print("No stock set")
  }
  
  for (t in 0) {
    dir.paste <- paste(dir, stock.set, "/", sep = "")
    SIMS.FUN(decide = decide.input, error = FALSE, run.num1 = t, directory = dir.paste)  #runs simulations
  }
}


# RUN SIMULATIONS & READ IN RESULTS

MASTER.FUN(nsims = 1, yrs = 1974:2015, stock.set = "East", dir = "C:/Users/mmorse1/OneDrive - UMASS Dartmouth/Research/Simulations_2/")


################################ END #######################################

