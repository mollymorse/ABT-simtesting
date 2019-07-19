#### Wrangle tuna transitions from lists into arrays ####

# The data objects are list objects with lengths = 1000, 166, 167, respectively
# Each list item is a list with length = 4, one for each season
# Each of these is a 7x7 transition matrix

# To get summaries it's easier to transform these lists to 4D arrays

library(plyr)


# Load these R data objects from Ben into the environment:
# alltrans_3D_juvenile_WABT_.Rdata
# alltrans_3D_nojuvs.Rdata

# list objects to use:
# allboxtrans
# west_3D_trans_large
# east_3D_trans_all


this_list <- allboxtrans  # CHANGE HERE: which list object to transform
list_len  <- length(this_list)

# make the array for the matrices
boxar = array(rep(NA, 7*7*4*list_len), dim = c(7, 7, 4, list_len), dimnames = list(paste0('start', 1:7), paste0('end',1:7), paste0('season', 1:4), paste0('run',1:list_len)))

for(i in 1:list_len){  # use nreps here
  for(j in 1:4){
    boxar[,,j,i] = this_list[[i]][[j]]
  }
}

# Summaries 
# boxmean = alply(apply(boxar, c(1,2,3), mean, na.rm=T), 3)  # here's what we've been using so far
# boxmedian = alply(apply(boxar, c(1,2,3), median, na.rm=T), 3)
# boxsd = alply(apply(boxar, c(1,2,3), sd, na.rm=T), 3)
# boxq.low = alply(apply(boxar, c(1,2,3), function(x) quantile(x, c(.025), na.rm=T)), 3)  # here's your quantiles..
# boxq.high = alply(apply(boxar, c(1,2,3), function(x) quantile(x, c(.975), na.rm=T)), 3)

# Save to respective arrays (all X# replicates)
west_juv <- boxar
# west_ad  <- boxar
# east_all <- boxar




#### Calculate annual regional residence (R) ####

# Annual, regional residence (R) derived from the sum of quarterly matrix elements that involve staying in the region, 
# multiplied over all four quarters
# X(i->j,t,r) movement coefficient from zone i to zone j in season t for fish from region r (eastern or western)
# R(west)=sum(X1-3->1-3,q1,west) * sum(X1-3->1-3,q2,west) * sum(X1-3->1-3,q3,west)  * sum(X1-3->1-3,q4,west)
# R(east)=sum(X4-7->4-7,q1,east) * sum(X4-7->4-7,q2,east) * sum(X4-7->4-7,q3,east)  * sum(X4-7->4-7,q4,east)

# RW_juv <- rep(NA, dim(west_juv)[4]) #western juveniles residence (one value per replicate)
# RW_ad  <- rep(NA, dim(west_ad)[4])  #western adults residence (one value per replicate)
# RE_all <- rep(NA, dim(east_all)[4]) #eastern all ages residence (one value per replicate)
# 
# # calculate residence
# for (i in 1:dim(west_juv)[4]) {
#   RW_juv[i] <- sum(west_juv[1:3, 1:3, 1, i]) * sum(west_juv[1:3, 1:3, 2, i]) * sum(west_juv[1:3, 1:3, 3, i]) * sum(west_juv[1:3, 1:3, 4, i])
# }
# for (i in 1:dim(west_ad)[4])  {
#   RW_ad[i]  <- sum(west_ad[1:3, 1:3, 1, i])  * sum(west_ad[1:3, 1:3, 2, i])  * sum(west_ad[1:3, 1:3, 3, i])  * sum(west_ad[1:3, 1:3, 4, i])
# }
# for (i in 1:dim(east_all)[4]) {
#   RE_all[i] <- sum(east_all[4:7, 4:7, 1, i]) * sum(east_all[4:7, 4:7, 2, i]) * sum(east_all[4:7, 4:7, 3, i]) * sum(east_all[4:7, 4:7, 4, i])
# }




ball.e = array(rep(NA, 7*7*list_len), dim = c(7, 7, list_len), dimnames = list(paste0('start', 1:7), paste0('end',1:7), paste0('run',1:list_len)))

# use matrix multiplication over all 4 quarters for each replicate
# west juveniles
for(i in 1:dim(west_juv)[4]){
  b2 = west_juv[,,,i]
  ball.e[,,i] = b2[,,1]%*%b2[,,2]%*%b2[,,3]%*%b2[,,4]
}

# west adults
for(i in 1:dim(west_ad)[4]){
  b2 = west_ad[,,,i]
  ball.e[,,i] = b2[,,1]%*%b2[,,2]%*%b2[,,3]%*%b2[,,4]
}

# east all
for(i in 1:dim(east_all)[4]){
  b2 = east_all[,,,i]
  ball.e[,,i] = b2[,,1]%*%b2[,,2]%*%b2[,,3]%*%b2[,,4]
}



# residence: sum over "home" zones for each replicate
west_juvenile = apply(ball.e, 3, function(x) sum(x[1:3, 1:3])) #west juv
west_adults   = apply(ball.e, 3, function(x) sum(x[1:3, 1:3])) #west adult
east_overall  = apply(ball.e, 3, function(x) sum(x[4:7, 4:7])) #east



# #### 75th percentile of residence ####
# 
# #west juvenile
# # boxplot(RW_juv)
# quant_tar <- quantile(RW_juv, 0.75)  #75th percentile
# target <- which(abs(RW_juv - quant_tar) == min(abs(RW_juv - quant_tar))) #realization of the 75th percentile
# RW_juv[which(abs(RW_juv - quant_tar) == min(abs(RW_juv - quant_tar)))] #R value of the 75th percentile
# west_juv_target <- west_juv[ , , , target]  #saves realization matrix of the 75th percentile
# 
# #west adult
# # boxplot(RW_ad)
# quant_tar <- quantile(RW_ad, 0.75)  #75th percentile
# target <- which(abs(RW_ad - quant_tar) == min(abs(RW_ad - quant_tar))) #realization of the 75th percentile
# RW_ad[which(abs(RW_ad - quant_tar) == min(abs(RW_ad - quant_tar)))] #R value of the 75th percentile
# west_ad_target <- west_ad[ , , , target]  #saves realization matrix of the 75th percentile
# 
# #east all
# # boxplot(RE_all)
# quant_tar <- quantile(RE_all, 0.75)  #75th percentile
# target <- which(abs(RE_all - quant_tar) == min(abs(RE_all - quant_tar))) #realization of the 75th percentile
# RE_all[which(abs(RE_all - quant_tar) == min(abs(RE_all - quant_tar)))] #R value of the 75th percentile
# east_all_target <- east_all[ , , , target]  #saves realization matrix of the 75th percentile


# calculate low movement (high residence) quantile (75th) and high movement (low residence) quantile (25th)
# takes all replicates that are in the top 25th percentile residency (& revised to also take all replicates
# in the bottom 25th percentile residency (top movement))
lowidx = which(west_juvenile >= quantile(west_juvenile, .75))# low movement out of the west
highidx = which(west_juvenile <= quantile(west_juvenile, .25)) # high movement out of the west

lowidx = which(west_adults >= quantile(west_adults, .75))# low movement out of the west
highidx = which(west_adults <= quantile(west_adults, .25)) # high movement out of the west

lowidx = which(east_overall >= quantile(east_overall, .75))# low movement out of the east
highidx = which(east_overall <= quantile(east_overall, .25)) # high movement out of the east


# save low (& high) movement replicates
# takes a random sample (the first sequential replicate) of the top 25th percentile residency (and bottom 25th for high movement)
# west juvenile
wjlow_ex = west_juv[,,,lowidx[2]]
wjhigh_ex = west_juv[,,,highidx[2]]

# west adult
walow_ex = west_ad[,,,lowidx[2]]
wahigh_ex = west_ad[,,,highidx[2]]

# east all
elow_ex = east_all[,,,lowidx[2]]
ehigh_ex = east_all[,,,highidx[2]]


# plot
seasons = c('Winter','Spring','Summer','Fall')

# low
par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, wjlow_ex[,, x], col = terrain.colors(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
  text(ex7[,1], ex7[,2], paste0(round(t(elow_ex[,, x]),2)))#,"\n" ,"(",round(t(boxq.low4[[x]]), 2), " - ", round(t(boxq.high4[[x]]), 2), ")"), font = 2)
  grid(7, 7, col = 'grey50')
  axis(1, at = 1:7, labels=paste0('e',1:7), cex.axis = 1.3, font.axis = 2)
  axis(2, at = 1:7, labels=paste0('s',1:7), cex.axis = 1.3, font.axis = 2)
  box()
  segments(3.5, y0 = 3.5, x1 = 7.5, y1 = 3.5, lwd = 2, col = 2, lty = 2)
  segments(3.5, y0 = 3.5, x1 = 3.5, y1 = 7.5, lwd = 2, col = 2, lty = 2)
  title(paste0(seasons[x], ' low movement example'))
}
)

# high
par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, wjhigh_ex[,, x], col = terrain.colors(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
  text(ex7[,1], ex7[,2], paste0(round(t(ehigh_ex[,, x]),2)))#,"\n" ,"(",round(t(boxq.low4[[x]]), 2), " - ", round(t(boxq.high4[[x]]), 2), ")"), font = 2)
  grid(7, 7, col = 'grey50')
  axis(1, at = 1:7, labels=paste0('e',1:7), cex.axis = 1.3, font.axis = 2)
  axis(2, at = 1:7, labels=paste0('s',1:7), cex.axis = 1.3, font.axis = 2)
  box()
  segments(3.5, y0 = 3.5, x1 = 7.5, y1 = 3.5, lwd = 2, col = 2, lty = 2)
  segments(3.5, y0 = 3.5, x1 = 3.5, y1 = 7.5, lwd = 2, col = 2, lty = 2)
  title(paste0(seasons[x], ' high movement example'))
}
)




#### Build new OM movement matrices ####

library(abind)

# low movement matrices should be used for export 

# West: use juv for ages 1-8, adult for ages 9-29
west_juv_target_2 <- array(rep(wjhigh_ex, 8), dim = c(7, 7, 4, 8), 
                           dimnames = list(paste0('start', 1:7), paste0('end', 1:7), paste0('season', 1:4), paste0('age', 1:8)))
west_ad_target_2  <- array(rep(wahigh_ex,  8), dim = c(7, 7, 4, 21), 
                           dimnames = list(paste0('start', 1:7), paste0('end', 1:7), paste0('season', 1:4), paste0('age', 9:29)))

west_new <- abind(west_juv_target_2, west_ad_target_2, along = 4)



# East: all ages same
east_new <- array(rep(ehigh_ex, 29), dim = c(7, 7, 4, 29), 
                           dimnames = list(paste0('start', 1:7), paste0('end',1:7), paste0('season', 1:4), paste0('age', 1:29)))


# New movement matrix to export at csv and use in OM
new_move <- abind(east_new, west_new, along = 5)  
dimnames(new_move) <- list(start = 1:7, end = 1:7, season = 1:4, age = 1:29, unit = 1:2)

# write to file
setwd(paste0("C:/Users/mmorse1/Documents/Simulations_lomov/R Code + Inputs"))
write.csv(new_move, "MoveMatrix.csv")

# new_move <- as.matrix(read.csv("MoveMatrix.csv", header = TRUE))
# new_move <- array(new_move[, -1], c(7, 7, 4, 29, 2))

# compare to original movematrix (from base case OM simulation)
old_move <- as.matrix(read.csv("C:/Users/mmorse1/Documents/Simulations_2/R Code + Inputs/MoveMatrix.csv"), header = T)
old_move <- array(old_move[1:7,2:1624],c(7,7,4,29,2),dimnames=list(zone=1:7,zone=1:7,quarter=1:4,age=1:29,unit=1:2))


