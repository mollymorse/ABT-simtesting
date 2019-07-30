

load("C:/Users/mmorse1/Documents/ABT-simtesting/Rscripts/Ben/trans_var_2016sims_2018-07-29.Rdata")

west_juv <- tenv$w_small
west_ad  <- tenv$w_large
east_all <- tenv$east
nrep <- 100

#MEAN
boxmean_wjuv = apply(west_juv, c(1,2,3), mean, na.rm=T) 
# boxmean_wjuv2= alply(apply(west_juv, c(1,2,3), mean, na.rm=T), 3) #test to make sure Ben's inital code producing a list gives same result
boxmean_wad  = apply(west_ad,  c(1,2,3), mean, na.rm=T) 
boxmean_eall = apply(east_all, c(1,2,3), mean, na.rm=T)

# convert to arrays with ages and shift seasons for OM (1 = winter -> spring)
wjuvmean <- array(c(boxmean_wjuv[, , 2], boxmean_wjuv[, , 3], boxmean_wjuv[, , 4], boxmean_wjuv[, , 1]), 
                  dim = c(7, 7, 4, 8),
                  dimnames = list(start = 1:7, end = 1:7, quarter = 1:4, age = 1:8))
wadmean  <- array(c(boxmean_wad[, ,  2], boxmean_wad[, ,  3], boxmean_wad[, ,  4], boxmean_wad[, ,  1]), 
                  dim = c(7, 7, 4, 21), 
                  dimnames = list(start = 1:7, end = 1:7, quarter = 1:4, age = 9:29))
wmean    <- abind(wjuvmean, wadmean, along = 4)
emean    <- array(c(boxmean_eall[, , 2], boxmean_eall[, , 3], boxmean_eall[, , 4], boxmean_eall[, , 1]), 
                  dim = c(7, 7, 4, 29), 
                  dimnames = list(start = 1:7, end = 1:7, quarter = 1:4, age = 1:29))

# Mean movement matrix to export at csv and use in OM
mean_move <- abind(emean, wmean, along = 5)  
dimnames(mean_move) <- list(start = 1:7, end = 1:7, season = 1:4, age = 1:29, unit = 1:2)
# setwd("C:/Users/mmorse1/Documents/Simulations_lomov/R Code + Inputs")
# write.csv(mean_move, "MoveMatrix_mean.csv")

seasons2= c('spring','summer','fall','winter')
ex7 = expand.grid(1:7, 1:7)
par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, mean_move[,,x,10,2], col = terrain.colors(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
  text(ex7[,1], ex7[,2], paste0(round(t(mean_move[,,x,10,2]),2)))#,"\n" ,"(",round(t(boxq.low4[[x]]), 2), " - ", round(t(boxq.high4[[x]]), 2), ")"), font = 2)
  grid(7, 7, col = 'grey50')
  axis(1, at = 1:7, labels=paste0('e',1:7), cex.axis = 1.3, font.axis = 2)
  axis(2, at = 1:7, labels=paste0('s',1:7), cex.axis = 1.3, font.axis = 2)
  box()
  segments(3.5, y0 = 3.5, x1 = 7.5, y1 = 3.5, lwd = 2, col = 2, lty = 2)
  segments(3.5, y0 = 3.5, x1 = 3.5, y1 = 7.5, lwd = 2, col = 2, lty = 2)
  title(paste0(seasons2[x], ' mean movement'))
}
)





#### Annual Regional Residence ####

ball = array(rep(NA, 7*7*nrep), dim = c(7, 7, nrep), dimnames = list(paste0('start', 1:7), paste0('end',1:7), paste0('run',1:nrep)))

# use matrix multiplication over all 4 quarters for each replicate
# west juveniles
for(i in 1:dim(west_juv)[4]){
  b2 = west_juv[,,,i]
  ball[,,i] = b2[,,1]%*%b2[,,2]%*%b2[,,3]%*%b2[,,4]
}
west_juvenile = apply(ball, 3, function(x) sum(x[1:3, 1:3])) 

# west adults
for(i in 1:dim(west_ad)[4]){
  b2 = west_ad[,,,i]
  ball[,,i] = b2[,,1]%*%b2[,,2]%*%b2[,,3]%*%b2[,,4]
}
west_adults   = apply(ball, 3, function(x) sum(x[1:3, 1:3])) 

# east all
for(i in 1:dim(east_all)[4]){
  b2 = east_all[,,,i]
  ball[,,i] = b2[,,1]%*%b2[,,2]%*%b2[,,3]%*%b2[,,4]
}
east_overall  = apply(ball, 3, function(x) sum(x[4:7, 4:7]))





#### 75th Percentile Residence ####

# calculate low movement (high residence) quantile (75th):
# takes all replicates that are in the top 25th percentile residency
low_wjuv  = which(west_juvenile >= quantile(west_juvenile, .75)) # low movement out of the west
# hi_wjuv = which(west_juvenile <= quantile(west_juvenile, .25)) # high movement out of the west

low_wad  = which(west_adults >= quantile(west_adults, .75)) # low movement out of the west
# hi_wad = which(west_adults <= quantile(west_adults, .25)) # high movement out of the west

low_e = which(east_overall >= quantile(east_overall, .75)) # low movement out of the east
# hi_e = which(east_overall <= quantile(east_overall, .25)) # high movement out of the east


# save low movement replicates
# takes a random sample (the first sequential replicate) of the top 25th percentile residency
# west juvenile
wjlow_ex  = west_juv[,,,low_wjuv[2]]
# wjhigh_ex = west_juv[,,,hi_wjuv[2]]

# west adult
walow_ex  = west_ad[,,,low_wad[2]]
# wahigh_ex = west_ad[,,,hi_wad[2]]

# east all
elow_ex  = east_all[,,,low_e[2]]
# ehigh_ex = east_all[,,,hi_e[2]]





#### Build new OM movement matrices ####

# West: use juv for ages 1-8, adult for ages 9-29; shift seasons to match OM (1 = winter -> spring)
west_juv_target_2 <- array(c(wjlow_ex[, , 2], wjlow_ex[, , 3], wjlow_ex[, , 4], wjlow_ex[, , 1]),  
                           dim = c(7, 7, 4, 8), 
                           dimnames = list(start = 1:7, end = 1:7, quarter = 1:4, age = 1:8))
west_ad_target_2  <- array(c(walow_ex[, , 2], walow_ex[, , 3], walow_ex[, , 4], walow_ex[, , 1]), 
                           dim = c(7, 7, 4, 21), 
                           dimnames = list(start = 1:7, end = 1:7, quarter = 1:4, age = 9:29))
west_new <- abind(west_juv_target_2, west_ad_target_2, along = 4)

# East: all ages same
east_new <- array(c(elow_ex[, , 2], elow_ex[, , 3], elow_ex[, , 4], elow_ex[, , 1]), 
                  dim = c(7, 7, 4, 29), 
                  dimnames = list(start = 1:7, end = 1:7, quarter = 1:4, age = 1:29))




# New movement matrix to export at csv and use in OM
new_move <- abind(east_new, west_new, along = 5)  
dimnames(new_move) <- list(start = 1:7, end = 1:7, season = 1:4, age = 1:29, unit = 1:2)

# write to file
setwd("C:/Users/mmorse1/Documents/Simulations_lomov/R Code + Inputs")
write.csv(new_move, "MoveMatrix_low.csv")

# plot
par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, west_new[,,x,8], col = terrain.colors(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
  text(ex7[,1], ex7[,2], paste0(round(t(west_new[,,x,8]),2)))#,"\n" ,"(",round(t(boxq.low4[[x]]), 2), " - ", round(t(boxq.high4[[x]]), 2), ")"), font = 2)
  grid(7, 7, col = 'grey50')
  axis(1, at = 1:7, labels=paste0('e',1:7), cex.axis = 1.3, font.axis = 2)
  axis(2, at = 1:7, labels=paste0('s',1:7), cex.axis = 1.3, font.axis = 2)
  box()
  segments(3.5, y0 = 3.5, x1 = 7.5, y1 = 3.5, lwd = 2, col = 2, lty = 2)
  segments(3.5, y0 = 3.5, x1 = 3.5, y1 = 7.5, lwd = 2, col = 2, lty = 2)
  title(paste0(seasons2[x], ' low movement'))
}
)


