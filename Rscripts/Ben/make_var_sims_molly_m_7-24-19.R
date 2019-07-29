## Generate variance from 2016 movement matrices for Molly M
# use files on UMASSD OneDrive:
#    abft_SIMS_WABFT_gt185_wNOAA_1000_v2.Rdata
#    abft_SIMS_WABFT_lt185_1000.Rdata
#    abft_SIMS_GBYP_ALL_1000_V2.Rdata
#
# 7/24/19
# BGaluardi

# install_github('galuardi/SatTagSim')
# library(SatTagSim)

#----------------------------------------------------------------------------------------#
# West Large
load('C:/Users/benjamin.galuardi/Downloads/OneDrive_1_7-23-2019/abft_SIMS_WABFT_gt185_wNOAA_1000_v2.Rdata')


# msims = 84 # simulations to be started in each month
npmon = 30 # number of simulation steps per month.
# nyears = 2 # number of years to simulate for each track
# nreps = 1000 # number of replicates for variance calculation

# set up number of replicates to obtain variance
## Get variance of trans prob by 200 sim sections; assuming 12000 fish
nreps = 100

# make the array for the matrices
boxar = array(rep(NA, 7*7*4*nreps), dim = c(7, 7, 4, nreps), dimnames = list(paste0('start', 1:7), paste0('end',1:7), paste0('season', 1:4), paste0('run',1:nreps)))

allboxtrans = list()


# make the array for the4-box  matrices
boxar4 = array(rep(NA, 4*4*4*nreps), dim = c(4, 4, 4, nreps), dimnames = list(paste0('start', 1:4), paste0('end',1:4), paste0('season', 1:4), paste0('run',1:nreps)))


allboxtrans4 = list()

# make the array for the 11-box matrices
boxar11 = array(rep(NA, 11*11*4*nreps), dim = c(11, 11, 4, nreps), dimnames = list(paste0('start', 1:11), paste0('end',1:11), paste0('season', 1:4), paste0('run',1:nreps)))


allboxtrans11 = list()


# fix sims not in actual boxes..

fix.sims <- function(simdat, npoints = 720){
  test = llply(simdat, function(x) x[x$lat<=90&x$lat>=-90,])
  idx = which(lapply(test, nrow) < npoints)
  for(k in idx) {
    test[[k]] = NULL
  }
  test
}

simdat = fix.sims(simdat)

# load the results and determine areas
#
allsimdat = vector(mode = 'list', length = length(simdat))
#

# set up number of replicates to obtain variance
## Get variance of trans prob by 200 sim sections; assuming 12000 fish
nreps = 100

for(k in 1:nreps){
  print(k)
  #   load(resfiles[k])
  simdat_sub = simdat[c((k*120-119)):(k*120)]
  datbox_sub = get.first.box(simdat_sub, 2000, box7, seas.len = 3*npmon)
  boxtrans_sub = get.trans.prob(datbox_sub, nyears=100, adims = c(7,7,4))
  allboxtrans[[k]] = boxtrans_sub
  #   datbox4 = get.first.box(simdat, 2000, box4, seas.len = 3*npmon)
  #   boxtrans4 = get.trans.prob(datbox4, nyears=100, adims = c(4,4,4))
  #   allboxtrans4[[k]] = boxtrans4
  #   datbox11 = get.first.box(simdat, 2000, box11, seas.len = 3*npmon)
  #   boxtrans11 = get.trans.prob(datbox11, nyears=100, adims = c(11,11,4))
  #   allboxtrans11[[k]] = boxtrans11
  #   # boxar[1:7,1:7,1:4,i] = boxtrans
  #   allsimdat[[k]] = ldply(simdat)
}

for(i in 1:nreps){
  for(j in 1:4){
    boxar[,,j,i] = allboxtrans[[i]][[j]]
    # boxar4[,,j,i] = allboxtrans4[[i]][[j]]
    # boxar11[,,j,i] = allboxtrans11[[i]][[j]]
  }
}


boxmean = alply(apply(boxar, c(1,2,3), mean, na.rm=T), 3)
boxmedian = alply(apply(boxar, c(1,2,3), median, na.rm=T), 3)
boxsd = alply(apply(boxar, c(1,2,3), sd, na.rm=T), 3)
boxq.low = alply(apply(boxar, c(1,2,3), function(x) quantile(x, c(.025), na.rm=T)), 3)
boxq.high = alply(apply(boxar, c(1,2,3), function(x) quantile(x, c(.975), na.rm=T)), 3)

ex7 = expand.grid(1:7, 1:7)


mycol = colorRampPalette(colors = c('lightblue','green','violet'))
seasons = c('winter','spring','summer','fall')
# mycol = colorRampPalette(colors = c('lightblue','green', 'darkred'))
# seasons = c('winter','spring','summer','fall')

par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, t(boxmean[[x]]/rowSums(boxmean[[x]])), col = mycol(49), xlab = '', ylab = '', zlim = c(0, 1))
  # text(ex7[,1], ex7[,2], round(as.vector(t(boxmean[[x]]/rowSums(boxmean[[x]]))), 2), font = 2)
  text(ex7[,1], ex7[,2], round( t(boxmean[[x]]/rowSums(boxmean[[x]])), 2), font = 2)
  grid(7,7,col = 'grey50')
  title(seasons[x])
}
)

tenv = new.env()

tenv$w_large = boxar

#----------------------------------------------------------------------------------------#
# West SMall
load('C:/Users/benjamin.galuardi/Downloads/OneDrive_1_7-23-2019/abft_SIMS_WABFT_lt185_1000.Rdata')

# make the array for the matrices
boxar = array(rep(NA, 7*7*4*nreps), dim = c(7, 7, 4, nreps), dimnames = list(paste0('start', 1:7), paste0('end',1:7), paste0('season', 1:4), paste0('run',1:nreps)))

allboxtrans = list()


# make the array for the4-box  matrices
boxar4 = array(rep(NA, 4*4*4*nreps), dim = c(4, 4, 4, nreps), dimnames = list(paste0('start', 1:4), paste0('end',1:4), paste0('season', 1:4), paste0('run',1:nreps)))


allboxtrans4 = list()

# make the array for the 11-box matrices
boxar11 = array(rep(NA, 11*11*4*nreps), dim = c(11, 11, 4, nreps), dimnames = list(paste0('start', 1:11), paste0('end',1:11), paste0('season', 1:4), paste0('run',1:nreps)))


allboxtrans11 = list()


# fix sims not in actual boxes..

function(simdat){
  test = llply(simdat, function(x) x[x$lat<=90&x$lat>=-90,])
  idx = which(lapply(test, nrow)<720)
  for(k in idx) {
    test[[k]] = NULL
  }
  test
}

simdat = fix.sims(simdat)

# load the results and determine areas
#
allsimdat = vector(mode = 'list', length = length(simdat))
#

# set up number of replicates to obtain variance
## Get variance of trans prob by 200 sim sections; assuming 12000 fish
nreps = 100

for(k in 1:nreps){
  print(k)
  #   load(resfiles[k])
  simdat_sub = simdat[c((k*120-119)):(k*120)]
  datbox_sub = get.first.box(simdat_sub, 2000, box7, seas.len = 3*npmon)
  boxtrans_sub = get.trans.prob(datbox_sub, nyears=100, adims = c(7,7,4))
  allboxtrans[[k]] = boxtrans_sub
  #   datbox4 = get.first.box(simdat, 2000, box4, seas.len = 3*npmon)
  #   boxtrans4 = get.trans.prob(datbox4, nyears=100, adims = c(4,4,4))
  #   allboxtrans4[[k]] = boxtrans4
  #   datbox11 = get.first.box(simdat, 2000, box11, seas.len = 3*npmon)
  #   boxtrans11 = get.trans.prob(datbox11, nyears=100, adims = c(11,11,4))
  #   allboxtrans11[[k]] = boxtrans11
  #   # boxar[1:7,1:7,1:4,i] = boxtrans
  #   allsimdat[[k]] = ldply(simdat)
}


for(i in 1:nreps){
  for(j in 1:4){
    boxar[,,j,i] = allboxtrans[[i]][[j]]
    # boxar4[,,j,i] = allboxtrans4[[i]][[j]]
    # boxar11[,,j,i] = allboxtrans11[[i]][[j]]
  }
}


boxmean = alply(apply(boxar, c(1,2,3), mean, na.rm=T), 3)
boxmedian = alply(apply(boxar, c(1,2,3), median, na.rm=T), 3)
boxsd = alply(apply(boxar, c(1,2,3), sd, na.rm=T), 3)
boxq.low = alply(apply(boxar, c(1,2,3), function(x) quantile(x, c(.025), na.rm=T)), 3)
boxq.high = alply(apply(boxar, c(1,2,3), function(x) quantile(x, c(.975), na.rm=T)), 3)

ex7 = expand.grid(1:7, 1:7)


mycol = colorRampPalette(colors = c('lightblue','green','violet'))
seasons = c('winter','spring','summer','fall')
# mycol = colorRampPalette(colors = c('lightblue','green', 'darkred'))
# seasons = c('winter','spring','summer','fall')

# par(mfrow=c(2,2))
# sapply(1:4, function(x) {
#   image.plot(1:7, 1:7, t(boxmean[[x]]/rowSums(boxmean[[x]])), col = mycol(49), xlab = '', ylab = '', zlim = c(0, 1))
#   # text(ex7[,1], ex7[,2], round(as.vector(t(boxmean[[x]]/rowSums(boxmean[[x]]))), 2), font = 2)
#   text(ex7[,1], ex7[,2], round( t(boxmean[[x]]/rowSums(boxmean[[x]])), 2), font = 2)
#   grid(7,7,col = 'grey50')
#   title(seasons[x])
# }
# )

par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, t(boxmean[[x]]), col = mycol(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
  text(ex7[,1], ex7[,2], paste0(round(t(boxmean[[x]]),2),"\n" ,"(",round(t(boxq.low[[x]]), 2), " - ", round(t(boxq.high[[x]]), 2), ")"), font = 2)
  grid(7, 7, col = 'grey50')
  axis(1, at = 1:7, labels=paste0('e',1:7), cex.axis = 1.3, font.axis = 2)
  axis(2, at = 1:7, labels=paste0('s',1:7), cex.axis = 1.3, font.axis = 2)
  box()
  title(seasons[x])
}
)



tenv$w_small = boxar

#----------------------------------------------------------------------------------------#
# East
load('C:/Users/benjamin.galuardi/Downloads/OneDrive_1_7-23-2019/abft_SIMS_GBYP_ALL_1000_V2.Rdata')

# set up number of replicates to obtain variance
## Get variance of trans prob by 200 sim sections; assuming 12000 fish
nreps = 100


# make the array for the matrices
boxar = array(rep(NA, 7*7*4*nreps), dim = c(7, 7, 4, nreps), dimnames = list(paste0('start', 1:7), paste0('end',1:7), paste0('season', 1:4), paste0('run',1:nreps)))

allboxtrans = list()


# make the array for the4-box  matrices
boxar4 = array(rep(NA, 4*4*4*nreps), dim = c(4, 4, 4, nreps), dimnames = list(paste0('start', 1:4), paste0('end',1:4), paste0('season', 1:4), paste0('run',1:nreps)))


allboxtrans4 = list()

# make the array for the 11-box matrices
boxar11 = array(rep(NA, 11*11*4*nreps), dim = c(11, 11, 4, nreps), dimnames = list(paste0('start', 1:11), paste0('end',1:11), paste0('season', 1:4), paste0('run',1:nreps)))


allboxtrans11 = list()


# fix sims not in actual boxes..

function(simdat){
  test = llply(simdat, function(x) x[x$lat<=90&x$lat>=-90,])
  idx = which(lapply(test, nrow)<720)
  for(k in idx) {
    test[[k]] = NULL
  }
  test
}

simdat = fix.sims(simdat)

# load the results and determine areas
#
allsimdat = vector(mode = 'list', length = length(simdat))
#

# set up number of replicates to obtain variance
## Get variance of trans prob by 200 sim sections; assuming 12000 fish
nreps = 100

for(k in 1:nreps){
  print(k)
#   load(resfiles[k])
  simdat_sub = simdat[c((k*120-119)):(k*120)]
  datbox_sub = get.first.box(simdat_sub, 2000, box7, seas.len = 3*npmon)
  boxtrans_sub = get.trans.prob(datbox_sub, nyears=100, adims = c(7,7,4))
  allboxtrans[[k]] = boxtrans_sub
#   datbox4 = get.first.box(simdat, 2000, box4, seas.len = 3*npmon)
#   boxtrans4 = get.trans.prob(datbox4, nyears=100, adims = c(4,4,4))
#   allboxtrans4[[k]] = boxtrans4
#   datbox11 = get.first.box(simdat, 2000, box11, seas.len = 3*npmon)
#   boxtrans11 = get.trans.prob(datbox11, nyears=100, adims = c(11,11,4))
#   allboxtrans11[[k]] = boxtrans11
#   # boxar[1:7,1:7,1:4,i] = boxtrans
#   allsimdat[[k]] = ldply(simdat)
}

for(i in 1:nreps){
  for(j in 1:4){
    boxar[,,j,i] = allboxtrans[[i]][[j]]
      # boxar4[,,j,i] = allboxtrans4[[i]][[j]]
      # boxar11[,,j,i] = allboxtrans11[[i]][[j]]
  }
}


boxmean = alply(apply(boxar, c(1,2,3), mean, na.rm=T), 3)
boxmedian = alply(apply(boxar, c(1,2,3), median, na.rm=T), 3)
boxsd = alply(apply(boxar, c(1,2,3), sd, na.rm=T), 3)
boxq.low = alply(apply(boxar, c(1,2,3), function(x) quantile(x, c(.025), na.rm=T)), 3)
boxq.high = alply(apply(boxar, c(1,2,3), function(x) quantile(x, c(.975), na.rm=T)), 3)

ex7 = expand.grid(1:7, 1:7)


mycol = colorRampPalette(colors = c('lightblue','green','violet'))
seasons = c('winter','spring','summer','fall')
# mycol = colorRampPalette(colors = c('lightblue','green', 'darkred'))
# seasons = c('winter','spring','summer','fall')

# par(mfrow=c(2,2))
# sapply(1:4, function(x) {
#   image.plot(1:7, 1:7, t(boxmean[[x]]/rowSums(boxmean[[x]])), col = mycol(49), xlab = '', ylab = '', zlim = c(0, 1))
#   # text(ex7[,1], ex7[,2], round(as.vector(t(boxmean[[x]]/rowSums(boxmean[[x]]))), 2), font = 2)
#   text(ex7[,1], ex7[,2], round( t(boxmean[[x]]/rowSums(boxmean[[x]])), 2), font = 2)
#   grid(7,7,col = 'grey50')
#   title(seasons[x])
# }
# )



# sr = dlply(simdatdf, 'seas', function(x) make.sim.raster(x, boxsize=60))

#
# par(mfrow=c(2,2))
# par(mar=c(2,2,2,2))
# for(i in 1:4){
#   plot(sr[[i]]/max(values(sr[[i]]), na.rm=T), zlim = c(0.01,1), interp=T, col=mycol(256), axes=F)
#   rpts = xyFromCell(sr[[i]], which(!is.na(values(sr[[i]]))))
#   world(add=T, col='grey80', fill=F)
#   #   plot(box7, add=T, border='salmon', lwd=2)
#   #   text(coordinates(box7)[,1], coordinates(box7)[,2], box7@data$ID, font=2, cex=1.2)
#   plot(box7, add=T, border='salmon', lwd=2)
#   text(coordinates(box7)[,1], coordinates(box7)[,2], box7@data$ID, font=2, cex=1.2)
#   degAxis(1)
#   degAxis(2)
#   title(seasons[i])
#   box()
# }

# plot with 95% range

par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, t(boxmean[[x]]), col = mycol(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
  text(ex7[,1], ex7[,2], paste0(round(t(boxmean[[x]]),2),"\n" ,"(",round(t(boxq.low[[x]]), 2), " - ", round(t(boxq.high[[x]]), 2), ")"), font = 2)
  grid(7, 7, col = 'grey50')
  axis(1, at = 1:7, labels=paste0('e',1:7), cex.axis = 1.3, font.axis = 2)
  axis(2, at = 1:7, labels=paste0('s',1:7), cex.axis = 1.3, font.axis = 2)
  box()
  title(seasons[x])
}
)


tenv$east = boxar

# save it all!

save(tenv, file = 'C:/Users/benjamin.galuardi/Documents/BLUEFIN/trans_var_2016sims_2018-07-29.Rdata')
