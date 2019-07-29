# look in to Molly's code
# 7-16-19
# ben galuardi

# make sure the dimensions are messed up.. look good
ex7 = expand.grid(1:7, 1:7)

par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, east_all[,, x, 1], col = terrain.colors(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
  text(ex7[,1], ex7[,2], paste0(round(t(east_all[,, x, 1]),2)))#,"\n" ,"(",round(t(boxq.low4[[x]]), 2), " - ", round(t(boxq.high4[[x]]), 2), ")"), font = 2)
  grid(7, 7, col = 'grey50')
  axis(1, at = 1:7, labels=paste0('e',1:7), cex.axis = 1.3, font.axis = 2)
  axis(2, at = 1:7, labels=paste0('s',1:7), cex.axis = 1.3, font.axis = 2)
  box()
  # title(seasons[x])
}
)


# modified from my vignette: https://github.com/galuardi/SatTagSim/blob/master/vignettes/matrix_variance.Rmd

ball.e = array(rep(NA, 7*7*list_len), dim = c(7, 7, list_len), dimnames = list(paste0('start', 1:7), paste0('end',1:7), paste0('run',1:list_len)))

for(i in 1:dim(east_all)[4]){
  b2 = east_all[,,,i]
  # ball.w = b1[[1]]%*%b1[[2]]%*%b1[[3]]%*%b1[[4]]
  ball.e[,,i] = b2[,,1]%*%b2[,,2]%*%b2[,,3]%*%b2[,,4]
  # (e.w = get.loglin(ball.e, ball.w))
}

# apply(ball.e, 3, function(x) sum(x[1:3, 1:3]))  # this would be for west.. 

east_overall = apply(ball.e, 3, function(x) sum(x[4:7, 4:7]))
lowidx = which(east_overall >= quantile(east_overall, .75))# low movement out of the east
highidx = which(east_overall >= quantile(east_overall, .25)) # high movement out of the east

# some plots
elow_ex = east_all[,,,lowidx[2]]

seasons = c('winter','Spring','Summer','Fall')

# low
par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, elow_ex[,, x], col = terrain.colors(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
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
ehigh_ex = east_all[,,,highidx[2]]

par(mfrow=c(2,2))
sapply(1:4, function(x) {
  image.plot(1:7, 1:7, ehigh_ex[,, x], col = terrain.colors(100), xlab = '', ylab = '', zlim = c(0, 1), axes = F) # highlights the mean value
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


