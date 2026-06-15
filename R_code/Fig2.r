library(VGAM); library(circular)

ncoef <- 200; 
i <- 1:ncoef

grid <- seq(-pi, pi, length = 500)

par(mfrow = c(2,2), mar = c(2,3,5,2))

main <- "Negligible error"; kappaE <- .05; kE <- 150; m <- 9
matplot(grid,cbind(dlaplace(grid, 0, kappaE),dvonmises(grid, 0, kE)),
	typ="l",main=main,lwd=2,lty=1,cex.main=2,cex.axis=2,ylim=c(0,m),ylab="")

main <- "Low error"; kappaE=.1; kE <- 90; m <- 8
matplot(grid,cbind(dlaplace(grid, 0, kappaE),dvonmises(grid, 0, kE)),
	typ="l",main=main,lwd=2,lty=1,cex.main=2,cex.axis=2,ylim=c(0,m),ylab="")

main <- "Moderate error"; kappaE <- .2; kE <- 30; m <- 5
matplot(grid,cbind(dlaplace(grid, 0, kappaE),dvonmises(grid, 0, kE)),
	typ="l",main=main,lwd=2,lty=1,cex.main=2,cex.axis=2,ylim=c(0,m),ylab="")

main <- "Abundant error"; kappaE <- .4; kE <- 10; m <- 5
matplot(grid,cbind(dlaplace(grid, 0, kappaE),dvonmises(grid, 0, kE)),
	typ="l",main=main,lwd=2,lty=1,cex.main=2,cex.axis=2,ylim=c(0,m),ylab="")


