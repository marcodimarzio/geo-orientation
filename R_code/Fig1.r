library(circular); library(VGAM)

dtwicing <- function(x,hk){
		exp(hk*cos(x))/(pi*besselI(hk,0))-exp(0.5*hk*cos(x))/(2*pi*besselI(0.5*hk,0))
		}

ncoef <- 100
i <- 1:ncoef
gridd <- seq(0,2*pi,length = 500); grid <- gridd[-1];

par(mfrow = c(2,3),mar = c(2,2,2,1))

#VON MISES
kap <- 10
vm <- besselI(kap,i)/besselI(kap,0)
Kvm <- vm*cos(outer(i,grid-pi,"*"))
K <- 1/(2*pi)*(1+2*apply(Kvm,2,sum))
plot(grid,K,typ = "l", main = "VON MISES")

#DECONVOLUTION VON MISES + LAPLACE (OS)
kap <- 10
kappaE <- .2
num <- besselI(kap,i)/besselI(kap,0)
den <- kappaE^(-2)/(i^2+kappaE^(-2))
a <- num/den
Kvmlap <- a*cos(outer(i,grid-pi,"*"))
K <- 1/(2*pi)*(1+2*apply(Kvmlap,2,sum))
plot(grid,K,typ = "l", main = "VON MISES + LAPLACE ")

#DECONVOLUTION VON MISES + VON MISES (SS)
kap <- 10
kappaE <- 20
num <- besselI(kap,i)/besselI(kap,0)
den <- besselI(kappaE,i)/besselI(kappaE,0)
a <- num/den
Kvmvm <- a*cos(outer(i,grid-pi,"*"))
K <- 1/(2*pi)*(1+2*apply(Kvmvm,2,sum))
plot(grid,K,typ = "l", main = "VON MISES + VON MISES")

#TWICING 
kap <- 10
hvm <- -besselI(kap/2,i)/besselI(kap/2,0)+2*besselI(kap,i)/besselI(kap,0)
Khvm <- hvm*cos(outer(i,grid-pi,"*"))
K <- 1/(2*pi)*(1+2*apply(Khvm,2,sum))
plot(grid,K,typ = "l", main = "TWICING")

#DECONVOLUTION TWICING + LAPLACE (OS)
kap <- 10
kappaE <- .2
num <- -besselI(kap/2,i)/besselI(kap/2,0)+2*besselI(kap,i)/besselI(kap,0)
den <- kappaE^(-2)/(i^2+kappaE^(-2))
a <- num/den
Khvmlap <- a*cos(outer(i,grid-pi,"*"))
K <- 1/(2*pi)*(1+2*apply(Khvmlap,2,sum))
plot(grid,K,typ = "l", main = "TWICING + LAPLACE ")

#DECONVOLUTION TWICING + VON MISES (SS)
kap <- 10
kappaE <- 20
num <- -besselI(kap/2,i)/besselI(kap/2,0)+2*besselI(kap,i)/besselI(kap,0)
den <- besselI(kappaE,i)/besselI(kappaE,0)
a <- num/den
Khvmvm <- a*cos(outer(i,grid-pi,"*"))
K <- 1/(2*pi)*(1+2*apply(Khvmvm,2,sum))
plot(grid,K,typ = "l", main = "TWICING  + VON MISES")



