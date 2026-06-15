library(dplyr); library(fda.usc); library(circular)

dtwicing <- function(x,hk){
		exp(hk*cos(x))/(pi*besselI(hk,0))-exp(0.5*hk*cos(x))/(2*pi*besselI(0.5*hk,0))
		}

dec <- function(coef, wwgg, xt, aa){
	est <- colMeans(1/(2*pi)*(1+2*colSums(aa*cos(outer(coef,wwgg,"*")))))
	cest <- pmax(0,est)
	mm <- cbind(xt,cest); om <- mm[order(mm[,1]),]
	con <- int.simpson2(om[,1],om[,2],equi=F)
	fest <- cest/con
	}

#DATA 

s <- read.table("beddip.txt"); 
names(s) <- c("DIP", "DIR", "group")
A <- subset(s,subset=group=="A") 
B <- subset(s,subset=group=="B")
y <- factor(c(A$group,B$group))

Along <- A$DIP; Blong <- B$DIP
t0 <- rad(Along)		
t1 <- rad(Blong) 		
t0 <- t0+pi; t1 <- t1+pi
t0 <- t0%%(2*pi); t1 <- t1%%(2*pi)
n0 <- length(t0); n1 <- length(t1) 
t <- c(t0,t1); n <- n0+n1 


#SETTINGS

KER="vM"

#Error density (von-Mises or Laplace)

# von-Mises Error (choose one of the following levels of error)

ER="vM"; kE=150 	#Negligible error
ER="vM"; kE=90 	#Low error
ER="vM"; kE=30 	#Moderate error
ER="vM"; kE=10	#Abundant error


# Laplace error (choose one of the following levels of error)

ER="wL"; kE=0.05 	#Negligible error
ER="wL"; kE=0.1 	#Low error
ER="wL"; kE=0.2 	#Moderate error
ER="wL"; kE=0.4 	#Abundant error


ncoef <- 200
i <- 1:ncoef

if(ER=="wL") den <- kE^(-2)/(i^2+kE^(-2))
if(ER=="vM") den <- besselI(kE,i)/besselI(kE,0)



# STANDARD KERNEL CLASSIFIER

bw0 <- bw.cv.ml.circular(circular(t0), upper=1000) 
bw1 <- bw.cv.ml.circular(circular(t1), upper=1000) 

gridd <- seq(0,2*pi,length=500); grid <- gridd[-1]
g <- (circular(c(grid,t))); ng <- length(g) 

W0 <- circular(t0); wg0 <- circular(outer(W0,g,"-"))%%(2*pi)
W1 <- circular(t1); wg1 <- circular(outer(W1,g,"-"))%%(2*pi)

kff0 <- density.circular(circular(t0), z=g, bw=bw0, kernel="vonmises"); kff0=kff0$y
kff1 <- density.circular(circular(t1), z=g, bw=bw1, kernel="vonmises"); kff1=kff1$y
kest <- cbind(kff0,kff1)

gg <- which(is.element(g,t))
kF0 <- kff0[gg]; kF1 <- kff1[gg]

kaa <- (kF0*n0/n-kF1*n1/n)<=0
ktab <- table(kaa,y) 
kmisclassified <- sum(diag(apply(ktab, 2, rev)))
KDE <- kmisclassified/n


# DECONVOLUTION CLASSIFIER  --------------------------------------------------------

#----------------- dec estimator: von Mises kernel -----------------

num0 <- besselI(bw0,i)/besselI(bw0,0); a0 <- num0/den; a0 <- replace(a0,a0>1,1)
num1 <- besselI(bw1,i)/besselI(bw1,0); a1 <- num1/den; a1 <- replace(a1,a1>1,1)
dff0 <- dec(i,wg0,g,a0)
dff1 <- dec(i,wg1,g,a1)
dest <- cbind(dff0,dff1)

daa <- ((dff0*n0/n)[gg]-(dff1*n1/n)[gg])<=0
dtab <- table(daa, y) 
misclassifiedd <- sum(diag(apply(dtab, 2, rev)))
vM <- misclassifiedd/n


#----------------- dec estimator: Twicing kernel -----------------

hnum0 <- -besselI(bw0/2,i)/besselI(bw0/2,0)+2*besselI(bw0,i)/besselI(bw0,0)
hnum1 <- -besselI(bw1/2,i)/besselI(bw1/2,0)+2*besselI(bw1,i)/besselI(bw1,0)
ha0 <- hnum0/den; ha0 <- replace(ha0,ha0>1,1)
ha1 <- hnum1/den; ha1 <- replace(ha1,ha1>1,1)

hdff0 <- dec(i,wg0,g,ha0)
hdff1 <- dec(i,wg1,g,ha1)
hdest <- cbind(hdff0,hdff1)

hdaa <- ((hdff0*n0/n)[gg]-(hdff1*n1/n)[gg])<=0
hdtab <- table(hdaa,y) 
hmisclassifiedd <- sum(diag(apply(hdtab, 2, rev)))
TW <- hmisclassifiedd/n


#----------------- dec estimator: Sheater & Jones kernel -----------------

g2 <- besselI(bw0,2)/besselI(bw0,0)
s2 <- 0.5*(1-g2);
num0 <- besselI(bw0,i)/besselI(bw0,0)
a0J <- num0/den-1/2*s2*(-i^2*num0/den)
a0J <- replace(a0J, a0J>1, 1)

g2 <- besselI(bw1,2)/besselI(bw1,0)
s2 <- 0.5*(1-g2);
num1 <- besselI(bw1,i)/besselI(bw1,0)
a1J <- num1/den-1/2*s2*(-i^2*num1/den)
a1J <- replace(a1J, a1J>1, 1)

Jdff0 <- dec(i,wg0,g,a0J)
Jdff1 <- dec(i,wg1,g,a1J)
Jdest <- cbind(Jdff0,Jdff1)

jdaa <- ((Jdff0*n0/n)[gg]-(Jdff1*n1/n)[gg])<=0
Jdtab <- table(jdaa, y) 
Jmisclassifiedd <- sum(diag(apply(Jdtab, 2, rev)))
JF <- Jmisclassifiedd/n


round(cbind(KDE,vM,TW,JF),4)*100

