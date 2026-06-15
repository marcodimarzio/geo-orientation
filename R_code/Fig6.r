library(circular)

s <- read.table("beddip.txt", header = FALSE)
names(s) <- c("DIP", "DIR", "group")
A <- subset(s, group == "A")
B <- subset(s, group == "B")

y <- factor(c(A$group, B$group))

Along <- A$DIP; Blong <- B$DIP
t0 <- rad(Along)
t1 <- rad(Blong)
t0 <- t0+pi; t0 <- t0%%(2*pi) 
t1 <- t1+pi; t1 <- t1%%(2*pi)

n0 <- length(t0); n1 <- length(t1) 
t <- c(t0,t1); n <- n0 + n1 


(bw0 <- bw.cv.ml.circular(circular(t0), upper=1000)) 
(bw1 <- bw.cv.ml.circular(circular(t1), upper=1000))
d0 <- density(circular(t0), bw=bw0, control.circular=list(units="radians"))
d1 <- density(circular(t1), bw=bw1, control.circular=list(units="radians"))

par(mar = c(4,4.5,.5,.5))

plot(as.numeric(d0$x), d0$y,typ = "l", col = 3, lwd = 2, xlab = expression(theta), ylab = "density", cex.lab = 2,cex.axis=1.5)
rug(t0, col=3)
points(as.numeric(d1$x), d1$y, typ = "l", col = 2, lwd = 2)
rug(t1, col=2)
