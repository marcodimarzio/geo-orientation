library(circular)

s <- read.table("beddip.txt", header = FALSE)
names(s) <- c("DIP", "DIR", "group")

A <- subset(s, group == "A")
B <- subset(s, group == "B")

pole_from_plane <- function(dip, dir) {
  plunge  <- 90 - dip
  azimuth <- (dir + 180) %% 360
  data.frame(plunge = plunge, azimuth = azimuth)
}

Ap <- pole_from_plane(A$DIP, A$DIR)
Bp <- pole_from_plane(B$DIP, B$DIR)

# Equal-area projection

pole_xy <- function(plunge, azimuth) {
  p <- plunge * pi / 180
  a <- azimuth * pi / 180

  r <- sqrt(2) * sin((pi/2 - p)/2)
  x <- r * sin(a)
  y <- r * cos(a)

  data.frame(x = x, y = y)
}

pA <- pole_xy(Ap$plunge, Ap$azimuth)
pB <- pole_xy(Bp$plunge, Bp$azimuth)

par(mfrow = c(1,2))
par(mar = c(1,1,2,1))
plot(NA, NA,
     xlim = c(-1.05, 1.05), ylim = c(-1.05, 1.05),
     asp = 1, axes = FALSE, xlab = "", ylab = "", main = "")

th <- seq(0, 2*pi, length.out = 400)
lines(cos(th), sin(th), lwd = 1.3)

text(0, 1.06, "0", cex=1.5)
text(1.06, 0, "90", cex=1.5)
text(0, -1.06, "180", cex=1.5)
text(-1.1, 0, "270", cex=1.5)

points(pA$x, pA$y, pch = 17, col = 3)
points(pB$x, pB$y, pch = 17, col = 2)


#Plot data on the circle

par(mar = c(1,1,2,1))
plot(circular(t1),col=2,pch=17,cex=1.3, axes=FALSE)
xt0 <- 1.02*cos(t0) 
yt0 <- 1.02*sin(t0)
points(xt0, yt0, pch = 17, cex = 1.3, col = 3)
angles <- c(0, pi/2, pi, 3*pi/2)
labels <- c(0, 90, 180, 270)

r <- 1.08
x <- r * sin(angles)
y <- r * cos(angles)
text(x, y, labels, cex=1.5)

