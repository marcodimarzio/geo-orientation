library(circular)

s <- read.table("bedding.txt", header = FALSE)
names(s) <- c("PLUNGE", "AZIMUTH", "group")

A <- subset(s, group == "A")
B <- subset(s, group == "B")

fix_lower_hemisphere <- function(plunge, azimuth) {
  azimuth <- azimuth %% 360
  neg <- plunge < 0
  plunge[neg]  <- abs(plunge[neg])
  azimuth[neg] <- (azimuth[neg] + 180) %% 360
  horiz <- plunge == 0
  azimuth[horiz] <- (azimuth[horiz] + 180) %% 360
  data.frame(plunge = plunge, azimuth = azimuth)
}

A2 <- fix_lower_hemisphere(A$PLUNGE, A$AZIMUTH)
B2 <- fix_lower_hemisphere(B$PLUNGE, B$AZIMUTH)


# Equal-area projection

line_xy <- function(plunge, azimuth) {
  p <- plunge * pi / 180
  a <- azimuth * pi / 180
  
  r <- sqrt(2) * sin((pi/2 - p) / 2)
  x <- r * sin(a)
  y <- r * cos(a)
  
  data.frame(x = x, y = y)
}

pA <- line_xy(A2$plunge, A2$azimuth)
pB <- line_xy(B2$plunge, B2$azimuth)

par(mfrow=c(1,2))
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

points(pA$x, pA$y, pch = 16, col = 3)
points(pB$x, pB$y, pch = 16, col = 2)


#Plot data on the circle

s <- read.table("bedding.txt")
names(s) <- c("PL", "APL", "group")
A <- subset(s, subset=group=="A") 
B <- subset(s, subset=group=="B")
y <- factor(c(A$group, B$group))

Along <- A$APL; Blong <- B$APL
t0 <- rad(Along)
t1 <- rad(Blong)
t0 <- t0+pi; t0 <- t0%%(2*pi) 
t1 <- t1+pi; t1 <- t1%%(2*pi)

par(mar = c(1,1,2,1))
plot(circular(t1),col=2,pch=17,cex=1.3, axes=FALSE)
xt0 <- 1.02*cos(t0) 
yt0 <- 1.02*sin(t0)
points(xt0, yt0, pch = 17, cex = 1.3, col = 3)
angles <- c(0, pi/2, pi, 3*pi/2)
labels <- c(0,90,180,270)

r <- 1.08
x <- r * sin(angles)
y <- r * cos(angles)
text(x, y, labels, cex=1.5)

