library(ggplot2)
library(ggforce)

#The span per beekeeper
loc <- locAll[!is.na(locAll$Beekeeper),]
bk <- unique(locAll$Beekeeper)
bkRange <- data.frame(Beekeeper = bk,
                      xRange = sapply(bk, FUN = function(x) diff(range(loc$X_COORDINATE[loc$Beekeeper == x]))),
                      yRange = sapply(bk, FUN = function(x) diff(range(loc$Y_COORDINATE[loc$Beekeeper == x]))))
summary(bkRange$xRange)
ggplot(data = bkRange, aes(x = xRange)) + geom_density(bins = 100) + xlim(c(0, 10000))
ggplot(data = bkRange, aes(x = yRange)) + geom_density(bins = 100) + xlim(c(0, 10000))

# Density of apiaries in Slovenia
areaSlo <- (diff(range(locAll$X_COORDINATE)) / 1000)  * (diff(range(locAll$Y_COORDINATE)) / 1000)
nrow(locAll) / areaSlo
(nrow(locAll) * 15) / areaSlo

# Plot Vipava valley colonies
locAll <- read.csv("~/Documents/1Projects/SIMplyBee_devel/Spatial/SLOLocations_standardised.csv")
ggplot(data = locAll, aes(x = X_COORDINATE, Y_COORDINATE)) + geom_point()

xMin <- 15000
xMax <- 30000
yMin <- 50000
yMax <- 65000

ggplot(data = locAll, aes(x = X_COORDINATE, Y_COORDINATE)) + geom_point() +
  geom_vline(aes(xintercept = xMin, col = "red")) + geom_vline(aes(xintercept = xMax, col = "red")) +
  geom_hline(aes(yintercept = yMin, col = "red")) + geom_hline(aes(yintercept = yMax, col = "red"))

vipava <- locAll[(locAll$X_COORDINATE > xMin) & (locAll$X_COORDINATE < xMax) &
                 (locAll$Y_COORDINATE > yMin) & (locAll$Y_COORDINATE < yMax),]

ggplot(vipava, aes(x = X_COORDINATE, y = Y_COORDINATE)) + geom_point()

# Compute the density
area <- diff(range(vipava$X_COORDINATE))  * diff(range(vipava$Y_COORDINATE)) / 1000000
nrow(vipava) / area
(nrow(vipava)*15) / area

# Plot vipava valley range 5km
head(vipava)
ggplot(vipava, aes(x = X_COORDINATE,y = Y_COORDINATE)) + geom_point() +
  geom_point(aes(x = 22092, 56650, colour = "red")) +
  geom_circle(aes(x0 = 22092, y0 = 56650, r = 5000))

# Plot SLO with a target colony in Vipava valley range 5km
ggplot(locAll, aes(x = X_COORDINATE,y = Y_COORDINATE)) + geom_point() +
  geom_point(aes(x = 22092, 56650, colour = "red")) +
  geom_circle(aes(x0 = 22092, y0 = 56650, r = 5000))


