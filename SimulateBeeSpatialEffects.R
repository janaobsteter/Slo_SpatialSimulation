library(INLA)
library(fields)
library(ggplot2)
library(viridisLite)

# Define a function for plotting
local.plot.field = function(field, mesh, xlim=c(0,10), ylim=c(0,10), ...){
  stopifnot(length(field) == mesh$n)
  proj = inla.mesh.projector(mesh, xlim = xlim,
                             ylim = ylim, dims=c(300, 300))
  field.proj = inla.mesh.project(proj, field)
  n.col = 20
  image.plot(list(x = proj$x, y=proj$y, z = field.proj),
             xlim = xlim, ylim = ylim, col = plasma(n.col), nlevel=n.col+1, ...)
}


# Read in SLO locations
df <- read.csv("~/Documents/1Projects/SIMplyBee_devel/Spatial/Data/SLOLocations_standardised.csv")
# locSLO <- unique(hivesSLO[, c("X_COORDINATE", "Y_COORDINATE", "KMG_MID")])
# colnames(locSLO) <- c("Y_COORDINATE", "X_COORDINATE", "Beekeeper") #Change x and y! They are the other way around for some reason
# locSLO[(is.na(locSLO$Y_COORDINATE)),]
# locSLO <- locSLO[!is.na(locSLO$Y_COORDINATE),]
# sum(is.na(locSLO$X_COORDINATE)); sum(is.na(locSLO$Y_COORDINATE))


# Inspect points
# hist(locSLO$X_COORDINATE)
# hist(locSLO$Y_COORDINATE)
# CDF <- ecdf(locSLO$X_COORDINATE)
# plot(CDF)
# quantile(locSLO$X_COORDINATE, c(seq(0, 1, 0.05)))
# quantile(locSLO$Y_COORDINATE, c(seq(0, 1, 0.05)))
#
# # New df
# # Boundaries:
# xmin = 194661.55
# xmax = 625999.74
# ymin = 31118.3
# ymax = 373217.65
# df <- locSLO[locSLO$X_COORDINATE > xmin & locSLO$X_COORDINATE < xmax,]
# df <- df[df$Y_COORDINATE > ymin & df$Y_COORDINATE < ymax,]
# # Standardise
# xminData <- min(df$X_COORDINATE)
# yminData <- min(df$Y_COORDINATE)
# df$X_COORDINATE <- df$X_COORDINATE - xminData
# df$Y_COORDINATE <- df$Y_COORDINATE - yminData
# write.csv(df[, c("X_COORDINATE", "Y_COORDINATE", "Beekeeper")], "~/Documents/1Projects/SIMplyBee_devel/Spatial/SLOLocations_standardised.csv", quote=F, row.names=F)

#Plot by scatter plot
plot(x = df$X_COORDINATE, y = df$Y_COORDINATE)

# Inspect points
hist(df$X_COORDINATE)
hist(df$Y_COORDINATE)

#demo("mesh2d")

# Construct a mesh
summary(df)

#1st attempt
range = diff(range(df$X_COORDINATE, na.rm=T))/3
max.edge = diff(range(df$X_COORDINATE, na.rm=T))/15
#max.edge = 0.95
max.edge
mesh1 = inla.mesh.2d(loc=cbind(df$X_COORDINATE,
                               df$Y_COORDINATE),
                     max.edge = max.edge)

plot(mesh1, main="1st attempt"); points(df$X_COORDINATE, df$Y_COORDINATE, col="blue")

#2nd attempt, with cutoff
bound.outer = range
mesh2 = inla.mesh.2d(loc=cbind(df$X_COORDINATE, df$Y_COORDINATE),
                     max.edge = c(1,5)*max.edge,
                     # - use 5 times max.edge in the outer extension/offset/boundary
                     cutoff = max.edge/5,
                     offset = c(max.edge, bound.outer))
plot(mesh2, main="2nd attempt"); points(df$X_COORDINATE, df$Y_COORDINATE, col="blue")

# Set up parameters (we don't know what a true range is!)
sigma.u = 15 # What should this number be? For now, is half of the sigmaE from Sreten's paper (var is 36, sigma is 6)
kappa = sqrt(8)/range
inla.seed = 200 #sample.int(n=1E6, size=1)

# Simulate spatial field (priors are not used for simulation, so just plugging in 0.5)
spde = inla.spde2.pcmatern(mesh2, prior.range = c(.5, .5), prior.sigma = c(.5, .5))

Qu = inla.spde.precision(spde, theta=c(log(range), log(sigma.u)))
u = inla.qsample(n=1, Q=Qu, seed = inla.seed)
u = u[ ,1] #Spatial effects - why only 1400? There are 2600 vertex of the triangles

# Plot
local.plot.field(u, mesh2, xlim=c(0, max(df$X_COORDINATE)), ylim = c(0, max(df$Y_COORDINATE))) # Why is it plotting like this?
len = range
# # - the true range
arrows(100000-0.5*len, 100000, 100000+0.5*len, 100000, length=500, angle=90, code=3, lwd=3) # What is this true range?


# Sample spatial effect at locations
A = inla.spde.make.A(mesh=mesh2, loc=as.matrix(df))
u = drop(A %*% u)
table(u)

quilt.plot(x=df$X_COORDINATE,y=df$Y_COORDINATE, z=u, nx=100, ny=100, #What is nx and ny - number of grid boxes?
           col = plasma(101), main="Field projected to data locations",
           zlim = range(u))

# Write the spatial effects out
df$Spatial <- u
write.csv(df, "~/Documents/1Projects/SIMplyBee_devel/Spatial/Data/SpatialEffects_SLOLocations_standardised.csv", quote = F, row.names = F )

