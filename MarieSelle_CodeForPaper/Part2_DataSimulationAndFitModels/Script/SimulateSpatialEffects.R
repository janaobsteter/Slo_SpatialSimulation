#Script name: SimulateSpatialEffects.R
#Author: Maria L. Selle
#Description: Simulate locations and spatial effects
library(INLA)

# Construct a mesh
plDom = cbind(c(0, 1, 1, 1, 0), c(0, 0, 1, 1, 1))
mesh = inla.mesh.2d(loc.domain = plDom, max.e = c(0.092, 0.2))

# Sample positions of villages and farms   
xLoc = runif(nVillages ,min = 0.05, max =0.95)
yLoc = runif(nVillages ,min = 0.05, max =0.95)
villageLoc = cbind(xLoc, yLoc)

herdLocTemp =  apply(villageLoc, 1, function(x) SimulateFarmLocation(x, nHerds = nHerdsPerVill, sigmaHerds = sigmaHerds )   ) 
xHerd =as.vector(herdLocTemp[ 1:nHerdsPerVill  ,] )
yHerd =as.vector(herdLocTemp[(nHerdsPerVill +1): (2*nHerdsPerVill ) ,] )
herdLoc = cbind(xHerd,yHerd)


# Simulate spatial effects
# Make mesh and A matrix
#  10 independent fields --> 8 dependent fields and 2 independent
nIndepFields = 10
sampleRanges = runif(n = nIndepFields, min = 0.1, max = 0.9)
sampleSd = sqrt(sample(x = c(0.2, 0.3 ), size = nIndepFields, replace = T))
sampleFields = cbind(sapply(1:10, function(x) SimulateSpdeField(mesh, n.fields = 1, range0 = sampleRanges[x],sigma0 = sampleSd[x]) ) )


# Create dependent fields
B1= diag(c(1,1,1))  
B2= diag(c(1,1,1))
B3= diag(c(1,1))


B1[lower.tri(B1)] = runif(3, min = -0.5, max = 0.5)
B2[lower.tri(B2)] = runif(3, min = -0.5, max = 0.5)
B3[lower.tri(B3)] = runif(1, min = -0.5, max = 0.5)

B = bdiag(B1,B2,B3)

dependentFields = B %*% t(sampleFields[,1:8])


# Our final field
trueField =colSums(dependentFields )



# Make spatial effects 
A.field = inla.spde.make.A(mesh = mesh, loc = herdLoc)
trueSpatialEffects = as.vector(A.field%*%trueField)


# Variance of each of the 8 fields
varEachField8 = B^2%*%(sampleSd[1:8])^2

# Variance of the sum of the 8 fields
varSpatialField = sum(colSums(B)^2 *(sampleSd[1:8] )^2)


# Variance of spatial effect in the locations mapped from full field
varSpatialFieldPts = var(trueSpatialEffects)
varTotal = varSpatialFieldPts/0.4

