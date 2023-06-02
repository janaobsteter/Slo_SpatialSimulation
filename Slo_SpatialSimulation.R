# Simulatea spatial mating for Slovenia
library(SIMplyBee)

# Read in locations
# Set up variables
nColonies = 2e05
nLocations = 2e04
nColoniesPerLocation = nColonies / nLocations
meanHY <- 20
phenoVar <- 175
h2 <- 0.25 # Heritability for honey yield
varA <- phenoVar * h2
nonAVar <- phenoVar - varA
apiaryYearVar <- 1/3 * nonAVar
spatialVar <- 1/3 * nonAVar
residualVar <- 1/3 * nonAVar

# Simulate founders
founderGenomes <- quickHaplo(nInd = 20000, nChr = 1, segSites = 1000)

# Set simulation parameters
SP <- SimParamBee$new(founderGenomes)
SP$addTraitA(nQtlPerChr = 100, mean = meanHY, var = varA)
SP$setVarE(varE = residualVar)

# Create a base population of 20,000 colonies
basePop <- newPop(founderGenomes)
