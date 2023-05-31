#Script name: SimulatePhenotype.R
#Author: Maria L. Selle
#Description: Make the phenotype


# Scale the genetic effect to have 0.1 of total variance
varGeneticEffect = 0.1 * varTotal # Genetic variance 
varGtemp  = var(phenoData$geneticEffect) # Current genetic variance


meanGeneticEffect11 = mean(sqrt(varGeneticEffect)*phenoData$geneticEffect/sqrt(varGtemp) )
phenoData$geneticEffect = scale(sqrt(varGeneticEffect)* phenoData$geneticEffect/sqrt(varGtemp) , scale = F)


# Expand the spatial effect and coordinates
nIndPerHerd = as.vector(table(phenoData$herdID))

phenoData$trueSpatialEffects = scale( ExpandVector(trueSpatialEffects, nIndPerHerd), scale = F )

xHerdExp = ExpandVector( xHerd, nIndPerHerd)
yHerdExp = ExpandVector( yHerd, nIndPerHerd)

phenoData$xHerd = xHerdExp
phenoData$yHerd = yHerdExp


# Simulate and expand a herd effect 
herdEffect = rnorm(n = nHerds, mean = 0, sd = sqrt(varTotal*0.25))
phenoData$herdEffect = scale( ExpandVector(herdEffect, nIndPerHerd) ,scale = F )

# Compute phenotype
phenoData$phenotype = phenoData$geneticEffect + phenoData$trueSpatialEffects + phenoData$herdEffect + scale(rnorm(n = nInd, mean = 0, sd = sqrt(0.25*varTotal)), scale = F )


phenoData$phenotypeStd = (phenoData$phenotype - mean(phenoData$phenotype))/sd(phenoData$phenotype)




