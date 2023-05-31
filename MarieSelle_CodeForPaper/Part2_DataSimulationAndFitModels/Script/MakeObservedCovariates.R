#Script name: MakeObservedCovariates.R
#Author: Maria L. Selle
#Description: Make the observed covariates


# Covariate fields, the 8 true dependent field + 2 independent fields
covariateFields = cbind( t(dependentFields), sampleFields[,9:10 ] ) 

# Map the covariate fields to the herd locations, the 8 true spatial effects + 2 independent effects
covariateEffects =data.frame( apply(covariateFields,2, function(x)  as.vector(A.field%*%x) ) )

# I. Obsered covariate effects, first 3 (climate for example) we assume to observe without error 
observedCovariateEffects = data.frame(X1 =ExpandVector(covariateEffects[,1] , nIndPerHerd  ), X2 =ExpandVector(covariateEffects[,2] , nIndPerHerd  ), X3 =ExpandVector(covariateEffects[,3] , nIndPerHerd  ) )   

# II. 3 next we observe with some normal distributed error 
observedCovariateEffects$X4 = ExpandVector( covariateEffects[,4] + rnorm(nHerds,sd = sqrt(varEachField8[4]*0.1)) , nIndPerHerd)
observedCovariateEffects$X5 = ExpandVector( covariateEffects[,5] + rnorm(nHerds,sd = sqrt(varEachField8[5]*0.1)) , nIndPerHerd)
observedCovariateEffects$X6 = ExpandVector( covariateEffects[,6] + rnorm(nHerds,sd = sqrt(varEachField8[6]*0.1)) , nIndPerHerd)


# III. 2 next we make one binary and one with 3 factors
treshold = runif(n = 1, min = (mean(covariateEffects[,7])- sd(covariateEffects[,7])), max =  (mean(covariateEffects[,7])+ sd(covariateEffects[,7])) )
observedCovariateEffects$X7 =factor( ExpandVector( as.numeric(covariateEffects[,7] > treshold ), nIndPerHerd), ordered = F)

tresholdL = runif(n = 1, min = (mean(covariateEffects[,8])- 2*sd(covariateEffects[,8])), max =  mean(covariateEffects[,8]) )
tresholdU = runif(n = 1, min = mean(covariateEffects[,8]), max =  (mean(covariateEffects[,8])+ 2*sd(covariateEffects[,8])) )
observedCovariateEffects$X8 =factor( ExpandVector( (covariateEffects[,8] > tresholdL) + (covariateEffects[,8] > tresholdU), nIndPerHerd) , ordered = F) 


# IV. 2 final are the dummy-covariates, which have not affected the phenotype
observedCovariateEffects$X9 = ExpandVector( covariateEffects[,9], nIndPerHerd)
observedCovariateEffects$X10 = ExpandVector( covariateEffects[,10], nIndPerHerd)


observedCovariateModelMatrix = model.matrix(~ -1 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10,  data = observedCovariateEffects) 


if(ncol(observedCovariateModelMatrix) < 12){
  cat("Observed covariate model matrix not properly constructed.")
  save(covariateEffects, observedCovariateEffects, observedCovariateModelMatrix, file = "Covariates.RData")
}


