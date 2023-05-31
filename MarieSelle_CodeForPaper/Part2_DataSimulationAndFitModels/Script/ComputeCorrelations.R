#Script name: ComputeCorrelations.R
#Author: Maria L. Selle
#Description: Compute correlation and CRPS for EBV and PBV



# Compute correlation with true breeding value
if(useMarkers){
	CorEbv11 = sapply(fitList, function(x) cor(x$summary.random$individualID$mean[1:nrow(phenoData)], phenoData$geneticEffect))
	estBreedingValue11 = sapply(fitList, function(x) x$summary.random$individualID$mean[1:nrow(phenoData)] )
	sdEstBreedingValue11 = sapply(fitList, function(x) x$summary.random$individualID$sd[1:nrow(phenoData)]) 
	
	
	# Predict next generation
	
	TrueBreedingValue12 = read.table(file = paste0(part1Results, "/Rep", rep, "/scenario", breedingScenario, "/12thGen/gv.txt"), header = F)[indPredict,1]
	
	CorEbv12 = sapply(fitList, function(x) cor(x$summary.random$individualID$mean[(nrow(phenoData)+1):nIndGenot],TrueBreedingValue12, use = "complete"))
	
	estBreedingValue12 = sapply(fitList, function(x) x$summary.random$individualID$mean[(nrow(phenoData)+1):nIndGenot] )
	sdEstBreedingValue12 = sapply(fitList, function(x) x$summary.random$individualID$sd[(nrow(phenoData)+1):nIndGenot]) 
	

	
}else{
	CorEbv11 = sapply(fitList, function(x) cor(x$summary.random$pedigreeID$mean[phenoData$pedigreeID], phenoData$geneticEffect))
	estBreedingValue11 = sapply(fitList, function(x) x$summary.random$pedigreeID$mean[phenoData$pedigreeID]) 
	sdEstBreedingValue11 = sapply(fitList, function(x) x$summary.random$pedigreeID$sd[phenoData$pedigreeID]) 
	
	
	
	# Predict next generation
	
  TrueBreedingValue12 = read.table(file = paste0( part1Results, "/Rep", rep, "/scenario", breedingScenario, "/12thGen/gv.txt"), header=F )[!sameInd,1]
  
  CorEbv12 = sapply(fitList, function(x) cor(x$summary.random$pedigreeID$mean[indPredictPedigree],TrueBreedingValue12))
  estBreedingValue12 = sapply(fitList, function(x) x$summary.random$pedigreeID$mean[indPredictPedigree]) 
  sdEstBreedingValue12 = sapply(fitList, function(x) x$summary.random$pedigreeID$sd[indPredictPedigree]) 

  }

# DIC 
dicFit = sapply(fitList, function(x) x$dic$dic)
  
# Mode status
modeStatus = sapply(fitList, function(x) x$mode$mode.status)

# Logarithmic score
logScore = sapply(fitList, function(x) -mean(log(x$cpo$cpo)) )

# Posterior variances 

# Residual variance
ResVarMean = sapply(fitList, function(x) inla.emarginal(function(y) 1/y, x$marginals.hyperpar$`Precision for the Gaussian observations`) )
ResVar0.025Median0.975 = sapply(fitList, function(x) inla.qmarginal(p = c(0.025,0.5, 0.975), inla.tmarginal(function(y) 1/y, x$marginals.hyperpar$`Precision for the Gaussian observations`) ) )

# Individual variance
if(useMarkers){
IndVarMean = sapply(fitList, function(x) inla.emarginal(function(y) 1/y, x$marginals.hyperpar$`Precision for individualID` ) )
IndVar0.025Median0.975 = sapply(fitList, function(x) inla.qmarginal(p = c(0.025,0.5, 0.975), inla.tmarginal(function(y) 1/y, x$marginals.hyperpar$`Precision for individualID`) ) )

}else{
	IndVarMean = sapply(fitList, function(x) inla.emarginal(function(y) 1/y, x$marginals.hyperpar$`Precision for pedigreeID` ) )
	IndVar0.025Median0.975 = sapply(fitList, function(x) inla.qmarginal(p = c(0.025,0.5, 0.975), inla.tmarginal(function(y) 1/y, x$marginals.hyperpar$`Precision for pedigreeID`) ) )
}

# Herd effect variance 
HerdVarMean = sapply(fitList[c(2,4,6)], function(x) inla.emarginal(function(y) 1/y, x$marginals.hyperpar$`Precision for herdID`) )
HerdVar0.025Median0.975 = sapply(fitList[c(2,4,6)], function(x) inla.qmarginal(p = c(0.025,0.5, 0.975), inla.tmarginal(function(y) 1/y, x$marginals.hyperpar$`Precision for herdID`) ) )

# Spatial variance 
SpatialVarMean = sapply(spdeList[c(1:4)], function(x) inla.emarginal(function(y) y, x$marginals.variance.nominal$variance.nominal.1))
SpatialVar0.025Median0.975 = sapply(spdeList[c(1:4)], function(x) inla.qmarginal(p = c(0.025,0.5,0.975), x$marginals.variance.nominal$variance.nominal.1))

# Spatial range 
SpatialRangeMean = sapply(spdeList[c(1:4)], function(x) inla.emarginal(function(y) y, x$marginals.range.nominal$range.nominal.1))
SpatialRange0.025Median0.975 = sapply(spdeList[c(1:4)], function(x) inla.qmarginal(p = c(0.025,0.5,0.975), x$marginals.range.nominal$range.nominal.1))

# "Covariate variance"   
VarWbeta = (fitList[[5]]$summary.fixed$mean[2:13] %*% (var(observedCovariateModelMatrix) %*% fitList[[5]]$summary.fixed$mean[2:13]))[1,1]

# Store intercept estimates 
estIntercept = sapply(fitList, function(x) x$summary.fixed[1,] )

# Store covariate estimates from models 5 and 6
estCov5 = fitList[[5]]$summary.fixed
estCov6 = fitList[[6]]$summary.fixed


