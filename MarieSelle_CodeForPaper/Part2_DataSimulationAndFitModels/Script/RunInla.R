#Script name: RunInla.R
#Author: Maria L. Selle
#Description: Run INLA 


# Make SPDE object
spdeStatSmall=inla.spde2.pcmatern(mesh=mesh,alpha = 2, prior.range = hyperRange, prior.sigma = hyperVarSpdeSmall) 
meshIndexSmall = inla.spde.make.index(name="fieldID",n.spde = spdeStatSmall$n.spde, n.repl = 1)

spdeStatLarge=inla.spde2.pcmatern(mesh=mesh,alpha = 2, prior.range = hyperRange, prior.sigma = hyperVarSpdeLarge) 
meshIndexLarge = inla.spde.make.index(name="fieldID",n.spde = spdeStatLarge$n.spde, n.repl = 1)



AField = inla.spde.make.A(mesh = mesh, loc = cbind(phenoData$xHerd, phenoData$yHerd) )

# Collect all relevant data in stack, one  for each SPDE model
stackSmall = inla.stack(data = list(phenotypeStd = phenoData$phenotypeStd), A = list(AField,1),effects =list(c(meshIndexSmall, list(intercept = 1)), c(list(herdID = phenoData$herdID), list(individualID = phenoData$individualID),  list(pedigreeID = phenoData$pedigreeID), list(X1 = observedCovariateModelMatrix[,1]) , list(X2 =  observedCovariateModelMatrix[,2]) , list(X3 =  observedCovariateModelMatrix[,3]) , list(X4 =  observedCovariateModelMatrix[,4]) , list(X5 =  observedCovariateModelMatrix[,5])  , list(X6 =  observedCovariateModelMatrix[,6]) , list(X70 =  observedCovariateModelMatrix[,7]), list(X71 =  observedCovariateModelMatrix[,8]) , list(X81 =  observedCovariateModelMatrix[,9]), list(X82 =  observedCovariateModelMatrix[,10]) , list(X9 = observedCovariateModelMatrix[,11]), list(X10 = observedCovariateModelMatrix[,12]))),tag = "est") 

stackLarge = inla.stack(data = list(phenotypeStd = phenoData$phenotypeStd), A = list(AField,1),effects =list(c(meshIndexLarge, list(intercept = 1)), c(list(herdID = phenoData$herdID), list(individualID = phenoData$individualID),list(pedigreeID = phenoData$pedigreeID), list(X1 = observedCovariateModelMatrix[,1]) , list(X2 =  observedCovariateModelMatrix[,2]) , list(X3 =  observedCovariateModelMatrix[,3]) , list(X4 =  observedCovariateModelMatrix[,4]) , list(X5 =  observedCovariateModelMatrix[,5])  , list(X6 =  observedCovariateModelMatrix[,6]) , list(X70 =  observedCovariateModelMatrix[,7]), list(X71 =  observedCovariateModelMatrix[,8]) , list(X81 =  observedCovariateModelMatrix[,9]), list(X82 =  observedCovariateModelMatrix[,10]) , list(X9 = observedCovariateModelMatrix[,11]), list(X10 = observedCovariateModelMatrix[,12]))),tag = "est") 



# Fit models with inla 
fitList = list()
spdeList = list()
teller = 0
for(i in 1:length(formulaNames)){
  
  cat(paste0("Fitting model ",formulaNames[i] ,"...\n"))
  
  if(formulaNames[i]=="S"){
    stack = stackLarge
    spdeStat = spdeStatLarge
  }else{
    stack = stackSmall
    spdeStat = spdeStatSmall
  }
  
  fitList[[i]] = inla(formula = formulas[[i]], data = inla.stack.data(stack),
                      family = "normal", control.predictor =list(A=inla.stack.A(stack),compute = T),
                      control.family=list(list(hyper=hyperResVarAllModels[[i]])),
                      control.compute = list(dic=T,cpo=F), verbose=F) # Computing the cpo requires much memory
  
  if(fitList[[i]]$mode$mode.status > 0){
    cat(paste0("Rerunning model ",formulaNames[i] ,"..."))
    fitList[[i]] = inla.rerun(fitList[[i]])
  }
  
  if(formulaNames[i] =="S" || formulaNames[i] =="HS" || formulaNames[i] =="C" || formulaNames[i] =="CH"){
    teller = teller +1
    spdeList[[teller]] = inla.spde.result(fitList[[i]],"fieldID", spdeStat)
  }

}

