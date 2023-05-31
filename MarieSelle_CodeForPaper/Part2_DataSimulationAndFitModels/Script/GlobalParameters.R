#Script name: GlobalParameters.R
#Author: Maria L. Selle
#Description: Assign global parameters and hyperpriors


nVillages = 100
nHerdsPerVill = 20
nHerds = nVillages*nHerdsPerVill


# Priors specific for each model

# Residual variance
hyperVarResSmall = list(theta=list(prior="pc.prec",param=c(0.15,0.5)))  
hyperVarResLarge = list(theta=list(prior="pc.prec",param=c(0.3,0.5)))  
# Residual variance for models G, H, S, HS, C, CH
hyperResVarAllModels = list(hyperVarResLarge, hyperVarResSmall, hyperVarResSmall,hyperVarResSmall,hyperVarResSmall,hyperVarResSmall)

# Genetic effect markers
hyperGenMar = list(theta = list(prior="pc.prec", param=c(0.1,0.5))) 
# Genetic effect pedigree
hyperGenPed = list(theta = list(prior="pc.prec", param=c(0.1,0.5)))

# Herd effect
hyperHerdSmall = list(theta = list(prior="pc.prec", param=c(0.15,0.5)))
hyperHerdLarge = list(theta = list(prior="pc.prec", param=c(0.25,0.5)))

# Spatial effect
hyperRange = c(0.6, 0.95)

hyperVarSpdeSmall = c(0.1, 0.5) 
hyperVarSpdeLarge = c(0.25, 0.5) 


