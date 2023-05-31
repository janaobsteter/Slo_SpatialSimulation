#Script name: MakeFormulas.R
#Author: Maria L. Selle
#Description: Make formulas and include prior distributions

formula0 = "phenotypeStd ~ -1 + intercept"

# 1. Genetic effect
if(useMarkers){ # Model genetic dependency using marker information
  
  # Genomic relationship matrix from the markers 
  
  formulaG = paste0(formula0, " + f(individualID, model = 'generic0', Cmatrix = Q,hyper = hyperGenMar)")
}else{ # Model genetic dependency using pedigree
  
  # Relationship matrix from the pedigree
  formulaG = paste0(formula0, " + f(pedigreeID, model = 'generic0', Cmatrix = Ainv,hyper = hyperGenPed)")
}

# 2. Genetic effect + herd effect
formulaH = paste0(formulaG," + f(herdID, model = 'iid', hyper = hyperHerdLarge)" )

# 3. Genetic effect + spatial field
formulaS = paste0(formulaG," +  f(fieldID, model = spdeStat)")

# 4. Genetic effect + herd effect + spatial field
formulaHS = paste0(formulaG," + f(herdID, model = 'iid', hyper = hyperHerdSmall) +  f(fieldID, model = spdeStat)")

# 5. Genetic effect + spatial field + observed covariates 
formulaC = paste0(formulaS, "+ X1 + X2 + X3 + X4 + X5 + X6 + X70 + X71 + X81 + X82 + X9 + X10" )  

# 6. Genetic effect + herd effect + spatial field + observed covariates 
formulaCH = paste0(formulaC, "+ f(herdID, model = 'iid', hyper = hyperHerdSmall)")


formulaG = as.formula(formulaG)
formulaH = as.formula(formulaH)
formulaS = as.formula(formulaS)
formulaHS = as.formula(formulaHS)
formulaC = as.formula(formulaC)
formulaCH = as.formula(formulaCH)

formulas = list(formulaG, formulaH, formulaS, formulaHS, formulaC, formulaCH)
formulaNames = c("G","H","S","HS", "C", "CH")
