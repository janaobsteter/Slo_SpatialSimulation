#Script name: RunSimulationStudy.R
#Author: Maria L. Selle
#Description: Script to run breeding simulation part 2


rm(list = ls())


rep = 1 # Replicate number
useMarkers = F # Use pedigree (F) or markers (T)
breedingScenario = "A" # Controls the genetic connectedness, Scenario A corresponds to weak genetic connectedness, scenario B to intermediate genetic connectedness, and scenario C to strong genetic connectedness. 
sigmaHerds = 0.0035 # Controls the spread of herds



part1Results = "../../../Part1_GeneticSimulation/ResultsPart1" # The directory of results from part 1



cat("Loading the required packages and functions ...\n")
source("Script/Functions.R")

# cat("Handling the arguments...\n")
# args = commandArgs(trailingOnly = TRUE)
# #          markers
# # args = c(T)
# if (length(args) < 2) {
#   stop("Must provide arguments!")
# }
# rep = as.numeric(args[1])
# useMarkers = args[2]
# breedingScenario = args[3]
# sigmaHerds = as.numeric(args[4])


scenarioName = paste("Results/", "Rep", rep,"_BreedingScenario", breedingScenario, "_Markers", useMarkers,  sep = "")
createAndSetScenarioFolder(x = scenarioName)



source("../../Script/GlobalParameters.R")

# Simulate genetic part
source("../../Script/SimulateGeneticEffect.R")

# Simulate spatial part 
source("../../Script/SimulateSpatialEffects.R")

# Simulate phenotype
cat("Simulating phenotype...\n")
source("../../Script/SimulatePhenotype.R")

# Prepare for fitting pedigree and genomic relationship matrix
cat("Prepare pedigree or marker data...\n")
source("../../Script/PreparePedigreeAndSnp.R")

# Make formulas
source("../../Script/MakeFormulas.R")

# Make covariates
source("../../Script/MakeObservedCovariates.R")

# Fit model
cat("Fitting models...\n")
source("../../Script/RunInla.R")
source("../../Script/ComputeCorrelations.R")

# Save information from the fit objects and the data
cat("Saving results...\n")
save(CorEbv11,CorEbv12, estBreedingValue11, sdEstBreedingValue11, 
TrueBreedingValue12, estBreedingValue12 , sdEstBreedingValue12 ,phenoData, dicFit, logScore, modeStatus,ResVarMean, ResVar0.025Median0.975, IndVarMean, IndVar0.025Median0.975,HerdVarMean,HerdVar0.025Median0.975, SpatialVarMean, SpatialVar0.025Median0.975, SpatialRangeMean, SpatialRange0.025Median0.975, estIntercept, estCov5, estCov6, VarWbeta, meanGeneticEffect11, sigmaHerds,file  = "Results.RData")
setwd("..")
cat("Done. \n")
