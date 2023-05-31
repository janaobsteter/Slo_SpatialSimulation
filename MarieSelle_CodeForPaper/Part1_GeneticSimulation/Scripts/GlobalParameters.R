#Script Name: GlobalParameters.R
#Author: Owen Powell
#Description: Specification of Breeding Population & Trait parameters

# Cleanup
#rm(list = ls())
#dir.create(path = Sys.getenv("R_LIBS_USER"), recursive = TRUE)

#options(repos = list(CRAN="http://cran.rstudio.com/"))

### SetUp Environment ------------------------
#install.packages('devtools',repo="http://cran.rstudio.com/")
#library(devtools)

#install.packages("AlphaSimR")
library(AlphaSimR)

### Population & Genetic Parameters --------------------


nCows = 5000
nAIBulls = 10
nNSBulls = 100
nBulls = nAIBulls + nNSBulls
MatingProp = 0.75
nVillages = 100
nFarms = 20
#HerdSizeDistr = "DGEA" #Herd Size Distribution sampled from small real dataset forn Dairy Genetics East Africa Project
HerdSizeDistr = "ZTPois" #Herd Size Distribution sampled from Zero Truncated Poisson (lambda = 1.5)
nYrs = 11

nHaplotypes = nCows*2
nChr = 10
nQtl = 1000
nSnp = 5000

#Cow Trait
initMeanG1 = 0
initVarG1 = 0.1
initVarEnv1 = 0.9
VarE1 = 0.9

#Sire Trait
initMeanG2 = 0
initVarG2 = 0.8
initVarEnv2 = 0.2
VarE2 = 0.2

