#Script Name: RunSim.R
#Author: Owen Powell
#Description: Script to run breeding simulation part 1


# Cleanup
rm(list = ls())

start_time = Sys.time()



cat("Handling the arguments...\n")
# args = commandArgs(trailingOnly = TRUE)
# Rep = as.integer(args[1])

Rep = 1 # Replicate number


for (rep in Rep){
  
  dir.create(paste0("./ResultsPart1/Rep",rep), recursive = T)
  setwd(paste0("./ResultsPart1/Rep",rep))
  print(getwd())
  ### Run once per Rep ###
  cat("Sourcing global parameters...\n")
  source("../../Scripts/GlobalParameters.R")
  cat("Creating parents...\n")
  source("../../Scripts/CreateParents.R")
  


  save.image(file='Founders.RData')
  

  cat("Assigning animals...\n")
  source("../../Scripts/AnimalAssignment.R")
  
  Generation <- vector(length = nYrs+1, mode = "list")
  Generation[[1]] <- Villages
  
  cat("Storing founders...\n")
  save.image(file='Founders.RData')
  
  
  
  scenario = "scenarioA"
  if (scenario == "scenarioA"){
    allBulls = c()
    source("../../Scripts/BullAssignmentA.R")
    cat("Breeding Scenario A...\n")
    for (yr in 2:(nYrs+1)){
      cat(paste0("Year ",yr," running..."))
      source("../../Scripts/ScenarioAMating.R")
      cat("Mating Completed...")
      source("../../Scripts/ScenarioASelection.R")
      cat("Animals Selected...")
      Generation[[yr]] = Villages
      cat(paste0("Year ",yr," Complete...\n"))
    }
  }
  source("../../Scripts/DataExtraction.R")
  
  cat("Loading old founders...\n")
  load("Founders.RData")
  scenario = "scenarioB"
  
  if (scenario == "scenarioB"){
    allBulls = c()
    source("../../Scripts/BullAssignmentB.R")
    cat("Breeding Scenario B...\n")
    for (yr in 2:(nYrs+1)){
      cat(paste0("Year ",yr," running..."))
      source("../../Scripts/ScenarioBMating.R")
      cat("Mating Completed...")
      source("../../Scripts/ScenarioBSelection.R")
      cat("Animals Selected...")
      Generation[[yr]] = Villages
      cat(paste0("Year ",yr," Complete...\n"))
    }
  }
  source("../../Scripts/DataExtraction.R")
  
  cat("Loading old founders...\n")
  load("Founders.RData")
  scenario = "scenarioC"
  if (scenario == "scenarioC"){
    allBulls = c()
    source("../../Scripts/BullAssignmentC.R")
    cat("Breeding Scenario C...\n")
    for (yr in 2:(nYrs+1)){
      cat(paste0("Year ",yr," running..."))
      source("../../Scripts/ScenarioCMating.R")
      cat("Mating Completed...")
      source("../../Scripts/ScenarioCSelection.R")
      cat("Animals Selected...")
      Generation[[yr]] = Villages
      cat(paste0("Year ",yr," Complete...\n"))
    }
  }
  source("../../Scripts/DataExtraction.R")
}

stop_time = Sys.time()
run_time = stop_time-start_time; cat(paste0(run_time," to run"))





