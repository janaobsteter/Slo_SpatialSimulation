#Script Name: ScenarioAMating.R
#Author: Owen Powell
#Description: Mate NS Bulls within Villages



######################## SCENARIO A ########################
######################## MATE NS BULLS WITH COWS WITHIN SAME VILLAGE ########################

Off_Villages = Villages #Duplicate Village Structure to store Next Generation

for (i in (1:nVillages)){ #For each Village
  for (each in (1:nFarms)){ #For each Farm within each Village
    crossplan = cbind(Villages[[i]][[each]]@id,rep(Villages[[i]][[nFarms+1]]@id,Villages[[i]][[each]]@nInd)) #build matingplan => NS Bull to each cow in the Farms
    Off_Villages[[i]][[each]] = makeCross2(males=Villages[[i]][[nFarms+1]],females=Villages[[i]][[each]],crossPlan=crossplan,simParam = SP) #generate offspring, nProgeny=1
  }
}
