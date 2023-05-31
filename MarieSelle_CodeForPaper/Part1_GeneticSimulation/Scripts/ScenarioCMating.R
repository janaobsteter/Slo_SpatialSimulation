#Script Name: ScenarioCMating.R
#Author: Owen Powell
#Description: Mate NS Bulls randomly to Dams across all Villages



######################## SCENARIO C ########################
######################## MATE NS BULLS RANDOMLY WITH COWS ACROSS ALL VILLAGES ########################

Off_Villages = Villages #Duplicate Village Structure to store Next Generation

nMatings = nVillages/nNSBulls

NSBullOrder = sample(VillBull@id,size=nVillages,replace=F) #randomly sample NSBullIDs

for (i in (1:nVillages)){ #For each Village
  for (each in (1:nFarms)){ #For each Farm within each Village
    crossplan = cbind(Villages[[i]][[each]]@id,rep(VillBull@id[i],Villages[[i]][[each]]@nInd)) #build matingplan => NS Bull to each cow in the Farms
    Off_Villages[[i]][[each]] = makeCross2(males=VillBull[i],females=Villages[[i]][[each]],crossPlan=crossplan,simParam = SP) #generate offspring, nProgeny=1
  }
}

