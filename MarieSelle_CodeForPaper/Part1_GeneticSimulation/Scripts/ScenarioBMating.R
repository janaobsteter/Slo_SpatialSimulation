#Script Name: ScenarioBMating.R
#Author: Owen Powell
#Description: Mate NS Bulls within Villages & AI Bulls across all Villages



######################## SCENARIO B ########################
######################## MATE AI BULLS WITH COWS ACROSS ALL VILLAGES ########################
######################## MATE NS BULLS WITH COWS WITHIN SAME VILLAGE ########################

Off_Villages = Villages #Duplicate Village Structure to store Next Generation

Matings = rbinom(sum(HerdSizes),size=1,MatingProp)

AIBullOrder = sample(AIStud@id,size=length(Matings[Matings==0]),replace=T) #randomly sample AIBullIDs
Matings[Matings==0] = AIBullOrder #Assign randomly sampled AIBull ID to NS vs AI Mating scheme
cowCounter = 1
for (i in (1:nVillages)){ #For each Village
  for (each in (1:nFarms)){ #For each Farm within each Village
    crossplan = matrix(nrow=Villages[[i]][[each]]@nInd,ncol=2)
    for (j in (1:Villages[[i]][[each]]@nInd)){
      if (Matings[cowCounter]=="1"){
        crossplan[j,] = cbind(Villages[[i]][[each]][j]@id,Villages[[i]][[nFarms+1]]@id) #build matingplan => NS Bull to cow on the Farm
      }
      else {
        crossplan[j,] = cbind(Villages[[i]][[each]][j]@id,Matings[cowCounter]) #build matingplan => AI Bull to cow on the Farm
      }
      cowCounter = cowCounter + 1
    }
    Off_Villages[[i]][[each]] = makeCross2(males=Sires,females=Villages[[i]][[each]],crossPlan=crossplan,simParam = SP) #generate offspring, nProgeny=1
  }
}
