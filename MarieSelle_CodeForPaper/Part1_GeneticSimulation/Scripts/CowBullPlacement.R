#Script Name: CowBullPlacement.R
#Author: Owen Powell
#Description: Placement of animals


Villages = vector("list",nVillages)
names(Villages) = paste0("Village",c(1:nVillages))

Farms = vector("list",(nVillages*nFarms))
names(Farms) = paste0("Farm",c(1:(nVillages*nFarms)))

diff = sum(HerdSizes)-length(cows@id)

for (i in (1:diff)){
  HerdSizes[which(HerdSizes==max(HerdSizes))[1]] = HerdSizes[which(HerdSizes==max(HerdSizes))[1]] - 1
}

CowOrder = sample(cows@id,size=sum(HerdSizes),replace=F)
coworder = CowOrder

for (i in (1:length(Farms))){
  #Farms[[i]]@nInd = HerdSizes[i]
  #Farms[[i]] = cows[which(cows@id==coworder[c(1:HerdSizes[i])])]
  Farms[[i]] = cows[match(coworder[c(1:HerdSizes[i])],cows@id)]
  coworder = coworder[-c(1:HerdSizes[i])]
}
  
#### Issue assigning Cows to Farms
#Check to check cows has been subsetted correctly
for (each in (1:(nVillages*nFarms))){
  print(unlist(Farms)[1:(nVillages*nFarms)][[each]]@id)
}

start=seq(1,(nVillages*nFarms),by=2)
stop=seq(2,(nVillages*nFarms),by=2)

for (each in (1:nVillages)){
  Villages[[each]] = Farms[start[each]:stop[each]]
}

#Bulls

VillBull = selectInd(bulls,nInd=nNSBulls,trait=2,use="pheno")

Bull = vector("list",nNSBulls)
names(Bull) = paste0("NSBull",c(1:length(Bull)))
bindex = sample(c(1:nNSBulls),nVillages,replace=F)

for (each in (1:nNSBulls)){
  Bull[[each]] = VillBull[bindex[each]]
}

for (each in (1:nVillages)){
  Villages[[each]][nFarms+1] = Bull[[each]]
  names(Villages[[each]])[nFarms+1] = names(Bull)[[each]]
}

Off_Villages = Villages

for (i in (1:nVillages)){
  for (each in (1:nFarms)){
    crossplan = cbind(Villages[[i]][[each]]@id,rep(Villages[[i]][[nFarms+1]]@id,length(Villages[[i]][[each]]@id)))
    Off_Villages[[i]][[each]] = makeCross2(males=Villages[[i]][[nFarms+1]],females=Villages[[i]][[each]],crossPlan=crossplan,simParam = SP)
  }
}

off = makeCross2(Villages$Village1$Farm1,Villages$Village1$NSBull1,crossPlan=crossplan,simParam = SP)


crossplan = rbind(crossplan,cbind(rep(Villages[[1]][[nFarms+1]]@id,length(Villages[[1]][[1]]@id)),Villages[[1]][[1]]@id))

