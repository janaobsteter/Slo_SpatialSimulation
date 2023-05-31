#Script Name: BullAssignmentB.R
#Author: Owen Powell
#Description: Select AI Bulls & Assign NS Bulls to Villages 


######################## SCENARIO B ########################
######################## SELECT AI BULLS ########################
######################## SELECT NS BULLS AND ASSIGN 1 to EACH VILLAGE ########################

AIStud = selectInd(bulls,nInd=nAIBulls,trait=2,use="pheno") #select best Bulls across population to be AI bulls
Bulls = bulls[!bulls@id%in%AIStud@id] #remove AIBulls from main bull pop
VillBull = selectInd(Bulls,nInd=nNSBulls,trait=2,use="pheno") #select village bulls from main bull population
rm(Bulls)

Bull = vector("list",nNSBulls)
names(Bull) = paste0("NSBull",c(1:length(Bull)))
bindex = sample(c(1:nNSBulls),nVillages,replace=F) #randomly sample BullIDs

for (each in (1:nNSBulls)){ #for each NSBull slot
  Bull[[each]] = VillBull[bindex[each]] #assign bull to NSBull
}

for (each in (1:nVillages)){ #for each Village
  Villages[[each]][nFarms+1] = Bull[[each]] #assign NSBull to Village
  names(Villages[[each]])[nFarms+1] = names(Bull)[[each]] #Change Name of Bull Popualtion in each Village to "NSBull"...
}

popList = list(VillBull,AIStud)
Sires = mergePops(popList)

rm(bindex,popList)
