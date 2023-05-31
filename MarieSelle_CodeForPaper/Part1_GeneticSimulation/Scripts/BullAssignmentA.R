#Script Name: BullAssignmentA.R
#Author: Owen Powell
#Description: Assign Bulls to Villages


######################## SCENARIO A ########################
######################## SELECT BULLS AND ASSIGN 1 to EACH VILLAGE ########################

VillBull = selectInd(bulls,nInd=nNSBulls,trait=2,use="pheno") #select village bulls from main bull population

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

rm(bindex)
