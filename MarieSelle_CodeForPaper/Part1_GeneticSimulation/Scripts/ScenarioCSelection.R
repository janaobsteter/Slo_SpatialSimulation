#Script Name: ScenarioCSelection.R
#Author: Owen Powell
#Description: Replace Cows with Female Calves, Identify Best New Bulls Across All Villages


######################## SCENARIO C ########################
######################## ALWAYS REPLACE OLD COW WITH NEW COWS - WITHIN FARM ########################
######################## SELECT BEST BULLS REGARDLESS OF AGE - ACROSS VILLAGES ########################

CandSires = VillBull

for (i in (1:nVillages)){ #For each Village
  for (each in (1:nFarms)){ #For each Farm within each Village
    if(as.numeric(length(which(Off_Villages[[i]][[each]]@gender=="F")))==Off_Villages[[i]][[each]]@nInd){ # If new offspring in farm are ALL FEMALE
      #print(names(Off_Villages[[i]][each]))
      Villages[[i]][[each]] = Off_Villages[[i]][[each]] #replace old cows with new cows - regardless of phenotype
      #print("Works to Cow Selection on ALL FEMALE Farms")
    }
    else if(as.numeric(length(which(Off_Villages[[i]][[each]]@gender=="M")))==Off_Villages[[i]][[each]]@nInd){ # If new offspring in farm are ALL MALE
      #print(names(Off_Villages[[i]][each]))
      popList = list(CandSires,Off_Villages[[i]][[each]]) #Create List of Old Bulls and Young Bull pops
      CandSires = mergePops(popList); rm(popList) #Add Young Bulls to Offspring Village Bull Pop
      #print("Works to Bull Selection on ALL MALE Farms")
    }
    else if(as.numeric((length(which(Off_Villages[[i]][[each]]@gender=="F")))<Off_Villages[[i]][[each]]@nInd)&as.numeric((length(which(Off_Villages[[i]][[each]]@gender=="M")))<Off_Villages[[i]][[each]]@nInd)){ # If new offspring in farm are A MIX OF FE/MALE
      b_index = which(Off_Villages[[i]][[each]]@gender=="M") #Identify Animal Indeces for All Young Bulls
      #print(b_index)
      #print(names(Off_Villages[[i]][each]))
      OCows = Villages[[i]][[each]][b_index] #Use Animal Indeces for All Young Bulls to create Dam Pop
      #print("Works to Old Cow Selection of Mixed Farms")
      popList = list(CandSires,Off_Villages[[i]][[each]][b_index]) #Create List of Old Bulls and Young Bull pops
      CandSires = mergePops(popList); rm(popList) #Add Young Bulls to Offspring Village Bull Pop
      c_index = which(Off_Villages[[i]][[each]]@gender=="F") #Identify Animal Indeces for All (New Cows) Heifers
      Heifers = Off_Villages[[i]][[each]][c_index] #Use Animal Indeces to create Heifer Pop
      #print("Works to Heifer Selection of Mixed Farms")
      Villages[[i]][[each]] = c(OCows,Heifers); rm(OCows,Heifers,b_index,c_index) #Merge Dam and Heifer Pops to create Farm for next year
    }
  }
}

VillBull = selectInd(CandSires,nInd = nNSBulls,trait = 2,use="pheno")


# Store genetic value of all new bulls - check that this works
NewVillageBulls = which(!(as.numeric(VillBull@id) %in%  allBulls[,1]))
allBulls = rbind(allBulls, cbind(as.numeric (VillBull@id), as.numeric (VillBull@gv))[NewVillageBulls,]  )
