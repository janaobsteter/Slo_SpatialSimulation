#Script Name: ScenarioBSelection.R
#Author: Owen Powell
#Description: Replace Cows with Female Calves, Identify Best New Bulls within (NS) & across (AI) Villages


######################## SCENARIO B ########################
######################## ALWAYS REPLACE OLD COW WITH NEW COWS - WITHIN FARM ########################
######################## SELECT BEST BULLS WITHIN (NS) & ACROSS (AI) VILLAGES ########################

CandSires = VillBull


### Scan for Bulls 
for (i in (1:nVillages)){ #For each Village
  for (each in (1:nFarms)){ #For each Farm within each Village
    if(as.numeric(length(which(Off_Villages[[i]][[each]]@gender=="M")))==Off_Villages[[i]][[each]]@nInd){ # If new offspring in farm are ALL MALE
      #print(names(Off_Villages[[i]][each]))
      popList = list(CandSires,Off_Villages[[i]][[each]]) #Create List of Old Bulls and Young Bull pops
      CandSires = mergePops(popList); rm(popList) #Add Young Bulls to Offspring Village Bull Pop
      #print("Works to Bull Selection on ALL MALE Farms")
    }
    else if(as.numeric((length(which(Off_Villages[[i]][[each]]@gender=="F")))<Off_Villages[[i]][[each]]@nInd)&as.numeric((length(which(Off_Villages[[i]][[each]]@gender=="M")))<Off_Villages[[i]][[each]]@nInd)){ # If new offspring in farm are A MIX OF FE/MALE
      b_index = which(Off_Villages[[i]][[each]]@gender=="M") #Identify Animal Indeces for All Young Bulls
      #print(b_index)
      #print(names(Off_Villages[[i]][each]))
      popList = list(CandSires,Off_Villages[[i]][[each]][b_index]) #Create List of Old Bulls and Young Bull pops
      CandSires = mergePops(popList); rm(popList) #Add Young Bulls to Offspring Village Bull Pop
      rm(b_index)
    }
  }
}


AIStud = selectInd(CandSires,nInd=nAIBulls,trait = 2,use = "pheno")

for (i in (1:nVillages)){ #For each Village
  #print(names(Off_Villages[[i]]))
  for (each in (1:nFarms)){ #For each Farm within each Village
    if(as.numeric(length(which(Off_Villages[[i]][[each]]@gender=="F")))==Off_Villages[[i]][[each]]@nInd){ # If new offspring in farm are ALL FEMALE
      #print(names(Off_Villages[[i]][each]))
      Villages[[i]][[each]] = Off_Villages[[i]][[each]] #replace old cows with new cows - regardless of phenotype
      #print("Works to Cow Selection on ALL FEMALE Farms")
    }
    else if(as.numeric(length(which(Off_Villages[[i]][[each]]@gender=="M")))==Off_Villages[[i]][[each]]@nInd){ # If new offspring in farm are ALL MALE
      #print(names(Off_Villages[[i]][each]))
      CandNSBulls = Off_Villages[[i]][[each]][!Off_Villages[[i]][[each]]@id%in%AIStud@id]
      if (CandNSBulls@nInd>0){
        bestPheno = max(CandNSBulls@pheno[,2]); bestYBull = which(CandNSBulls@pheno[,2]==bestPheno)
        if(CandNSBulls[bestYBull]@pheno[,2]>=Villages[[i]][[nFarms+1]]@pheno[,2]){ 
          Villages[[i]][[nFarms+1]] = CandNSBulls[bestYBull]
        } #replace old bull with best young bull - selected on trait 2 (h2=0.8)
      }
      rm(CandNSBulls)
      #print("Works to Bull Selection on ALL MALE Farms")
    }
    else if(as.numeric((length(which(Off_Villages[[i]][[each]]@gender=="F")))<Off_Villages[[i]][[each]]@nInd)&as.numeric((length(which(Off_Villages[[i]][[each]]@gender=="M")))<Off_Villages[[i]][[each]]@nInd)){ # If new offspring in farm are A MIX OF FE/MALE
      b_index = which(Off_Villages[[i]][[each]]@gender=="M") #Identify Animal Indeces for All Young Bulls
      #print(b_index)
      #print(names(Off_Villages[[i]][each]))
      OCows = Villages[[i]][[each]][b_index] #Use Animal Indeces for All Young Bulls to create Dam Pop
      #print("Works to Old Cow Selection of MIXED SEX Farms")
      CandNSBulls = Off_Villages[[i]][[each]][b_index][!Off_Villages[[i]][[each]][b_index]@id%in%AIStud@id]
      if (CandNSBulls@nInd>0){
        bestPheno = max(CandNSBulls@pheno[,2]); bestYBull = which(CandNSBulls@pheno[,2]==bestPheno)
        if(CandNSBulls[bestYBull]@pheno[,2]>=Villages[[i]][[nFarms+1]]@pheno[,2]){ 
          Villages[[i]][[nFarms+1]] = CandNSBulls[bestYBull]
        } #replace old bull with best young bull - selected on trait 2 (h2=0.8)
      }
      rm(CandNSBulls)
      #print("Works to Best Young Bull Selection on MIXED SEX Farms")
      c_index = which(Off_Villages[[i]][[each]]@gender=="F") #Identify Animal Indeces for All (New Cows) Heifers
      Heifers = Off_Villages[[i]][[each]][c_index] #Use Animal Indeces to create Heifer Pop
      #print("Works to Heifer Selection of Mixed Farms")
      popList = list(OCows,Heifers)
      Villages[[i]][[each]] = mergePops(popList); rm(OCows,Heifers,b_index,c_index,popList) #Merge Dam and Heifer Pops to create Farm for next year
    }
  }
}

Sires = AIStud 

for (each in (1:nVillages)){ #for each Village
  popList = list(Sires,Villages[[each]][[nFarms+1]])
  Sires = mergePops(popList); rm(popList)
}


# Store genetic value of all new bulls - check that this works
NewVillageBulls = which(  !(as.numeric(Sires@id) %in%  allBulls[,1]) )
allBulls = rbind(allBulls, cbind(as.numeric(Sires@id), as.numeric(Sires@gv))[NewVillageBulls,]  )



