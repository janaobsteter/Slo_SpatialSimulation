#Script Name: ScenarioASelection.R
#Author: Owen Powell
#Description: Replace Cows with Female Calves, Identify Best New Bulls


######################## SCENARIO A ########################
######################## ALWAYS REPLACE OLD COW WITH NEW COWS - WITHIN FARM ########################
######################## REPLACE OLD BULLS WITH WITH BEST NEW BULL - WITHIN VILLAGE ########################

for (i in (1:nVillages)){ #For each Village
  for (each in (1:nFarms)){ #For each Farm within each Village
    if(as.numeric(length(which(Off_Villages[[i]][[each]]@gender=="F")))==Off_Villages[[i]][[each]]@nInd){ # If new offspring in farm are ALL FEMALE
      #print(names(Off_Villages[[i]][each]))
      Villages[[i]][[each]] = Off_Villages[[i]][[each]] #replace old cows with new cows - regardless of phenotype
      #print("Works to Cow Selection on ALL FEMALE Farms")
    }
    else if(as.numeric(length(which(Off_Villages[[i]][[each]]@gender=="M")))==Off_Villages[[i]][[each]]@nInd){ # If new offspring in farm are ALL MALE
      #print(names(Off_Villages[[i]][each]))
      bestPheno = max(Off_Villages[[i]][[each]]@pheno[,2]); bestYBull = which(Off_Villages[[i]][[each]]@pheno[,2]==bestPheno)
      if(Off_Villages[[i]][[each]][bestYBull]@pheno[,2]>=Villages[[i]][[nFarms+1]]@pheno[2]){ 
          Villages[[i]][[nFarms+1]] = Off_Villages[[i]][[each]][bestYBull]
          
          # Store genetic value of new bull
          allBulls = rbind(allBulls, as.numeric(c(Off_Villages[[i]][[each]][bestYBull]@id, Off_Villages[[i]][[each]][bestYBull]@gv[1])  ) )
          
        } #replace old bull with best young bull - selected on trait 2 (h2=0.8)
      #print("Works to Bull Selection on ALL MALE Farms")
    }
    else if(as.numeric((length(which(Off_Villages[[i]][[each]]@gender=="F")))<Off_Villages[[i]][[each]]@nInd)&as.numeric((length(which(Off_Villages[[i]][[each]]@gender=="M")))<Off_Villages[[i]][[each]]@nInd)){ # If new offspring in farm are A MIX OF FE/MALE
      b_index = which(Off_Villages[[i]][[each]]@gender=="M") #Identify Animal Indeces for All Young Bulls
      #print(b_index)
      #print(names(Off_Villages[[i]][each]))
      OCows = Villages[[i]][[each]][b_index] #Use Animal Indeces for All Young Bulls to create Dam Pop
      #print("Works to Old Cow Selection of Mixed Farms")
      for (b in b_index){ #For Each Young Bull within  Farm
        if (Off_Villages[[i]][[each]][b]@pheno[,2]>=Villages[[i]][[nFarms+1]]@pheno[2]){ #If Young Bull has higher phenotype than previous best Bull in Village
          Villages[[i]][[nFarms+1]] = Off_Villages[[i]][[each]][b] # Replace previous Bull with Young Bull - selected on trait 2 (h2=0.8) 
          
          
            allBulls = rbind(allBulls, as.numeric(c(Off_Villages[[i]][[each]][b]@id, Off_Villages[[i]][[each]][b]@gv[1])  ) )

        }
      }
      c_index = which(Off_Villages[[i]][[each]]@gender=="F") #Identify Animal Indeces for All (New Cows) Heifers
      Heifers = Off_Villages[[i]][[each]][c_index] #Use Animal Indeces to create Heifer Pop
      #print("Works to Heifer Selection of Mixed Farms")
      Villages[[i]][[each]] = c(OCows,Heifers); rm(OCows,Heifers,b_index,c_index) #Merge Dam and Heifer Pops to create Farm for next year
    }
  }
}

