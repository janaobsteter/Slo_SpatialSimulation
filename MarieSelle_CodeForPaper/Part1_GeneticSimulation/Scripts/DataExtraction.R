#Script Name: DataExtraction.R
#Author: Owen Powell
#Description: Script to extract Covariate IDs, Pedigree and Genotypes from AlphaSimR Populations

Gen=c(11:12)

for (gen in Gen){
  cat(paste0("Storing generation ",gen,"...\n"))
  cwd = getwd()
  dir.create(paste0(cwd,"/",scenario)); dir.create(paste0(cwd,"/",scenario,"/",gen,"thGen"))
   
  VillageID = vector("list",1)
  FarmID = vector("list",1)
  for (i in 1:nVillages){
    for(j in 1:nFarms){
      writeRecords(pop=Generation[[gen]][[i]][[j]],dir=paste0(cwd,"/",scenario,"/",gen,"thGen"),useQtl = F,includeHaplo = F,append = T,simParam = SP)
      for (k in 1:Generation[[gen]][[i]][[j]]@nInd){
        VillageID = append(VillageID,names(Generation[[gen]][i]))
        FarmID = append(FarmID,names(Generation[[gen]][[i]][j]))
      }
    }
  }
  
  #Remove "Village"/"Farm" string from IDs
  VillageID = VillageID[-1]; VillageID = gsub("Village", "", VillageID)
  FarmID = FarmID[-1]; FarmID = gsub("Farm", "", FarmID)
  
  #Read Info
  info = read.table(file=paste0(cwd,"/",scenario,"/",gen,"thGen/info.txt"),sep=" ",header = T)
  names(info) = c("id","mother","father","VillageID","FarmID") #Rename Columns
  info$VillageID = VillageID; info$FarmID = FarmID #Write Village/FarmIDs to dataframe
  write.table(info,file=paste0(cwd,"/",scenario,"/",gen,"thGen/info.txt"),quote = F,sep=" ",row.names = F)
  
  #write.table(allBulls, file = paste0(cwd,"/",scenario,"/",gen,"thGen/gvBull.txt"),quote = F,sep=" ",row.names = F)
  
  
  write.table(SP$pedigree,file=paste0(cwd,"/",scenario,"/",gen,"thGen/ped.txt"),quote = F, sep=" ",row.names = F)
  cat("DataFiles Written...\n")
  
  
 # save.image(file = paste0("./",scenario,"/",gen,"thGen/BreedingPop.RData"))
 # cat("Breeding Population Saved...")

}
