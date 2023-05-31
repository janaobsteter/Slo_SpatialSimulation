#Script Name: AnimalAssignment.R
#Author: Owen Powell
#Description: Assign Animals to Villages & Farms


######################## DISTRIBUTION OF HERD SIZES #######################

if (HerdSizeDistr == "DGEA"){
  ### Raphael Esimates; 1 = 38.9%, 2 = 44.59%, 3 = 12.3%, 4 = 2.44%
  HS = c(1,2,3,4,6,8)
  HerdSizes = sample(size=(nVillages*nFarms),HS,replace=T,prob = c(0.389,0.446,0.123,0.024,0.013,0.005))
  hist(HerdSizes,col="grey",main = "Raph Numbers Sampled HerdSizes") ; table(HerdSizes); summary(HerdSizes); var(HerdSizes)
}


if (HerdSizeDistr == "ZTPois"){

### Zero Truncated Poisson Distribution
  install.packages('countreg',repos="http://R-Forge.R-project.org")
  library('countreg')

  AvgHS = 1.5
  HerdSizes=rztpois(n= (nVillages*nFarms),lambda =AvgHS)
  hist(HerdSizes,col="grey",main = "ZeroTruncated Poisson Sampled HerdSizes") ; table(HerdSizes); as.vector((table(HerdSizes)))/sum(as.vector((table(HerdSizes)))); summary(HerdSizes); var(HerdSizes)
}

######################## CHECK FOR DIFFERENCES IN SAMPLED HERDSIZES & NO. OF SIMULATED COWS ########################

diff = sum(HerdSizes)-length(cows@id)
extra_bulls = initPop[initPop@gender=="M"]; extra_bulls = extra_bulls[!extra_bulls@id%in%bulls@id]
extra_cows = selectInd(extra_bulls,nInd=diff,use="rand"); extra_cows@gender <- rep("F",extra_cows@nInd)
popList = list(cows,extra_cows); cows = mergePops(popList = popList)

rm(extra_bulls,extra_cows)

######################## CREATE LISTS FOR VILLAGES & FARMS ########################

Villages = vector("list",nVillages)
names(Villages) = paste0("Village",c(1:nVillages))

Farms = vector("list",(nVillages*nFarms))
names(Farms) = paste0("Farm",c(1:(nVillages*nFarms)))

######################## ASSIGN COWS TO FARMS ########################

CowOrder = sample(cows@id,size=sum(HerdSizes),replace=F) #randomly sample CowIDs

for (i in (1:length(Farms))){ #For each Farm
  Farms[[i]] = cows[match(CowOrder[c(1:HerdSizes[i])],cows@id)] #Match randomised CowID with CowPopulation, Subset full cow population into Farm Pop & store Farm Pop
  CowOrder = CowOrder[-c(1:HerdSizes[i])] #Remove sampled CowIDs
}


######################## ASSIGN FARMS TO VILLAGES ########################

start=seq(1,(nVillages*nFarms),by=nFarms) #starting index for subsetting Farm list
stop=seq(nFarms,(nVillages*nFarms),by=nFarms)  #Stop index for subsetting Farm list

for (each in (1:nVillages)){ #For each village
  Villages[[each]] = Farms[start[each]:stop[each]] #Assign the next 'nFarms' to the Village
}

