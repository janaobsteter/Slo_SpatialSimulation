#Script name: SimulateGeneticEffect.R
#Author: Maria L. Selle
#Description: Extract genetic effects from part 1


gv = read.table(paste0(part1Results, "/Rep", rep, "/scenario",breedingScenario,"/11thGen/gv.txt"))
villageTable = read.table(paste0(part1Results,"/Rep", rep, "/scenario",breedingScenario,"/11thGen/info.txt"), header = T)

nInd = nrow(villageTable)
phenoData = data.frame(villageID=villageTable$VillageID, herdID=villageTable$FarmID, individualID = 1:nInd, geneticEffect = gv$V1, pedigreeID = villageTable$id)


