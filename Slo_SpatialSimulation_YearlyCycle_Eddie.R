# Set the working directory
#setwd("~/Documents/1Projects/SIMplyBee_devel/Spatial/")
# Load packages
library(SIMplyBee)
library(tictoc)
library(R6)
library(dplyr)
library(tidyr)
library(Matrix)
library(AGHmatrix)
library(tidyverse)
library(INLA)

xMin = 115000; xMax = 125000; yMin = 5000; yMax = 30000
rangeProportionChar = "SLO"
spatialVarProportionChar = "1/3"
spatialInput = 1

args = commandArgs(trailingOnly=TRUE)
xMin = as.integer(args[1])
xMax = as.integer(args[2])
yMin = as.integer(args[3])
yMax = as.integer(args[4])
spatialInput = as.logical(as.integer(args[3]))
#xMin = 115000; xMax = 125000; yMin = 5000; yMax = 30000
rangeProportionChar = as.character(args[3]) # Either proportional to the area or  (meaning whole Slovenija)
spatialVarProportionChar = as.character(args[4]) # Proportion of the varA
if (nchar(spatialVarProportionChar) == 1) {
  spatialVarProportion <- as.integer(spatialVarProportionChar)
} else {
  spatialVarProportion = as.integer(strsplit(spatialVarProportionChar, "/")[[1]][[1]]) /
    as.integer(strsplit(spatialVarProportionChar, "/")[[1]][[2]])
}
spatialInput = as.logical(as.integer(args[5]))


# Define functions
maintainPopulationSize <- function(age0 = NULL, age1 = NULL, popSize = NULL) {
  if ((nColonies(age0) + nColonies(age1)) > popSize) { # check if the sum of all colonies is greater than apiary size
    IDsplits <- getId(age0)[hasSplit(age0)] # get the IDs of age 0 that are splits
    splits0 <- pullColonies(age0, ID = IDsplits) # pull the splits out of age 0
    age0split <- splits0$pulled # create an object for age 0 splits
    age0swarm <- splits0$remnant # create an object for swarms and superseded colonies
    age0needed <- popSize - nColonies(age1) # calculate the number of age 0 colonies that are needed to fill up the apiary
    splitsNeeded <- age0needed - nColonies(age0swarm) # calculate the number of splits needed
    if (age0needed <= nColonies(age0swarm)) { # check if the number of age 0 colonies needed is lower or equal to age 0 swarms
      swarmID <- sample(getId(age0swarm), age0needed) # if yes, select the ids of swarms that will stay in apiary
      swarmTMP <- pullColonies(age0swarm, ID = swarmID) # pull out those selected age0 swarms
      age0 <- swarmTMP$pulled # put selected swarms to age 0 object
    } else if (age0needed > nColonies(age0swarm)) { # in case when age 0 needed is grater than number of swarm select splits
      nSplitsNeeded <- age0needed - nColonies(age0swarm) # calculate the number of splits needed
      splitId <- sample(getId(age0split), nSplitsNeeded) # select ids of splits
      splitTmp <- pullColonies(age0split, ID = splitId) # pull the splits
      splits <- splitTmp$pulled # select pulled splits
      age0 <- c(age0swarm, splits) # combine splits and swarms in age 0 object
    }
    return(age0)
  }
}

sampleBeekeepersLocation <- function(locDF, beekeeper = NULL, currentLocation = NULL, excludeCurrentLoc = FALSE, n = 1, replace = TRUE) {
  beekeeperLoc <- locDF[(locDF$Beekeeper %in% beekeeper), ]
  beekeeperLocList <- Map(c, beekeeperLoc$X_COORDINATE, beekeeperLoc$Y_COORDINATE)
  if (excludeCurrentLoc) {
    if (length(beekeeperLocList) > 1) {
      beekeeperLocList <- beekeeperLocList[!beekeeperLocList %in% list(c(currentLocation))]
    }
  }
  ret <- beekeeperLocList[sample(1:length(beekeeperLocList), size = n, replace = replace)]
}

setBeekeeper <- function(x, beekeeper = NULL) {
  if (isColony(x)) {
    x@misc$Beekeeper <- as.character(beekeeper[1])
  } else if (isMultiColony(x)) {
    for (c in 1:nColonies(x)) {
      x[[c]] <- setBeekeeper(x = x[[c]], beekeeper = beekeeper[c])
    }
  }
  return(x)
}

getBeekeeper <- function(x) {
  if (isColony(x)) {
    ret <- as.character(x@misc$Beekeeper)
  } else if (isMultiColony(x)) {
    ret <- sapply(1:nColonies(x), FUN = function(z) getBeekeeper(x[[z]]))
  }
  names(ret) <- getId(x)
  return(ret)
}

getBeekeepersColonies <- function(multicolony, beekeeper, size = NULL) {
  if (is.null(size)) {
    size <- nColonies(multicolony)
  }
  bk <- getBeekeeper(multicolony)
  coloniesIDs <- names(bk)[bk == beekeeper]
  return(multicolony[sample(coloniesIDs, size = size, replace = FALSE)])
}

getBeekeepersColoniesID <- function(multicolony, beekeeper, size = NULL) {
  if (is.null(size)) {
    size <- nColonies(multicolony)
  }
  bk <- getBeekeeper(multicolony)
  coloniesIDs <- names(bk)[bk == beekeeper]
  return(sample(coloniesIDs, size = size, replace = FALSE))
}

reQueenWithBeekeepersDonor <- function(virginMultiColony, donorMultiColony) {
  # Sample a colony for each beekeeper to get virgin queens from
  # First choose one colony for each beekeeper
  bkDonorColony <- sapply(unique(getBeekeeper(virginMultiColony)),
                          FUN = function(x) getBeekeepersColoniesID(donorMultiColony, beekeeper = x, size = 1))

  splitVirginQueens <- list()
  for (splitCol in getId(virginMultiColony)) {
    donorColonyId <- bkDonorColony[getBeekeeper(virginMultiColony[[as.character(splitCol)]])]
    splitVirginQueens <- c(splitVirginQueens, createVirginQueens(donorMultiColony[[donorColonyId]], nInd = 1))
  }

  # Requeen the splits --> queens are now 0 years old --> I think I need to requeen after each even when I still know "mother" colonies
  virginMultiColony <- reQueen(virginMultiColony, queen = mergePops(splitVirginQueens))
  return(virginMultiColony)
}

computeSpatialPheno <- function(colonies, locDF, trait = 1, beekeepers, yearEff, resVar) {
  myMapCasteToColonyGv <- function(colony) {
    yield <- mapCasteToColonyGv(colony,
                                queenTrait = trait,
                                queenFUN = function(x) x,
                                workersTrait = NULL,
                                checkProduction = FALSE)
  }
  # Compute the phenotype of the colony
  gv <- data.frame(Gv = calcColonyGv(x = colonies, FUN = myMapCasteToColonyGv)) %>%
    dplyr::mutate(colonyID = rownames(.))

  # Get location and beekeeper of the colonies
  locMultiColony <- as.data.frame(getLocation(colonies, collapse = TRUE)) %>%
    dplyr::mutate(colonyID = rownames(.)) %>%
    rename(X_COORDINATE = V1, Y_COORDINATE = V2)
  beekeeperMultiColony <- data.frame(Beekeeper = getBeekeeper(colonies)) %>%
    dplyr::mutate(colonyID = rownames(.))

  gvLoc <- list(gv, locMultiColony, beekeeperMultiColony) %>%
    reduce(full_join, by = "colonyID") %>%
    dplyr::mutate(Beekeeper = as.integer(Beekeeper))

  gvLocSpatial <- list(gvLoc, locDF) %>%
    reduce(left_join, by = c("X_COORDINATE", "Y_COORDINATE", "Beekeeper")) %>%
    list(., beekeepers) %>% reduce(left_join, by = "Beekeeper") %>%
    dplyr::mutate(Residual = sampleEffect(n = nrow(.), var = resVar)) %>%
    dplyr::mutate(FullPheno = Gv + BeekeeperEffect + c(yearEff) + BeekeeperYearEffect + SpatialEffect + Residual)
  return(gvLocSpatial) #%>% select(colonyID, Gv, FullPheno, Beekeeper, SpatialEffect, BeekeeperEffect))
}

sampleEffect = function(n, var) {
  cbind(rnorm(n = n, sd = sqrt(var)))
}

# Prepare recording function
data_rec <- function(datafile, colonies, year) {
  queens = mergePops(getQueen(colonies))
  location = getLocation(colonies, collapse = TRUE)
  datafile = rbind(datafile,
                   data.frame(colonies             = c(deparse(substitute(colonies))),
                              year                 = c(year),
                              colonyID             = getId(colonies),
                              queenID              = getCasteId(colonies, caste = "queen", collapse = TRUE),
                              locationX            = location[,1],
                              locationY            = location[,2],
                              nFathers             = nFathers(queens),
                              nDPQ                 = sapply(getFathers(queens), function(x) length(unique(x@mother))),
                              nCsdAlColony         = sapply(colonies@colonies, function(x) nCsdAlleles(x, collapse = TRUE)),
                              nCsdApiary           = rep(nCsdAlleles(colonies, collapse = TRUE), queens@nInd),
                              pHomBrood            = calcQueensPHomBrood(queens),
                              gvQueens_QueenTrait  = sapply(getGv(colonies, caste = "queen"), function(x) x[1,1])
                   ))}

pheno_rec <- function(phenofile, colonies, year, locDF, trait = 1, beekeepers, yearEff, resVar) {
  phenofile <- rbind(phenofile,
                     computeSpatialPheno(colonies = colonies,
                                         locDF = locDF,
                                         trait = trait,
                                         beekeepers = beekeepers,
                                         yearEff = yearEff, resVar = resVar) %>%  dplyr::mutate(Year = year))
}

# Locations - x, y, and the beekeper
locAll <- read.csv("Data/SLOLocations_standardised.csv")
print(paste0("Number of all locations is ", nrow(locAll)))
nColoniesPerLocation <- 5 #In reality, it's 15
#locAll$Beekeeper <- as.factor(locAll$Beekeeper)
#ggplot(data = locAll, aes(x = X_COORDINATE, Y_COORDINATE)) + geom_point() + geom_hline(aes(yintercept = 75000, col = "red"))+ geom_hline(aes(yintercept = 74700, col = "red"))

# Sample locations for testing from one region - so they are close together - but have 5 colonies at each location
loc <- locAll[(locAll$X_COORDINATE > xMin) & (locAll$X_COORDINATE < xMax) & (locAll$Y_COORDINATE > yMin)& (locAll$Y_COORDINATE < yMax) ,]
loc <- loc[!is.na(loc$Beekeeper),]

print(paste0("Number of locations is ", nrow(loc)))
locList <- rep(Map(c, loc$X_COORDINATE, loc$Y_COORDINATE), nColoniesPerLocation)
colonyBeekeeper <- rep(loc$Beekeeper, nColoniesPerLocation)

# Founder population parameters -------------------------------------------------------------------
nColonies = nrow(loc) * nColoniesPerLocation              # Number of colonies in Slovenia, using a smaller number for now
print(paste0("Number of colonies is ", nColonies))
nFounderQueens = nColonies                                # How many queens do we need to start colonies
nFounderDPQ <- round(nFounderQueens * 0.25)                      # How many queens do we need to create initial drones
nFounders <- nFounderQueens + nFounderDPQ                 # How many founders all together
nChr = 1                                                  # Number of chromosomes
nDronesPerQueen = 100                                     # Number of drones created per founder DPQ
nSegSites = 2000                                          # Number of segregating sites per chromosome
nQtlPerChr = 2000                                          # Number of QTLs per chromosome

# Population parameters -------------------------------------------------------------------
nRep <- 1                     # Number of repeats
nYear <- 20                   # Number of years
popSize <- 20000              # Number of queen colonies
noWorkers <- 10                # Number of workers in a full colony
noDrones <- 1                  # Number of drones in a full colony (typically nWorkers * 0.2 (not in the example))
noFathers <- nFathersPoisson   # Number of drones the queen mates with (could also be a function)
noVirginQueens <- 1            # Number of created virgin queens
spatialMating <- spatialInput  # Whether to implement random (FALSE) or spatial (TRUE) mating
matingRange <- 5000            # Range of mating in metres!

# Period parameters -------------------------------------------------------------------
# Period 1 (spring)
p1swarm <- 0.05              # Percentage of colonies that swarm in period 1
p1supersede <- 0.05          # Percentage of colonies that supersede in period 1
p1collapse <- 0.10           # Percentage of colonies that  collapse in period 1

# Period2 (summer)
p2swarm <- 0.01              # Percentage of colonies that swarm in period 2
p2supersede <- p1supersede   # Percentage of colonies that supersede in period 2
p2collapse <- p1collapse     # Percentage of colonies that collapse in period 2

# Period3 (winter)
p3collapseAge0 <- 0.25      # Percentage of age 0 colonies that collapse in period 3
p3collapseAge1 <- 0.3       # Percentage of age 2 colonies that collapse in period 3

# Trait parameters - this is for honey yield based on Slovenian data from 2022
h2 <- 0.25
mean <- 20
phenoVar <- 175
varA <- phenoVar * h2
nonAVar <- phenoVar - varA
beekeeperYearVar <- 1/3 * nonAVar
beekeeperVar <- 1/3 * beekeeperYearVar
yearVar <- 1/3 * beekeeperYearVar
beekeeperYearVar <- 1/3 * beekeeperYearVar
spatialVar <- spatialVarProportion * nonAVar
residualVar <- 1/3 * nonAVar


# Create a directory
dirName <- paste0(ifelse(spatialMating, "Spatial", "Random"), "_NoLoc_", nrow(loc), "_SpVar_",
                  gsub("/", "_", spatialVarProportionChar), "_Range_", gsub("/", "_", rangeProportionChar))
dir.create(dirName)
setwd(dirName)

# Create data frames for recording the number of age0 and age1 colonies, csd variability and for recording cpu time
loopTime <- data.frame(Rep = NA, tic = NA, toc = NA, msg = NA, time = NA)
functionsTime <- data.frame(Function = NA, Rep = NA, Year = NA, Period = NA, nColonies = NA, Time = NA)
write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

# Start of the rep-loop ---------------------------------------------------------------------
for (Rep in 1:nRep) {
  colonyRecords = NULL
  phenoRecords <- NULL
  # Rep <- 1 (you can use this to check if your code is running alright for one whole loop)
  cat(paste0("Rep: ", Rep, "/", nRep, "\n"))
  tic(paste0(nYear, 'y loop'))         # Measure cpu time


  # Simulate the spatial effects ---------------------------------------------------------
  # Create a mesh
  if (rangeProportionChar == "SLO") {
    range = bound.outer = diff(range(locAll$X_COORDINATE))/3 #This is range
    max.edge = range / 5
    mesh = inla.mesh.2d(loc=cbind(locAll$X_COORDINATE, locAll$Y_COORDINATE),
                        max.edge = c(1,2)*max.edge,
                        # - use 2 times max.edge in the outer extension/offset/boundary
                        cutoff = max.edge/5,
                        offset = c(max.edge, bound.outer))
  } else {
    tmp <- strsplit(rangeProportionChar, "/")[[1]]
    if (length(tmp) == 1) {
      rangeProportion = as.integer(rangeProportionChar)
    } else {
      rangeProportion = as.integer(tmp[[1]]) / as.integer(tmp[[2]])
    }

    range = bound.outer = diff(range(loc$X_COORDINATE)) * rangeProportion
    max.edge = range / 5
    mesh = inla.mesh.2d(loc=cbind(loc$X_COORDINATE, loc$Y_COORDINATE),
                        max.edge = c(1,2)*max.edge,
                        # - use 2 times max.edge in the outer extension/offset/boundary
                        cutoff = max.edge/5,
                        offset = c(max.edge, bound.outer))
  }

  # Set up parameters (we don't know what a true range is!)
  sigma.u = sqrt(spatialVar)
  range = bound.outer
  kappa = sqrt(8)/range
  inla.seed = sample.int(n=1E6, size=1)

  # Simulate spatial field (priors are not used for simulation, so just plugging in 0.5)
  spde = inla.spde2.pcmatern(mesh, prior.range = c(.5, .5), prior.sigma = c(.5, .5))
  Qu = inla.spde.precision(spde, theta=c(log(range), log(sigma.u)))
  u = inla.qsample(n=1, Q=Qu, seed = inla.seed)
  u = u[ ,1] #Spatial effects

  # Obtain spatial effect for the locations
  if (rangeProportionChar == "SLO") {
    A = inla.spde.make.A(mesh=mesh, loc=as.matrix(locAll[,c("X_COORDINATE", "Y_COORDINATE")]))
    u = drop(A %*% u)
    locAll$SpatialEffect <- u
    loc <- left_join(loc, locAll)
  } else {
    A = inla.spde.make.A(mesh=mesh, loc=as.matrix(loc[,c("X_COORDINATE", "Y_COORDINATE")]))
    u = drop(A %*% u)
    loc$SpatialEffect <- u
  }


  # Add to the location table
  write.csv(loc, paste0("SpatialEffect_", Rep, ".csv"), quote=FALSE, row.names=FALSE)

  # Sample the beekeeper effect
  beekeepersID <- unique(loc$Beekeeper)
  beekeeperEffect <- sampleEffect(n = length(beekeepersID), var = beekeeperVar)
  beekeepers <- data.frame(Beekeeper = beekeepersID,
                           BeekeeperEffect = beekeeperEffect)


  # Founder population ---------------------------------------------------------
  # Using simulateHoneyBeeGenomes is a great way to get a basic founder gene pool
  # founderGenomes <- simulateHoneyBeeGenomes(nMelN = nMelN,
  #                                           nCar = nCar,
  #                                           nChr = nChr,
  #                                           nSegSites = nSegSites)
  #
  # save(founderGenomes, file="founderGenomes_ThreePop.RData")
  # Or you can load your previously made founder population
  # print("Loading in the founderData")
  # load("FounderGenomes_ThreePop_16chr.RData")
  # load("~/Desktop/GitHub/lstrachan_honeybee_sim/YearCycleSimulation/PlottingData/FounderGenomes_ThreePop_16chr.RData")
  # STEP 2: Create SP object and write in the global simulation/population parameters
  print("Simulate the founders")
  founderGenomes <- quickHaplo(nInd = nFounders, nChr = nChr, segSites = nSegSites)
  print("Done simulating the founders")
  SP <- SimParamBee$new(founderGenomes, csdChr = ifelse(nChr >= 3, 3, 1), nCsdAlleles = 128)
  SP$nWorkers <- noWorkers
  SP$nDrones <- noDrones
  SP$nFathers <- noFathers
  SP$nVirginQueens <- noVirginQueens
  SP$swarmP <- 0.5                # Probability of swarming? (TODO: double check this)
  SP$splitP <- 0.3
  SP$setTrackPed(TRUE)            # Track the pedigree
  SP$setTrackRec(TRUE)            # Track the recombination
  #SP$addSnpChip(nSnpPerChr = 1000)   # Add a SNP chip with 1000 SNPs per chromosome
  csdChr <- SP$csdChr             # define csd chromomsome

  # Add trait - simulating only queen trait for now!
  mean <- mean
  varA <- varA
  SP$addTraitA(nQtlPerChr = nQtlPerChr, mean = mean, var = varA,
               name = c("honeyYield"))

  # Set residual variance (this is not environmental variance!)
  SP$setVarE(varE = residualVar)

  # STEP 3: Set up your base population
  print("Create the base virgin queens")
  # Create a base population - for now, we simulate all the founder but this will not the case with larger numbers
  virginQueens <- createVirginQueens(x = founderGenomes)
  print("Create base drones")
  # Create drones
  drones <- createDrones(x = virginQueens[1:nFounderDPQ], nInd = nDronesPerQueen)
  # Get fathers for Mel, MelCross and Car
  #fathers <- pullDroneGroupsFromDCA(drones$Mel, n = nInd(virginQueens$Mel[1:apiarySize]), nDrones = nFathersPoisson)

  # Mate virgin queens with fathers to make them queens
  print("Mate base queens")
  start = Sys.time()
  Rprof(filename = "Cross.out", memory.profiling = TRUE)
  queens <-  SIMplyBee::cross(x = virginQueens[(nFounderDPQ + 1): nFounders],
                              drones = drones,
                              crossPlan = "create",
                              spatial = FALSE)
  Rprof(NULL)
  end = Sys.time()
  print("Done creating base population")
  functionsTime <- rbind(functionsTime,
                         c(Function = "CrossInitialVirginQueens", Rep = Rep, Year = 0, Period = "0", nColonies = nInd(queens), Time = difftime(end, start, units = "secs")))
  write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

  #Compute base AF
  baseAF <- calcBeeAlleleFreq(x = getQtlGeno(queens),
                              sex = queens@sex)
  # baseAF <- calcBeeAlleleFreq(x = getSnpGeno(queens),
  #                             sex = queens@sex)


  # Start the year-loop ------------------------------------------------------------------
  for (year in 1:nYear) {
    print("Starting the cycle")
    cat(paste0("Year: ", year, "/", nYear, "\n"))

    # If this is the first year, create some colonies to start with
    if (year == 1) {
      print("Creating initial colonies")
      start = Sys.time()
      Rprof(filename = "CreateMultiColony.out", memory.profiling = TRUE)
      age1 <- createMultiColony(x = queens)
      Rprof(NULL)
      end = Sys.time()
      print(difftime(end, start, units = "secs"))
      print("Done creating initial colonies")
      functionsTime <- rbind(functionsTime,
                             c(Function = "CreateInitialColonies", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = difftime(end, start, units = "secs")))
      write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)


      # Set Location
      print("Setting initial location")
      start = Sys.time()
      age1 <- setLocation(age1, location = locList)
      end = Sys.time()
      print(difftime(end, start, units = "secs"))
      print("Done setting the location")
      functionsTime <- rbind(functionsTime,
                             c(Function = "SetInitialLocation", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = difftime(end, start, units = "secs")))
      write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

      print("Setting the initial beekeeper")
      start = Sys.time()
      # Set the beekeeper to the colonies
      for (colony in 1:nColonies(age1)) {
        age1[[colony]]@misc$Beekeeper <- colonyBeekeeper[colony]
      }
      end = Sys.time()
      print("Done setting the beekeeper")
      functionsTime <- rbind(functionsTime,
                             c(Function = "SetInitialBeekeeper", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = difftime(end, start, units = "secs")))
      write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)


      print("Record initial colonies")
      start = Sys.time()
      colonyRecords <- data_rec(datafile = colonyRecords, colonies = age1, year = year)
      end = Sys.time()
      print("Done recording")
      functionsTime <- rbind(functionsTime,
                             c(Function = "RecordInitialColonies", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = difftime(end, start, units = "secs")))
      write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

      # Compute the relationship matrix
      print("Computing GRM")
      start = Sys.time()
      queenGeno <- getQtlGeno(getQueen(age1, collapse = TRUE))
      #queenGeno <- getSnpGeno(getQueen(age1, collapse = TRUE))
      Gmatrix <- calcBeeGRMIbs(x = queenGeno,
                               sex = rep("F", nColonies(age1)),
                               alleleFreq = baseAF)
      end = Sys.time()
      print("Done recording")
      functionsTime <- rbind(functionsTime,
                             c(Function = "ComputeGRM", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = difftime(end, start, units = "secs")))
      write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

      # Save the Gmatrix
      dir.create("RData")
      save(Gmatrix, file = paste0("RData/GRMInitial_", year, ".Rdata"))

      # If not, promote the age0 to age1, age1 to age2 and remove age2 colonies
    } else {
      age2 <- age1
      age1 <- age0
      age0 <- NULL
      age0p1 <- NULL
      age0p2 <- NULL
    }

    # Period1 ------------------------------------------------------------------
    print("PERIOD 1")
    # Build-up the colonies
    print(paste0("Building up the colonies to ", noWorkers, " and ", noDrones))
    start = Sys.time()
    age1 <- buildUp(age1)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "BuildUp", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    if (year > 1) {
      age2 <- buildUp(age2)
    }

    # Split all age1 colonies
    print("Splitting the colonies")
    start = Sys.time()
    tmp <- SIMplyBee::split(age1)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Split", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)
    age1 <- tmp$remnant

    # Set the location of the splits - if the beekeeper has another location, it samples another one
    # If the beekeeper has only one location, it samples the same one
    start = Sys.time()
    newSplitLoc <- sapply(1:nColonies(age1),
                          FUN = function(x) sampleBeekeepersLocation(locDF = loc, beekeeper = getBeekeeper(age1[[x]]),
                                                                     currentLocation = getLocation(age1[[1]]), n = 1,
                                                                     excludeCurrentLoc = TRUE))
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "SampleSplitLocations", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    start = Sys.time()
    tmp$split <- setLocation(tmp$split, location = newSplitLoc)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "SetLocation", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(tmp$split), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    # Set the beekeeper of the splits - same beekeeper
    start = Sys.time()
    tmp$split <- setBeekeeper(tmp$split, beekeeper = getBeekeeper(age1))
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "SetBeekeeper", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(tmp$split), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)


    # Requeen with a donor colony of a beekeeper
    start = Sys.time()
    tmp$split <- reQueenWithBeekeepersDonor(virginMultiColony = tmp$split, donorMultiColony = age1)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "RequeenSplits", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(tmp$split), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    # The queens of the splits are 0 years old
    age0p1 <- tmp$split

    if (year > 1) {
      # Split all age2 colonies
      tmp <- split(age2)
      age2 <- tmp$remnant
      # Set the location of the splits - if the beekeeper has another location, it samples another one
      # If the beekeeper has only one location, it samples the same one
      newSplitLoc <- sapply(1:nColonies(age2),
                            FUN = function(x) sampleBeekeepersLocation(locDF = loc, beekeeper = getBeekeeper(age2[[x]]),
                                                                       currentLocation = getLocation(age2[[1]]), n = 1,
                                                                       excludeCurrentLoc = TRUE))
      tmp$split <- setLocation(tmp$split, location = newSplitLoc)
      tmp$split <- setBeekeeper(tmp$split, beekeeper = getBeekeeper(age2))
      tmp$split <- reQueenWithBeekeepersDonor(virginMultiColony = tmp$split, donorMultiColony = age2)
      # The queens of the splits are 0 years old
      age0p1 <- c(age0p1, tmp$split)
    }

    # Swarm a percentage of age1 colonies
    start = Sys.time()
    print("Swarm colonies, P1")
    tmp <- pullColonies(age1, p = p1swarm)
    age1 <- tmp$remnant
    if (nColonies(tmp$pulled) > 0) {
      swarmBeekeeper <- getBeekeeper(tmp$pulled)
    }
    tmp <- swarm(tmp$pulled) # No need to set the location to swarms - they inherit the mother location (unless sampleLocation = TRUE)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Swarm", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(tmp$remnant), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    # Set the beekeeper
    start = Sys.time()
    if (nColonies(tmp$remnant) > 0) {
      tmp$remnant <- setBeekeeper(tmp$remnant, beekeeper = swarmBeekeeper)
    }
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "SetBeekeeper", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(tmp$remnant), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    age0p1 <- c(age0p1, tmp$remnant)

    if (year > 1) {
      # Swarm a percentage of age2 colonies
      tmp <- pullColonies(age2, p = p1swarm)
      age2 <- tmp$remnant
      if (nColonies(tmp$pulled) > 0) {
        swarmBeekeeper <- getBeekeeper(tmp$pulled)
      }
      tmp <- swarm(tmp$pulled)
      # Set the beekeeper
      if (nColonies(tmp$remnant) > 0) {
        tmp$remnant <- setBeekeeper(tmp$remnant, beekeeper = swarmBeekeeper)
      }
      age0p1 <- c(age0p1, tmp$remnant)
      #age2 <- c(age2, tmp$swarm)
    }

    # Supersede age1 colonies
    start = Sys.time()
    print("Supersede colonies, P1")
    tmp <- pullColonies(age1, p = p1supersede)
    age1 <- tmp$remnant
    tmp <- supersede(tmp$pulled)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Supersede", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(tmp), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)


    age0p1 <- c(age0p1, tmp)

    if (year > 1) {
      # Supersede age2 colonies
      tmp <- pullColonies(age2, p = p1supersede)
      age2 <- tmp$remnant
      tmp <- supersede(tmp$pulled)
      age0p1 <- c(age0p1, tmp)
    }

    # Mate the split colonies
    print("Mate split colonies, P1")
    if (year == 1) {
      # Plot colonies in space
      # locations = data.frame(x = sapply(getLocation(c(age0p1, age1)), FUN = function(x) x[[1]]),
      #                        y = sapply(getLocation(c(age0p1, age1)), FUN = function(x) x[[2]]),
      #                        Colonies = as.factor(c(rep("Virgin", nColonies(age0p1)), rep("Drone", nColonies(age1)))),
      #                        ColonyID = getId(c(age0p1, age1)))
      # ggplot(data = locations, aes(x = x, y = y, colour = Colonies, size = Colonies)) +
      #   geom_point()
      age1start = Sys.time()
      age0p1 <- cross(x = age0p1,
                      droneColonies = age1,
                      nDrones = nFathersPoisson,
                      crossPlan = "create",
                      spatial = spatialMating,
                      radius = matingRange,
                      checkCross = "warning")
      # Potentially, we need to kill the colonies that didn't mate successfully
      end = Sys.time()
      functionsTime <- rbind(functionsTime,
                             c(Function = "Cross", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age0p1), Time = difftime(end, start, units = "secs")))
      write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    } else {
      age0p1 <- cross(age0p1,
                      droneColonies = c(age1, age2),
                      nDrones = nFathersPoisson,
                      crossPlan = "create",
                      spatial = spatialMating,
                      radius = matingRange,
                      checkCross = "warning")
    }

    # Collapse
    print("Collapse colonies, P1")
    age1 <- selectColonies(age1, p = 1 - p1collapse)
    if (year > 1) {
      age2 <- selectColonies(age2, p = 1 - p1collapse)
    }

    # Period2 ------------------------------------------------------------------
    print("PERIOD 2")
    # Swarm a percentage of age1 colonies
    # Mellifera
    print("Swarm colonies, P2")
    start = Sys.time()
    tmp <- pullColonies(age1, p = p2swarm)
    age1 <- tmp$remnant
    if (nColonies(tmp$pulled) > 0) {
      swarmBeekeeper <- getBeekeeper(tmp$pulled)
    }
    tmp <- swarm(tmp$pulled, sampleLocation = FALSE)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Swarm", Rep = Rep, Year = year, Period = "2", nColonies = nColonies(tmp$remnant), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)


    start = Sys.time()
    if (nColonies(tmp$remnant) > 0) {
      tmp$remnant <- setBeekeeper(tmp$remnant, beekeeper = swarmBeekeeper)
    }
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "setBeekeeper", Rep = Rep, Year = year, Period = "2", nColonies = nColonies(tmp$remnant), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    # The queens of the remnant colonies are of age 0
    age0p2 <- tmp$remnant
    #age1 <- c(age1, tmp$swarm) # We're loosing the swarms

    if (year > 1) {
      # Swarm a percentage of age2 colonies
      tmp <- pullColonies(age2, p = p2swarm)
      age2 <- tmp$remnant
      if (nColonies(tmp$pulled) > 0) {
        swarmBeekeeper <- getBeekeeper(tmp$pulled)
      }
      tmp <- swarm(tmp$pulled)
      if (nColonies(tmp$remnant) > 0) {
        tmp$remnant <- setBeekeeper(tmp$remnant, beekeeper = swarmBeekeeper)
      }
      # The queens of the remnant colonies are of age 0
      age0p2 <- tmp$remnant
      #age2 <- c(age2, tmp$swarm)
    }

    # Supersede a part of age1 colonies
    print("Supersede colonies, P2")
    start = Sys.time()
    tmp <- pullColonies(age1, p = p2supersede)
    age1 <- tmp$remnant
    tmp <- supersede(tmp$pulled)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Supersede", Rep = Rep, Year = year, Period = "2", nColonies = nColonies(tmp), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    # The queens of superseded colonies are of age 0
    age0p2 <- c(age0p2, tmp)

    if (year > 1) {
      # Supersede a part of age2 colonies
      tmp <- pullColonies(age2, p = p2supersede)
      age2 <- tmp$remnant
      tmp <- supersede(tmp$pulled)
      # The queens of superseded colonies are of age 0
      age0p2 <- c(age0p2, tmp)
    }

    # Mate the colonies
    # Import p percentage of carnica colonies into mellifera DCA
    print("Mate colonies, P2")

    if (year == 1) {
      start = Sys.time()
      age0p2 <- cross(x = age0p2,
                      droneColonies = age1,
                      nDrones = nFathersPoisson,
                      crossPlan = "create",
                      spatial = spatialMating,
                      radius = matingRange,
                      checkCross = "warning")
      end = Sys.time()
      functionsTime <- rbind(functionsTime,
                             c(Function = "Cross", Rep = Rep, Year = year, Period = "2", nColonies = nColonies(age0p2), Time = difftime(end, start, units = "secs")))
      write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    } else {
      age0p2 <- cross(age0p2,
                      droneColonies = c(age1, age2),
                      nDrones = nFathersPoisson,
                      crossPlan = "create",
                      spatial = spatialMating,
                      radius = matingRange,
                      checkCross = "warning")
    }

    # Collapse
    age1 <- selectColonies(age1, p = 1 - p2collapse)
    if (year > 1) {
      age2 <- selectColonies(age2, p = 1 - p2collapse)
    }

    # Merge all age 0 colonies (from both periods)
    age0 <- c(age0p1, age0p2)
    # Record colonies and gvs
    colonyRecords <- data_rec(datafile = colonyRecords, colonies = age0, year = year)
    # Records phenotypes
    yearEff <- sampleEffect(n = 1, var = yearVar)
    beekeepers$BeekeeperYearEffect <- sampleEffect(n = length(beekeepersID), var = beekeeperYearVar)
    phenoRecords <- pheno_rec(phenofile = phenoRecords, colonies = age0, year = year,
                              locDF = loc, trait = 1, beekeepers = beekeepers, yearEff = yearEff,
                              resVar = residualVar)
    phenoRecords <- pheno_rec(phenofile = phenoRecords, colonies = age1, year = year,
                              locDF = loc, trait = 1, beekeepers = beekeepers, yearEff = yearEff,
                              resVar = residualVar)
    if (year > 1) {
      phenoRecords <- pheno_rec(phenofile = phenoRecords, colonies = age2, year = year,
                                locDF = loc, trait = 1, beekeepers = beekeepers, yearEff = yearEff,
                                resVar = residualVar)
    }

    # Compute the relationship matrix
    print("Computing GRM")
    start = Sys.time()
    #queenGeno <- getSnpGeno(getQueen(age0, collapse = TRUE))
    queenGeno <- getQtlGeno(getQueen(age0, collapse = TRUE))
    Gmatrix <- calcBeeGRMIbs(x = queenGeno,
                             sex = rep("F", nColonies(age0)),
                             alleleFreq = baseAF)
    end = Sys.time()
    print("Done recording")
    functionsTime <- rbind(functionsTime,
                           c(Function = "ComputeGRM", Rep = Rep, Year = year, Period = "2", nColonies = nColonies(age0), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)

    # Save the Gmatrix
    save(Gmatrix, file = paste0("RData/GRMAge0_", year, ".Rdata"))

    # Period3 ------------------------------------------------------------------
    # Collapse age0 queens
    print("PERIOD 3")
    print("Collapse colonies, P3")

    start = Sys.time()
    age0 <- selectColonies(age0, p = (1 - p3collapseAge0))
    age1 <- selectColonies(age1, p = (1 - p3collapseAge1))
    age2 <- NULL
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Collapse", Rep = Rep, Year = year, Period = "3", nColonies = (nColonies(age0) + nColonies(age1)), Time = difftime(end, start, units = "secs")))
    write.csv(functionsTime, "FunctionsTime.csv", quote = F, row.names = F)



    # Maintain the number of colonies ------------------------------------------
    # Keep all of age1, age0 swarmed so we build it up with some splits, while we remove (sell) the other splits
    print("Maintain the number, P2")
    age0 <- maintainPopulationSize(age0 = age0, age1 = age1, popSize = nColonies)

    if ((nColonies(age0) + nColonies(age1)) != nColonies) {
      stop(paste0("The number of colonies does not match the apiary size!"))
    }


  } # Year-loop

  a <- toc()
  loopTime <- rbind(loopTime, c(Rep, a$tic, a$toc, a$msg, (a$toc - a$tic)))
  write.csv(colonyRecords, paste0("ColonyRecords_", Rep, ".csv"), quote=F, row.names=F)
  write.csv(phenoRecords, paste0("PhenoRecords_", Rep, ".csv"), quote=F, row.names=F)

} # Rep-loop


print("Saving image data")
save.image(paste0("RData/SloSpatialSimulation_", Rep, ".RData"))

#
# # Plot alive colonies
# alive <- c(age0, age1)
# locAlive <- getLocation(alive)
# aliveDF <- data.frame(x = sapply(locAlive, FUN = function(x) x[1]),
#                       y = sapply(locAlive, FUN = function(x) x[2]),
#                       gv = getQueenGv(alive, collapse = TRUE))
# ggplot(data = aliveDF, aes(x = x, y = y, colour = honeyYield)) +
#   geom_point() +
#   scale_colour_viridis()
#
# # Compute the distance from (0,0)
# head(aliveDF)
# aliveDF$DistanceFromO <- sqrt(aliveDF$x**2 + aliveDF$y**2)
#
# model <- glm(aliveDF$honeyYield ~ aliveDF$DistanceFromO)
# summary(model)
#
# # Spatial autocorrelation?

# n <- nColonies(alive)
# y <- aliveDF$honeyYield
# ybar <- mean(y)
#
# dy <- y - ybar
# g <- expand.grid(dy, dy)
# yiyj <- g[,1] * g[,2]
# pm <- matrix(yiyj, ncol=n)
