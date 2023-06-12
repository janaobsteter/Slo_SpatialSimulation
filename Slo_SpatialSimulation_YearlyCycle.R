# Set the working directory
setwd("~/Documents/1Projects/SIMplyBee_devel/Spatial/")
# Load packages
library(SIMplyBee)
library(ggplot2)
library(tictoc)
library(R6)
library(Matrix)
library(dplyr)
library(tidyr)
library(viridis)

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

sampleBeekeepersLocation <- function(locDF, currentLocation = NULL, excludeCurrentLoc = FALSE, n = 1, replace = TRUE) {
  beekeeper <- as.character(unique(locDF$Beekeeper[locDF$X_COORDINATE == currentLocation[1] & locDF$Y_COORDINATE == currentLocation[2]]))
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




# Locations - x, y, and the beekeper
locAll <- read.csv("SLOLocations_standardised.csv")
nColoniesPerLocation <- 5 #In reality, it's 15
locAll$Beekeeper <- as.factor(locAll$Beekeeper)
ggplot(data = locAll, aes(x = X_COORDINATE, Y_COORDINATE)) + geom_point()

# Sample locations for testing from one region - so they are close together - but have 5 colonies at each location
loc <- locAll[locAll$X_COORDINATE < 15000 & locAll$Y_COORDINATE < 25000 ,]
loc <- loc[!is.na(loc$Beekeeper),]
nrow(loc)
locList <- rep(Map(c, loc$X_COORDINATE, loc$Y_COORDINATE), nColoniesPerLocation)
colonyBeekeeper <- rep(loc$Beekeeper, nColoniesPerLocation)
ggplot(loc, aes(x = X_COORDINATE,y = Y_COORDINATE)) + geom_point()
length(unique(loc$Beekeeper))

# OR sample the colonies of 20 beekeepers
# selBeekeeper <- sample(unique(locAll$Beekeeper), size = 50, replace = FALSE)
# loc <- locAll[locAll$Beekeeper %in% selBeekeeper,]
# nrow(loc)
# ggplot(loc, aes(x = X_COORDINATE,y = Y_COORDINATE, colour = Beekeeper)) + geom_point()

# Founder population parameters -------------------------------------------------------------------
nColonies = nrow(loc) * nColoniesPerLocation              # Number of colonies in Slovenia, using a smaller number for now
nFounderQueens = nColonies                                # How many queens do we need to start colonies
nFounderDPQ <- round(nFounderQueens * 0.25)                      # How many queens do we need to create initial drones
nFounders <- nFounderQueens + nFounderDPQ                 # How many founders all together
nChr = 1                                                  # Number of chromosomes
nDronesPerQueen = 100                                      # Number of drones created per founder DPQ
nSegSites = 2000                                          # Number of segregating sites per chromosome
nQtlPerChr = 100                                          # Number of QTLs per chromosome

# Population parameters -------------------------------------------------------------------
nRep <- 1                     # Number of repeats
nYear <- 10                   # Number of years
popSize <- 20000              # Number of queen colonies
noWorkers <- 10                # Number of workers in a full colony
noDrones <- 1                  # Number of drones in a full colony (typically nWorkers * 0.2 (not in the example))
noFathers <- nFathersPoisson   # Number of drones the queen mates with (could also be a function)
noVirginQueens <- 1            # Number of created virgin queens

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
mean <- 20
phenoVar <- 175
h2 <- 0.25 # Heritability for honey yield
varA <- phenoVar * h2
nonAVar <- phenoVar - varA
apiaryYearVar <- 1/3 * nonAVar
spatialVar <- 1/3 * nonAVar
residualVar <- 1/3 * nonAVar

# Create data frames for recording the number of age0 and age1 colonies, csd variability and for recording cpu time
loopTime <- data.frame(Rep = NA, tic = NA, toc = NA, msg = NA, time = NA)
functionsTime <- data.frame(Function = NA, Rep = NA, Year = NA, Period = NA, nColonies = NA, Time = NA)

# Prepare recording function
data_rec <- function(datafile, colonies, year) {
  queens = mergePops(getQueen(colonies))
  location = getLocation(colonies, collapse = TRUE)
  datafile = rbind(datafile,
                   data.frame(colonies             = deparse(substitute(colonies)),
                              year                 = year,
                              Id                   = queens@id,
                              MId                  = queens@mother,
                              FId                  = queens@father,
                              locationX            = location[1,],
                              locationY            = location[2,],
                              nFathers             = nFathers(queens),
                              nDPQ                 = sapply(getFathers(queens), function(x) length(unique(x@mother))),
                              nCsdAlColony         = sapply(colonies@colonies, function(x) nCsdAlleles(x, collapse = TRUE)),
                              nCsdApiary           = rep(nCsdAlleles(colonies, collapse = TRUE), queens@nInd),
                              pHomBrood            = calcQueensPHomBrood(queens),
                              gvQueens_QueenTrait  = sapply(getGv(colonies, caste = "queen"), function(x) x[1,1])
                   ))}


# Start of the rep-loop ---------------------------------------------------------------------
for (Rep in 1:nRep) {
  colonyRecords = NULL
  # Rep <- 1 (you can use this to check if your code is running alright for one whole loop)
  cat(paste0("Rep: ", Rep, "/", nRep, "\n"))
  tic(paste0(nYear, 'y loop'))         # Measure cpu time
  Rprof()                              # Start profiling


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
  founderGenomes <- quickHaplo(nInd = nFounders, nChr = nChr, segSites = nSegSites)
  SP <- SimParamBee$new(founderGenomes, csdChr = ifelse(nChr >= 3, 3, 1), nCsdAlleles = 128)
  SP$nWorkers <- noWorkers
  SP$nDrones <- noDrones
  SP$nFathers <- noFathers
  SP$nVirginQueens <- noVirginQueens
  SP$swarmP <- 0.5                # Probability of swarming? (TODO: double check this)
  SP$splitP <- 0.3
  SP$setTrackPed(TRUE)            # Track the pedigree
  SP$setTrackRec(TRUE)            # Track the recombination
  SP$addSnpChip(nSnpPerChr = 1000)   # Add a SNP chip with 1000 SNPs per chromosome
  csdChr <- SP$csdChr             # define csd chromomsome

  # Add trait - simulating only queen trait for now!
  mean <- mean
  varA <- varA
  SP$addTraitA(nQtlPerChr = nQtlPerChr, mean = mean, var = varA,
               name = c("honeyYield"))

  # Set residual variance (this is not environmental variance!)
  SP$setVarE(varE = residualVar)

  # STEP 3: Set up your base population
  # Create a base population - for now, we simulate all the founder but this will not the case with larger numbers
  virginQueens <- createVirginQueens(x = founderGenomes)
  # Create drones
  drones <- createDrones(x = virginQueens[1:nFounderDPQ], nInd = nDronesPerQueen)
  # Get fathers for Mel, MelCross and Car
  #fathers <- pullDroneGroupsFromDCA(drones$Mel, n = nInd(virginQueens$Mel[1:apiarySize]), nDrones = nFathersPoisson)

  # Mate virgin queens with fathers to make them queens
  queens <-  SIMplyBee::cross(x = virginQueens[(nFounderDPQ + 1): nFounders],
                              drones = drones,
                              crossPlan = "create",
                              spatial = FALSE)

  # Start the year-loop ------------------------------------------------------------------
  for (year in 1:nYear) {
    print("Starting the cycle")
    cat(paste0("Year: ", year, "/", nYear, "\n"))

    # If this is the first year, create some colonies to start with
    if (year == 1) {
      print("Creating initial colonies")
      start = Sys.time()
      age1 <- createMultiColony(x = queens)
      end = Sys.time()
      functionsTime <- rbind(functionsTime,
                             c(Function = "CreateInitialColonies", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = end-start))

      # Set Locatoin
      start = Sys.time()
      age1 <- setLocation(age1, location = locList)
      end = Sys.time()
      functionsTime <- rbind(functionsTime,
                             c(Function = "SetInitialLocation", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = end-start))
      # Set the beekeeper to the colonies
      age1 <- setBeekeeper(age1, beekeeper = colonyBeekeeper)
      #table(getBeekeeper(age1))

      print("Record initial colonies")
      colonyRecords <- data_rec(datafile = colonyRecords, colonies = age1, year = year)

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
                           c(Function = "BuildUp", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = end-start))

    if (year > 1) {
      age2 <- buildUp(age2)
    }

    # Split all age1 colonies
    print("Splitting the colonies")
    start = Sys.time()
    tmp <- split(age1)
    age1 <- tmp$remnant

    # Set the location of the splits - if the beekeeper has another location, it samples another one
    # If the beekeeper has only one location, it samples the same one
    newSplitLoc <- sapply(getLocation(age1),
                          FUN = function(x) sampleBeekeepersLocation(locDF = loc, currentLocation = x, n = 1, excludeCurrentLoc = TRUE))
    tmp$split <- setLocation(tmp$split, location = newSplitLoc)

    # Set the beekeeper of the splits
    tmp$split <- setBeekeeper(tmp$split, beekeeper = getBeekeeper(age1))


    # Sample a colony for each beekeeper to get virgin queens from
    age1Bk <- getBeekeeper(age1)
    for (bk in unique(age1Bk)) {
      beekeepersVirginColony[[bk]] <- sample(names(age1Bk)[which(age1Bk == bk)], size = 1)
    }

    # Requeen the virgin queens in the split from another virgin queens from the same beekeeper
    for (colony in 1:nColonies(tmp$split)) {
      splitBk <- getBeekeeper(tmp$split[[colony]])
      virginQueen <- createVirginQueens(x = age1[[beekeepersVirginColony[[splitBk]]]], nInd = 1)
      tmp$split[[colony]] <- reQueen(tmp$split[[colony]], queen = virginQueen)
    }

    # The queens of the splits are 0 years old
    age0p1 <- tmp$split
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Split", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age1), Time = end-start))

    if (year > 1) {
      # Split all age2 colonies
      tmp <- split(age2)
      age2 <- tmp$remnant
      # Set the location of the splits - if the beekeeper has another location, it samples another one
      # If the beekeeper has only one location, it samples the same one
      newSplitLoc <- sapply(getLocation(age2),
                            FUN = function(x) sampleBeekeepersLocation(locDF = loc, currentLocation = x, n = 1, excludeCurrentLoc = TRUE))
      tmp$split <- setLocation(tmp$split, location = newSplitLoc)
      # The queens of the splits are 0 years old
      age0p1 <- c(age0p1, tmp$split)
    }

    # Create virgin queens
    # Sample colony for the virgin queens
    print("Create virgin queens, period 1")
    virginDonor <- sample.int(n = nColonies(age1), size = 1)
    # Virgin queens for splits!
    virginQueens <- createVirginQueens(age1[[virginDonor]], nInd = nColonies(age0p1))

    # Requeen the splits --> queens are now 0 years old
    age0p1 <- reQueen(age0p1, queen = virginQueens)

    # Swarm a percentage of age1 colonies
    start = Sys.time()
    print("Swarm colonies, P1")
    tmp <- pullColonies(age1, p = p1swarm)
    age1 <- tmp$remnant
    tmp <- swarm(tmp$pulled) # No need to set the location to swarms - they inherit the mother location (unless sampleLocation = TRUE)
    age0p1 <- c(age0p1, tmp$remnant)
    age1 <- c(age1, tmp$swarm)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Swarm", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(tmp$remnant), Time = end-start))

    if (year > 1) {
      # Swarm a percentage of age2 colonies
      tmp <- pullColonies(age2, p = p1swarm)
      age2 <- tmp$remnant
      tmp <- swarm(tmp$pulled)
      age0p1 <- c(age0p1, tmp$remnant)
      age2 <- c(age2, tmp$swarm)
    }

    # Supersede age1 colonies
    start = Sys.time()
    print("Supersede colonies, P1")
    tmp <- pullColonies(age1, p = p1supersede)
    age1 <- tmp$remnant
    tmp <- supersede(tmp$pulled)
    age0p1 <- c(age0p1, tmp)
    end = Sys.time()
    functionsTime <- rbind(functionsTime,
                           c(Function = "Supersede", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(tmp), Time = end-start))

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
      locations = data.frame(x = sapply(getLocation(c(age0p1, age1)), FUN = function(x) x[[1]]),
                             y = sapply(getLocation(c(age0p1, age1)), FUN = function(x) x[[2]]),
                             Colonies = as.factor(c(rep("Virgin", nColonies(age0p1)), rep("Drone", nColonies(age1)))),
                             ColonyID = getId(c(age0p1, age1)))
      ggplot(data = locations, aes(x = x, y = y, colour = Colonies, size = Colonies)) +
        geom_point()
      age1start = Sys.time()
      age0p1 <- cross(x = age0p1,
                      droneColonies = age1,
                      nDrones = nFathersPoisson,
                      crossPlan = "create",
                      spatial = TRUE,
                      radius = 5,
                      checkCross = "warning")
      end = Sys.time()
      functionsTime <- rbind(functionsTime,
                             c(Function = "Cross", Rep = Rep, Year = year, Period = "1", nColonies = nColonies(age0p1), Time = end-start))
    } else {
      age0p1 <- cross(age0p1,
                      droneColonies = c(age1, age2),
                      nDrones = nFathersPoisson,
                      crossPlan = "create",
                      spatial = FALSE)
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
    tmp <- pullColonies(age1, p = p2swarm)
    age1 <- tmp$remnant
    tmp <- swarm(tmp$pulled, sampleLocation = FALSE)
    # The queens of the remnant colonies are of age 0
    age0p2 <- tmp$remnant
    age1 <- c(age1, tmp$swarm) # Decide whether to loose the swarms? In that case, just remove this line

    if (year > 1) {
      # Swarm a percentage of age2 colonies
      tmp <- pullColonies(age2, p = p2swarm)
      age2 <- tmp$remnant
      tmp <- swarm(tmp$pulled)
      # The queens of the remnant colonies are of age 0
      age0p2 <- tmp$remnant
      age2 <- c(age2, tmp$swarm)
    }

    # Supersede a part of age1 colonies
    print("Supersede colonies, P2")

    tmp <- pullColonies(age1, p = p2supersede)
    age1 <- tmp$remnant
    tmp <- supersede(tmp$pulled)
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
                      spatial = TRUE,
                      radius = 5,
                      checkCross = "warning")
      end = Sys.time()
      functionsTime <- rbind(functionsTime,
                             c(Function = "Cross", Rep = Rep, Year = year, Period = "2", nColonies = nColonies(age0p2), Time = end-start))

    } else {
      age0p2 <- cross(age0p2,
                      droneColonies = c(age1, age2),
                      nDrones = nFathersPoisson,
                      crossPlan = "create",
                      spatial = TRUE,
                      radius = 5,
                      checkCross = "warning")
    }

    # Collapse
    age1 <- selectColonies(age1, p = 1 - p2collapse)
    if (year > 1) {
      age2 <- selectColonies(age2, p = 1 - p2collapse)
    }

    # Merge all age 0 colonies (from both periods)
    age0 <- c(age0p1, age0p2)
    #colonyRecords <- data_rec(datafile = colonyRecords, colonies = age0, year = year, population = "Pop")

    # Period3 ------------------------------------------------------------------
    # Collapse age0 queens
    print("PERIOD 3")
    print("Collapse colonies, P3")

    age0 <- selectColonies(age0, p = (1 - p3collapseAge0))
    age1 <- selectColonies(age1, p = (1 - p3collapseAge1))
    age2 <- NULL


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
} # Rep-loop

print("Saving image data")
save.image(paste0("SloSpatialSimulation_", Rep, ".RData"))
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

