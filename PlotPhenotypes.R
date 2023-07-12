library(ggplot2)
library(dplyr)
library(tidyr)

# Plot the GRM for each year
homeDir <- "/home/jana/EddieDir/Honeybees/SloSpatialSimulation/"
homeDir <- "/home/jana/Documents/1Projects/SIMplyBee_devel/Spatial/"
spatial <-  TRUE
noLoc <- 50
rep <- 1
nYear <- 20
spatialMating <- ifelse(spatial, "Spatial", "Random")
dir = paste0(homeDir, spatialMating, "_NoLoc_", noLoc, "/")
setwd(dir)

colonyRecords <- read.csv(paste0("ColonyRecords_", rep, ".csv"))
phenoRecords <- read.csv(paste0("PhenoRecords_", rep, ".csv"))

# Plot the phenotypic trend
phenoRecords %>%  group_by(Year) %>%  summarise(mean = mean(FullPheno), sd = sd(FullPheno)) %>%
  ggplot(aes(x = Year, y = mean)) + geom_line()
# Plot the SpatialEffect
phenoRecords %>%  group_by(Beekeeper) %>%  summarise(mean = mean(SpatialEffect), sd = sd(FullPheno)) %>%
  ggplot(aes(x = Beekeeper, y = mean)) + geom_line()

# Do "pheno variogram" by year
for (Year in 1:nYear) {
  print(Year)

  phenoRecords_year <- phenoRecords[phenoRecords$Year == Year,]

  # Create a distance matrix
  distMatrix <- as.matrix(colonyRecords %>%  filter(colonyID %in% phenoRecords_year$colonyID) %>%
                            select(colonyID, locationX, locationY) %>%
                            unique(.) %>%
                            `rownames<-`(.$colonyID) %>%
                            select(locationX, locationY) %>%  dist(.))

  if (!all(phenoRecords_year$colonyID %in% rownames(distMatrix))) {
    stop("Not all colonies have the location information!")
  }

  # Transform to a long format - take only the lower triangle without diagonal
  distMatrix_l <- as.data.frame(distMatrix) %>%  dplyr::mutate(ID1 = rownames(distMatrix)) %>%
    pivot_longer(cols = colnames(distMatrix), names_to = "ID2", values_to = "Dist")


  # Produce a matrix of phenotypic difference between all colonies
  #phenoVector <- phenoRecords_year$FullPheno %>% `names<-`(phenoRecords_year$colonyID)
  spatialEffVector <- phenoRecords_year$SpatialEffect %>% `names<-`(phenoRecords_year$colonyID)

  spatialEffDiff <- outer(X = spatialEffVector, Y = spatialEffVector,
                     FUN = function(x, y) x - y)
  spatialEffDiffLower <- spatialEffDiff
  spatialEffDiffLower[upper.tri(spatialEffDiff, diag = FALSE)] <- NA
  spatialEffDiffLower_l <- as.data.frame(spatialEffDiffLower) %>%
    dplyr::mutate(ID1 = rownames(spatialEffDiffLower)) %>%
    pivot_longer(cols = colnames(spatialEffDiffLower), names_to = "ID2", values_to = "SpatialEffDiff")
  spatialEffDiffLower_l <- spatialEffDiffLower_l[!is.na(spatialEffDiffLower_l$SpatialEffDiff),]

  #Combine distance and phenotypes
  distSpatialEffDiff <- right_join(distMatrix_l, spatialEffDiffLower_l)
  if (sum(is.na(distSpatialEffDiff$Dist)) > 0) {
    stop("Some colony-pairs don't have a distance!")
  }

  distSpatialEffDiff$SpatialEffDiff <- as.numeric(distSpatialEffDiff$SpatialEffDiff)
  png(paste0("SpatialEffDiffDistance", Year, ".png"))
  distPlot <- ggplot(distSpatialEffDiff, aes(x = Dist, y = SpatialEffDiff)) + geom_point(alpha = 0.7) +
    ggtitle(paste0("Year", Year)) +
    ylim(-30, 30)
  print(distPlot)
  dev.off()

  # # Try to plot the variance manually
  # distSpatialEffDiff$DistClass <- 0
  # distSpatialEffDiff$DistClass[distSpatialEffDiff$Dist <= 3000] <- 1
  # distSpatialEffDiff$DistClass[distSpatialEffDiff$Dist >= 3000 & distSpatialEffDiff$Dist < 6000] <- 2
  # distSpatialEffDiff$DistClass[distSpatialEffDiff$Dist >= 6000 & distSpatialEffDiff$Dist < 9000] <- 3
  # distSpatialEffDiff$DistClass[distSpatialEffDiff$Dist >= 9000 & distSpatialEffDiff$Dist < 12000] <- 4
  # distSpatialEffDiff$DistClass[distSpatialEffDiff$Dist >= 12000 & distSpatialEffDiff$Dist < 15000] <- 5
  # distSpatialEffDiff$DistClass[distSpatialEffDiff$Dist >= 15000] <- 6
  # sum(table(distSpatialEffDiff$DistClass)) == nrow(distSpatialEffDiff)
  # distSpatialEffDiff[distSpatialEffDiff$DistClass == 0,]
  # distSpatialEffDiff %>%  group_by(DistClass) %>%  summarise(Variance = sum(SpatialEffDiff**2),
  #                                                            N = length(SpatialEffDiff),
  #                                                            gama = Variance / (2*N)) %>%
  #   ggplot(aes(x = DistClass, y = gama)) + geom_point()


}

# This gif should not be changing - it's just colonies dying off, but the spatial effect is sampled only once per simulation and should hence not change
system(paste0("convert -delay 100 -loop 0 ", paste0("SpatialEffDiffDistance", 1:nYear, ".png", collapse = " "), " SpatialEffDiffDistance.gif"))


table(distPhenoDiff$PhenoDiff[distPhenoDiff$Dist == 0]) # This is ok now!
head(distSpatialEffDiff)
