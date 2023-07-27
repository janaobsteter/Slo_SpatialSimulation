library(ggplot2)
library(dplyr)
library(tidyr)
library(gstat)
library(sp)


# Plot the GRM for each year
homeDir <- "/home/jana/EddieDir/Honeybees/SloSpatialSimulation/"
homeDir <- "/home/jana/Documents/1Projects/SIMplyBee_devel/Spatial/"
spatial <-  FALSE  # TRUE
noLoc <- 50
spVar <- "1_3"
range <- "SLO"
rep <- 1
nYear <- 20
spatialMating <- ifelse(spatial, "Spatial", "Random")
dir = paste0(homeDir, spatialMating, "_NoLoc_", noLoc, "_SpVar_", spVar, "_Range_", range)
setwd(dir)

colonyRecords <- read.csv(paste0("ColonyRecords_", rep, ".csv"))
phenoRecords <- read.csv(paste0("PhenoRecords_", rep, ".csv"))
colonyRecords$colonyID <- as.character(colonyRecords$colonyID)
phenoRecords$colonyID <- as.character(phenoRecords$colonyID)
spatialEffects <- read.csv(paste0("SpatialEffect_", rep, ".csv"))

dir.create("Variograms")

for (Year in 1:nYear) {
  print(Year)
  # Plot the full phenotype
  phenoRecords_year <- phenoRecords[(phenoRecords$Year == Year) &
                                      (phenoRecords$colonyID %in% colonyRecords$colonyID[colonyRecords$year == Year & colonyRecords$colonies == "age0"]),]
  pheno <- phenoRecords_year %>% select(colonyID, FullPheno, Gv)
  location <- unique(colonyRecords %>%  select(colonyID, locationX, locationY) %>%
                       filter(.$colonyID %in% pheno$colonyID))

  nrow(pheno); nrow(unique(location))
  sum(duplicated(pheno$colonyID)); sum(duplicated(location$colonyID))

  phenoLocation <- full_join(pheno, location) %>%  select(locationX, locationY, FullPheno)

  # Transform the dataframe into SpatialPointDataFrame
  coordinates(phenoLocation)=~locationX+locationY
  class(phenoLocation)

  # Create a variogram
  Vario_Pheno <- variogram(FullPheno ~ 1,
                           data = phenoLocation)
  # Plot the variogram
  png(paste0("Variograms/PhenoVariogram", Year, ".png"))
  print(plot(Vario_Pheno, ylim = c(0, 300), xlim = c(0, 9000),
             main = paste0("Year", Year)))
             #plot.numbers = 0.05, np = TRUE))
  dev.off()


  # Plot the additive genetic effect variogram
  gvLocation <- full_join(pheno, location) %>%  select(locationX, locationY, Gv)

  # Transform the dataframe into SpatialPointDataFrame
  coordinates(gvLocation)=~locationX+locationY
  class(gvLocation)

  # Create a variogram
  Vario_Gv <- variogram(Gv ~ 1,
                        data = gvLocation)
  # Plot the variogram
  png(paste0("Variograms/GvVariogram", Year, ".png"))
  print(plot(Vario_Gv, ylim = c(0, 100), main = paste0("Year", Year)))
  dev.off()
}

system(paste0("convert -delay 100 -loop 0 ", paste0("Variograms/PhenoVariogram", 1:nYear, ".png", collapse = " "), " GIFs/PhenoVariogram.gif"))
system(paste0("convert -delay 100 -loop 0 ", paste0("Variograms/GvVariogram", 1:nYear, ".png", collapse = " "), " GIFs/GvVariogram.gif"))

# Plot the spatial effect - just once for the simulation
spatialLocation <- spatialEffects %>% select(X_COORDINATE, Y_COORDINATE, SpatialEffect)
# Transform the dataframe into SpatialPointDataFrame
coordinates(spatialLocation)=~X_COORDINATE+Y_COORDINATE
class(spatialLocation)
# Create a variogram
Vario_SpatialEff <- variogram(SpatialEffect ~ 1,
                              data = spatialLocation)
# Plot the variogram
png("Variograms/SpatialEffVariogram.png")
print(plot(Vario_SpatialEff))
dev.off()
