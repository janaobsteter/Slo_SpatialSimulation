library(ggplot2)
library(dplyr)
library(tidyr)
library(gstat)
library(sp)


# Plot the GRM for each year
homeDir <- "/home/jana/EddieDir/Honeybees/SloSpatialSimulation/"
homeDir <- "/home/jana/Documents/1Projects/SIMplyBee_devel/Spatial/"
spatial <-  TRUE
noLoc <- 50
rep <- 1
nYear <- 20
spatialMating <- ifelse(spatial, "Spatial", "Random")
range <- 1
dir = paste0(homeDir, spatialMating, "_NoLoc_", noLoc, "/")
dir = paste0(homeDir, spatialMating, "_NoLoc_", noLoc, "_Range", range, "/")
setwd(dir)

colonyRecords <- read.csv(paste0("ColonyRecords_", rep, ".csv"))
spatialEffects <- read.csv(paste0("SpatialEffect_", rep, ".csv"))

for (Year in 1:nYear) {
  print(Year)
  # Plot the full phenotype
  phenoRecords_year <- phenoRecords[phenoRecords$Year == Year,]
  pheno <- phenoRecords_year %>% select(colonyID, FullPheno)
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
  png(paste0("PhenoVariogram", Year, ".png"))
  print(plot(Vario_Pheno, ylim = c(100, 500), main = paste0("Year", Year)))
  dev.off()
}

system(paste0("convert -delay 100 -loop 0 ", paste0("PhenoVariogram", 1:nYear, ".png", collapse = " "), " PhenoVariogram.gif"))

# Plot the spatial effect - just once for the simulation
spatialLocation <- spatialEffects %>% select(X_COORDINATE, Y_COORDINATE, SpatialEffect)
# Transform the dataframe into SpatialPointDataFrame
coordinates(spatialLocation)=~X_COORDINATE+Y_COORDINATE
class(spatialLocation)
# Create a variogram
Vario_SpatialEff <- variogram(SpatialEffect ~ 1,
                              data = spatialLocation)
# Plot the variogram
png("SpatialEffVariogram.png")
print(plot(Vario_SpatialEff))
dev.off()
