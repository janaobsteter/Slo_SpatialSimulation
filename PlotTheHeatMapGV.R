library(ggplot2)

# Plot the GRM for each year
spatial = TRUE
spatialMating <- ifelse(spatial, "Spatial", "Random")
setwd(paste0("~/Documents/1Projects/SIMplyBee_devel/Spatial/", spatialMating, "_NoLoc_50/"))

colonyRecords <- read.csv("ColonyRecords_1.csv")

for (year in 1:10) {
  yearDF <- colonyRecords[colonyRecords$year == year, ]
  yearDF$gvQueens_QueenTrait <- as.numeric(yearDF$gvQueens_QueenTrait)
  png(paste0("HeatMapGV_", year, ".png"))
  print(ggplot(data = yearDF, aes(x = as.character(locationX), y = as.character(locationY), fill = gvQueens_QueenTrait)) +
          geom_tile() + ggtitle(paste0("Year", year)))
  dev.off()
}


system(paste0("convert -delay 100 -loop 0 ", paste0("HeatMapGV_", 1:nYear, ".png", collapse = " "), " HeatMapGV.gif"))

