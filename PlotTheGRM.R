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

for (year in 1:nYear) {
  rm(Gmatrix)
  # Read in the GRM
  load(paste0("GRMAge0_", year, ".Rdata"))

  #1) Plot the relationships against the distance
  # Create a distance matrix
  distMatrix <- as.matrix(colonyRecords %>%  filter(Id %in% rownames(Gmatrix)) %>%
                          .[match(rownames(Gmatrix), .$Id),] %>% # Make sure the distance matrix is in the same order as GRM
                          `rownames<-`(.$Id) %>%
                          select(locationX, locationY) %>%  dist(.))


    # Transform to a long format - take only the lower triangle without diagonal
  distMatrix_l <- as.data.frame(distMatrix) %>%  dplyr::mutate(ID1 = rownames(distMatrix)) %>%
    pivot_longer(cols = colnames(distMatrix), names_to = "ID2", values_to = "Dist")

  # Transform the GRM to a long format - only lower triangular, excluding the diagonal
  GmatrixLower <- Gmatrix
  GmatrixLower[upper.tri(GmatrixLower, diag = TRUE)] <- NA
  GmatrixLower_l <- as.data.frame(GmatrixLower) %>%
    dplyr::mutate(ID1 = rownames(GmatrixLower)) %>%
    pivot_longer(cols = colnames(GmatrixLower), names_to = "ID2", values_to = "IBS")
  GmatrixLower_l <- GmatrixLower_l[!is.na(GmatrixLower_l$IBS),]

  # Combine distance and GRM
  distGRM <- right_join(distMatrix_l, GmatrixLower_l)
  png(paste0("GrmDistance", year, ".png"))
  distPlot <- ggplot(distGRM, aes(x = Dist, y = IBS)) + geom_point(alpha = 0.7) +
    geom_smooth() +
    ylim(c(0, 2)) +
    ggtitle(paste0("Year", year))
  print(distPlot)
  dev.off()


  #2) Create X-ordered GRM
  # Order GRM by the X-location
  # xOrderedId <- colonyRecords %>%  filter(Id %in% rownames(Gmatrix)) %>%
  #   select(Id, locationX) %>%  arrange(locationX) %>% select(Id)
  # # Order the Gmatrix by X
  # xMatch <- match(as.character(xOrderedId$Id), rownames(Gmatrix))
  # xOrderedG <- Gmatrix[xMatch, xMatch]
  #
  # # Transform matrix to a long format
  # xOrderedG_l <- as.data.frame(xOrderedG) %>%  dplyr::mutate(ID1 = rownames(xOrderedG)) %>%
  #   pivot_longer(cols = colnames(xOrderedG), names_to = "ID2")
  # xOrderedG_l$value <- as.numeric(xOrderedG_l$value)
  # xOrderedG_l$ID1 <- factor(xOrderedG_l$ID1, levels = colnames(xOrderedG))
  # xOrderedG_l$ID2 <- factor(xOrderedG_l$ID2, levels = colnames(xOrderedG))
  #
  # #Plot the X ordered GRM
  # png(paste0("xOrderedGRM_", year, ".png"))
  # #print(image(as(xOrderedG, "Matrix"), main = paste0("Year", year)))
  # yearPlot <- ggplot(data = xOrderedG_l, aes(x = ID1, y = ID2, fill = value)) +
  #   geom_tile() +
  #   theme(axis.text = element_blank(),
  #         axis.ticks = element_blank()) +
  #   scale_fill_gradientn(colors = hcl.colors(10),
  #                        values = c(-0.2, 0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.8, 2),
  #                        limits = c(-0.2, 2)) +
  #   ggtitle(paste0("Year", year))
  # print(yearPlot)
  # dev.off()
  #
  # #3) Create Y-ordered GRM
  # # Order by the Y-location
  # yOrderedId <- colonyRecords %>%  filter(Id %in% rownames(Gmatrix)) %>%
  #   select(Id, locationY) %>%  arrange(locationY) %>% select(Id)
  # # Order the Gmatrix by X
  # yMatch <- match(as.character(yOrderedId$Id), rownames(Gmatrix))
  # yOrderedG <- Gmatrix[yMatch, yMatch]
  #
  # # Transform matrix to a long format
  # yOrderedG_l <- as.data.frame(yOrderedG) %>%  dplyr::mutate(ID1 = rownames(yOrderedG)) %>%
  #   pivot_longer(cols = colnames(yOrderedG), names_to = "ID2")
  # yOrderedG_l$value <- as.numeric(yOrderedG_l$value)
  # yOrderedG_l$ID1 <- factor(yOrderedG_l$ID1, levels = colnames(yOrderedG))
  # yOrderedG_l$ID2 <- factor(yOrderedG_l$ID2, levels = colnames(yOrderedG))
  #
  # # Plot the Y ordered GRM
  # png(paste0("yOrderedGRM_", year, ".png"))
  # #print(image(as(xOrderedG, "Matrix"), main = paste0("Year", year)))
  # yearPlot <- ggplot(data = yOrderedG_l, aes(x = ID1, y = ID2, fill = value)) +
  #   geom_tile() +
  #   theme(axis.text = element_blank(),
  #         axis.ticks = element_blank()) +
  #   scale_fill_gradientn(colors = hcl.colors(10),
  #                        values = c(-0.2, 0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.8, 2),
  #                        limits = c(-0.2, 2)) +
  #   ggtitle(paste0("Year", year))
  # print(yearPlot)
  # dev.off()
}

system(paste0("convert -delay 100 -loop 0 ", paste0("xOrderedGRM_", 1:nYear, ".png", collapse = " "), " xOrderedGRM.gif"))
system(paste0("convert -delay 100 -loop 0 ", paste0("yOrderedGRM_", 1:nYear, ".png", collapse = " "), " yOrderedGRM.gif"))
system(paste0("convert -delay 100 -loop 0 ", paste0("GrmDistance", 1:nYear, ".png", collapse = " "), " GrmDistance.gif"))
#system(paste0("convert -delay 100 -loop 0 ", paste0("X_twoPlot_", 1:nYear, ".png", collapse = " "), " X_twoPlot.gif"))
#system(paste0("convert -delay 100 -loop 0 ", paste0("Y_twoPlot_", 1:nYear, ".png", collapse = " "), " Y_twoPlot.gif"))
