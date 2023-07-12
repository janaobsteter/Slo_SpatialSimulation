library(profr)

# Profile the code
summary <- parse_rprof("~/EddieDir/Honeybees/SloSpatialSimulation/Spatial_NoLoc_554/CreateMultiColony.out", interval = 0.02)
ggplot.profr(summary, minlabel = 0.2)

# Summarise cross
crossOut <- summaryRprof("~/EddieDir/Honeybees/SloSpatialSimulation/Spatial_NoLoc_554/Cross.out")

# Plot

crossOut$by.self  %>%
  filter(self.pct > 1) %>%
  mutate(Function = reorder(rownames(.), -self.pct)) %>%
  ggplot(aes( x= Function, y = self.pct)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 18))

# Summarise create initial colonies
createOut <- summaryRprof("~/EddieDir/Honeybees/SloSpatialSimulation/Spatial_NoLoc_554/CreateMultiColony.out")

# Plot

createOut$by.self  %>%
  filter(self.pct > 5) %>%
  mutate(Function = reorder(rownames(.), -self.pct)) %>%
  ggplot(aes( x= Function, y = self.pct)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 18))

# Summarise everything
allOut <- summaryRprof("~/EddieDir/Honeybees/SloSpatialSimulation/Spatial_NoLoc_554/")

# Plot

createOut$by.self  %>%
  filter(self.pct > 5) %>%
  mutate(Function = reorder(rownames(.), -self.pct)) %>%
  ggplot(aes( x= Function, y = self.pct)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 18))
