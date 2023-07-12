# Plot the timing
library(ggplot2)
library(dplyr)

eddieDir <- "~/EddieDir/Honeybees/SloSpatialSimulation/"
homeDir <- "~/Documents/1Projects/SIMplyBee_devel/Spatial/"

setwd(eddieDir)
fTimeS <- read.csv("Spatial_NoLoc_554/FunctionsTime.csv")[-1,]
fTimeS$Mating <- "Spatial"
fTimeR <- read.csv("Random_NoLoc_554/FunctionsTime.csv")[-1,]
fTimeR$Mating <- "Random"
fTime <- rbind(fTimeS, fTimeR)
fTime$TimeMin <- fTime$Time / 60

# Year 1
y1 <- fTime[fTime$Year == 1,]
y1$TimePerColony <- y1$Time / y1$nColonies
y1P1 <- y1[y1$Period == 1,]

# Plot
ggplot(data = y1P1, aes(x = Function, y = TimePerColony)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))


# Plot year 1
y1$Mating <- as.factor(y1$Mating)
y1$Period <- as.factor(y1$Period)
y1$Function <- as.factor(y1$Function)
y1 %>% dplyr::group_by(Mating, Period, Function) %>%
  summarise(Time = sum(Time)) %>%
  mutate(PeriodFunction = paste0(Period, "_", Function)) %>%
  ggplot(aes(x = PeriodFunction, y = Time, fill = Period)) +
  geom_col() + theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  facet_grid(rows = vars(Mating))


y1 %>% dplyr::group_by(Mating, Period, Function) %>%
  summarise(TimePerColony = sum(TimePerColony)) %>%
  dplyr::mutate(PeriodFunction = paste0(Period, "_", Function)) %>%
  ggplot(aes(x = PeriodFunction, y = TimePerColony, fill = Period)) +
  geom_col() + theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  facet_grid(rows = vars(Mating))


# Combine all runs
s22 <- read.csv(paste0(homeDir, "Spatial_NoLoc_22/FunctionsTime.csv"))[-1,]
s22$Mating <- "Spatial"
s50 <- read.csv(paste0(homeDir, "Spatial_NoLoc_50/FunctionsTime.csv"))[-1,]
s50$Mating <- "Spatial"
s500 <- read.csv(paste0(eddieDir, "Spatial_NoLoc_554/FunctionsTime.csv"))[-1,]
s500$Mating <- "Spatial"
s1000 <- read.csv(paste0(eddieDir, "Spatial_NoLoc_1050/FunctionsTime.csv"))[-1,]
s1000$Mating <- "Spatial"

r22 <- read.csv(paste0(homeDir, "Random_NoLoc_22/FunctionsTime.csv"))[-1,]
r22$Mating <- "Random"
r50 <- read.csv(paste0(homeDir, "Random_NoLoc_50/FunctionsTime.csv"))[-1,]
r50$Mating <- "Random"
r500 <- read.csv(paste0(eddieDir, "Random_NoLoc_554/FunctionsTime.csv"))[-1,]
r500$Mating <- "Random"
r1000 <- read.csv(paste0(eddieDir, "Random_NoLoc_1050/FunctionsTime.csv"))[-1,]
r1000$Mating <- "Random"

all <- rbind(s22, s50, s500, s1000, r22, r50, r500, r1000)

#Create clonies
all %>% filter(Function == "CreateInitialColonies") %>%
  ggplot(aes(x = nColonies, y = Time)) + geom_line() +
  ggtitle("CreateColonies")

#Cross
all %>% filter(Function == "Cross") %>% group_by(Mating) %>%
  ggplot(aes(x = nColonies, y = Time, colour = Mating)) + geom_line() +
  ggtitle("Cross")


#Buildup
all %>% filter(Function == "BuildUp") %>%
  group_by(nColonies) %>%
  summarise(Time = mean(Time)) %>%
  ggplot(aes(x = nColonies, y = Time)) + geom_line() +
  ggtitle("Build-up")

#Split
all %>% filter(Function == "Split") %>%
  group_by(nColonies) %>%
  summarise(Time = mean(Time)) %>%
  ggplot(aes(x = nColonies, y = Time)) + geom_line() +
  ggtitle("Split")

#Set location
all %>% filter(Function == "SetLocation") %>%
  group_by(nColonies) %>%
  summarise(Time = mean(Time)) %>%
  ggplot(aes(x = nColonies, y = Time)) + geom_line()+
  ggtitle("SetLocation")


#Set beekeeper
all %>% filter(Function == "SetBeekeeper") %>%
  group_by(nColonies) %>%
  summarise(Time = mean(Time)) %>%
  ggplot(aes(x = nColonies, y = Time)) + geom_line()+
  ggtitle("SetBeekeper")


