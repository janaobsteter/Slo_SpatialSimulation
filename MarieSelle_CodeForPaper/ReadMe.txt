Spatial & Genetic Analysis of Smallholder Dairy Farms

Description of simulation for spatial and genetic modelling of smallholder breeding systems using INLA. 

*** Part 1, author: Owen Powell ***

This part consists of simulation of smallholder cattle populations, and can be broken down in to 5 simple steps:
1. Creation of the Base Population
2. Assignment of cows in the Base Population to Villages and Farms. 
3. Assignment of bulls in the Base Population to Villages and Farms. 
4. Mating of animals.
5. Selection of animals.

Scenario A corresponds to weak genetic connectedness, scenario B to intermediate genetic connectedness, and scenario C to strong genetic connectedness. 

To run the simulation, scripts for these steps are called in a master script called ‘RunSim.R’.

*** Part 2, author: Maria L. Selle ***

This part consists of simulation of all effects besides the genetic effects, which are simulated in part 1, and of the model fitting using INLA. The simulation can be broken down into 5 simple steps:
1. Simulation of spatial effects
2. Simulation of phenotypes
3. Preparation of covariates and model formulae
4. Fitting models 
5. Computing and storing results 

To run the simulation, scrips for these steps are called in a master script called ´RunSimulationStudy.R´. Part 1 must be completed before starting part 2, and stored in a directory which must be specified in ´RunSimulationStudy.R´.

