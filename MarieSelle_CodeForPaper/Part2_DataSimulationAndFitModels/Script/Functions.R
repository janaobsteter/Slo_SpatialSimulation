#Script name: Functions.R
#Author: Maria L. Selle
#Description: Libraries and functions


cat("Loading packages...\n")
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
# install.packages("fields")
library(fields)
library(MASS)
#install.packages("pedigreemm",repos = "http://cran.us.r-project.org")
library(package="pedigreemm")
#install.packages("pryr",repos = "http://cran.us.r-project.org")
library(pryr)


createAndSetScenarioFolder <- function(x) {
  if (dir.exists(paths=x)) {
    unlink(x=x, recursive=TRUE)
  }
  dir.create(path=x, recursive = T)
  setwd(dir=x)
}


# Simulate random field
SimulateSpdeField <- function(mesh, n.fields,range0,sigma0){
  # mesh: an inla mesh
  # n.fields: how many simulated fields
  # range0: number
  # sigma0: number
  
  # Compute values from known range and variance
  kappa0 <- sqrt(8)/range0
  tau0 <- 1/(sqrt(4*pi)*kappa0*sigma0)
  
  # Define the spde object
  spde.stat <- inla.spde2.matern(mesh=mesh,B.tau = matrix(c(0,1,0), nrow=1,ncol = 3), B.kappa=matrix(c(0,0,1), nrow = 1,ncol = 3))
  
  # Compute the precision matrix of the spde object
  Q.stat <- inla.spde2.precision(spde = spde.stat, theta = c(log(tau0),log(kappa0)))
  
  # Draw sample
  sample.field <- as.vector(inla.qsample(n = n.fields, Q = Q.stat))
  
  return(sample.field)
}

# Plot SPDE field
PlotSpdeField = function(mesh,sample.field1,zlim =NULL){
  proj <- inla.mesh.projector(mesh,dims=c(300, 300))
  field.proj1 <- inla.mesh.project(proj, sample.field1)
  image.plot(list(x = proj$x, y=proj$y, z = (field.proj1)),zlim=zlim, ylim = c(0,1), xlim= c(0,1))
}


# Check that farms lie inside the domain
SimulateFarmLocation = function(villageLoc, nHerds, sigmaHerds) {
  
  # Generate locations until all are inside the domain
  herdLocations = mvrnorm(n = nHerds, mu = villageLoc, Sigma = sigmaHerds*matrix(c(1,0,0,1),ncol = 2)) 

  # If some herds are outside the 0 1 square
  while( sum(herdLocations[,1] >= 1 | herdLocations[,1] <= 0 | herdLocations[,2] >= 1 | herdLocations[,2] <= 0)>0 ){
    whichRowReplace = which( rowSums( herdLocations < 0 | herdLocations > 1 )  >0  )
    
    herdLocations[whichRowReplace, ] = mvrnorm(n = length(whichRowReplace), mu = villageLoc, Sigma = sigmaHerds*matrix(c(1,0,0,1),ncol = 2)) 
  }
  return(herdLocations)
} 

ExpandVector = function(vectorToExpand, expandBy){
  n = length(expandBy)
  return(  unlist(sapply(1:n, function(x) rep(vectorToExpand[x], expandBy[x])  )) )
  
  
}

