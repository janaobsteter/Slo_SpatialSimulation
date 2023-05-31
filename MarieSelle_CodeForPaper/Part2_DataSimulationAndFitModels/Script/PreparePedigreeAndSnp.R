#Script name: PreparePedigreeAndSnp.R
#Author: Maria L. Selle
#Description: Prepare for fitting pedigree and genomic relationship matrix


if(useMarkers){
  # Get genotypes 
  genotypes11 = read.table(paste0(part1Results, "/Rep", rep, "/scenario",breedingScenario,"/11thGen/genotype.txt"))
  genotypes12 = read.table(paste0(part1Results, "/Rep", rep, "/scenario",breedingScenario,"/12thGen/genotype.txt"))
  
  
  info11 = read.table(paste0(part1Results, "/Rep", rep, "/scenario",breedingScenario,"/11thGen/info.txt"), header = T)
  info12 = read.table(paste0(part1Results, "/Rep", rep, "/scenario",breedingScenario,"/12thGen/info.txt"), header = T )
  sameInd = sapply(info12$id, function(x)  x %in% info11$id)

  # Sample 200 individuals from generation 12 randomly
  nIndPredict = 200
  indPredict = sample(x = (1:nrow(genotypes11))[!sameInd], size = nIndPredict, replace = F) 
  genotypes12 = genotypes12[indPredict,]
  
  genotypes = rbind(genotypes11,genotypes12)
  
  nMarkers = ncol(genotypes)
  nIndGenot = nrow(genotypes)
  
  # First center and scale genotypes
  Maf = rep(NA, times=nMarkers)
  n2 = 2 * nIndGenot
  for (Marker in 1:nMarkers) {
    Maf[Marker] = sum(genotypes[, Marker]) / n2
    genotypes[, Marker] = genotypes[, Marker] - 2 * Maf[Marker]
  }
  (Scale = 2 * sum(Maf * (1 - Maf)))
  (Scale = sqrt(Scale))
  genotypes = genotypes/Scale
  
  # Sample covariance
  V = tcrossprod(as.matrix(genotypes) ) 

  Q = solve(V + diag(x=rep(0.01), nrow=nrow(V))) # fudge diagonal
  
  print(dim(Q))
  rm(V, genotypes11, genotypes12, genotypes)
  
  

  
  
}else{
  
  
  
  
  # Individuals to predict 

  info11 = read.table(paste0(part1Results, "/Rep", rep, "/scenario",breedingScenario,"/11thGen/info.txt"), header = T)
  info12 = read.table(paste0(part1Results, "/Rep", rep, "/scenario",breedingScenario,"/12thGen/info.txt"), header = T)
   

  
  # Predict on all individuals that are not phenotyped 
  sameInd = sapply(info12$id, function(x)  x %in% info11$id)
  indPredictPedigree = info12$id[!sameInd]

  
  
  # Take out pedigree for generations 12, 11, 10, 9, 8
  ped = read.table(paste0(part1Results, "/Rep", rep, "/scenario",breedingScenario,"/12thGen/ped.txt"), header=T)

  ped$mother[1:(5000 + nInd*6)] = 0
  ped$father[1:(5000 + nInd*6)] = 0
  # 
  ped2 = data.frame(id = rownames(ped),  mid = ped$mother,fid = ped$father)
  
  # Pedigree object for pedigreemm functions
  pedPMM = pedigree(sire=ped2$fid, dam=ped2$mid, label=ped2$id)

  # Precision matrix (A inverse)
  Tinv    = as(pedPMM, "sparseMatrix") ## T^{-1} in A^{-1} = (T^{-1})' D^{-1} T^{-1}
  DmatTemp = pedigreemm::Dmat(pedPMM)
  D       = Diagonal(x=DmatTemp)   ## D      in A = TDT'
  Dinv    = solve(D)                   ## ...
  Ainv = t(Tinv) %*% Dinv %*% Tinv  ## ...

  rm(Tinv, D, Dinv, pedPMM,ped,ped2) 
}
