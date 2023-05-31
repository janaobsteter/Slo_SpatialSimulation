#Script Name: CreateParents.R
#Author: Owen Powell
#Description: Specification of Historical Population Parameters and Creating of Founder Haplotypes

### ----------------------------------------------------

macsCommand = function(size){
  paste(1.00E+08,"-t",4.00E-05,"-r",4.00E-05,"-I 1",size,
        "-eN 0.0045 1.25 -eN 0.006 2 -eN 0.0385 3 -eN 0.1135 4 -eN 0.1635 5 -eN 0.4385 10 -eN 0.5885 15 -eN 0.8385 25 -eN 8.2885 100 -s",sample.int(1e8,1))
}


FOUNDERPOP = runMacs(nInd= nCows,
                     nChr=nChr,
                     segSites=nQtl+nSnp,
                     manualCommand = macsCommand(nHaplotypes),manualGenLen = 1)#,nThreads = 5)




SP = SimParam$new(FOUNDERPOP)
SP$setTrackPed(TRUE) #creates a tracked pedigree. Usefull for retrospective problem solving
SP$setGender("yes_rand")


if(nSnp>0){
  SP$addSnpChip(nSnpPerChr=nSnp)
}

SP$addTraitA(nQtlPerChr=nQtl,mean=c(0,0),var=c(1,1),corA=matrix(1,nrow=2,ncol=2))$setVarE(h2=c(0.3,0.8))

### FOUNDER POPULATION 

initPop = newPop(FOUNDERPOP)

cows = initPop[initPop@gender=="F"]
bulls = initPop[initPop@gender=="M"]
bulls = selectInd(bulls,n=(nAIBulls+nNSBulls),use="gv")

