# Load in real progeny-test data
ptSLO <- read.csv("~/Documents/1Projects/SIMplyBee_devel/Spatial/PhenoData/ProgeniTest.csv")
ptSLO <- ptSLO[, c("zap_st", "sifra_ceb", "sifra_linije", "sif_linija", "linija_ID", "NASLOV", "ST_MATICE", "VZREJA", "LINIJA", "MED", "LETOM", "LETOP")]
table(ptSLO$LETOM) #Year of the mother
table(ptSLO$LETOP) #Year of the progeny
table(ptSLO$LETOM, ptSLO$LETOP)

# Understand the data
nrow(ptSLO)
length(unique(ptSLO$sifra_ceb))
length(unique(ptSLO$sifra_linije))
length(unique(ptSLO$sif_linija))
length(unique(ptSLO$linija_ID))
table(ptSLO$VZREJA)

# Year 2022
ptSLO22 <- ptSLO[ptSLO$LETOP == 2022 ,]
nrow(ptSLO22)

# Inspect honey yield
ptSLO22$MED <- as.numeric(ptSLO22$MED)
summary(ptSLO22$MED)
mean(ptSLO22$MED, na.rm=TRUE)
var(ptSLO22$MED, na.rm=TRUE)
sd(ptSLO22$MED, na.rm=TRUE)

# Load in the direct test data
dirSLO <- read.csv("~/Documents/1Projects/SIMplyBee_devel/Spatial/PhenoData/DirektniTest2022.csv")
nrow(dirSLO)
length(unique(dirSLO$rodovniška))
dirSLO$med <- as.numeric(dirSLO$med)
mean(dirSLO$med, na.rm=TRUE)
sd(dirSLO$med, na.rm=TRUE)
var(dirSLO$med, na.rm=TRUE)

# Progeny and direct test honey-yield together (from 2022)
hy22 <- as.numeric(c(ptSLO$MED, dirSLO$med))
mean(hy22, na.rm=T); sd(hy22, na.rm=T); var(hy22, na.rm=T)
