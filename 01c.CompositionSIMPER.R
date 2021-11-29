#01c.CompositionSIMPER
#follows 01b.SeedvsPlugComposition

###SIMPER comparing seed vs plug composition dissimilarity

#f.cover.plug is made in 00b.load.for.analyses
cov2019 <- read.csv("../DATA/2019_VegetationCover/COVER.sppCovMat.2019.MAX.csv")
cov2019df <- as.matrix((subset(cov2019, select = Agalinis_tenuifolia:Zizia_aurea)))
cov2019df[is.na(cov2019df)] <- 0 #replace NAs 

#finalSurvey <- rbind(seed2019, plug2019)
#finalSurvey[is.na(finalSurvey)] <- 0 #replace NAs 
cov2019[,1:4] -> final.env
sim <- with(final.env, simper(cov2019df, type, permutations = 999))
summary(sim) -> simper.out

###VS P/A
#cov <- rbind(seed2019, plug2019)
#cov[,1:4] -> cov.ev
#cov.df <- as.matrix((subset(cov, select = Agalinis_tenuifolia:Zizia_aurea)))
#cov.df[is.na(cov.df)] <- 0 #replace NAs 
#PA <- decostand(x=cov.df, method="pa") #convert to P/A
#sim <- with(cov.ev, simper(PA, type, permutations = 1000))

contribution.value <- cbind(rownames(simper.out$Seed_Plug), simper.out$Seed_Plug$average)

#sim is ordered by cusum not species; replace cumsum with average 
as.data.frame(contribution.value) -> contribution.value
names(contribution.value) <- c("species", "cumsum") #I'm labeling it cumsum but it's really average
#reorder contribution.value alphabetically
contribution.value.ordered <- contribution.value[order(contribution.value$species),]
#as.numeric(contribution.value.ordered$cumsum) -> contribution.value.ordered$cumsum


sim.dat<- as.data.frame(contribution.value$cumsum)
rownames(sim.dat) <- contribution.value$species

phy <- read.tree("../DATA/phylogeny.analyzed.2016-01-05b.tre") 
phy$tip.label    #140
phy$tip.label[which(phy$tip.label == "Symphyotrichum_novae-angliae")] <-
  "Symphyotrichum_novae.angliae"
tree <- drop.tip(phy, which(!phy$tip.label %in% rownames(sim.dat))) 
multi2di(tree) -> tree
ReorderData(tree, sim.dat) -> sim.dat.ordered
#rownames(plot.df.PA.ordered) -> plot.df.PA.ordered$Species
names(sim.dat.ordered) <- "aveSim"
sim.dat.ordered$aveSim <- as.numeric(sim.dat.ordered$aveSim)
Physig <- phylosig(tree, sim.dat.ordered$aveSim, method="lambda", test=TRUE)
PhysigK <- phylosig(tree, sim.dat.ordered$aveSim, method="K", test=TRUE)

