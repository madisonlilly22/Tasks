library(learnPopGen)
coalescent.plot(n=4, ngen=8, col.order = "alternating")
coalescent.plot(n=6, ngen=11, col.order = "alternating")
coalescent.plot(n=8, ngen=14, col.order = "alternating")
coal4 <- coalescent.plot(n=4, ngen=8, col.order = "alternating")
coal5 <- coalescent.plot(n=6, ngen=11, col.order = "alternating")
coal6 <- coalescent.plot(n=8, ngen=14, col.order = "alternating")




install.packages("learnPopGen")
library(learnPopGen)
install.packages("coala")
library(coala)
install.packages("phytools")
library(phytools)
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
feat_mutation(10) +
feat_recombination(10)+
sumstat_trees() +
sumstat_nucleotide_div()
#run simulation, change nsim
stats <- simulate(model, nsim=1)
#pi is avg # of diff at locus btwn 2 individ.
Diversity <-stats$pi
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
Age1 <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()

par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)

t1_1 <-read.tree(text=stats$trees[[1]][1])
t1_2 <-read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for (locus in 1:Nloci) {
	ntrees <-length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if (locus== 1 && n==1) {
			outPhy <- read.tree(text=stats$trees[[locus]][n])
			}
			else {
				outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
				}
				}
				}
par(mfrow=c (1,1))
densityTree(outPhy)	
model3 <- coal_model(10,50)+
feat)mutation(par_prior("theta", sample.int(100,1)))+
sumstat_nucelotide_div()
stats <- simulate(model3, nsim = 40)	
mean_pi <-sapply(stats, function(x) mean(x$pi))	
theta <- sapply(stats, function (x) x$pars[["theta"]])	