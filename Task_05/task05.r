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