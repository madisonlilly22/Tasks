setwd('~/Desktop/Evolution/Tasks/Task_09')
#Unite data and phlyogenies into 1 big thing

library('phytools')
tree <-read.tree('https://jonsmitchell.com/data/anolis.tre')
plot(tree, type='fan')
tree$tip.label
#1 There are 82 tips and the branch lengths are present.

data <-read.csv('https://jonsmitchell.com/data/svl.csv', stringsAsFactors=F, row.names=1)
data[,1]
#2 "data" shows us a list of lizard species and snout-vent length. There are 100 dimensions.

#covert object into vector
svl <- setNames(data$svl, rownames(data))

Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
#3 The etsimated values are stored in a list containing the estimates. The CI95 element is the 95% confidence interval of the stored estimates.
#4 The assumptions are that the function will re-root the tree at the internal nodes and that the contrasting state will be at the root each time.

#plot all together
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)

#Add points instead of labels
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])

#Add ancestral states
nodelabels(pch=16, cex=0.25*Ancestors$ace)

obj <-contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))

#Add fossils in
fossilData <-data.frame(svl=log(c(25.4, 32.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))

#5
fossilNodes<-c()
nodeN <-c()
{
	for(i in 1:nrow(fossilData))
	i<-1
	if(i==1) {
		print(Ancestors) 
		}
}

Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i]<- fossilData[i, "svl"]
nodeN[i]<-Node

names(fossilNodes) <-nodeN

#Estimate ancesttral states
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)

#7
Ancestors_withFossils
Ancestors_woFossils <- fastAnc(tree, svl, CI=TRUE, var=TRUE)
Ancestors_woFossils
plot(Ancestors_withFossils$ace, Ancestors_woFossils$ace, xlab='fossils', ylab='no fossils')
#7 The estimated ancestral state is increased by fossils

#8-10
install.packages('geiger')
library('geiger')
?fitContinuous
fitContinuous(tree, svl, model='EB')
fitContinuous(tree, svl, model='OU')
fitContinuous(tree, svl, model='BM')
			
# EB has the lowest value so it should be the best fit for the data.
?fastAnc
#fastAnc uses BM so it is different. I would still say the best fit would be EB			