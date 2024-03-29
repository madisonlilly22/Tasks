install.packages("phytools")
library(phytools)
install.packages("ape")
library(ape)
text.string<-
"(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)

nodelabels(frame="circle", bg='white', cex=1)
#1 A shark

vert.tree

str(vert.tree)
#2 No, no branch lengths

tree<-read.tree(text="(((A,B), (C,D)),E);")
plotTree(tree,offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)

tree$tip.label

tree$edge

AnolisTree <-force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))

par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0,6))

tipEdges <-which(AnolisTree$edge[,2]<= Ntip(AnolisTree))
Lengths <-AnolisTree$edge.length
names(Lengths) <-AnolisTree$tip.label
names(Lengths)[which(Lengths== min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs <-sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

plot.phylo
#3
plotTree(AnolisTree, offset=1, show.tiplabel=FALSE)
#4
anolis <-collapseTree(AnolisTree)
#5
plotTree(AnolisTree, offset=1, show.tip.label=TRUE, tip.color="red")

#6,7,8
AnolisTree$edge
EdgesThatAreTips <- which(AnolisTree$edge[,2] <=Ntip(AnolisTree))
TipLengths <-AnolisTree$edge.length[EdgesThatAreTips]
AnolisTree$edge.length
AnolisShortestTips <-which(TipLengths == min(TipLengths))
Anolistree_no_short <- drop.tip(tree, AnolisShortestTips[1])
plot(Anolistree_no_short)


ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
# The line is always positive. No, it never goes down because species keep appearing. The slope is always positive, which tells you there is always a new type of lizard.
#10
fit.bd()