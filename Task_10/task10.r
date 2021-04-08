setwd('~/Desktop/Evolution/Tasks/Task_10')
library('phytools')

#1-3
trees <-list()
births <- c()
Fractions <-c()

for(i in 1:100) {
	births[i]<- runif(1)
	Fractions[i]<- runif(1)
	trees[[i]] <- pbtree(b = births[i], d = (births[i]* Fractions[i]), n = 100, nsim=1)
}
trees
trees[[i]]
plot(trees[[i]])
library('geiger')

#4
install.packages('TreeTools')
library('TreeTools')

tips <- sapply(trees, NTip)
logtips <- log(tips)
diversification <-sapply(trees, bd.ms)
plot(diversification, logtips, xlab='net diversification', ylab='log of total number of tips')
abline(lm(diversification~logtips), col='red')
# There is a positive correlation between net diversification and the log of total number of tips.

#5
speciation <-sapply(trees, bd.km)
i<-1
numtips <- c()
avgBL <-c()

for(i in 1:length(trees)) {
	y <- trees[[i]]
	numtips[i] <- Ntip(y)
	avgBL[i] <- mean(y$edge.length)
}

plot(speciation, avgBL, xlab='speciation rate', ylab= 'average branch length')
# Branch length is inversely proportional to speciation rate.

#6
cor(speciation, avgBL)
#correlation = -0.17

#7
which.max(tips)
bigTree <-trees[[28]]
plot(bigTree)

rates <-c()
traits <-list()
for (i in 1:100) {
	rates[i] <-runif(1)
	traits[[i]] <-fastBM(tree=bigTree, sig2= rates[i])
	}
	
#8
avgtrait <-sapply(traits, mean)
avgtrait
avgrate <-sapply(rates,mean)
avgrate
correlation <-cor(avgtrait,avgrate)
print(correlation)
plot(avgrate, avgtrait)
abline(lm(avgrate~avgtrait), col='blue')
# The correlation I got was -0.122

#9
vartraits <-sapply(traits,var)
cor(vartraits, rates)
# The correlation is low but positive. I got 0.69

#10
trait1 <- traits[1]
trait1
trait2 <-traits[2]
trait2
traitmat <-cbind(traits[[1]], traits[[2]])
traitmat
var(traitmat)
cor(traitmat[,1], traitmat[,2])
# The correlation is 1.17. I would say it is not significant because it is a lower number and close to zero.

plot(traitmat[,1], traitmat[,2])
abline(lm(traitmat[,1]~traitmat[,2]), col='pink')
