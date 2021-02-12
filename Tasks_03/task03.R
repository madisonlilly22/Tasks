# Make our populations
setwd('~/Desktop/Evolution/Tasks/Tasks_03')
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean1, trueSD1)
# Sample of each population
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
# compare samples w/ boxplot
boxplot(Sample1, Sample2)
# The populations were different but not a significant amount

source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
head(MatGrandma)
MatGrandpa <- makeFounder("grandpa_mom")
head(MatGrandpa)
PatGrandma <- makeFounder("grandma_da")
head(PatGrandma)
PatGrandpa <- makeFounder("grandpa_da")
head(PatGrandpa)
# make Alan
Alan <- makeBaby(PatGrandma, PatGrandpa)
# make Brenda
Brenda <- makeBaby(MatGrandma, MatGrandpa)
# First child, focus child
Focus <- makeBaby(Brenda, Alan)
ToMom <- length( grep("mom", Focus))/ length(Focus)
ToMom
# The number is 0.5. I figured it would be around that.
ToMomMom <- length(grep("grandma_mom", Focus))/ length(Focus)
ToMomMom
# The number is 0.4439
ToMomDad <- length(grep( "grandma_mom", Focus))/ length(Focus)
ToMomDad
# The number is 0.4439
# The number is the same for both maternal grandparents and it is actually higher than I expected.
Sibling_01 <- makeBaby(Brenda, Alan)
# I figure it will be a less than 0.5.
ToSib <- length(intersect(Focus, Sibling_01))/ length(Focus)
Tosib
# The number was a lot less than I expected
# 1000 siblings
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/ length(Focus))
# summarize data
quantile(ManySiblings)
mean(ManySiblings)
# Plot data
plot(density(ManySiblings), main= "", xlab="proportion shared genes")
# The values range because you do not inherit all/every gene from everyone in your family tree.

HWE <- function(p) {
	aa <- p^2
	ab <- 2*p*(1-p)
	bb <- (1-p)^2
	return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
# aa=0.25 ab=0.50 bb=0.25
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
# Plot known vs expected
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
# Shows inc in geno freq
lines(p, GenoFreq[, "ab"], lwd=2, col="purple")
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")

Pop<- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
# No it doesn't
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#

install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
x <- genetic.drift(Ne=400, nrep=5, pause=0.01)
x <- genetic.drift(Ne=700, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)

Extra Credit
Line2 <-lm(tExt~Samples+0)
summary(Line2)
Line2$coef
plot(Samples, tExt)
abline(Line2)
