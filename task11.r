setwd('~/Desktop/Evolution/Tasks/Task_11')
install.packages("diversitree")
library('diversitree')

#parameters
transition_0to1 <-0.1
transition_1to0 <- 0.1

speciation_0 <- 0.2
extinction_0 <-0.15

speciation_1 <-0.4
extinction_1 <-0.1

#stop parameters
maxN <-1e3
maxT <-50

Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)

simTree <-tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
#?tree.bisse()

stateTable <-table(simTree$tip.state)
stateTable / sum(stateTable)
# state 0 and 1... 0=0.246 1=0.754

# Yes, when the net diversification of 1 was very close below state 0
Frequencies <- c('State 0', 'State 1')
Colors <- c('orange', 'red')
Data <- matrix(c(0.22, 0.86, 0.54, 0.67, 0.31, 0.49, 0.82, 0.57, 0.35, 0.26, 0.63, 0.46), nrow=2, ncol=6, byrow=TRUE)
Data
Difference <- c(0.12, 0.2, 0.025, 0.04, 0.03, 0.03)
Freq1 <- c(0.25, 0.43, 0.31, 0.72, 0.32, 0.28)
Freq0 <- c(0.82, 0.43, 0.62, 0.58, 0.44, 0.64)

pdf('Question1.pdf', height=6, width=6)
barplot(Data, names.arg=Difference, main= 'Changes in Frequency of States based on Variation in R Values', xlab= 'Difference in Diversification Rate', ylab= 'Frequency', beside=TRUE, col= c('orange', 'red')
)
legend('topright', Frequencies, fill= Colors)
dev.off()

# I never got it exactly to zero, but I got it close.
Frequencies <- c('State 0', 'State 1')
Colors <- c('blue', 'yellow')
Data <- matrix(c(0.82, 0.8, 0.96, 0.85, 0.63, 0.9, 0.926, 0.923, 0.959, 0.955, 0.945, 0.968, 0.977, 0.963, 0.978, 0.984, 0.973, 0.18, 0.2, 0.04, 0.14, 0.37, 0.088, 0.074, 0.077, 0.041, 0.045, 0.055, 0.032, 0.023, 0.037, 0.022, 0.016, 0.027), nrow=2, ncol=17, byrow=TRUE)
Data
Difference <- c(0.05, 0.05, 0, 0, 0, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.45, 0.45, 0.45)
pdf('Question2.pdf', height=8, width=8)
barplot(Data, names.arg=Difference,
main='How Close State 1 is to Zero When Transition Rate is Nonzero',
xlab='Difference in Diversification Rate', 
ylab='Frequencies', 
col=c('blue', 'yellow')
)
legend('topright', Frequencies, fill= Colors)
dev.off()

# There actually was a lot of variation.
Data <- read.csv('~/Desktop/Evolution/Tasks/Task_11/Question3_Data.csv', stringsAsFactors=F)
head(Data)
Freq1_Trial1 <- Data[,2]
Freq1_Trial2 <- Data[,5]
Freq1_Trial3 <- Data[,8]
Variance1 <- var(Freq1_Trial1)
Variance2 <- var(Freq1_Trial2)
Variance3 <- var(Freq1_Trial3)
Variance1
Variance2
Variance3
VarianceMatrix <- c(Variance1, Variance2, Variance3)
VarianceMatrix
Trial <- c(1,2,3)
pdf('Question3.pdf', height=8, width=8)
barplot(VarianceMatrix, names.arg=Trial,
	main='Variance of Frequency 1 in Each Trial', 
	ylim= c(0,0.5),
	xlab='Trial Number',
	ylab='Variance in Frequencies',
	col='purple')
dev.off()

# Something such as migration, interbreeding or selection

Data <- read.csv('~/Desktop/Evolution/Tasks/Task_11/MyTrend.csv', stringsAsFactors=F)
head(Data)
Freq_0 <-Data[,2]
Freq_0
NDR_0 <- Data[,1]
pdf('MyTrend.pdf', height=8, width=8)
plot(NDR_0, Freq_0, xlab='Net Diversification Rate of State 0', ylab='Frequency of State 0', main='How Net Diversification Rate Influences Frequency')
abline(lm(Freq_0~NDR_0), col='yellow', lty='dahsed')
dev.off()
Freq_1 <-Data[,7]
NDR_1 <- Data[,5]
pdf('MyTrend.pdf', height=8, width=8)
plot(NDR_1, Freq_1, xlab='Net Diversification Rate of State 1', ylab='Frequency of State 1', main='How Net Diversification Rate Influences Frequency')
abline(lm(Freq_1~NDR_1), col='red', lty='dashed')
dev.off()



























































