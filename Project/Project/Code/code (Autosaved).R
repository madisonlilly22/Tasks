setwd('~/Desktop/Evolution/Tasks/Project/Project/Code')
Data <- read.csv('~/Desktop/Evolution/Tasks/Project/Project/Data/Mexican cavefish data.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)