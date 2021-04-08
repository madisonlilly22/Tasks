setwd('~/Desktop/Evolution/Tasks/Project/Project/Code')
Data <- read.table('~/Desktop/Evolution/Tasks/Project/Project/Data/Mexican cavefish data.csv', stringsAsFactors=F, sep=";")

# Reformat data
Colnames <- Data[1,]
Cols <- c(6,8,9)

Data2 <- Data[-1,]
Data2[,Cols[1]] <- gsub(",", ".", Data2[,Cols[1]])
Data2[,Cols[2]] <- gsub(",", ".", Data2[,Cols[2]])
Data2[,Cols[3]] <- gsub(",", ".", Data2[,Cols[3]])

colnames(Data2) <- c("date", "time", "start", "fishType", "runID", "fishMass", "photoperiod", "expTime", "oxygen", "", "")

write.csv(Data2, 'formatted_data.csv', quote=F)
###
setwd('~/Desktop/Evolution/Tasks/Project/Project/Code')
Data <- read.csv("formatted_data.csv", row.names=1, header=T)
head(Data)

Surface <- which(Data$fishType == "Surface")
Pachon <- which(Data$fishType == "Pachon")
plot(Data$fishMass, Data$oxygen, type="n", xlab="Mass", ylab="Oxygen")
points(Data$fishMass[Surface], Data$oxygen[Surface], bg='pink', pch=21)
points(Data$fishMass[Pachon], Data$oxygen[Pachon], bg='yellow', pch=21)

abline(lm(Data$oxygen[Surface]~Data$fishMass[Surface]), lty=2, col="pink", lwd=2)

abline(lm(Data$oxygen[Pachon]~Data$fishMass[Pachon]), lty=2, col="yellow", lwd=2)