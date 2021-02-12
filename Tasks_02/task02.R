setwd('~/Desktop/Evolution/Tasks/Tasks_02')
Data <- read.csv ('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds ,]
head(berenMilk)
Feeds <- which(Data[, 'event'] == 'bottle')
Feeds <- which (Data$event == 'bottle')
dayID <- apply (Data, 1, function(x) paste (x[1:3], collapse= '-'))
dateID <- sapply (dayID, as.Date, format = "%Y-%m-%d" , origin= "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]

#Correlation doesn't mean causation 
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value [Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <-aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds]~ beren3$caregiver[Feeds], xlab= "who have the bottle" , ylab = "amount of milk consumed (oz)")
?par
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b" , pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16 xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
#The data would be hard to interpret because it is only looking at the amount Beren drinks at the nursery, not anywhere else such as his home.

Bonus
beren4 <- beren3[Naps,]
startHour <- (beren4$start_hour)
startMin <- (beren4$start_minute)
stopHour <- (beren4$end_hour)
stopMin <- (beren4$end_minute)
startHour
startMin
stopHour
stopMin
beren4$sleepTime <- ((stopHour - startHour)*60)+(stopMin - startMin)
beren4
totalNap <-tapply(beren4$sleepTime, beren4$age, sum)
TotalNap
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalNap)), totalNap, type="b", pch=16, xlab="age in days",ylab="Nap time in minutes")
cor.test(beren4$start_hour, beren4$sleepTime)
#The relationship will be negative

#My hypothesis is that over time Berens head will increase in size.
Circumference <- which(Data[,9] == 'trait_head_circum')
berenCircumference <- Data[Circumference,]
Circumference <- which(Data[,'event'] == 'trait_head_circum')
Circumference <- which(Data$event == 'trait_head_circum')
Circumference <- which (berenCircumference$event == 'trait_head_circum')
avgCircumference <-mean(berenCircumference$value[Circumference])
avgCircumference <-tapply(berenCircumference$value[Circumference], berenCircumference$age[Circumference], mean)
varCirumference <-tapply(berenCircumference$value[Circumference], berenCircumference$age[Circumference], var)
totalCircumference <-tapply(berenCircumference$value[Circumference], berenCircumference$age[Circumference], sum)
numCircumference <-tapply(berenCircumference$value[Circumference], berenCircumference$age[Circumference], length)
cor(berenCircumference$value[Circumference], berenCircumference$age[Circumference])
cor.test(berenCircumference$value[Circumference], berenCircumference$age[Circumference], method="spearm",alternative="greater")
CircumferenceCor <-cor.test(berenCircumference$value[Circumference], berenCircumference$age [Circumference], method="spearm", alternative="greater")
summary(trait_head_circumCor)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)