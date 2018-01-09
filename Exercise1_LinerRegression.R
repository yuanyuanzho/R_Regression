#example1
summary(cars)
View(cars)

plot(cars,col='blue',pch=20,cex=2,main="Relationship between Speed and Stopping Distance for 50 Cars", xlab="Speed in mph",ylab="Stopping distance in feet")

set.seed(122)
# scale函数是将一组数进行处理
# 默认情况下是将一组数的每个数都减去这组数的平均值后再除以这组数的均方根
# 其中有两个参数，center=TRUE，默认的，是将一组数中每个数减去平均值，若为false，则不减平均值；
# scale=TRUE：默认的，是将一组数中每个数除以均方根(根号下 x1^2 + x2^2  / n )
speed.c = scale(cars$speed, center=TRUE, scale=FALSE) 
mod1 = lm(formula = dist ~ speed.c,data = cars)
plot(mod1)
summary(mod1)

#example2. using import dataset
species.fit = lm(SpeciesData$NUMSPECIES ~ SpeciesData$AREASQKM + SpeciesData$MAXELEV+ SpeciesData$NUMSOILTYPES + SpeciesData$MIDLATITUDE + SpeciesData$DISTFROMBRITAIN + SpeciesData$NUMSPECIES)
summary(species.fit)

library(corrplot)
M.SpeciesData = cor(SpeciesData[,2:7])
corrplot(M.SpeciesData, method="number")
plot(SpeciesData$MIDLATITUDE, SpeciesData$NUMSPECIES)

#example2. using csv
species = read.csv("/Users/Rui/Documents/Academic/Fall 2017/INFO7390/Week1and2/SpeciesData.csv")
View(species)
species.fit = lm(species$NUMSPECIES ~ species$AREASQKM + species$MAXELEV+ species$NUMSOILTYPES + species$MIDLATITUDE + species$DISTFROMBRITAIN + species$NUMSPECIES)
summary(species.fit)





