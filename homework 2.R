climate = read.csv("climate_change.csv")
#moneyball = subset(baseball, Year < 2002)
climate_training = subset(climate, Year < 2007)
climate_test = subset(climate, Year > 2006)
model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + 
              TSI + Aerosols, data = climate_training )
summary(model1)
cor(climate_training)
model2 = step(model1)
summary(model2)
summary(model1)
#step fuction does not address colinearity
#PointsPredictions = predict(PointsReg4, newdata=NBA_test)
ClimatePred = predict(model2, newdata=climate_test)
ClimatePred

#SSE = sum((PointsPredictions - NBA_test$PTS)^2)
#SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
#R2 = 1 - SSE/SST
SSE = sum((ClimatePred - climate_test$Temp)^2)
SST = sum((mean(climate_training$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2


#Part 2. reading test scores

pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

str(pisaTrain)
malestraining = subset(pisaTrain, male == 1)
str(malestraining)
#sort(tapply((CPS$Race == "Asian"), CPS$MetroArea, mean))
femalestraining = subset(pisaTrain, male == 0)
mean(femalestraining$readingScore)
mean(malestraining$readingScore)
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

str(pisaTrain)
summary(pisaTrain)
#remove na values 
pisaTrain = na.omit(pisaTrain)
pisaTest=na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)
table(pisaTrain$raceeth)

#setting the reference level of the factor
pisaTrain$raceeth = relevel(pisaTrain$raceeth,"White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
ImScore = lm(readingScore ~ ., data = pisaTrain)
summary(ImScore)

#PointsReg$residuals
#SSE = sum(PointsReg$residuals^2)
#SSE

# Root mean squared error
#RMSE = sqrt(SSE/nrow(NBA))
#RMSE
SSE = sum(ImScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE
ImScoregrade = lm(readingScore ~ grade, data = pisaTrain)
summary(ImScoregrade)
plot(pisaTrain$readingScore, pisaTrain$grade)
mean(pisaTrain$readingScore)
summary(ImScore)

#predict 
#ClimatePred = predict(model2, newdata=climate_test)
predTest = predict(ImScore, newdata=pisaTest)
summary(predTest)
353.2 - 637.7

# Compute out-of-sample R^2
#SSE = sum((PointsPredictions - NBA_test$PTS)^2) test data and predict result
#SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
#R2 = 1 - SSE/SST


# Compute the RMSE
#RMSE = sqrt(SSE/nrow(NBA_test))
SSE = sum((predTest - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE
mean(pisaTest$readingScore)
SST = sum((mean(pisaTest$readingScore) - pisaTest$readingScore)^2)
SST
R2 = 1-SSE/SST
R2

#part 3 flu
FluTrain = read.csv("FluTrain.csv")
max(FluTrain$ILI)
which.max(FluTrain$ILI)
FluTrain$Week[which.max(FluTrain$ILI)]
hist(FluTrain$ILI)
plot(log(FluTrain$ILI), FluTrain$Queries)
plot(FluTrain$ILI, FluTrain$Queries)
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
cor(log(FluTrain$ILI), FluTrain$Queries)

FluTest = read.csv("FluTest.csv")
predTest1=exp(predict(FluTrend1,newdata=FluTest))
summary(predTest1)
FluTest$Week
0.49934 + 0.4329349*2.96129
#still don't understand problem 3.1 
(2.2934216-2.187383)/2.2934216
#SSE = sum((predTest - pisaTest$readingScore)^2)
SSE = sum((predTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
#installing and using packages
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)
plot(log(ILILag2), log(FluTrain$ILI))
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)
FluTest$ILILag2[1]=FluTrain$ILI[416]
FluTest$ILILag2[2]=FluTrain$ILI[417]
predTest2=exp(predict(FluTrend2,newdata=FluTest))
SSE = sum((predTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
