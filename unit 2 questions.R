read.csv("wine.csv")

model1 = lm(Price ~ HarvestRain + WinterRain,
            data=wine)
model1
summary(model1)
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine$Price, wine$HarvestRain)
cor(wine)
model5 = lm(Price ~ AGST +HarvestRain + WinterRain, data=wine)
summary(model5)
cor(wine$HarvestRain, wine$WinterRain)
wineTest =read.csv("wine_test.csv")
str(wineTest)
predictTest = predict(model4, newdata=wineTest)
predictTest
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 -SSE/SST

#baseball.csv
baseball = read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

#new lecture
str(moneyball)
RunsReg = lm(RS ~ OBP +SLG + BA, data=moneyball)
summary(RunsReg)
RunsReg = lm(RS ~ OBP +SLG, data=moneyball)
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2013)
teamRank
wins2012
moneyball
