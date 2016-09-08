NBA = read.csv("NBA_train.csv")
str(NBA)
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
WinsReg = lm(W~PTSdiff, data = NBA)
summary(WinsReg)
# w = 41 + 0.0326*PTSdiff
41 + 0.0326*PTSdiff

#video 3
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(NBA))
RMSE #how much are we off on average from the mean? 
#184 points from a mean of 8370 per season
mean(NBA$PTS)
#remove highest p value first when removing variables. 
PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB
               + STL + BLK, data = NBA)
summary(PointsReg2)
PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB
                + STL + BLK, data = NBA)
summary(PointsReg3)
PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB
                + STL, data = NBA)
summary(PointsReg4)
SSE_4 = sum(PointsReg4$residuals^2)
RMSE_4 = sqrt(SSE_4/nrow(NBA))
RMSE_4
#root mean squared error increased slightly, but model is simpler and more interpretable
#video 4
NBA_Test= read.csv("NBA_test.csv")
PointsPrediction = predict(PointsReg4, newdata = NBA_Test)
#need to calculate out of sample r^2
SSE = sum((PointsPrediction - NBA_Test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_Test$PTS)^2)
R2 = 1 -SSE/SST
R2
RMSE = sqrt(SSE/nrow(NBA_Test))
RMSE
