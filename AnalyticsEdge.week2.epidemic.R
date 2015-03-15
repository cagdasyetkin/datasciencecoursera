FluTrain <- read.csv("FluTrain.csv")
hist(FluTrain$ILI)
plot(FluTrain$Queries, log(FluTrain$ILI))

#linear model 1
FluTrend1 <- lm(log(ILI)~Queries, data=FluTrain) #bingo

#correlation 
cor(log(FluTrain$ILI), FluTrain$Queries)

FluTest <- read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

(Observed ILI - Estimated ILI)/Observed ILI

#RMSE for the first model
sqrt(mean((PredTest1 - FluTest$ILI)^2))

ILILag2 <- lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 <- coredata(ILILag2)

FluTest$ILILag2[1] <- FluTrain$ILI[416]
FluTest$ILILag2[2] <- FluTrain$ILI[417]

ILILag2 = na.omit(ILILag2)

#linear model 2
FluTrend2 <- lm(log(FluTrain$ILI) ~ FluTrain$Queries + log(FluTrain$ILILag2), data = FluTrain)

#RMSE for the first model:
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
sqrt(mean((PredTest1 - FluTest$ILI)^2))

#RMSE for the second model:
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))



