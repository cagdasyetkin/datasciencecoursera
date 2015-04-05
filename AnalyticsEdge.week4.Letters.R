letters <- read.csv("letters_ABPR.csv") 
letters$isB = as.factor(letters$letter == "B")
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

table(test$isB)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
PredictCARTb <- predict(CARTb, newdata=test, type="class")
table(test$isB, PredictCARTb)
set.seed(1000)
LetterBForest <- randomForest(isB ~ . - letter, data = train)
PredictForestB <- predict(LetterBForest, newdata=test)
table(test$isB, PredictForestB)

letters$letter = as.factor( letters$letter )
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)
table(test$letter)
401/(395+383+401+379) #baseline accuracy

CARTletter = rpart(letter ~ . - isB, data=train, method="class")
PredictCARTletter <- predict(CARTletter, newdata=test, type="class")
table(test$letter, PredictCARTletter)

set.seed(1000)
LetterForest <- randomForest(letter ~ . - isB, data = train)
PredictForestLetter <- predict(LetterForest, newdata=test)
table(test$letter, PredictForestLetter)








