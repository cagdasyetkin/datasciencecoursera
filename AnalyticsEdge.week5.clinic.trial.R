trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
which.max(nchar(trials$abstract))
nchar(trials$abstract)[664]
sort(nchar(trials$abstract))
table((nchar(trials$abstract))==0)
which.min(nchar(trials$title))
trials$title[1258]
sort(nchar(trials$title))

library(tm)
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english")) 
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
sparseTitle = removeSparseTerms(dtmTitle, 0.95)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)
colnames(sparseTitle) = make.names(colnames(sparseTitle))
colnames(sparseAbstract) = make.names(colnames(sparseAbstract))

dtmTitle = as.data.frame(as.matrix(sparseTitle))
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))

ncol(dtmTitle)
sort(colSums(dtmAbstract)) 
which.max(colSums(dtmAbstract)) 

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

set.seed(144)
library(caTools)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)
table(train$trial)

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)

predTrain = predict(trialCART)[,2]
summary(predTrain)
table(predictCART[,1], predictCART[,2])
summary(predictCART)

table(train$trial, predTrain)


predictCART.test = predict(trialCART, newdata=test, type="class")
table(test$trial, predictCART.test)
library(ROCR)
predTest = predict(trialCART, newdata=test)[,2]

ROCRpred <- prediction(predTest, test$trial)
as.numeric(performance(ROCRpred, "auc")@y.values)









