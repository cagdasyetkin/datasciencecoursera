wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
length(stopwords("english"))
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
set.seed(132)
library(caTools)

split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
trainWiki = subset(wikiWords, split==TRUE)
testWiki = subset(wikiWords, split==FALSE)

library(rpart)
library(rpart.plot)

table(testWiki$Vandal)

#***************
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, spl==TRUE)
wikiTest = subset(wikiWords, spl==FALSE)
table(wikiTest$Vandal)
wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
table(wikiTest$Vandal, testPredictCART)
prp(wikiCART)

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, testPredictCART2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, testPredictCART3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART4 = predict(wikiCART4, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART4)

prp(wikiCART4)

