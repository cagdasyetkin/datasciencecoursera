songs = read.csv("songs.csv") #read dataset

#buid 3 logistic regression models and check multi collerality.

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

#make your predictions and build your confusion matrix

predTest = predict(SongsLog3, type = "response", newdata = SongsTest)
table(SongsTest$Top10, predTest >= 0.45)

#sensivity = TP / (TP+FN)
#specificity = TN / (TN+FP)


#           PREDICTED=0    PREDICTED = 1            
#ACTUAL = 0   TN              FP
#ACTUAL = 1   FN              TP
