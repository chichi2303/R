rm(list=ls()); cat("\014") # clear all

library(tm)
library(class) # Using kNN

groups <- system.file("texts", '20Newsgroups', package = "tm")
#Choose 2 subjects to analyze (sci.space and rec.autos)
#sci.space 
Doc1.Train<- DirSource(file.path(groups, '20news-bydate-train', 'sci.space'))
Doc1.Test<- DirSource(file.path(groups, '20news-bydate-test', 'sci.space'))
#rec.autos
Doc2.Train<- DirSource(file.path(groups, '20news-bydate-train', 'rec.autos'))
Doc2.Test<- DirSource(file.path(groups, '20news-bydate-test', 'rec.autos'))

#Create corpus 
Doc1.Train <- Corpus(URISource(Doc1.Train$filelist[1:100]),readerControl=list(reader=readPlain))
Doc1.Test <- Corpus(URISource(Doc1.Test$filelist[1:100]),readerControl=list(reader=readPlain))
Doc2.Train <- Corpus(URISource(Doc2.Train$filelist[1:100]),readerControl=list(reader=readPlain))
Doc2.Test <- Corpus(URISource(Doc2.Test$filelist[1:100]),readerControl=list(reader=readPlain))

#Merge all 4 corpus 
Doc <- c(Doc1.Train, Doc1.Test, Doc2.Train, Doc2.Test)
#Preprocessing (Removing numbers, punctuation, stopwords, turn words to uppercase )
Doc.pre <- tm_map(Doc, removeNumbers, lazy = FALSE)
Doc.pre <- tm_map(Doc, removePunctuation, lazy = FALSE)
Doc.pre <- tm_map(Doc,  removeWords,stopwords("english"), lazy = FALSE)
Doc.pre <- tm_map(Doc,  content_transformer(toupper), lazy = FALSE)

#Document-Term Matrix 
dtm <- DocumentTermMatrix(Doc.pre, control = list(minWordLength=2, minDocFreq=5))

#Split the DTM 
Train.doc <- dtm[c(1:100,201:300),]
Test.doc <- dtm [c(101:200,301:400),]

#Create tag 
Tags <- factor(c(rep("Sci",100), rep("Rec",100)))

#knn test
set.seed(0) 
prob.test <- knn(Train.doc, Test.doc, Tags, k = 2, prob=TRUE)

#Display Classification results 
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
d<- prob.test==Tags 
result <- data.frame(Doc = a, Predict = b, Prob = c, Correct = d)
head(result)

#Calculating percentage of correct 
correct.perc <- sum(prob.test==Tags)/length(Tags) 
correct.perc

#Estimate the effectiveness of the classification: 
##Create the confusion matrix 
AutoCM <- table (prob.test, Tags)
AutoCM

##Mark the values TP, TN, FP, FN 
### Consider "Rec" as Positive and "Sci" as Negative
RecClassified <- (prob.test==Tags)[101:200] # Classified as "Rec" (Positive)
TP <- sum(RecClassified=="TRUE") # Actual "Rec" classified as "Rec
FN <- sum(RecClassified=="FALSE") 
SciClassified <- (prob.test==Tags)[1:100] # Classified as "Sci" (Negative)
FP <- sum(SciClassified == "FALSE")
TN <- sum(SciClassified == "TRUE")

##Precision
precision <- TP/(TP+FP)
precision

##Recall
Recall <- TP/(TP+FN)
Recall

##F-score 
Fscore <- 2*(precision*Recall)/(precision+Recall)
Fscore

save(prob.test,dtm, file = "ProbExp1")
load(file = "ProbExp1")

