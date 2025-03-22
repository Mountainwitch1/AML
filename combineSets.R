train<-read.csv("train.csv")
test<-read.csv("test.csv")

#To combine datasets and preserve information from which datasets 
#records are we need to create the artificcial variable
#the variable will be called isTrain and will have value "yes" for training records
#and "no" for testing records

train$isTrain<-"yes"
test$isTrain<-"no"

#combine datasets
comb<- rbind(train, test)

#use comb variabke for EDA nad preprocessing analysis.
#remember isTrain is indicating from which dataset the record comes from. Do not report that variable
#in analysis

# when you finish task #undertanding data and #preprocess and this is what you submit for PART I

#for Part II
#you need to split  comb back to test and train before #task 6

train<-comb[comb$isTrain=="yes",]
test<-comb[comb$isTest=="no", ]

#remove variable isTrainfrom both train and test
train$isTrain<-NULL
test$isTrain<-NULL

#you can start task 6 modelling from here
