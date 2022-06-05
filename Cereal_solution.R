Test_cereal <- read.csv(file.choose())
head(Test_cereal)
str(Test_cereal)

version
install.packages("DataExplorer")
library(DataExplorer)
install.packages("igraph")
create_report
library()
Test_Leslie<-read_exc

str(Dataset_LeslieSalt)
attach(Dataset_LeslieSalt)
library(stats)
library(ggplot2)
hist(Price)
ggplot(Dataset_LeslieSalt, aes())

Leslie_lm<-lm(Price~1, data = Dataset_LeslieSalt)
summary(Leslie_lm)
library(caTools)
Train_Leslie<- sample.split(Dataset_LeslieSalt$Price, SplitRatio = 0.8)
Train_Leslie

set.seed(123)
split = sample.split(Dataset_LeslieSalt$Price, SplitRatio = 0.75)
traindata = subset(Dataset_LeslieSalt, split == TRUE)
testdata = subset(Dataset_LeslieSalt, split == FALSE)
traindata
testdata
LM_train <-lm(traindata$Price~., traindata)
summary(LM_train)
traindata$predict<-predict(LM_train, newdata = traindata)
#numrows <- nrow(dataset)
#samrow <- sample(seq_len(numrows), numrows*0.7, replace=F) ##This is to split data into 70% and 30%.
#It helps in creating customer number of datasets like Train data, validation data and Test data.
#DTrain = data.frame[samrow,]  DTest = df[-Samrow]


LM_test<-lm(testdata$Price~., testdata)
testdata$predict<-predict(LM_test, newdata = testdata)
summary(LM_test)
?seq_len
predict.lm(LM_train)
