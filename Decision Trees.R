
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

data("readingSkills")
head(readingSkills)
sample_data = sample.split(readingSkills, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)

#Create the decision tree model using ctree and plot the model 
ctree_<- ctree(nativeSpeaker ~ ., train_data)
plot(ctree_)

#Interpretation
#From the tree, it is clear that those who have a score less than or equal to 31.08 and whose age is less than or equal to 6 are not native speakers and for those whose score is greater than 31.086 under the same criteria, they are found to be native speakers.

#Making a prediction  
# testing the people who are native speakers
# and those who are not
predict_model<-predict(ctree_, test_data)

# creates a table to count how many are classified
# as native speakers and how many are not
table_mat <- table(test_data$nativeSpeaker, predict_model)
table_mat
#The model has correctly predicted 13 people to be non-native speakers but classified an additional 13 to be non-native, and the model by analogy has misclassified none of the passengers to be native speakers when actually they are not.
#Determining the accuracy of the model developed 
ac_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test is found to be', ac_Test))

#Here the accuracy-test from the confusion matrix is calculated and is found to be 0.74. Hence this model is found to predict with an accuracy of 74 %.

#Inference 
#Thus Decision Trees are very useful algorithms as they are not only used to choose alternatives based on expected values but are also used for the classification of priorities and making predictions. It is up to us to 
#determine the accuracy of using such models in the appropriate applications. 


#Decision Tree with classification and Prediction for Categorical IV
#Dataset = Titanic
library(car)
library(caret)
#install.packages("caTools")
library(caTools)
#install.packages("party")
library(party)
library(rpart)
#install.packages("rpart")
library(rpart.plot)
#install.packages("rpart.plot")
#install.packages("car")
titanicdf <- read.csv("train.csv", header = TRUE)
dim(titanicdf)
table(complete.cases(titanicdf))
str(titanicdf)

#Create dataframe
nt <- as.data.frame(titanicdf)
str(nt)
ifelse(nt$Age == "", "BLANK", nt$Age)
View(nt)

#Create Partition
set.seed(1234)
part<-createDataPartition(nt$Survived, p=0.7, list = FALSE)


traintitanic <- nt[part, ]
testtitanic <- nt[-part, ]
dim(traintitanic)
dim(testtitanic)
#Create a decision tree model
control<-trainControl(method = "repeatedcv" , number = 10, repeats = 3)
dtreemodel1<-train(Survived~Pclass+Sex+Age, data = traintitanic, method = "rpart",
                   parms = list(split = "information"),
                   trControl = control, tuneLength = 10)

#Displaying the model
dtreemodel1
prp(dtreemodel1$finalModel, box.palette = "Green", tweak = 1.2)
#predicting the decision tree model
dtree_predict1 <- predict(dtreemodel1, newdata = traintitanic)
dtree_predict2 <- predict(dtreemodel1, newdata = testtitanic)
str(dtree_predict2)
#creating a confusion matrix
confusionMatrix(as.factor(dtree_predict1), as.factor(traintitanic$Survived))

confusionMatrix(as.factor(dtree_predict2), as.factor(testtitanic$Survived))
____________________________
