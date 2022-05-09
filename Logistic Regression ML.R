#Exercise--1
#Logistic Regression
#PimaIndiansDiabetes dataset available in package mlbench

library(mlbench)
data(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)

PID <- PimaIndiansDiabetes
head(PID)

#Ensure no NA values exist in any coloumns, if exists they should be removed

table(complete.cases(PID))

#install caret package(Classification and  Training)
#contains functions to create predictive models like data splitting, preprocessing etc.


library(caret)
# Create two partitions 70% and 30% of the dataset
set.seed(900)
partition <- createDataPartition(y=PID$diabetes, p = 0.7, list = FALSE)

trainingdata<-PID[partition, ]
testdata <- PID[-partition, ]

#Create a logisti model
pidmodel <- glm(formula = diabetes~., data = trainingdata, family = binomial())
 
#Display the summary of the model
summary(pidmodel)
 
 #for determining the accuracy of the model- confusion matrix is implemented
 #its a crosstab of actual vs predicted value

 #Use predict function with test data
anspredict <-predict(pidmodel, newdata= testdata, type = "response")
 
 #display the details
summary(anspredict)
#Minimum value is zerp and maximum value is 0.97, predicted value lies between 0 and 1
#We have to convert these predicted values(stored in anspredict) into two groups neg and pos
#We will set the threshold level at 50%
 
# converting the value
convert <- ifelse(anspredict < 0.5, "neg", "pos")
table(convert)
 #shows that 168 neg and 68 pos values are there in convert
 
 
#Display the confusion matrix
newPid <- data.frame(predicted = convert,actual = testdata$diabetes)
#newPid has two cols, predicted and actuals

result <- confusionMatrix(factor(newPid$actual), factor(newPid$predicted))
#The above command compares values of actual and predicted of each and every record
#of new PID and stores in result 
result
#The confusion matrix shows original data had 150 records  in which
##Actual diabetes neg is 150(134+16)  and pos is 80(39 + 41) out of which
#134 neg is predicted correctly and 41pos is predicted correctly
#16 values predicte neg are actually pos, 39 values predicted pos are actually neg

#The output accuracy of 0.7609 shows that the model is 76.09%  accurate
#The kappa value( inter-rater agreement) of 0.4439 shows that the model 
#has moderate agreement

#Kappa Statistics
#0 = agreement equivalent to chance
#<0 No agreement
#0 - .20 Slight
#.21 - .40 Fair
#.41 - .60 Moderate
#.61 - .80 Substantial
#.81-1.0 Perfect

#The  pidmodel shows that age, pressure and triceps are insignificant 
#so make a model with only significant var 

newmodel<- glm(formula = diabetes~pregnant + glucose+mass+pedigree+insulin,
               data = trainingdata, family = binomial())

summary(newmodel)

#Predicting the values from test sample
ansnew <- predict(newmodel, newdata = testdata, type = "response")
summary(ansnew)

#Convertt the predicted values
convertnew <- ifelse(ansnew<0.5, "neg", "pos")
table(convertnew)

#Using Confusion matrix for comparision
newPid2 <- data.frame(predicted = convertnew, actual = testdata$diabetes)
newresult <- confusionMatrix(factor(newPid2$actual), factor(newPid2$predicted))
newresult
#The results show that kappa is 0.4033, and accuracy is 75.22%
#There is no  improvement in the model

#We can use chisq test using anova() function to determine whether
#there is significant difference in the models

#Using anova()
#Ho: There is no significant difference between  the two models
anova(pidmodel, newmodel, test = "Chisq")

#Interpretation
#Check the p value to state whether there is significant difference in the two models





#Exercise--2
#Logistic Regression
#install ISLR package for data set
#smarket dataset available in package ISLR

library(ISLR)

Smarket
dim(Smarket)
str(Smarket)

names(Smarket)

# displaying variables using Histograms
for(i in 1:8) {
  hist(Smarket[,i], main=names(Smarket)[i], col = "orange")
}

# displaying variables using Boxplots
for (i in 1:8) {
  boxplot(Smarket[,i],main=names(Smarket)[i], col = 'red')
}

#finding correlation between each pair of numeric variable and then ploting it using corrplot
library(corrplot)
corr<-cor(Smarket[,1:8])
corrplot(corr,method = "circle")


#Ensure no NA values exist in any coloumns, if exists they should be removed
table(complete.cases(Smarket))
# there is no NA values we can continue  

#install caret package(Classification and  Training)
#contains functions to create predictive models like data splitting, preprocessing etc.
library(caret)

df<-Smarket

# Create two partitions 70% and 30% of the dataset
set.seed(900)
part <- createDataPartition(y=df$Direction, p = 0.7, list = FALSE)
dim(part)

train_set<-df[part, ]    #training dataset 70%
test_set <- df[-part, ]   #testing dataset 30%

#Create a logisti model
model_glm<-glm(formula=Direction~.,data = train_set,family = binomial())

#Display the summary of the model
summary(model_glm)

#for determining the accuracy of the model- confusion matrix is implemented
#its a crosstab of actual vs predicted value

#Use predict function with test data
smpredict <-predict(model_glm, newdata= test_set, type = "response")
smpredict

#display the details
summary(smpredict)
#Minimum value is zerp and maximum value is 1.000, predicted value lies between 0 and 1
#We have to convert these predicted values(stored in presmdict) into two groups up and down
#We will set the threshold level at 50%

# converting the value
convert1 <- ifelse(smpredict > 0.5, "Up", "Down")
table(convert1)
#shows that 179 IS DOWN and 195 IS UP values are there in convert

#Display the confusion matrix
newsm <- data.frame(predicted = convert1,actual = test_set$Direction)
#newPid has two cols, predicted and actuals

result1 <- confusionMatrix(factor(newsm$actual), factor(newsm$predicted))
#The above command compares values of actual and predicted of each and every record
#of newsm and stores in result1 
result1

#The confusion matrix shows original data had 374 records  in which
##Actual Direction up is 194(194)  and down is 180(179+1) out of which
#194 up is predicted correctly and 179 down is predicted correctly
#0 values predicte down are actually up,1 values predicted up are actually down

#The output accuracy of 0.9973 shows that the model is 99.73%  accurate
#The kappa value( inter-rater agreement) of 0.9946 shows that the model 
#has Perfect agreement

#Kappa Statistics
#0 = agreement equivalent to chance
#<0 No agreement
#0 - .20 Slight
#.21 - .40 Fair
#.41 - .60 Moderate
#.61 - .80 Substantial
#.81-1.0 Perfect


#The  model_glm shows that the model is over fitted 
#so make a model with less fitted

model_glm2<-glm(formula = Direction~Lag1+Lag2+Lag3,data = train_set,family = binomial())
#Display the summary of the model
summary(model_glm2)

#Predicting the values from test sample
newsm1 <- predict(model_glm2, newdata = test_set, type = "response")
#Display the summary 
summary(newsm1)
#Minimum value is 0.3574 and maximum value is 0.6182, predicted value lies between 0 and 1
#We have to convert these predicted values(stored in anspredict) into two groups up and down
#We will set the threshold level at 50%

#Convertt the predicted values
convertnew1 <- ifelse(newsm1>0.5, "Up", "Down")
table(convertnew1)
#shows that 92 Down and 282 Up values are there in convert

#Using Confusion matrix for comparision
newsm2 <- data.frame(predicted = convertnew1, actual = test_set$Direction)
newresult1 <- confusionMatrix(factor(newsm2$actual), factor(newsm2$predicted))
newresult1
dim(newresult1)
#The confusion matrix shows original data had 374 records  in which
##Actual Direction  up is 194(151+43)  and down is 170(49+131 )out of which
#151 up is predicted correctly and 49 down is predicted correctly
#43 values predicte down are actually up,131 values predicted up are actually down

#The results show that kappa is 0.0515 , and accuracy is 53.48%
#There is no  improvement in the model

#We can use chisq test using anova() function to determine whether
#there is significant difference in the models

#Using anova()
#Ho: There is no significant difference between  the two models
anova(model_glm, model_glm2, test = "Chisq")

#Interpretation
#Check the p value to state whether there is significant difference in the two models

#The model_glm is 99.73%  accurat  so i will chosse this and The kappa value( inter-rater agreement) 
#of 0.9946 shows that the model has Perfect agreement so i will choose this model_glm

#model_glm2 is 0.5348 accurat and the kappa value is 0.0515 the both value is less accurat and kappa value is Moderate 
#  and this model (model_glm2)  is less accurat then model(model_glm)  thats why didn't chosse the this model.











