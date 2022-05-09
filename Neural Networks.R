library(xlsx)
data<-read.xlsx("C:/Users/arunk/Downloads/Binary.xlsx", 1)

str(data)

# Min-Max Normalization (all variables should be between 0 and 1)
data$gre <- (data$gre - min(data$gre))/(max(data$gre) - min(data$gre))
data$gpa <- (data$gpa - min(data$gpa))/(max(data$gpa) - min(data$gpa))
data$rank <- (data$rank - min(data$rank))/(max(data$rank)-min(data$rank))


# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]

#install.packages("neuralnet")
library(neuralnet)

n <- neuralnet(admit~gre+gpa+rank,
               data = training,
               hidden = 1,
               err.fct = "ce",
               linear.output = FALSE)
summary(n)
plot(n)

# Prediction
# use compute ;-1 y variable should by excluded
output <- compute(n, training[,-1])

head(output$net.result)
head(training[1,])

# Node Output Calculations with Sigmoid Activation Function out4
in4 <- 0.0455 + (0.82344*0.7586206897) + (1.35186*0.8103448276) + (-0.87435*0.6666666667)
in4
out4 <- 1/(1+exp(-in4))
out4
in5 <- -7.06125 +(8.5741*out4)
out5 <- 1/(1+exp(-in5))
out5

# Confusion Matrix & Misclassification Error - training data
output <- compute(n, training[,-1])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, training$admit)
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix & Misclassification Error - testing data
output <- compute(n, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testing$admit)
tab2
d<-sum(diag(tab2))/sum(tab2)

print(paste('accuracy',d))
``
p<-predict(n,testing)
pr<-ifelse(p>0.5,1,0)
m<-table(testing$admit,pr)
