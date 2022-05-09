#Simple Linear Regression
library(readxl)
ageandheight <- read_excel("C:/Users/arunk/Downloads/ageandheight.xls")
 #creating the relationship between age and height
lmHeight <- lm(height ~ age, data = ageandheight)
lmHeight

#Assumption for Linear Regression - Normality, Linearity

library(car)
outlierTest(lmHeight)

## Shapiro Wilk Test for confirmation of Normality
residuals <- residuals(object = lmHeight)
shapiro.test(x = residuals)

# W should be close to 1 and p>0.05 to say that the data is normally distributes

## skewness and kurtosis
#install.packages("moments")
library(moments)
skewness(ageandheight$age)
skewness(ageandheight$height)

kurtosis(ageandheight$age)
kurtosis(ageandheight$height)
# coefficient of skewness should be between -1 to +1
#coefficient of kurtosis should be between -3 to +3

#Linearity Assumption is established by checking the
#correlation between the dependent and the independent variables
#Null Hyp : There is no significant correlation between age and height
cor.test(ageandheight$height, ageandheight$age)

#Since p-value < 0.05 we can say that the NULL Hyp is rejected 
#The alternate hypothesis that there is a significant correlation between age and height is accepted
#the correlation coefficicient is 0.9943661 which is very strong correlation between the age and height




#Simple Linear Regression : 

#displaying the structure of mtcars
str(mtcars)

#Establish relationship between IV and DV
relation <- lm(formula = hp~disp, data = mtcars)

#Assumption Check
#Assumption for Normality of Data - 
#Outlier Test, Shapiro Wilk Test, skewness, kurtosis and graph

library(car)
outlierTest(relation)  
#outlier Test indicates that there are significant outliers
#and therefore data is not normal. Maserati Bora observation is the farthest outlier
residuals1 <- residuals(object = relation)
shapiro.test( x= residuals1)

#The value of W is 0.84 and p is 0.0000(<0.05) using Shapiro Wilk Test.
#Hence data cannot be considered as normal

#Skewness and kurtosis

library(moments)
attach(mtcars)
skewness(hp)
skewness(disp)
kurtosis(hp)
kurtosis(disp)
#Since kurtosis for disp is more than 3, data is not normal


#Non-Linearity Assumption
#Determine corr bteween IV and DV
#H0: There is no significan corr between hp and disp
cor.test(hp, disp)

#Since p<0.05, H0 is rejected which means there is a significant corr 
#between hp and disp. This is also TRUE because coeff of corr is 0.791
#Thus the assumption of Linearity is fulfilled

#Displaying the modelsummary
summary(relation)
#The summary of the model shows that the value of multiple 
#Rsquared is 0.6256, which is good. But we cannot build an
#equation as the assumption of Regression is not fulfilled
#We should also check if we can improve the model

#Since normality is not fulfilled, we can achieve normality 
#by removing unusual observations like outliers , influential observations and 
#high leverage observations
#The outlier Test showed Maserati Bora as the major outlier. 
#We can use plot functionwith 4 as the second argument

#Determining outliers graphically
plot(relation, 4)
#The plot shows that there are 3 distinct unusual observations, Maserati Bora, Ford Pantera, Cadillac Fleetwood
#Their record numbers are determined from mtcars which are, 31, 29, 15

#Next Remove these 3 outliers from mtcars

mtcars1 <- mtcars[-31, ]
mtcars2 <- mtcars1[-29, ]
newmtcars <- mtcars2[-15, ]
str(newmtcars)


#In the above code the 3 outliers from the dtaset mtcars 
#are removed one by one and a new dataset newmtcars is 
#created with 29 observations

#Assumptions of Nornality and Linearity for after removing outliers
#newmtcars dataset
#outlier Test
#creating relationship between hp and disp of newmtcars
newrelation <- lm(formula = hp ~ disp, data = newmtcars)

#outlier Test
outlierTest(newrelation)

#The outlier Test shows that the data is normal since p < 0.05
#However Ferrari Dino is farthest from the line

#b.Shapiro Wilk Test
residuals2 <- residuals(newrelation)
shapiro.test(residuals2)
# The value of W is 0.95 and that of p is 0.25(>0.05)
# hence data can be considered normal

#c. skewness and kurtosis
library(moments)
skewness(newmtcars$disp)
skewness(newmtcars$hp)
# skewness values are between -1 to 1
kurtosis(newmtcars$disp)
kurtosis(newmtcars$hp)
#kurtosis coeffs are btween -3 to +3
#hence data is normal

#2. Linearity Assumption - corr between IV and DV
#Null hyp H0: There is no significant corr btween hp and disp
cor.test(newmtcars$hp, newmtcars$disp)

#Since p-value is < 0.05, H0 is rejected, which means 
#significant corr exists between hp and disp of new dataset newmtcars
#This is also true as the coeff of corr is 0.866. It can be observed
#that the coeff of corr has increased from 0.79 to 0.86 
#after removing the outliers. Thus the assumption of Linearity is fulfilled

# Graphical Evaluation of the Statistical Assumtions for Regression
#use plot function to the model using values from 1 to 2 for the second argumen
#and using crPlots()
par(mar = c(2,2,2,2))
par(mfrow = c(3, 1))
#Assumption for Normality - Normal Q-Q plot
plot(newrelation, 2)

#Assumption Check for Linearity - Residual vs Fitted Plot
plot(newrelation, 1)

# Creating a component plus residual plot
crPlots(newrelation)
##The first image shows QQ curve for depicting normality
#Since all the points fall on  a straight line, this assumption is fulfilled
#The last two plots clearly show that linearity assumption is fulfilled
#as there is no systematic relationship between residuals and predicted values

##Apply Regression Analysis
summary(newrelation)
#The summary displays the value of the intercept(45.23)
#and the coeff of x=disp (0.41). Thus the equation formed is:
# hp = disp*(0.41) + 45.23
#The value of multiple R-squared has increased from
#65.26 to 75.15, hence this model can be considered as the best model for prediction
#Since there are 3 * signs besideintercept and disp,
#it means change in hp can be explained to a great level by changing disp

##Predicting the value of hp for disp = 195
newdisp <- data.frame(disp = 195)
newhp <- predict(newrelation, newdisp)
print(newhp)

#The predicted value of hp is 125.55 which is nearly 
#equalto the calculated value of hpequation(0.41*195 + 45.23 = 125.23)  by the
hp=45.23553+0.41191 *195

