#Multiple Linear Regression : 
#Example1 :Predicting sales based on the advertising budget invested in youtube, facebook and newspaper,
#dataset : marketing dataset

#We'll use the marketing data set [datarium package], which contains the impact of the amount of money spent on three advertising medias (youtube, facebook and newspaper) on sales.
load("C:/Users/arunk/Downloads/marketing.rda")
marketing
head(marketing,5)
dim(marketing)

#Displaying the details of marketing
str(marketing)
# there are 200 observations and 4 variables
model <- lm(formula = sales~., data = marketing)

#The above command creates a relationship with sales as the dependent variable
#and  as youtube+facebook+newspaper independent var

#Assumption Check
#1. Normality of Data
#a.Outlier Test
library(car)
outlierTest(model)
plot(model)
#The outlier test shows that there are  no significant outliers for p value 0.001095 p<0.05
#Hence we can say data is normal, Nevada is the farthest from the line

#b.Shapiro Wilk  -Ho: There are no significant residuals, the sample is normally distributed

residuals <-residuals(object = model)
shapiro.test(x = residuals)

#Since the value of W is 0.91767(close to 1), and  that of p value is 0.0005(<0.05)
#hence data is normal
#2. Linearity Assumption
cor.test(marketing$sale, marketing$youtube)
#0.7822244
cor.test(marketing$sale, marketing$facebook)
#0.5762226
cor.test(marketing$sale, marketing$newspaper)
#0.228299

# We can say that correlation exists between the DV and IV 
#but a strong correlation does not exist. However Linearity Assumption is satisfied


#3.Independence of Errors: There should be little or no autocorrelation in the residuals
#Autocorrelation occurs when the residuals are not independent of each other
##Durbin Watson Test is used to test this assumption. This test
#tests the Ho that the residuals are not linearly autocorrelated
#D can take values from 0 to 4, values in the range 1.5 to 2.5 show
#that there is no autocorrelation in the residuals

#Durbin Watson Test is available in the car library
durbinWatsonTest(model)

#Since p-value(0.526)is > 0.05, there is not enough evidence to reject H0
#which means that there is no significant linear autocorrelation in the residuals
#Hence independence of errors assumption is met and D-W Statistic in  2.083648 

#4. Homoscedasticity (Homogeneity of Variance): Residual terms should be homoscedastic
#Variance of errors(residuals) is the same across all levels of IVs
#should not form fanning / funneling pattern
#Homoscedastic = constant error variance
#Heteroscedastic = non constant error variance(fanning / funnelling pattern)

#Breusch Pagan Test  - uses ncv() (non constant error variance Test - in car library
#ncvTest tests the Ho :  There is no significant non constant error variance(heteroscedasticity) 


ncvTest(model)

##H0: homoskedasticity
library(lmtest)
bptest(model)

#Since p-value(0.1623) is >0.05, we accept Ho, ie there is no significant heteroscedasticity
#ie non constant error variance among the residuals for all IVs
#Therefore homoscedasticity assumption is met

#5. Multicollinearity Assumption: IVs should not be multicollinear
#Occurs when two or more IVs are highly correlated with each other
#This leads to problems with understanding which IV contributes to the variance explained in the DV
#MC is detected by  variance Inflation factor(VIF). 
#sqrt(VIF)>2 indicates multicollinearity exists
#use the vif() available in car package

sqrt(vif(model)) > 2  # True=multicollinear, False = not multicollinear
#Since the answer is FALSE for all IV, multicollinearity assumption is met

#Graphical Evaluation
#Since all assumptons of Multiple Regression are met
par(mar = c(2,2,2,2))

par(mfrow = c(2,2))
plot(model)

#Explanation : 
#QQ plot - check for Normality - if DV is normally distributed
#for a set of predictor values,then the residuals should be normally
#distributed with a mean of zero. 
# if normality assumption is met the points on the graph will 
#fall on a st line.

#REsiduals vs Fitted: Independence of Error Terms & Linearity Assumption(corr Test) 
#- if DV and IVs are linearly related, then 
#there should not be a systematic relationship between the residuals and the fitted values

#Scale Location: Homoskedasticity- There should 
#be a random band of residuals around the horizontal line

# #Residuals vs Leverage - Outliers and Influential Points-Cooks Distance

par(mfrow = c(2,2))

crPlots(model)
## componenet plus rsidual plots examines the 
#linearity of relationship between each IV and DV

# Since all the assumptions are met, we can display the mode
model

summary(model)


#Multiple Rsquared  = 0.8972, we need to check if we can increase thevalue
#We see that youtube and facebook is only signigficant hence delete other insignificant IV newpaper

#CREATE A NEW MODEL with only significant var
newmodel<-lm(formula = sales~youtube+facebook,data = marketing)

#Assumption Check
#1. Normality of Data
#a.Outlier Test
library(car)
outlierTest(newmodel)
plot(newmodel)
#The outlier test shows that there are  no significant outliers for p<0.05
#Hence we can say data is normal, Nevada is the farthest from the line

#b.Shapiro Wilk  -Ho: There are no significant residuals, the sample is normally distributed

newresiduals <-residuals(object = newmodel)
shapiro.test(x = newresiduals)

#Since the value of W is 0.94059(close to 1), and  that of p is0.0000000000000776 (<0.050)
#hence data is normal
#2. Linearity Assumption
cor.test(marketing$sale, marketing$youtube)
#0.7822244
cor.test(marketing$sale, marketing$facebook)
#0.5762226


# We can say that correlation exists between the DV and IV 
#but a strong correlation does not exist. However Linearity Assumption is satisfied


#3.Independence of Errors: There should be little or no autocorrelation in the residuals
#Autocorrelation occurs when the residuals are not independent of each other
##Durbin Watson Test is used to test this assumption. This test
#tests the Ho that the residuals are not linearly autocorrelated
#D can take values from 0 to 4, values in the range 1.5 to 2.5 show
#that there is no autocorrelation in the residuals

#Durbin Watson Test is available in the car library
durbinWatsonTest(newmodel)

#Since p-value(0.526)is > 0.05, there is not enough evidence to reject H0
#which means that there is no significant linear autocorrelation in the residuals
#Hence independence of errors assumption is met and D-W Statistic in  2.083648 

#4. Homoscedasticity (Homogeneity of Variance): Residual terms should be homoscedastic
#Variance of errors(residuals) is the same across all levels of IVs

#Breusch Pagan Test  - uses ncv() (non constant error variance Test - in car library
#ncvTest tests the Ho :  There is no significant non constant error variance(heteroscedasticity) 


ncvTest(newmodel)

##H0: homoskedasticity
library(lmtest)
bptest(newmodel)

#Since p-value( 0.0903) is >0.05, we accept Ho, ie there is no significant heteroscedasticity
#ie non constant error variance among the residuals for all IVs
#Therefore homoscedasticity assumption is met

#5. Multicollinearity Assumption: IVs should not be multicollinear
#Occurs when two or more IVs are highly correlated with each other
#This leads to problems with understanding which IV contributes to the variance explained in the DV
#MC is detected by  variance Inflation factor(VIF). 
#sqrt(VIF)>2 indicates multicollinearity exists
#use the vif() available in car package

sqrt(vif(newmodel)) > 2  # True=multicollinear, False = not multicollinear
#Since the answer is FALSE for all IV, multicollinearity assumption is met

#Graphical Evaluation
#Since all assumptons of Multiple Regression are met
par(mar = c(2,2,2,2))

par(mfrow = c(2,2))
plot(newmodel)

#Explanation : 
#QQ plot - check for Normality - if DV is normally distributed
#for a set of predictor values,then the residuals should be normally
#distributed with a mean of zero. 
# if normality assumption is met the points on the graph will 
#fall on a st line.

#REsiduals vs Fitted: Independence of Error Terms & Linearity Assumption(corr Test) 
#- if DV and IVs are linearly related, then 
#there should not be a systematic relationship between the residuals and the fitted values

#Scale Location: Homoskedasticity- There should 
#be a random band of residuals around the horizontal line

# #Residuals vs Leverage - Outliers and Influential Points-Cooks Distance

par(mfrow = c(2,2))
crPlots(newmodel)

#display the model
newmodel
summary(newmodel)
##Interpretation: The value of Multiple R sqauredm 0.8972
#which is very good. Hence model can be used for further analysis and prediction
#Regression Equation : sales =  3.50532+(0.04575)*youtube+(0.18799)*facebook

#Predicting the value of sales for different 
#values of youtube+facebook
newrecord <- data.frame( youtube= 150, facebook = 200 )

newsales <- predict(newmodel, newrecord)

newsales
#47.96739
##Compare predicted value with calculated value- should be nearly the same

3.50532+(0.04575)*150+(0.18799)*200
#47.96582
# both predicted value are nearly the same 