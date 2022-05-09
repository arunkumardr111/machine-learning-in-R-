#Multiple Linear Regression : 


relation <- lm(formula = Murder~Population+Income+Area+Frost, data = state)

#The above command creates a relationship with Murder as the dependent variable
#and Population+Income+Area+Frost as independent var

#Assumption Check
#1. Normality of Data
#a.Outlier Test
library(car)
outlierTest(relation)
plot(relation,4)

#The outlier test shows that there are  no significant outliers for p<0.05
#Hence we can say data is normal, Nevada is the farthest from the line

#b.Shapiro Wilk  -Ho: There are no significant residuals, the sample is normally distributed

residuals1 <-residuals(object = relation)
shapiro.test(x = residuals1)
 
#Since the value of W is 0.95(close to 1), and  that of p is 0.06(>0.050)
#hence data is normal

#2. Linearity Assumption
cor.test(state$Murder, state$Population)
#0.34
cor.test(state$Murder, state$Income)
#-0.23
cor.test(state$Murder, state$Frost)
#-0.54
cor.test(state$Murder, state$Area)
#0.23
# We can say that correlation exists between the DV and IV 
#but a strong correlation does not exist. However Linearity Assumption is satisfied

#3.Independence of Errors: There should be little or no autocorrelation in the residuals
#Autocorrelation occurs when the residuals are not independent of each other
##Durbin Watson Test is used to test this assumption. This test
#tests the Ho that the residuals are not linearly autocorrelated
#D can take values from 0 to 4, values in the range 1.5 to 2.5 show
#that there is no autocorrelation in the residuals

#Durbin Watson Test is available in the car library
durbinWatsonTest(relation)

#Since p-value(0.094)is > 0.05, there is not enough evidence to reject H0
#which means that there is no significant linear autocorrelation in the residuals
#Hence independence of errors assumption is met

#Problem : Durbin-Watson result inconsistent for same regression model with same data
##It is likely that the data file was sorted between the Linear Regression runs 
#that returned discrepant D-W results. Because the Durbin-Watson is based on 
#the differences between the residuals of adjacent cases, sorting the file will 
#change the pairs of cases whose residuals are differenced to compute the D-W, 
#leading to a different D-W statistic. This difference in D-W values would occur 
#on Linear Regression runs with the same version of Statistics, provided that the
#case order had been rearranged between runs.


#https://boostedml.com/2019/03/linear-regression-plots-fitted-vs-residuals.html
#4. Homoscedasticity (Homogeneity of Variance): Residual terms should be homoscedastic
#Variance of errors(residuals) is the same across all levels of IVs
#should not form fanning / funneling pattern
#Homoscedastic = constant error variance
#Heteroscedastic = non constant error variance(fanning / funnelling pattern)

#Breusch Pagan Test  - uses ncv() (non constant error variance Test - in car library
#ncvTest tests the Ho :  There is no significant non constant error variance(heteroscedasticity) 


ncvTest(relation)

##H0: homoskedasticity
library(lmtest)
bptest(relation)

#Since p-value(0.39873) is >0.05, we accept Ho, ie there is no significant heteroscedasticity
#ie non constant error variance among the residuals for all IVs
#Therefore homoscedasticity assumption is met

#5. Multicollinearity Assumption: IVs should not be multicollinear
#Occurs when two or more IVs are highly correlated with each other
#This leads to problems with understanding which IV contributes to the variance explained in the DV
#MC is detected by  variance Inflation factor(VIF). 
#sqrt(VIF)>2 indicates multicollinearity exists
#use the vif() available in car package

sqrt(vif(relation)) > 2  # True=multicollinear, False = not multicollinear
#Since the answer is FALSE for all IV, multicollinearity assumption is met

#Graphical Evaluation
#Since all assumptons of Multiple Regression are met
par(mar = c(2,2,2,2))

par(mfrow = c(2,2))
 plot(relation)
 
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
 
 #Residuals vs Leverage - Outliers and Influential Points-Cooks Distance
 
 par(mfrow = c(2,2))
 #par("mar")
 #par(mar = c(1,1,1,1))
 
 crPlots(relation)
## componenet plus rsidual plots examines the 
 #linearity of relationship between each IV and DV
 
 
 
# Since all the assumptions are met, we can display the mode
relation

summary(relation)

#based on the intercept and coeff values we create the 
#regression equation :
#murder = 1.704e+01  + 2.257 * 10^-4Population -1.985e-03*Income + 1.582e-05 * Area
#-2.806e-02 * Frost



#Predicting the value of murder for different 
#values of population, area, income and frost
newrecord <- data.frame(Population = 8000, Income = 4800,
                        Area = 7000, Frost = 60)
newmurder <- predict(relation, newrecord)
newmurder
#The predicted value of murder is nearly equal to the 
#calculated value using the regression equatuion

1.704e+01  + 2.257e-04 *8000 -1.985e-03*4800 + 1.582e-05 * 7000-2.806e-02 * 60

##Developing Best Regression Model- 
#Stepwise Backward Approach when Regression assumptions are not fulfilled

#Use dataset longley
#library(openxlsx)
#longleydf1 <- read.xlsx("longley1.xlsx",1)

str(longley)

#relation
longleymodel <- lm(Employed ~ Unemployed+
                     GNP+GNP.deflator+
                     Armed.Forces + Year, data = longley)
#Normality Assumption - Outlier Tesgt
library(car)
outlierTest(longleymodel)

#Interpretation: ___________________________

#Shapiro Wilk Test
resid1<- residuals(object = longleymodel)
shapiro.test(resid1)

#interpretation _________________________

#NonLinearity Assumption
cor.test(longley$Employed, longley$Unemployed)

cor.test(longley$Employed, longley$GNP)
cor.test(longley$Employed, longley$GNP.deflator)
cor.test(longley$Employed, longley$Armed.Forces)
cor.test(longley$Employed, longley$Year)
# check for cor between the DV and all independent 
#Give the  results and Interpretation _________________________


#Independence of Errors- No autocorrelation : Durbin Watson Test
durbinWatsonTest(longleymodel)
#Interpretation: ___________________________

#/Homoscedascticity
ncvTest(longleymodel)
#Interpretation : _________________

#multicollinearity
sqrt(vif(longleymodel)) >2

#Interpretation : Assumption not met

#All assumtions are met except the last one. 
#Hence we will follow backward stepwise approach to get the best model

summary(longleymodel)
#We see that Unemployed, Armed.Forces, and Year is only signigficant
#at 99% CI, 1% significance level, hence delete other insignificant IV

#CREATE A NEW MODEL with only significant var
longleymodel2  <- lm(Employed ~ Unemployed+
                       Armed.Forces + Year, data = longley)

#Check all assumptions of Regression for the new model : longleymodel2
#Normality Assumption - Outlier Test
outlierTest(longleymodel2)
#Interpretation_______________________

#Normality validation < shapiroTest
residuals1 <- residuals(object = longleymodel2)
shapiro.test(x =residuals1 )
#Interpretation_________________________

#Durbinwatson Test for independence of Errors test 
durbinWatsonTest(longleymodel2)
#Interpretation_________________________

#Homoscedasticity using ncvTest
ncvTest(longleymodel2)
#Interpretation_________________________

#Multicollinearity Assumption using vif()- sqrt(vif()) should not be > 2 
sqrt(vif(longleymodel2))> 2
#Interpretation_________________________

## All assumptions of Regression are met.
#Hence we can use the model for prediction

#display the model
longleymodel2
summary(longleymodel2)

##Interpretation: The value of Multiple R sqaured(coeff of determination) is 0.99
#which is very good. Hence model can be used for further analysis and prediction
#Regression Equation : Employed = Intercept + B1*Unemployed+B2*Armed.Forces + b3*Year          

#Predicting the value using predict()
newrec <- data.frame(Unemployed = 400, Armed.Forces = 350, Year = 1964)
newemployed <- predict(longleymodel2, newrec)
newemployed

##Compare predicted value with calculated value- should be nearly the same


#Regression Modeling using Stepwise Forward Approach
#Prestige dataset, car package

library(car)
str(Prestige)

model<-lm(formula = prestige~education + women, data = Prestige)

# Tests for Assumptions of linear Regression model
outlierTest(model)
residuals_pres <- residuals(model)
shapiro.test(residuals_pres)
attach(Prestige)
cor.test(prestige,education )
cor.test(prestige,women )
durbinWatsonTest(model)
ncvTest(model)
sqrt(vif(model))>2

#Since all assumptions are met, apply regression analysis on the propsed model

summary(model)
#Multiple Rsquared  = 0.7521, we need to check if we can increase the
#value of the same by stepwise forward approach
#To determine which all IV can be considered significant, lets include 
#all the var in the new model

model1 <- lm(prestige ~ education+women+income)


