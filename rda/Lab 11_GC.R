################################################################################
# Biostatistics for Public Health
# George Cedeño
# New York University College of Global Public Health
#
# Module 11: Multiple Regression
#
# This is THE R SYNTAX for the lab required for Module 11. 
# You can execute the code all at once, or
# you can highlight part of the code and execute it.
#
#*Section 11.1: Regression Diagnostics: Linearity 
#*Section 11.2: Normality Q-Q plots 
#*Section 11.3: Residual heteroscedasticity test 
#*Section 11.4: Residual Statistics 
#*Section 11.5: Mean Centering 
#*Section 11.6: Polynomial and Dummy Regression 
#
#*use heights4.dta
#
#There are many sections in this lab session. Let's focus on the topics below:
#(1) Use a residual-vs-fitted plot to test for the linearity assumption (11.1)
#(2) Use a residual-vs-fitted plot to test for the equal residual variance
#    assumption (11.1)
#(3) Use a Q-Q plot to test for the normality assumption (11.2)
#(4a) calculate standardized residuals to detect for outliers (11.4)
#(4b) calculate cook'd to detect for influential outliers (11.4)
#(5) How to do mean centering (11.5)
#(6) Run regression analysis with dummy coded variables (11.6)
################################################################################

rm(list=ls())
library(haven)

#####################################################
#                                                   #
#                                                   #
#       SET YOUR OWN WORKING DIRECTORY!!!!          #
#                                                   #
#####################################################

work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

heights4 <-read_dta(file = "heights4.dta")

################################################################################
#*Section 11.1: Regression Diagnostics: Linearity
#*Question: Is the assumption of Linearity violated?
#
#* The regression diagnostics are required test the assumption that there is a 
#* linear relationship between x and y, or linearity assumption. 
#* Residual plots can be used to visualize possible nonlinearities. ?
#* We need to first run regression in order to request information on the 
#* predicted values, standardized residuals, leverage statistics, and distance 
#* statistics.
################################################################################

plot(heights4$height,heights4$weight,  
                 main="Scatterplot of weight and height", 
                 xlab="Height in CMS",
                 ylab="Weight in kilos") 
# adding a reference line  
abline(lm(weight ~ height, data = heights4), col = "blue")

model1 <- lm(weight ~ height, data=heights4)
summary(model1)
# summary(model1)
#
#Call:
#lm(formula = weight ~ height, data = heights4)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-22.2766  -5.7871  -0.6781   5.4981  21.5139 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -92.6062    23.3991  -3.958  0.00026 ***
#height        0.9570     0.1395   6.859 1.48e-08 ***
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 10.32 on 46 degrees of freedom
#  (3 observations deleted due to missingness)
#Multiple R-squared:  0.5057,	Adjusted R-squared:  0.4949 
#F-statistic: 47.05 on 1 and 46 DF,  p-value: 1.481e-08

# Interpretation: 
# One unit change in height translates to a increase in weight by 0.9570. 
# If height were 0, weight level would be -92.6062.
# Height variable is statistically sig & multiple r^2 is 0.5057, or 50.5%. 
# Meaning the predictor/explanatory variable of height explains ~51% of the variance
# in the outcome of weight - ok model.

# Equation:
# e0 <- -92.6062 + 0.9570*(height)

#*To calculate Predicted values for the model
model1_predY <- fitted(model1) # predicted values
model1_resiY <- residuals(model1) # residuals
influ        <- influence(model1)

plot(model1_resiY ~ model1_predY)
# Notice that there does not seem to be linearity. 
# Assumption 1 is violated.


################################################################################
# Section 11.2:Normality Q-Q plots
#* We use the standardized residual to test the normality assumption, using the 
#* "Q-Q Plot"
################################################################################

#Package installation
# install.packages("car") 
#Libraries
library(car) #Companion to Applied Regression

qqPlot(model1, main="QQ Plot")
# *Question: what are your findings based on this Q-Q plot?
# Linearity does not imply normality at first glance! 
# Per this QQ plot, the errors are independent & data is normally distributed. 
# Assumption 2 is not violated. 

################################################################################
#*Section 11.3: Residual heteroscedasticity test
################################################################################

plot(fitted(model1), residuals(model1))
#*Question: what are your findings based on this residual-vs-fitted plot?
# Heteroscedascticity. 
# Notice that there does not seem to be linearity. 
# Assumption 1 is violated.

################################################################################
# Section 11.4: Residual and Influential points
################################################################################

inflm.SR <- influence.measures(model1)
inflm.SR # print out influential statistics including Cook'd, debeta 
         # The last column, inf, indicates the identified influential data points
         
# model1_resiY <- residuals(model1) # residuals
library(psych)
describe(model1$residuals)
# or
model1_resiY <- residuals(model1) # residuals
describe(model1_resiY)

#Question: can you identify any outliers (using standardized residuals)?
standard_res <- rstandard(model1) 
data <- heights4[-c(49,50,51),] # remove these 3 rows because the contain NAs for height & sex
final_data <- cbind(data, standard_res)
# order residuals from largest to smallest
final_data[order(-standard_res),]
#plot predictor variable vs. standardized residuals
plot(final_data$height, standard_res, ylab='Standardized Residuals', xlab='x') 
#add horizontal line at 0
abline(0, 0)
# the 3 outliers exist in rows 1, 18, & 5

################################################################################
# *Section 11.5: Mean centering
################################################################################
#Centering. To center, create a new variable. This dataset already has height_cent,
# so we'll create a second one.

mean(heights4$height, na.rm=TRUE)
#> mean(heights4$height, na.rm=TRUE)
#[1] 167.4306
heights4$height_cent <- heights4$height-167.4306

# Recall, we had the model before:

# model1 <- lm(weight ~ height, data=heights4)
#> summary(model1)
#
#Call:
#lm(formula = weight ~ height, data = heights4)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-22.2766  -5.7871  -0.6781   5.4981  21.5139 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -92.6062    23.3991  -3.958  0.00026 ***
#height        0.9570     0.1395   6.859 1.48e-08 ***
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 10.32 on 46 degrees of freedom
#  (3 observations deleted due to missingness)
#Multiple R-squared:  0.5057,	Adjusted R-squared:  0.4949 
#F-statistic: 47.05 on 1 and 46 DF,  p-value: 1.481e-08

#regression model with centered height (height_cent)
model1.centered <- lm(weight ~ height_cent, data=heights4)
summary(model1.centered)

#> summary(model1.centered)
#
#Call:
#lm(formula = weight ~ height_cent, data = heights4)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-22.2766  -5.7871  -0.6781   5.4981  21.5139 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  67.6241     1.4901  45.384  < 2e-16 ***
#height_cent   0.9570     0.1395   6.859 1.48e-08 ***
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 10.32 on 46 degrees of freedom
#  (3 observations deleted due to missingness)
#Multiple R-squared:  0.5057,	Adjusted R-squared:  0.4949 
#F-statistic: 47.05 on 1 and 46 DF,  p-value: 1.481e-08

# Equation:
# e0 <- 67.6241 + 0.9570*(height)

#Question: (1) How to intepret this intercept coefficient?
#          (2) Whether the slope coefficients are different before and after centering?
#		       (3) Whether model fit statistics are different before and after centering?
# Interpretation: 
# One unit change in height translates to a increase in weight by 0.9570. 
# If height were 0 in cm, weight level would be 67.6241.
# Height variable is statistically sig & multiple r^2 is 0.5057, or 50.5%. 
# Meaning the predictor/explanatory variable of height explains ~51% of the variance
# in the outcome of weight - ok model.



################################################################################
# Section 11.6: Polynomial and Dummy Regression
################################################################################

#*To create a quadratic regression model predicting weight as a function of height.
#First, we need to create our quadratic term, which as we know from lecture is 
#Height*Height. Once we've computed squared height we can include it in the model.

heights4$heightsq <- heights4$height * heights4$height

model1.sq <- lm(weight ~ height + heightsq, data=heights4)
summary(model1.sq)
#> summary(model1.sq)
#
#Call:
#lm(formula = weight ~ height + heightsq, data = heights4)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-22.129  -5.516  -1.033   6.235  22.398 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)  
#(Intercept) 437.306970 269.081777   1.625   0.1111  
#height       -5.350858   3.194487  -1.675   0.1009  
#heightsq      0.018695   0.009459   1.976   0.0543 .
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 10.01 on 45 degrees of freedom
#  (3 observations deleted due to missingness)
#Multiple R-squared:  0.5451,	Adjusted R-squared:  0.5249 
#F-statistic: 26.97 on 2 and 45 DF,  p-value: 2.006e-08

# Interpretation: 
# One unit change in heightsq translates to a increase in weight by 0.018695. 
# If height & height sq were 0, weight level would be 437.306970.
# Height variable is minimally statistically sig & adj r^2 is 0.5249, or 52.5%. 
# Meaning the predictor/explanatory variable of heightsq explains ~52.5% of the variance
# in the outcome of weight - better model than the last.

# Equation:
# e0 <- 437.306970 - 5.350858*(height) + 0.018695*(heightsq)

# Recall, we had the model before:

# model1 <- lm(weight ~ height, data=heights4)
#> summary(model1)
#
#Call:
#lm(formula = weight ~ height, data = heights4)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-22.2766  -5.7871  -0.6781   5.4981  21.5139 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -92.6062    23.3991  -3.958  0.00026 ***
#height        0.9570     0.1395   6.859 1.48e-08 ***
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 10.32 on 46 degrees of freedom
#  (3 observations deleted due to missingness)
#Multiple R-squared:  0.5057,	Adjusted R-squared:  0.4949 
#F-statistic: 47.05 on 1 and 46 DF,  p-value: 1.481e-08

# Question: Whether adding a quadratic term (Model 2) statistically improve
# the model fit (compared with Model 1)?  [Question from Page 13 on Slide 11.4]

# Answer: Adding a quadratic term improved the model fit, even though variables were not as
# statistically sig anymore


summary(model1)
summary(model1.sq)
library(lmtest) # library for regression blocks
anova(model1, model1.sq) # Creates a regression block

#> anova(model1, model1.sq)
#Analysis of Variance Table
#
#Model 1: weight ~ height
#Model 2: weight ~ height + heightsq
#  Res.Df    RSS  Df   Sum of Sq   F  Pr(>F)  
#1     46 4902.2                              
#2     45 4510.7  1    391.53 3.9061 0.05426 .
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1

#*Q1: Write out the estimated regression equation for the model in Block 1 (Model 1: model1).
# e1 <- 67.6241 + 0.9570*(height)

#*Q2: Write out the estimated regression equation for the model in Block 2 (Model 2: model1.sq).
# e2 <- 437.306970 - 5.350858*(height) + 0.018695*(heightsq)

#*Q3: Are Model 1 and Model 2 nested models? Explain why (or why not).
# Model 1 & 2 are nested models as we are comparing whether or not to include the heightsq coeff.

#*Q4: What is the conclusion based on the results above?
# Conclusion: F-test for R-squared change in nested models
# F-statistics = 3.9061
# R-squared change =  0.5451 -  0.5057    =  0.0394
# p-value for the F-test (for R-squared change in nested models) = 0.05426
# Thus, we fail to reject the null; two models have the same model fit.
# Therefore, while the R^2 was slightly better, according to this hypothesis test -
# adding the heightsq coeff was pointless.


#######################################
#Curve fitting for logarithmic 
#linear model using a log transformation

heights4$height.log <-  log(heights4$height)
logmodel <- lm(weight ~ height.log, data=heights4) 
summary(logmodel)  

#> summary(logmodel)
#
#Call:
#lm(formula = weight ~ height.log, data = heights4)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-22.6045  -5.8665  -0.5587   5.3260  23.3237 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -744.74     121.43  -6.133 1.83e-07 ***
#height.log    158.71      23.72   6.690 2.66e-08 ***
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 10.45 on 46 degrees of freedom
#  (3 observations deleted due to missingness)
#Multiple R-squared:  0.4931,	Adjusted R-squared:  0.4821 
#F-statistic: 44.75 on 1 and 46 DF,  p-value: 2.663e-08   

# Interpretation: 
# One unit change in height.log translates to a increase in weight by 158.71. 
# If height were 0, weight would be -744.74.
# Height.log variable is statistically sig & r^2 is 0.4931, or 49.3%. 
# Meaning the predictor/explanatory variable of height.log explains ~49.3% of the variance
# in the outcome of weight - worse than the last.

# Equation:
# e0 <- -744.74 + 158.71*(height.log) 

########################################

#Dummy coding, factor variables
#There are a few ways to declare categorical variables in R for regression. 

model4 <- lm(weight ~ factor(race), data=heights4)
summary(model4)

#> summary(model4)
#
#Call:
#lm(formula = weight ~ factor(race), data = heights4)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-24.318 -11.901   0.348   9.737  47.071 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    66.8750     4.3004  15.551   <2e-16 ***
#factor(race)2   3.0536     5.8605   0.521    0.605    
#factor(race)3  -1.7841     6.2184  -0.287    0.776    
#factor(race)4   0.9432     6.2184   0.152    0.880    
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 14.9 on 44 degrees of freedom
#  (3 observations deleted due to missingness)
#Multiple R-squared:  0.01532,	Adjusted R-squared:  -0.05181 
#F-statistic: 0.2282 on 3 and 44 DF,  p-value: 0.8762

## use dummy variables
##  race: 1 = white (reference), 2 = black, 3 = hispanic, 4 = other
table(heights4$race)
#> table(heights4$race)
#
# 1  2  3  4 
#12 15 12 11 
heights4$black <- NA
heights4$black[heights4$race==1]<-0
heights4$black[heights4$race==2]<-1
heights4$black[heights4$race==3]<-0
heights4$black[heights4$race==4]<-0

heights4$hispanic <- NA
heights4$hispanic[heights4$race==1]<-0
heights4$hispanic[heights4$race==2]<-0
heights4$hispanic[heights4$race==3]<-1
heights4$hispanic[heights4$race==4]<-0

heights4$other <- NA
heights4$other[heights4$race==1]<-0
heights4$other[heights4$race==2]<-0
heights4$other[heights4$race==3]<-0
heights4$other[heights4$race==4]<-1

table(heights4$black)
table(heights4$hispanic)
table(heights4$other)
#> table(heights4$black)
# 0  1 
#35 15 
#> table(heights4$hispanic)
# 0  1 
#38 12 
#> table(heights4$other)
# 0  1 
#39 11 

model.dummy <- lm(weight ~ black + hispanic + other, data=heights4) 
summary(model.dummy)
#> summary(model.dummy)
#
#Call:
#lm(formula = weight ~ black + hispanic + other, data = heights4)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-24.318 -11.901   0.348   9.737  47.071 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  66.8750     4.3004  15.551   <2e-16 ***
#black         3.0536     5.8605   0.521    0.605    
#hispanic     -1.7841     6.2184  -0.287    0.776    
#other         0.9432     6.2184   0.152    0.880    
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 14.9 on 44 degrees of freedom
#  (3 observations deleted due to missingness)
#Multiple R-squared:  0.01532,	Adjusted R-squared:  -0.05181 
#F-statistic: 0.2282 on 3 and 44 DF,  p-value: 0.8762

# Equation:
# e0 <- 66.8750 + 3.0536*(black) - 1.7841*(hispanic) + 0.9432*(other)



