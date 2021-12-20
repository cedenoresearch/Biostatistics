# Biostatistics for Public Health
# Homework #9
# George Cedeño
# New York University

# This is a  R  file required for Homework 9
# It contains all syntax to complete the modules in Lesson 9. You can execute the 
# code all at once, or you can highlight part of the code and execute it.

# Data file for this exercise: "tests.dta"


# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets

work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)
#============================================================================================
# Biostatistics Homework 9
#============================================================================================

#Remove all from previous environment
rm(list = ls())

#Package installation
install.packages("Hmisc") 

#Libraries
library(Hmisc) #functions for data analysis, high-level graphics, utility, etc.
library(plyr) #grammar of data manipulation
library(dplyr) # tool set for breaking down data
library(readstata13)
library(DescTools) # toolbox for descriptive and data analysis
library(pastecs) # Package for Analysis of Space-Time Ecological Series
library(haven)
options(scipen = 25) # forcing scientific notation

# Load data frame, tests.dta
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

tests <- read_dta("tests.dta")

## Q1:
# Which one of the following Pearson correlation coefficent (r) indicates the strongest
# correlation?
# A: -0.9

## Q2:
# A study shows that the sig correlation coeff between salt intake per day (grams) and
# BP (mm/Hg) is 0.5. How might one interpret this?
# A: Researchers found in this study that if there is more salt ina  subject's diet, he
# /she tends to have higher BP

## Q3: It is not appropriate to use Pearson's corr coeff (r) to quantify the correlation
# between the pair of variables in the follwing (curvilinear) scatterplot because the 
# relationship is not linear.
# A: True

## Q4: For 2 variables X and Y, the correlation between X and Y is the same as the corr
# between Y and X. (Put it mathematically, r(X,Y) = r(Y,X))
# A: True

## Q5: For Q5-Q7, let Y be BMI, X be # of hours exercised per week. Suppose there is a 
# sig linear relationship between BMI and # of hrs exercised per week as follows, 
# Y = 30 - 1.1*X. Which of the following statement is true based on this relationship?
# A: If one spends one additional hour exercising, one's BMI decreases by 1.1 unit.

## Q6: If a person exercises 5 hours per week, which of the following is the best
# prediction of their BMI? Round to the nearest whole number.
# A:
x <- 5
(y = 30 - 1.1*x) # 24.5

## Q7: What kind of effect does exercise have on BMI?
# A: Negative; as X (hours spent on exercise) increases, Y (BMI) decreases.

## Q8: The dataset "tests.dta" has a number of variables on various cognitive tests
# independent living scale, academic aptitude test, social adjustment scale, etc). 
# Calculate the Pearson's r-correlation between 'ils' (independent living scale) and 
# each of the following variables:
# scs (self-confidence scale)
# ssi (social skills inventory)
# aas (academic aptitude test)
# pas (personal adjustment scale)
# age (age of student)
# sas (social adjustment score)
rcorr(as.matrix(tests), type="pearson")
# or
x<-tests[1]
y<-tests[c(2:5,6,7)]
cor(y,x, use="complete.obs", method="pearson")

## Q9: WHich variables are neg related to ILS?
# A: age: age of student

## Q10: Which variable has the strongest correlation to ILS?
# A: age: age of student

## Q11: Using the tests.dta dataset, determine the line that best describes the linear relationship
# between self-confidence score (Y-variable) and social adjustment (X variable). The slope of the relationship
# is ____ (3 decimal points).
# Choose the correct answer from below. 
# A: 
model1 <- lm(scs ~ sas, data=tests)
summary(model1) #request summary table for model
confint(model1, level=.95) #request CIs for model parameters
anova(model1) #request anova table


## Q12: The intercept of the above relationship is ____(3 decimal points).
# Choose the correct answer from below.
# A:
model1 <- lm(weight ~ height, data=heights2)
summary(model1) #request summary table for model
confint(model1, level=.95) #request CIs for model parameters
anova(model1) #request anova table
























































