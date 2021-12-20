# Biostatistics for Public Health
# George Cedeño
# New York University

# Notes: The following code is to conduct R procedures contained within the Biostatistics for 
# Public Health lab, which is taught using Stata. Only the code and associated datasets are provided. 
# No instruction in R is given nor any course support, however the R code is annotated such 
# that students can follow along. Using R is not a requirement of the course and is only for 
# interested students. Note that a '#' character indicates the proceeding text is a comment.

#============================================================================================
# Biostatistics Lab 10
#============================================================================================

rm(list=ls())

library(haven)
#setwd("C://Users//davida02//Documents//Biostatistics for Public Health//HW 10")
# the above syntax is typical of Windows OS; notice the C://
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)
estradl.data<-read_dta("estradl.dta")

#if you want to see the first few lines of the data
head(estradl.data)

#estimate model of estradiol regressed on BMI
BMI.model <- lm(ES_1~BMI,data=estradl.data) # BMI = X, ES_1 (Estradiol) = Y

#look at output of regression model
summary(BMI.model)
# Estriadol = 54.9462 + -0.4569(BMI)
# Interpretation: 
# One unit change in BMI translates to a decrease in estradiol by 0.4569. 
# If BMI were 0, estradiol level would be 54.9462.
# BMI variable is statistically sig & multiple r^2 is 0.00758, or .7%. 
# Meaning the predictor/explanatory variable of BMI explains ~.2% of the variance
# in the outcome of estradiol - poor model.

#plot standardized residuals against predicted values (fitted values = predictions)
plot(rstandard(BMI.model)~fitted(BMI.model)) # base r functions
# Homoskedasticity.
# Notice that there are outliers and there seems to be linearity. 
# Assumption 1 is not violated.

#qqplot of residuals
qqnorm(rstandard(BMI.model))
qqline(rstandard(BMI.model)) # line of best fit
# Errors are independent & data is normally distributed. 
# Assumption 2 is not violated. 

#create the equation from the output, and plug in 42.24
# intercept + slope*42.24
e0 <- 54.9462 + -0.4569*42.24
e0

#create a box plot of estradiol values by race
#the value 2 are Whites, the value 1 are Blacks
boxplot(ES_1~Race, data=estradl.data)
# Overall, Black women have, on average, less estradiol than White women.

#estimate a regression model of estradiol regressed on race (binary variable)
estra.race <- lm(ES_1~factor(Race),data=estradl.data)
summary(estra.race)
# Estriadol = 38.121 + 16.986(race_2)
# Interpretation: 
# One unit change in race, from black to white, translates in an increase in estradiol by 16.986. 
# If race were 0, estradiol level would be 38.121. 
# Race variable is statistically sig & adj-r is 0.07332, or 7.3%. 
# Meaning the predictor/explanatory variable of race explains ~7% of the variance
# in the outcome of estradiol - not the best, but better than previous model.


