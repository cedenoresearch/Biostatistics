# Biostatistics for Public Health
# George Cedeño
# New York University

# Notes: This code contains all syntax to complete the modules in Lesson 13. 
# This lab will review logistic regression and model fit.

#===========================================================================================================
# Biostatistics Lab 13
#===========================================================================================================

#Remove all from previous environment
rm(list = ls())

#Package installation
install.packages("ResourceSelection")
install.packages("caret")

#Libraries
library(ResourceSelection)
library(caret)
library(haven)

#Load data frame, heights5.dta
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

heights5 <- read_dta("heights5.dta")
View(heights5)
attach(heights5) # with this, no need to indicate "heights5$variable", just "variable"

# print first few observations
head(heights5)
names(heights5)

## 1. Logisitic regression with 1 predictor:
# Logistic regression uses the glm() function. In glm, build your model, specify your data frame, 
# and finally denote that you are to use the binomial family
# Note: output from glm is defaulted in log odds format
model1 <- glm(diabetes ~ daysugargm, data=heights5, family = "binomial", na.action=na.omit)
summary(model1)
confint.default(model1) #produce confidence intervals
logLik(model1) #Log Likelihood: This number can be used to compare nested models.

# You can produce odds ratios and 95% CIs from the previous results by simply exponentiating the coef
# and confint functions:
exp(cbind(OR=coef(model1), confint.default(model1)))

# The package "ResourceSelection" has a number of useful functions 
# hoslem.test from that pacakge can be used for Hosmer-Lemeshow Goodness of Fit (GOF) Test.
hoslem.test(heights5$diabetes[1:50], fitted(model1))

# Estimate predicted probabilities, save variable to dataframe
heights5$pre1 <- predict(model1, newdata=heights5, type="response")

# save the predicted classifications 
heights5$pred_class <- 0
heights5$pred_class[heights5$pre1>=0.5] <- 1

# cross tabulation of true values and prediction 
table(heights5$diabetes, heights5$pred_class)
confusionMatrix(as.factor(heights5$diabetes), as.factor(heights5$pred_class)) 
# Note 1: you need caret library for this;
# Note 2: A confusion matrix is used for eval performance of a classification model; precision/accuracy
# Note 3: 'Sensitivity' describes how well the model detects an outcome in all who truly have the outcome,
#          or the percent of individuals with the outcome who were correctly identified. 
#         'Specificity' describes how well the the model detects no outcome in all who truly do not have 
#          the outcome, or the percent of individuals without the outcome who were correctly identified. 
#         'PPV' = % of positive outcomes that are truly positive; tests how well classification works
#         'NPV' = % of negative outcomes that are truly negative; tests how well classification works

## 2. Logisitic Regression with multiple predictors
model2 <- glm(diabetes ~ daysugargm + sodium + blood_cotinine + acrylamide2 + age + weight + sex, 
              data=heights5, family="binomial", na.action=na.omit)
summary(model2)

# odds ratios and 95% CIs 
exp(cbind(OR=coef(model2), confint.default(model2)))

# Steps to obtain "Predicted Probabilities" statistics:
heights5$pre2 <- predict(model2, newdata=heights5, type="response")

# Obtain "Goodness of Fit" statistics
hoslem.test(heights5$diabetes[! is.na(heights5$pre2)], fitted(model2))
# note that R deletes the records where one or more of the predictor is missing values 
# therefore when computing goodness of fit we have to exclude those values as well 

# Classification table with sensitivity/specificity results
# save the predicted classifications 
heights5$pred_class_m2 <- 0
heights5$pred_class_m2[heights5$pre2>=0.5] <- 1
confusionMatrix(as.factor(heights5$diabetes), as.factor(heights5$pred_class_m2)) 
# note you need caret library for this 

## 3. Logistic Regression with Multiple Predictors
model3 <- glm(diabetes ~ daysugargm + age,
              data=heights5, family="binomial", na.action=na.omit)
summary(model3)

#odds ratios and 95% CIs 
exp(cbind(OR=coef(model3), confint.default(model3)))


## 4. Logistic Regression with Categorical Option
model4 <- glm(diabetes ~ age + daysugargm + height + factor(race), 
              data=heights5, family="binomial")
summary(model4)

#odds ratios and 95% CIs 
exp(cbind(OR=coef(model4), confint.default(model4)))

# Steps to obtain "Predicted Probabilities" statistics:
heights5$pre4 <- predict(model4, newdata=heights5, type="response")

# Obtain "Goodness of Fit" statistics
hoslem.test(heights5$diabetes[! is.na(heights5$pre4)], fitted(model4))
# note that R deletes the records where one or more of the predictor is missing values 
# therefore when computing goodness of fit we have to exclude those values as well 

# Classification table with sensitivity/specificity results
# save the predicted classifications 
heights5$pred_class_m4 <- 0
heights5$pred_class_m4[heights5$pre4>=0.5] <- 1
confusionMatrix(as.factor(heights5$diabetes), as.factor(heights5$pred_class_m4)) 







