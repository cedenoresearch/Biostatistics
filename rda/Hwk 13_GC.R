### Biostatistics for Public Health
# Homework #13
# George Cedeño
# New York University

# This is a  R  file required for Homework 13
# It contains all syntax to complete the modules in Lesson 13. You can execute the 
# code all at once, or you can highlight part of the code and execute it.

# Data files for this exercise: 
# "hdl12.dta"
# "birthwt3.dta"

# Clear global env
rm(list=ls())

# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)


# importing the data required for homework
library(haven)
hdl2 <- read_dta("hdl2.dta")

## Q1: 
model1 <- glm(factor(lohdl) ~ block, data=hdl2, family = "binomial", na.action=na.omit)
summary(model1) # 1.5805

## Q2: SE of ln(OR)
summary(model1) # 0.5528

## Q3: 
exp(cbind(OR=coef(model1), confint.default(model1))) # 1.643760282 14.3523582

# Q4:
library(haven)
birthwt <- read_dta("birthwt3.dta")
#Run a logistic regression to predict the outcome using Smoke, 
#Black and Other as the independent variables.
model1 <- glm(LBW ~ SMOKE+BLACK+OTHER, data=birthwt, family = "binomial", na.action=na.omit)
#Odds ratio
exp(cbind(OR=coef(model1), confint.default(model1)))

## Q5: We cannot make which of the following conclusions based on the results above?
# A: We can not say "The effect of smoking on the risk of having low birth weight baby is higher 
# among black women than among white women", because we did not include interaction in the equation. 
# The model assumes the effect of independent variables are additive; therefore the effect of each 
# independent variable is the same for different values of other variables.

## Q6: The odds of having low birth weight baby for a Black women who smoked during pregnancy
# can be obtained from
summary(model1)
-1.8405+1.1160+1.0841
exp(.3596)

## Q7: Run a second model. Which one of the following is incorrect?
model2 <- glm(LBW ~ SMOKE+BLACK+OTHER+MOTH_AGE+MOTH_WT, data=birthwt, family = "binomial", na.action=na.omit)
summary(model2)
# A: After controlling for mother's weight, race, and smoking status, older mothers have a lower risk
# of having LBW baby. This finding is sig @ 5% level.

## Q8: the odds of having low birth weight baby for a 25 year-old Black woman who
# weighs 120 pounds and who did not smoke during pregnancy
# use the output
summary(model2)
x <- 0.332452 + 1.054439*0 + 1.231671*1 + 0.943263*0 - 0.022478*25 - 0.012526*120
exp(x) #.606

## Q9: Run a 3rd log regression to further include variables about medical hx including:
# history of premature labor, HTN, urinary irritation, and # of physician visits.
# Run the Hosmer-Lemeshow goodness-of-fit test. Also, obtain the predicted probability of positive
# outcome.

# Which of the following variables re: medical hx has a sig imapct (at 5% level) on the risk of
# women having low birth weigh babies?
model3 <- glm(LBW ~ SMOKE+BLACK+OTHER+MOTH_AGE+MOTH_WT+PREM+HYPER+
                URIN_IRR+PHYS_VIS, data=birthwt, family = "binomial", na.action=na.omit)
summary(model3)
# A: Hx of HTN
exp(cbind(OR=coef(model3), confint.default(model3)))

## Q10: What overall percentage of the sample is correctly predicted?
library(ResourceSelection)
library(caret)
hoslem.test(birthwt$LBW, fitted(model3)) # model fits bc we fail to reject the null
# Estimate predicted probabilities, save variable to dataframe
birthwt$pre1 <- predict(model3, newdata=birthwt, type="response")
# save the predicted classifications 
birthwt$pred_class <- 0
birthwt$pred_class[birthwt$pre1>=0.5] <- 1
# cross tabulation of true values and prediction 
table(birthwt$LBW, birthwt$pred_class)
confusionMatrix(as.factor(birthwt$LBW), as.factor(birthwt$pred_class)) 

## Q11: For the participant on line 73 (ID number 164.00), 
# what is the predicted probability of LBW using model 3.
birthwt$pre1[birthwt$ID==164]
# note that you can also click on the dataset from environment window on right 
# and then look for ID 164 in row and then look under colum pre1

## Q12: 
# McFadden's Pseudo R square for model 3
# we can use Desctool pacakge for this
library(DescTools)
PseudoR2(model3, which = "McFadden") 

# What is the value of the Akaike Information Criterion (AIC) for model 3?
summary(model3) # 221.28

## Q13: What is the null hypothesis for the Hosmer-Lemeshow test?
# A: The logistic model fits the data well

## Q14: Hosmer-Lemeshow test on model 3; does it fit well?
hoslem.test(birthwt$LBW, fitted(model3))
# A: True










