### Biostatistics for Public Health
# Homework #10/11
# George Cedeño
# New York University

# This is a  R  file required for Homework 10/11
# It contains all syntax to complete the modules in Lesson 10/11. You can execute the 
# code all at once, or you can highlight part of the code and execute it.

# Data files for this exercise: 
# "estradl.dta"
# "typhyslife.dta"
# "Systollic_bppractice.dta"
# "mortal1.dta"

# Clear global env
rm(list=ls())

# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

# All datasets are STATA data sets so you must use the haven library in order to import them into R.
library(haven)

#============================================================================================
# Biostatistics Homework 10/11
#============================================================================================

## Part 1:
# For Q1-6, use the dataset "estradl.dta". Subsequent questions use the datasets
# "typhyslife.dta", "Systollic_bppractice.dta" & "mortal1.dta". All of these datasets 
# are STATA data sets so you must use the haven library in order to import them into R.

# Q1: Using the dataset "estradl.dta", run a regression using (BMI) as the IV, and estradiol levels
# (ES_1) as the DV. Save the standardized residuals (y-axis) and the predicted values (x-axis).
# Which of the following statements about heteroscedasticity (i.e. violatio of the common
# variance assumption) is best based on the output?
# import the data 
est <- read_dta("estradl.dta")
# first remove any missing values (NAs) in DV or IV 
est_clean <- est[!is.na(est$ES_1),]
est_clean <- est_clean[!is.na(est_clean$BMI),]
# run the regression 
model1 <- lm(ES_1~BMI, data=est_clean)
#save Standardized Predicted Values 
est_clean$std_pv <- scale(model1$fitted.values)
# save Standardized Residuals 
est_clean$std_re <- scale(model1$residuals)
# Scatterplot of the standardized residuals (y axis) and the standardized predicted values (x axis)
plot(est_clean$std_pv, est_clean$std_re)
# Answer: there is no clear evidence of heteroscedasticity if we remove the two outliers in the 
# upper right portion of the graph.city)

# Q2: Run a QQ plot of the residuals. We use this plot to assess normality (more normal data
# will appear closer to the line). Does the graph from output show that the residuals are more normal
# than the graph from lecture?
#Q-Q plot
par(mfrow = c(2,2)) # to see all plots in one frame 
plot(model1)
par(mfrow = c(1,1)) #change back the setting 
# or 
library(car) # Companion to Applied Regression
qqPlot(model1, main="QQ Plot")
# Answer: 
# While there does appear to be normality, I would not say that the graph looks MORE normal
# than the output presented in the lecture graph.

# Q3: The estradiol level for subject # 4400 is missing. Predict the level of estradiol given
# her BMI is 42.24, up to 2 decimal points.
# To ontain the prediction: take the intercept and slope (coeff of BMI) from the R output
# and plug in the value 42.24.
# e0 <- intercept + slope*42.24
summary(model1)
# estradiol level for subject number 4400 is 
e0 <- 54.9462 - 0.4659*42.24 
e0 # 35.26658

# Q4: Plot the boxplot of estradiol (using "ES_1" as the variable) for the two ethnic groups 
# (using variable "race: as the category axis). Which of the following statements is the most
# accurate? White is 2, Black is 1.
# Use ggplot library here (or base R if preferred)
library(ggplot2)
est_clean$Race <- as.factor(est_clean$Race)
GenderPlot1 <- ggplot(est_clean, aes(x = Race, y = ES_1)) + geom_boxplot() 
GenderPlot1
# Answer: Excluding outliers, the spread of estradiol of both ethnic groups are relatively similar.

# Q5: Compare the mean estradiol of the 2 ethnic groups (Race=1 or 2) using regression.
# What is the mean diff in EST_1 between Race=2 (White) and Race=1 (Black) women (up to 3 decimals).
# Choose the correct answer:
# first, dummy code the variable
est$black<-NA
est$black[est$Race==1]<-1
est$black[est$Race==2]<-0
# second, regress
qu5 <- lm(ES_1~ black, data=est)
summary(qu5)  
# difference in estradiol between White and Black women is -16.986

# Q6: Do we reject the null hypothesis which says that the mean estradiol levels are the same
# between the 2 groups? 
# Answer: Yes

# For Q7-12, use the dataset "tvphyslife.dta"

# Q7: Revisiting "tvphyslife.dta" looking at life expectancy and people per physician, create a new
# variable, equal to the square term of people per physician. Now run a quadratic regression and
# include both people per physician and people per physician squared in the model (y= a + b1x + b2x2).
# What is the coefficient of the linear term in the R output (b1) (3 decimal points)?
# importing the data 
tvphy <- read_dta("tvphyslife.dta")
# square term: people per physician
tvphy$squar_phy <- tvphy$Peoplephysician*tvphy$Peoplephysician
# quadratic regression 
qu7 <- lm(Lifeexpectancy ~ Peoplephysician + squar_phy, data=tvphy)
summary(qu7)
# coefficient for linear term is -0.002 (pr -2.003e-03)

# Q8: Does the linear term coefficient (b1) from the quadratic regression model represent the slope?
# Answer: No; The relationship is not linear, so there is no line to have a slope, and the
# coefficient does not represent the slope in this form of equation.

# Q9: When we ran a linear regression the this same dataset & variables last week, the R^2 was
# .444. Does the R^2 value you just calculated using the quadratic model above suggest a better fit?
# Answer: Yes; R^2 from summary(qu7) = 0.5868, meaning the explanatory variables included in this model
# predict 59% of the outcome variable's variance. This suggests a better fit.

# Q10: Run a regression using the natural log of people per physician as the IV and life expectancy
# as the DV. What is the coefficient on the natural log of people per physician? 
tvphy$naturallog <- log(tvphy$Peoplephysician)
qu10 <- lm(Lifeexpectancy ~ naturallog, data=tvphy)
summary(qu10)
# Answer: Coefficient on the natural log of people per physician is -4.9744

# Q11: Of the 3 models we have run (linear, quadratic, ln) of people per physician on life expectancy, 
# which has the best fit according to the r^2?
# Recall:
qu11 <- lm(Lifeexpectancy ~ Peoplephysician, data=tvphy)
summary(qu11)
# Linear R^2 = .444
summary(qu7)
# Quadratic R^2 = .5868
summary(qu10)
# Ln R^2 = .6926
# Answer: the natural log model


## Part 2:
# Requires analysis of 2 datasets w/ STATA or R. 

# For the first dataset, "systolic_bppractice.dta" you will be asked to code a categorical
# variable into dummy variables. Then perform a multiple regression using dummy variables. 
# This dataset contains the following variables: 
# Age: respondent's age (in yr)
# Sex: gender of the respondent (1=M, 2=F)
# Fat: Fat consumed per day (in gram)
# Sysbp: Systolic bp
# Fatg: Categorical measure of fat (1 if fat intake is low [<50g per day], 
#                                   2 if fat intake is medium [(>=50g and <100g),
#                                   3 if fat intake is high (>=100g)])

# The second dataset for this homework is contained in the data file "mortal.dta". The 
# datafile contains the results of an observational study conducted by General Motors to
# evaluate whether air pollution, as indicated by nitric oxide concentrations, plays a role 
# in mortality. The study collected data on 60 urban areas throughout the US and the following 
# variables:
# city: City Name
# mortal: Age adjusted mortality (essentially a standardized mortality)
# educ: Median education
# popdens: Population density
# nw_pct: Percent non-white
# wc_pct: Percent white collar workers
# totpop: Total population
# lognox: Log (Base 10) nitric oxide
# nox_grp: Nitric oxide group factor (1=Low, 2=Moderate, 3=Significant, 4=High) based on log
#          transformed measures.
# wc_grp: White collar group factor (1=Mostly non-white collar, 2=Mostly white collar)
# dens_grp: Density group (1=lower density, 2=higher density)

# Throughout this homework, unless specified, the level of the test is assumed to be 5%

# Q12: Open dataset "systolic_bppractice.dta" in R, code the variable "fatg" into 3 dummy variables:
# "fat_low", "fat_medium", and "fat_high". 
# low = 1 [<50g per day], 
# medium = 2 [(>=50g and <100g),
# high = 3 (>=100g)])
syssbp <- read_dta("Systolic_bppractice.dta")
library(psych)
#dummy coding fat category variable 
a <-dummy.code(syssbp$fatg)  # dummy.code from psych pacakge 
syssbp <- cbind(syssbp, a) # combine the dummy variable and df using the cbind() function
# Rename a column in R
colnames(syssbp)[colnames(syssbp)=="1"] <- "Fat_low"
colnames(syssbp)[colnames(syssbp)=="2"] <- "Fat_medium"
colnames(syssbp)[colnames(syssbp)=="3"] <- "Fat_high"
#dummy coding  sex variable 
syssbp$male <- NA
syssbp$male[syssbp$sex==1]<- 1
syssbp$male[syssbp$sex==2]<- 0
#verifying the recoding 
table(syssbp$fatg)
table(syssbp$Fat_high)
table(syssbp$Fat_low)
table(syssbp$Fat_medium)
table(syssbp$sex)
table(syssbp$male)
# Now, if we were interested in comparing the BP between medium fat intake group and low fat intake
# group as well as the statistical significance, which one of the following multiple regressions
# is appropriate?
# Answer: Use sysBP as dependent variable, use two dummy variables (fat_medium and fat_high) as 
# independent variables. The baseline/reference group for this regression would be the low fat 
# intake group. Therefore we would be comparing our coefficients for the high_fat and medium_fat 
# variables with the low fat intake group. This is what the question wants.

# Q13: Run 2 multiple regressions using male as the IV in block 1, fat_low & fat_medium in block 2, 
# and sysbp as the DV. The mean diff in systolic BP (absolute value) between men & women before
# controlling for fat consumption is:
#blockwise regression 
reg1 <- lm(sysbp ~ male, data=syssbp)
summary(reg1)  
# Answer: Mean diff in systolic blood pressure (absolute value) between men and women 
# before controlling for fat consumption is 9.3092

# Q14: When comparing the 2 models run consecutively, which is true about the compared R^2 value 
# between the models. 
reg2 <- lm(sysbp ~ + male + Fat_low + Fat_medium, data=sysbp)
summary(reg2)
anova(reg1, reg2)
# Conclusion: F-test for R-squared change in nested models
# F-statistics = 37.099
# R-squared change = 0.2252 - 0.0404  =  0.184842
# p-value for the F-test (for R-squared change in nested models) = < 2.2e-16
# Answer: Thus, we reject the null; the two models do not have the same model fit.
# Therefore, while difference in R^2 was slight (0.185), the model was statistically sig -
# controlling for fat consumption improved the model.
# alternatively, another way to compute R^2 change:
rs1<-summary(reg1)$r.squared
rs2<-summary(reg2)$r.squared
rs.change <- rs1-rs2
rs.change 

# Q15: Are the fat_med & fat_low variables protective against high systolic BP with regards to
# the reference group?
#Code to obtain standardised Beta coefficients:
library(QuantPsyc)
lm.beta(reg1) 
lm.beta(reg2) # coeff for male decreases as fat_low & medium are added to the model.
# Answer: Yes

# Q16: Open the dataset "mortal.dta" in R. Run a simple regression to evaluate the effect of the log
# transformed Nitric Oxide (lognox) on Mortality (mortal). Based on the simple regression result, 
# ____ of the variation in mortal is explained by lognox. (Report the value as it is shown in R,
# such as 0.002 rather than 0.2%).
moratl <- read_dta("mortal1.dta")
rm1 <- lm(mortal~lognox, data=moratl)
summary(rm1) 
# Answer: R^2 = 0.0871

# Q17: Run a multiple regression to include the following demographic variables: Nitric Ox (lognox),
# median education (educ), population density (popdens), non-white percent (nw_pct), & white collar
# percent (wc_pct). Which variable has sig protective effects on mortality (5% level)? (In this case,
# protective effect means that if the IV increases, mortality decreases)
rm2 <- lm(mortal ~ lognox + educ + popdens + nw_pct + wc_pct, data=moratl)
summary(rm2)
# Answer: median education (educ); Has a negative impact, and statistically significant
lm.beta(rm1) 
lm.beta(rm2)

# Q18: Based on the regression results in Q17, the increase in which variables will result in sig
# increase in mortality (5% level)?
summary(rm2)
# Answer: popdens, nw_pct

# Q19: Of all the IVs used in the regression in Q18, which has the greatest impact on DV "mortal"?
# Answer: nw_pct

# Q20: Again from Q18, ___ of the variance in mortal is explained by this set of demographic IV?
# Answer: Adj-R^2 = 0.603

# Q21: The diff between the regression in question 16 and the regresion in question 17 is the
# inclusion of the additional population level descriptive variables. What is the change in 
# r^2 (absolute value) due to the inclusion of these variables?
## computing R^2 change
rs1<-summary(rm1)$r.squared
rs2<-summary(rm2)$r.squared
rs.change <- rs2-rs1
rs.change
# Answer: 0.5493781

# Q22: Based on the results of the 2 regressions that use lognox as an explanatory variable
# (Q16 & Q17), which one of the following statements is correct?
# Answer:
# - All of the demographic variables (*all of the IVs except lognox) have significant effects 
# on mortality (at level 10%)
# - When considered alone, the lognox seems to affect mortality significantly, but this relationship
# might be spurious
# - Controlling the demo variables, air pollution (measured by lognox) does not have a sig
# impact on mortality
















































































































