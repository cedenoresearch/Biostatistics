# Biostatistics for Public Health
# Created by : George Cedeno
# New York University College of Global Public Health
# Last updated October 12th 2021

# This is a  R  file required for Homework 6
# It contains all syntax to complete the modules in Lesson 6. You can execute the code all at once,
# or you can highlight part of the code and execute it.

# Data file for this exercise: "survey2.dta"


# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets

work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

library(plyr)
library(readstata13)
library(haven)

survey2 <- read_dta("survey2.dta")

# Q1:
# A genetic theory says that a cross between two pink flowering plants will produce red
# flowering plants a proportion p = 0.25 of the time. To test the theory, 100 crosses are
# made and 31 of them produce a red flowering plant. What are the null and alternative 
# hypotheses for testing whether this theory is wrong?
# A: H0: p=0.25, Ha: p!=0.25

# Q2:
# Derive the test statistics for the hypothesis testing problem in Question 1 to THREE decimals. 
# Choose the correct answer from below."
N <- 100
p0 <- 0.25
p1 <- (31/100)
z <- ((p1-p0)/sqrt(p0*(1-p0)/N))
z # 1.386

# Q3:
# This is a 2-sided hypothesis testing problem
# A: TRUE

# Q4:
# Use a normal table (see attached) to find out the p-value for this problem to THREE decimals.
# (If you cannot find an exact number from normal table, which is common, use the best approximation.)
# A: the Z is 1.386
2*pnorm(z, lower.tail = FALSE)

# Q5:
# At level 10%, we don't have enough statistical evidence to reject the null hypothesis that the cross
# between two pink flowering plants produce red flowering plants with a proportion of 25%.
# A: TRUE

# Q6:
# A large company that produces allergy medications claims that Americans lose an average of 40 hours 
# of work to problems related to seasonal allergies. A consumer advocacy group believes that this 
# claim is actually just "hype" (exaggeration) intended to sell more medication, and the real loss 
# is not as high. The advocacy group would like to obtain statistical evidence about this issue and 
# takes a random sample of 100 American workers. They find that these 100 people lost an average of 
# 38 hours with a standard deviation of 8.6 hours. What are the null and alternative hypotheses in
# this situation?
# A: null hypothesis : true mean = 40, alternative hypothesis: true mean < 40

# Q7:
# Derive the test statistic (t score) for this hypothesis testing problem to TWO decimals. 
# Choose the correct answer from below.
N <- 100
x <- 38
mu <- 40
sd <- 8.6

sx <- sd/sqrt(N)
t <- (x - mu)/sx

# Q8: 
# Use a normal table to find out the p-value for this problem to THREE decimals.____
# A: 
(1-.9906) # using normal table to get .0094 

# Q9:
# At significance level of 5%, we don't have enough evidence to reject the null hypothesis 
# that the Americans lose an average of 40 hours of work to problems related to seasonal allergies.
# A: FALSE

# Q10:
# Use survey2.dta (Under Resources>Homework Files), to test the hypothesis that the mean BMI of the 
# students is lower than 24. First of all, choose the right set of null and alternative hypotheses.
# A: H0: mean BMI=24, Ha: mean BMI <24

# Q11: What's the p-value for this test to FOUR decimals? (Note: check if the above test is one-sided 
# or two-sided first). Choose the correct answer from below.
# A: Test hypothesis that mean BMI of students is lower than 24
# H0: Mean BMI = 24, Ha: mean BMI <24
summary(survey2$BMI)
t.test(survey2$BMI,mu = 24, alternative = 'less')

# Q12:
# At significance level 5%, we can reject the null hypothesis and claim that the mean BMI is less 
# than 24 for the student population of interest.
# A: TRUE
