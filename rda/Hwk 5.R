# Biostatistics for Public Health
# Created by : George Cedeno
# New York University College of Global Public Health
# Last updated October 12th 2021

# This is a  R  file required for Homework 5
# It contains all syntax to complete the modules in Lesson 5. You can execute the code all at once,
# or you can highlight part of the code and execute it.

# Data file for this exercise: "BP.dta"


# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets


work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

## Q1: Suppose we wish to estimate the mean total serum cholesterol in a single target population. 
# Based on a random sample (N=3310), we found the sample mean to be 200.3 and the sample sd to be 36.8.
# Construct a 95% CI (using the rule of thumb multiplier) for the mean serum cholesterol.
# What is the LCI?
# Answer: 
n <- 3310
mu <- 200.3
sd <- 36.8
p <- .5
se <- sd/sqrt(n)
LCI <- mu - 2*se
LCI

## Q2: What is the UCI?
# Answer: 
UCI <- mu + 2*se
UCI

## Q3: (Continued from the previous question) Supposed a researcher was able to access a subset of the 
# above sample. The size of the subset is 100, and he calculated the sample mean based on the subsample
# to be 202.3 and the sd to be 37.7. Based on the subsample, construct a 95% CI (using rule of thumb 
# multiplier) for the mean serum cholesterol. 
# What is the LCI?
# Answer: 
n <- 100
mu <- 202.3
sd <- 37.7
p <- .5
se <- sd/sqrt(n)
LCI <- mu - 2*se
LCI

## Q4: What is the UCI?
# Answer: 
UCI <- mu + 2*se
UCI

## Q5: The CI based on the subsample is much wider than based on the full sample due to the difference
# in sample size.
# Answer: TRUE

## Q6: The table below shows the # of men and women w/ and w/o CVD in a sample. The CVD prevalence
# proportion for men is ?
# Answer: 244/1792; 13.61%

## Q7: The SE of the CVD prevalence proportion for men is ? 
# (Note, for this question, assume that the variance = pq and that the SE = sd/sqrt(n))
# Answer:
n <- 1792
p <- 244/n
q <- 1-p
var <- p*q
sd <- sqrt(var)
se <- sd/sqrt(n)
se

## Q8: Construct a 95% CI for the population CVD prevalence for men, the LCI is:
LCI <- p - 2*se
LCI

## Q9: Construct a 95% CI for the population CVD prevalence for men, the UCI is:
UCI <- p + 2*se
UCI

## Q10: CIs for men BP = (127.4, 138) ; for women BP = (121.6, 125.2)
# Answer: The mean BP value for males is significantly different from the mean BP of females BP 
# where p-value < .05



