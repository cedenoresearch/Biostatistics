# Biostatistics for Public Health
# Created by : George Cedeno
# New York University College of Global Public Health
# Last updated September 25th 2021
# Uploaded September 14, 2021 by A. Davidow

# This is a  R  file required for Lesson 3, HW #3
# It contains all syntax to complete the modules in Lesson 3. You can execute the code all at once,
# or you can highlight part of the code and execute it.

# Data file for this exercise: "BP.dta"


# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets


work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

# import the BP.dta file 

install.packages("readstata13")
library(readstata13)
install.packages("haven")
library(haven)
BP <- read_dta("BP.dta")

head(BP)

## Q1: Which of the following statements regarding the Central Limit Theorem is correct?
# A: The mean and variance of any size sample will approximate the mean and variance of the population
# if the sample is randomly selected.

## Q2: IQ scores are normally distributed w/ mean 100 and standard deviation of 16. If a student's IQ
# score is 104, by how many standard deviations is his IQ score higher than the mean score?
# A: His IQ score is 1/4 standard deviation above the mean score.

## Q3: In a random sample of 100 students, the sample mean IQ score is 101.6 points. Use the standard 
# deviation from the previous question to calculate the standard error associated with this sample mean.
# How many standard errors is this sample mean different from the population mean IQ score?
# sd = 16
# n = 100
# SE = sd/sqrt(n) = 1.6
# population mean IQ score = 100
# sample mean IQ score = 101.6
# A: The sample mean is one SE higher than the population mean

## Q4: In a clinical trial, we measure the systolic blood pressure of a group of patients with high 
# blood pressure (BP). The average BP for these subjects is 125, with a SE of the sample mean of 0.16. 
# If the sample variance is 256, what is the sample size of this study?
# mu = 125
# SE = .16 = 16/sqrt(10000)
# var = 256
# sd = sqrt(256) = 16
# A: n = 10000

## Q5: Dataset BP.dta contains the BP readings of 315 patients. Using R, identify the following 
# statistics for BP for male patients.
library(dplyr)
install.packages("moments")
library(moments)
males <- BP %>% filter(sex < 2) %>% select(BP)
var(males)
skewness(males)
sd(unlist(males))
kurtosis(males)
count(males)
mean(unlist(males))
sd(unlist(males))/sqrt(count(males))

## Q6: The sample mean BP of both male & female are presented in the mean plot below. Note that the error
# bar represents 1 SE of the sample mean. Which one of the reasons below best explains the diff in SE
# of mean BP between male & female?
females <- BP %>% filter(sex > 1) %>% select(BP)
var(females)
skewness(females)
sd(unlist(females))
kurtosis(females)
count(females)
mean(unlist(females))
sd(unlist(females))/sqrt(count(females))






