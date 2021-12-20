# Biostatistics for Public Health
# Created by : George Cedeno
# New York University College of Global Public Health
# Last updated Septembter 19th, 2021

# This is a  R  file required for Lesson 2, Homework 2


# Data file for this exercise: "hw2.csv"


# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

# import the hw2 .csv file 
hw2 <- read.csv(file="hw2.csv", header=TRUE, sep=",")

View(hw2)

library(dplyr)

## Q1: 
# Recode values of BMI that are greater than 100 as "system missing". In R, missing values are 
# represented by the symbol NA. In Stata, we can use "." to indicate a missing value. Match the 
# following statistics with their reported values.
hw2$BMI[hw2$BMI > 100] <- "NA"
hw2$BMI <- as.numeric(hw2$BMI)
str(hw2)
summary(hw2$BMI)
# Q3: = 24.3
# Median = 21.9
# Mean = 22.56
# Q1 = 20.1
# Max = 37
# Number of NA: 10
# IQR = 4.2
# Min = 15.5
sd(hw2$BMI, na.rm = T)
# Standard Deviation = 3.91
# Valid Sample Size = 93 obs - 10 NAs = 83

## Q2: 
# The median is less affected by extreme values in the data than is the mean
# TRUE

## Q3: 
# Which of the following statistical quantiles are measures of central tendency?
# Median & Mean

## Q4: 
# Which of the following statistical quantities are measures of spread?
# Standard Deviation, Variance, IQR, & MAD (Mean Absolute Deviation)

## Q5:
g1 <- c(2,4,6,8,10,12)
g2 <- c(6,6,7,7,8,8)
g3 <- c(1,1,3,11,13,13)
smoke <- matrix(c(g1,g2,g3), ncol = 3)
smoke <- data.frame(smoke)
class(smoke)

var(smoke$X1)
sd(smoke$X1)

var(smoke$X2)
sd(smoke$X2)

var(smoke$X3) # 35.2
sd(smoke$X3) # 5.9


