# Biostatistics for Public Health
# Created by : George Cedeno
# New York University College of Global Public Health
# Last updated September 30th 2021


# This is a  R  file required for Lesson 4, HW #4
# It contains all syntax to complete the modules in Lesson 4. You can execute the code all at once,
# or you can highlight part of the code and execute it.

# Data file for this exercise: "Survey2.dta"


# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets


work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

# import the BP.dta file 

install.packages("readstata13")
library(readstata13)
install.packages("haven")
library(haven)
survey <- read_dta("Survey2.dta")

head(survey)

## Q1: Use R to find mean of age 
mean(survey$age, na.rm = T) # 29.17021

## Q2: Use R to find the sd of age
sd(survey$age, na.rm = T) # 7.906222

## Q3: Use R to compute the z-score for age using the mean & sd you obtained from questions 1&2. 
# Create a new variable "zage" and label it "Z score of age". Verify that the mean of age is 0 and
# sd is 1. You might observe some rounding errors but the differences should be very small and no
# bigger than .01. If your answers don't agree, go back and do it again.
# Which of the follwing is correct?
survey$zage <- (survey$age-mean(survey$age, na.rm = T))/sd(survey$age, na.rm = T)
mean(survey$zage, na.rm = T) 
sd(survey$zage, na.rm = T) 
# Answer: All of the above; If student's zage is close to zero, then student's age is close to average 
# of class. If less than zero, student is younger than average of the class. For each student, zage 
# measures how many sd away from the mean age.

## Q4: Now if we assume that age is normally distributed, w/ mean and sd from Q1 & Q2, then zscore 
# of age should have:
# Answer: Standard normal distribution w/ mean 0 and var 1 (b/c sd is 1; 1^2 = 1)

## Q5: Now keep the assumption that age is normally distributed w/ mean and variance as given in your
# answers to questions 1 & 2. Let's randomly choose a student from this class, and calculate the 
# probability that this student is older than 43.5. You can use the normal table below to calc this
# probability. [Hint: you will need to write down the equivalent Z-score expression, i.e. [Age >= 43.5]
# is equivalent to [Z score for age is >=?]. Then figure out the probability for this event using the 
# normal table. If you have trouble doing this, see notes for second half of class lecture on Z scores 
# for details. You will have to round the Z-score to two decimal points in order to find a corresponding
# probability].
# First, what is the z-score of 43.5? x-mu / sd
(43.5 - 29.17021)/7.906222 # z-score of 1.81247, then look up answer in the normal table
pnorm(1.81) # or simply input the z score here, this outputs the probability that a student is <43.5
1 - pnorm(1.81) # this finds out what the prob that a student is above 43.5

## Q6: Based on your answer in Q5, we expect that there are X% of students older than 43.5 in this class.
(1-pnorm(1.81))*100 # 3.5%

## Q7: Use R to find out the 90th percentile of age based on data (round to nearest tenth).
quantile(survey$age, probs = seq(.1, .9, by = .1), na.rm = T) # 41.7 or 42
quantile(survey$age, probs = .9, na.rm = T)

## Q8: T/F: There are actually 10% of the students in this class that are older than 42
# True

## Q9: Why do answers 6 & 8 not match?
# Unfortunately, there is no histogram to observe - only the Normal Table. Relative to the mean, 
# the z-score for 43.5 is 1.81. This means those with an age of 43.5 are almost 2 standard deviations 
# away from the mean. On the unit normal table, 1.81 corresponds with a percentile between .96 & .97. 
# This led me to believe the prob of being 43.5 or younger (below the value) was .965. If I subtract 
# the area under the curve, 1, minus .965 = .35 x 100 = 3.5%. According to the z-score calculations,
# this means there is a 3.5% probbility of ages in the distribution being equal to or above 43.5. Whereas, 
# if I look at percentiles, the median (50%) is 26.5. Technically, this means that 50% of ages are at or
# below 26.5; however, this is not taking into consideration the average (which was 29.17021). Similarly,
# according to the percentiles, all ages below 41.7 fall within the 90th percentile. Therefore, my
# best guess as to why answers 6 & 8 don't match are because percentiles use the median as the average 
# (50th percentile), while standardized z-scores use the mean as the average (z-score of 0).

## Q10: Using the abbreviated normal table, find out the following probabilities (report all to 2 
# decimal places. For instance, if the p is .5, do not write 50 or 50%, write .50)
# P(Z < -1.405)
pnorm(-1.405) # 0.8

## Q11: P(Z > .994)
1 - pnorm(.994) # 0.16

## Q12: P(|Z| > .994)
2*(1-pnorm(.994)) # 0.32





































