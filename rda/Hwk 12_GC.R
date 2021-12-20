### Biostatistics for Public Health
# Homework #12
# George Cedeño
# New York University

# This is a  R  file required for Homework 12
# It contains all syntax to complete the modules in Lesson 12. You can execute the 
# code all at once, or you can highlight part of the code and execute it.

# Data files for this exercise: 
# "Hw12ldl.dta"

# Clear global env
rm(list=ls())

# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

# All datasets are STATA data sets so you must use the haven library in order to import them into R.
library(haven)
library(gmodels)
library(epibasix)
library(DescTools)

est <- read_dta("Hw12ldl.dta") #fyi, 1 = yes, 2 = no

attach(est)

## Q1. Beta Blockers are commonly used to control cardiovascular conditions (i.e. HTN & MI). In a 
# study, researchers are interested in whether a side effect of Beta Blockers is an increased risk
# of having low HDL cholesterol levels. In this study, which variable is the risk factor and 
# which is the outcome?
# A: RF - Beta Blocker use, Outcome - Low HDL

## Q2: The risk factor in this ex. can also be thought of as the IV.
# A: True

## Q3: Let's examine this relationship by running a 2x2 table.
# Match the following percentages from your output.
CrossTable(est$block, est$lohdl)
row_pct_total <- prop.table(table(est$block)) # for beta- & non-beta blocker
col_pct_tolal <- prop.table(table(est$lohdl)) # for lohdl & normalhdl
row_pct <- prop.table(table(est$block, est$lohdl), 1)
col_pct <- prop.table(table(est$block, est$lohdl), 2)
# A: 1. Percent of individuals who are beta blocker users = 57.7%
#    2. Percent of individuals with low HDL levels among non-beta blocker users = 50%
#    3. Percent of individuals w/ low hdl levels = 69%
#    4. Percent of individuals who are beta blocker users among low hdl levels = 82.9%
#    5. Percent of individuals w/ low hdl among beta blocker users = 69.4%
#    6. Percent of individuals who are beta blocker users among individuals w/ normal (not low) HDL = 31.8%

## Q4: Assuming this is a case-control study: better to use probability than odds?
# A: False

## Q5: Calc odds of having low hdl levels among beta blocker users 
table(block, lohdl)
p <- 34/(34+7)
q <- 1 - p
p/q
# A: 4.857143

## Q6: Calc odds of having low hdl among non-beta blocker users
table(block, lohdl)
p <- 15/(15+15)
q <- 1 - p
p/q
# A: 1

## Q7: Calc OR of low hdl of beta blocker users as compared to non-beta blocker users.
table(block, lohdl)
a <- 34
b <- 7
c <- 15
d <- 15
OR <- (a*d)/(b*c)
OR
# A: 4.857143; similar to the odds of having low hdl among beta blocker users

## Q8: Run a 2-way freq. What is the chi-square value for the relationship between beta blocker use
# and low HDL levels?
# x <- table(block, lohdl) 
# chisq.test(x, correct=FALSE)
# or
chisq.test(table(est$block, est$lohdl), correct=FALSE)
# A: 8.783

## Q9: Taking into account the DF, and sig value (5% level), we can reject H0 and assume there is
# a relationship between blocker use and low HDL levels.
# A: True

## Q10: Run 2-way freq. between HDL and other variables in the dataset and conduct a pearson's chi-
# square test between HDL and each one of the 5 variables. Looking at the chi-sq stats for each
# of the 5 explanatory variables, which are sig at the 5% level? (Choose all correct answers)
chisq.test(table(block, lohdl), correct=FALSE) # p: 0.00304; sig
chisq.test(table(alch, lohdl), correct=FALSE) # p: 0.0004955; sig
chisq.test(table(smk, lohdl), correct=FALSE) # p: 0.1586; not sig
chisq.test(table(heavy, lohdl), correct=FALSE) # p: 0.2223; not sig
chisq.test(table(older, lohdl), correct=FALSE) # p: 0.02181; sig
# A: Beta blocker use, alcohol use, & old age

## Q11: Since we know which variables have a sig relationship with low HDL levels, we can
# determine between which variables are protective factors and which are risk factors from 
# the chi-sq value, DF, and sig.
# A: False

## Q12: If HDL levels were given in the dataset as a continuous (quantitative) variable, and
# all the other variables were still given as dichotomous variables, which of the following 
# statistical tools can we use to explore the relationship between a continous quantitative DV
# and one or more binary IVs (binary meaning taking only 2 values)? (choose all correct answers)
# A: regression, anova, ttest

## Q13: It is likely that the beta blocker use and age both relate to HDL levels. Suppose we want
# to see if there is confounding between these variables (i.e. what happes if older people are more
# or less likely to use beta blockers). Calc OR using the dataset "hdl2dta." Before we calc OR
# we need to run "all possoble 2-way freq table" to stratify by "beta-blocker use".
hdl <- read_dta("hdl2.dta") 
#load the libraries 
library(epitools)
# we can use oddsratio and put the outcome variable first- here lohdl and exposure variable second - here older
oddsratio.wald(table(est$lohdl[est$block==1], est$older[est$block==1]))
# A: Odds ratio of low HDL between older and younger people among the beta blocker users is 6.75

## Q14: Calc OR of low HDL between older and younger people among the non-beta blocker users. Before we calc OR 
# we need to run "all possible 2-way freq table" to stratify by "beta-blocker user".
oddsratio.wald(table(est$lohdl[est$block==2], est$older[est$block==2]))
# A: Odds ratio of low HDL between older and younger people among the beta 
# blocker users is 6.00

## Q15: Calculate (gross) OR before adjusting for beta blocker use w/ this dataset:
hdl_recode <- read_dta("hdl_recode.dta") 
table(hdl_recode$block, hdl_recode$older)
oddsratio.wald(table(hdl_recode$lohdl, hdl_recode$older)) 
# A: Odds ratio before adjusting for beta blocker use is 3.383

## Q16: Adjusted OR after controlling for beta blocker use
mantelhaen.test(hdl_recode$lohdl, hdl_recode$older , hdl_recode$block, correct=FALSE)
# A: 6.295

## Q17: 95% CI for M-H odds ratio:
# A: from above output is 1.678-23.616

## Q18: Controlling for betab-blocker use, we can assume LOW HDL is not independent of age
# (i.e. that the odds M-H OR is not = to 1) [5% sig level test]
x<-xtabs(~hdl_recode$lohdl+hdl_recode$older+hdl_recode$block)
BreslowDayTest(x)
# p value is 0.93 so we failed to reject the null hypothesis for Breslow-day statstics
# A: There's no interactive effect






