# Biostatistics for Public Health
# Completed: George Cedeno
# New York University College of Global Public Health
# Last updated October 26th 2021

# This is a  R  file required for Homework 7
# It contains all syntax to complete the modules in Lesson 7. You can execute the code all at once,
# or you can highlight part of the code and execute it.

# Data file for this exercise: "calcium.dta" & "muscle.dta"


# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets

rm(list=ls())
set.seed(1234)

work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

library(plyr) # tool set for breaking down data
library(readstata13)
library(DescTools) # toolbox for descriptive and data analysis
library(pastecs) # Package for Analysis of Space-Time Ecological Series
library(haven)
options(scipen = 25) # forcing scientific notation


### Questions 1-22 ###
# You will analyze the dataset "calcium.dta" whic contains results from a randomized comparative experiment
# that aimed to investigate the effect of calcium on blood pressure in hypertensive men. A treatment group
# of 10 men recieved a calcium supplement for 12 weeks and a control group of 11 men received a placebo
# during the same period. All subjects had their blood pressure tested before and after the 12-week period.

# The following chart is an illustration of the experiment:
# Treatment group: Begin ---> Calcium ---> End
# Control group:   Begin ---> No Calcium --> End

# Variable Names in Calcium.dta:
# 1. Group: Whether subject received calcium ("2") or placebo ("1")
# 2. Begin: Seated systolic BP before tx (baseline)
# 3. End Seated systolic BP after tx (12 wks later)
# 4. Change: changes in BP (Begin - End)

calcium <- read_dta("calcium.dta")

## Q1: 
# The following 2 group comparisons can be formulated as which type of two-samples t-test?
# Comparing the BP between treatment and control group at the beginning of the study:
# A: Independent-samples t-test

## Q2: 
# Comparing the BP between treatment and control group at the end of the study
# A: Independent-samples t-test

## Q3: 
# Comparing the BP before and after the study for the tx group
# A: Paired-Samples T-test

## Q4: 
# Comparing the BP before and after the study for the control group:
# A: Paired-Samples T-test

## Q5: 
# Comparing the changes in BP between treatment and control group
# A: Independent-Samples T-test

## Q6: 
# Comparing the BP before and after the study for all the subjects (including both treatment and
# control groups)
# A: Paired-Samples T-test

## Q7: 
# Researchers are interested in finding out whether at the baseline, the treatment and control groups
# have the same mean level of BP
# Which one is an appropriate H0 for this research question?
# A: There is no difference in BP level between the tx and control group at baseline.

## Q8: 
# What is an appropriate Ha?
# A: There is a difference in BP level between tx group and control group at the baseline.

## Q9: 
# Perform an independent-samples t test to test the hypotheses of differences between treatment and
# control groups at baseline. Report the following values:
# P-value for robust equal variance test (to four decimals)
# First, ensure that Group conditions are factors. 
calcium$Group <- as.factor(calcium$Group)
table(calcium$Group)
boxplot(Begin~Group==2,data=calcium, ylab="BP at Beginning of Study")
bartlett.test(Begin~Group==2, data=calcium) # equal variance assumption hold
LeveneTest(Begin~Group==2, data=calcium) # equal variance assumption hold
t.test(calcium$Begin~calcium$Group==2,var.equal = TRUE) 
# A: p-val for Levene is 0.1674 ??? but the answer was .1689

## Q10: 
# Report the mean BP for the treatment group at baseline (to 2 decimals)
# A: 144.80 based on earlier t-test

## Q11: 
# Report the mean BP for the control group at baseline (to 2 decimals)
# A: 144.72

## Q12: 
# Report the t-score for this independent-sample t-test at baseline (to 2 decimals)
# A: -0.02

## Q13: 
# Report the p-value for the independent-sample t-test (up to 4 decimals)
# A: 0.9843

## Q14:
# We can conclude that:
# A: The BP level between treatment group and control group were NOT statistically sig different at baseline

## Q15:
# Further, researchers are interested in whether BP of all participants of the study (21 subjects)
# significantly changes from baseline to 12 weeks later.
# Think about the H0 & Ha. Is the Ha one-sided or two-sided?
# A: Two-sided; change could be pos or neg

## Q16:
# Conduct a paired-sample t-test to test the hypothesis formulated in the preceding question and report the
# following results:
# Mean of the diffrences in BP between baseline and 12 week follow-up
t.test(calcium$Begin, calcium$End, paired=T) 

## Q17:
# Report the t-score
# A: 10.639

## Q18: 
# Report degrees of freedom
# A: 20

## Q19:
# Report the p-value for the test
# A: 0.000000001105

## Q20:
# Based on the statistical evidence, we conclude that for all the subjects, the BP did not change from
# baseline to 12 weeks later (alpha = 0.05)
# A: False

## Q21:
# The results of this test are equivalent to a one-sample t-test testing the variable "changes" in BP is 0
# A: True; a paired-sample ttest to assess if there was a change in a repeated measures condition is 
# essentially testing if there was a "change" rather than "no change". 
# Let's see: 
t.test(calcium$change, mu=0) # exactly the same

## Q22:
# A p-value of 0.000 in Stata or R means:
# A: There is a prob of error (or outcome due to chance), but it is less than .001


### Questions 23-29 ###
# You will analyze the dataset "muscle.dta" with records normalized muscle activities comparing pre and
# post treatment performance of 3 alternative therapies. There are 4 variables in this dataset:
# 1. Gender: 1 = male, 2 = female
# 2. Group: 1 = tx A, 2 = tx B, 3 = tx C
# 3. Pre: Muscle activity before tx
# 4. Post: Muscle activity after tx

muscle <- read_dta("muscle.dta")

# Run one-way ANOVA to test the global hypothesis that there is no difference in pre-treatment muscle
# activity between the 3 experimental groups
stat.desc(muscle[,c("pre", "group")])
muscle$group<-factor(muscle$group)
summary(muscle$group)
bartlett.test(pre~group, data=muscle) # Equal variances assumed
LeveneTest(pre~group, data=muscle) # Equal variances assumed
res.aov <- aov(pre~group, data=muscle)  
# Summary of the analysis
summary(res.aov)

## Q23:
# True or False: At sig level 5%, we cannot reject the H0 that there is no difference in muscle
# activities between the 3 groups pre-treatment.
# TRUE

## Q24: Compare the mean muscle activities after (post) treatment of three therapy groups. In ascending 
# order (from low to high), the three therapy groups are:
stat.desc(muscle[,c("post", "group")])
summary(muscle$group)
bartlett.test(post~group, data=muscle) # Equal variances assumed
LeveneTest(post~group, data=muscle) # Equal variances assumed
res.aov <- aov(post~group, data=muscle)  
# Summary of the analysis
summary(res.aov)
PostHocTest(aov(post ~ group, data = muscle),  method = c("bonferroni"), conf.level = 0.95, ordered = TRUE)
PostHocTest(aov(post ~ group, data = muscle),  method = c("hsd"), conf.level = 0.95, ordered = TRUE)
# A: A-B-C; 2-3 is most sig, then 3-1, then 2-1 is least sig 
# C-A-B... is the answer

# Run a one-way ANOVA to test the global hypothesis of that post-treatment, the means of muscle activity
# are different across 3 groups. Choose the correct answer to complete the sentences.

## Q25: 
# Between group sum of squares (SSB): 
# A: SSb = 13.97

## Q26: 
# SSb associated df: 
# A: 2

## Q27: 
# Within group sum of squares (SSW):
# A: 40.590

## Q28:
# SSw associated df: 
# A: 31

## Q29:
# F=ratio
# A: 5.34

## Q30: 
# P-value
# A: 0.01

## Q31: 
# Based on the above test, at sig level 5%, we reject the null that there is no diff in muscle activities
# between the 3 groups post-tx
# A: True

## Q32: 
# Compute a new variable "change=post-pre", and run a one-way ANOVA to test whether the change between pre 
# and post treatment is sig different across the three therapy groups. 
# What is the p-value of this F test?
muscle$change <- muscle$post - muscle$pre
res.aov <- aov(change~group, data=muscle)  
# Summary of the analysis
summary(res.aov)
# A: 0.0001 ***

## Q33: 
# Run a post-hoc analysis comparing pairwise means between any 2 therapies using Tukey's HSD adjustment
# for multiple hypothesis testing. At sig level 5%, the post-treatment muscle activities are sig diff
# between:
PostHocTest(aov(post ~ group, data = muscle),  method = c("hsd"), conf.level = 0.95, ordered = TRUE)
# A: Treatment B & C

## Q34: 
# Conduct a post-hoc pair wise comparisons using Tukey's adj for the ANOVA test using change scores. 
# Overall, we observe more sig results (smaller p-vals) for the post-hoc mean comparisons when comparing 
# the changes in muscle activities across three groups, than comparing the post-treatment measure alone.
PostHocTest(aov(change ~ group, data = muscle),  method = c("hsd"), conf.level = 0.95, ordered = TRUE)
# A: True















