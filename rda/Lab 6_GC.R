# Biostatistics for Public Health
# George Cedeño
# New York University

# Updated by Amy Davidow, October 17, 2021;
# New York University School of Global Public Health
# Pratice problemsfor lesson 6

#########################################################################
#                                                                       #
#                                                                       #
#                                                                       #
#              #import the heights dataset required for lab             #
#                                                                       #
#           DON'T FORGET TO SET YOUR WORKING DIRECTORY                  #
#           DON'T FORGET TO SET YOUR WORKING DIRECTORY                  #
#                                                                       #  
#########################################################################


work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

# READING IN THE DATA
height<- read.csv(file="heights.csv", header=TRUE, sep=",")

#####################################################################
#                                                                   #   
# Section 6.1: One sample t-tests: Comparing an observed            #
#         mean to a hypothetical value                              #
#     Is our sample's mean significantly different from 160 cm?     #
#                                                                   #
#                                                                   #
#####################################################################

summary(height$height)

# Notice that THERE are MISSING VALUES IN THE DATA SET:  CODED AS -1            
#   REPLACE THESE VALUES BY "NA" - NOT AVAILABLE.
height$height[height$height==-1]<- NA  # recoding -1 as missing 


# CHECK THAT REPLACEMENT WORKED:
summary(height$height)

# NOW YOU'RE READY TO TEST THAT THE MEAN HEIGHT (mu) IS EQUAL TO 160:
t.test(height$height,mu = 160)


###########################################################################
#                                                                         #
#   section 6.2 Comparing observed proportion to a hypothetical value     #
#                                                                         #
#                                                                         #
###########################################################################

#     Categorize age into 5 age groups:                                    
#     0-10, 10-20, 20-30, 30-40, 40+ 
height$age_groups <- cut(height$age, c(0,10,20,30,40,90), right = FALSE, labels = c(1:5))

# CHECK THAT THE COMMAND WORKED:
summary(height$age_groups)

# NOTICE THAT THERE ARE 4 SUBJECTS WITH MISSING VALUES

# Test whether the proportions of the variable for categorical_age is significantly different from a
# hypothesized population proportions (20%, 20%, 20%, 20%, 20%) using a CHI-SQUARE TEST
# this hypothesis is stating the 5 categories each represent 1/5 (20%) of the data.
# 

table(height$age_groups)

# these are the frequencies of the five age groups.
age_prop <-c(0,7,6,12,21)

# testing that these proportions are different from a distribution where the age groups are equal
chisq.test(age_prop, p=c(0.20, 0.20, 0.20, 0.20, 0.20))

# what is the p-value? 2.155e-05
# Can the null hypothesis be rejected? Yes, the proportions are different from a distribution where
# age groups are equal
# we usually reject the null hypothesis when the p-value is less than 0.05.

###################################################################
#                                                                 #
#                                                                 #  
#           Section 6.3: Independent samples t-test               #
#           NOTE:  NEED A DIFFERENT DATA FILE FOR THIS SECTION    #
#                                                                 #
###################################################################

# IMPORTING THE DATA FOR THIS SECTION:

heights1= read.csv(file="heights1.csv", header=TRUE, sep=",")

summary(heights1)

# For now, assume acrylamide1 was our measured variable. 
# Using the same treatment condition 
# variable, is there a significant difference in treatment condition for acrylamide?

#Generate box-plots to look at the sample distributions for control and treatment groups to assess 
# normality assumptions.

boxplot(acrylamide1~treat,data=heights1, ylab="Blood cotinine level")


# the usual t-test assumes that the variances for each group are the same
# checking this now for acrylamide1
# testing for equal variance using Bartlett's test (homogeneity of variance assumption)

bartlett.test(acrylamide1~treat, heights1)

#   since the p-value for this test is > 0.05 (actually a lot greater), 
#   there is no evidence that the variances for the different treatments
#   are different.  Can proceed with the usual t-test

# equal variance assumption met
t.test(heights1$acrylamide1~heights1$treat,var.equal = TRUE) 

# what is the p-value? 0.4211
# there is no significant difference in acrylamide1 values, correct? Correct

#################################################################################
#                                                                               #
#               Section 6.4:Paired samples t-test                               #
#                                                                               #       
#                                                                               #
#################################################################################

# Using a paired-samples t-test, did our sample of TFA in blood serum 
#(tfal variable in the heights1 data set)
# reduce significantly? 
t.test(heights1$tfa1, heights1$tfa2, paired=T)

# what is the p-value? 4.942e-06
# is the p-value larger than 0.05?
# the p-value is very small, right?
# it is much smaller than 0.05.

# conclusion:  there is a statistically significant difference in tfal


