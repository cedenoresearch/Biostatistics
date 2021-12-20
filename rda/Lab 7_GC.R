# Biostatistics for Public Health
# George Cedeño
# Created by:Jinal Shah 
# Updated October 25, 2021 by Prof. Davidow
# New York University College of Global Public Health

# This is a R script required for Lesson 7. It contains all syntax to 
# complete the modules in Lesson 7. You can execute the code all at once,
# or you can highlight part of the code and execute it.
# Data file(s) for this exercise: "heights2.csv" 



# Change the current directory to the folder on your computer
# This is the folder on MY COMPUTER!!!!!!!!!!!!!!!1
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

# READING IN THE DATA

# load the required libraries for the lab
library(DescTools) # toolbox for descriptive and data analysis

  
# import the heights2 dataset required for lab 
# the file comes from STATA

library(haven)
heights2<-read_dta("heights2_recoded.dta")

########################### Section 7.1: One-way ANOVA ###################


library(pastecs) # Package for Analysis of Space-Time Ecological Series
options(scipen = 25) # forcing scientific notation


# summary statistics 
# with labels for each column of interest (there are many other columns)
stat.desc(heights2[,c("height", "treat")])



# Compute the analysis of variance
# To conduct a one way ANOVA, we use the aov function.  
# The model Y~A:  
# Y is the outcome 
# A the conditioning variable - HERE IT IS the various treatments. 
# 

# First, ensure that treat is factor. Then conduct the aov() function
heights2$treat<-factor(heights2$treat)

# Now if you summarize the treatment variable "treat", a frequency table will appear

summary(heights2$treat)

# NOTICE THAT THERE ARE THREE DIFFERENT TREATMENTS:  



# Here we use Bartlet's test for equal variances to check the ANOVA 
# assumption that the variances are equal
# this is the same assumption for a two-sample t-test:  
# each group has approximately the same variance; if they didn't we would use Levene
bartlett.test(height~treat, data=heights2) # fails to reject the null, therefore e
# equal variance assumption holds

# this is a test where not obtaining a p-value less than 0.05 is a good thing!
# it means that variances are not different
# the ANOVA depends upon the assumption that the variances are not different.

# in the next step, we run the ANOVA and save the results in res.aov 
res.aov <- aov(height~treat, data=heights2)  
# Summary of the analysis
summary(res.aov)


############# Section 7.2: Post-hoc procedures ########################
# Note in this example our F-statistic was not significant.           #
# Normally this means that we would not do any post-hoc tests.        #
# We simply do it here for demonstration purposes.                    #
#                                                                     #
#                                                                     #
# There are different ways to perform various post-hoc procedures     # 
#             METHOD 1:  BONFERRONI APPROACH                          #
#             METHOD 2:  TUKEY APPROACH                               #
#                                                                     #
#######################################################################

# To use the post-hoc tests , we use "PostHocTest" function 
# FIRST ARGUMENT:   aov() results (THE ANALYSIS OF VARIANCE RESULTS)
# SECOND ARGUMENT:  methods to perform various post-hoc procedures 
# THE FIRST EXAMPLE, THE METHOD IS BONFERRONI

PostHocTest(aov(height ~ treat, data = heights2),  method = c("bonferroni"), conf.level = 0.95, ordered = FALSE)

# the output shows the differences Group 1 vs. Group 0, Group 2 vs. Group 0, and Group 2 vs. Group 1.
# there's a lower bound for the 95% CI:  lwr.ci
# there's an upper bound for hte 95% CI:  upr.ci
# there a p-value (pval) for testing whether the difference (diff) is significantly different from zero.
# all of these p-values are much bigger than 0.05, right!?

# THE SECOND EXAMPLE, THE METHOD IS THE TUKEY (HSD) APPROACH

# to get the Tukey (HSD) pairwise comparisons change the method argument in above function

PostHocTest(aov(height ~ treat, data = heights2),  method = c("hsd"), conf.level = 0.95, ordered = FALSE)

# AGAINN, ALL OF THESE P-VALUES ARE GREATER THAN 0.05, RIGHT?
# SO THERE ARE NO STATISTICALLY SIGNIFICANT PAIRWISE DIFFERENCES.


