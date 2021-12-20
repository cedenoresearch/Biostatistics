# Biostatistics for Public Health
# George Cedeño
# New York University

# Updated by Amy Davidow, October 5, 2021

#####################################################################
#                                                                   #
#                                                                   #
#             setting the working directory                         #
#                                                                   #
#       THIS IS THE WORKING DIRECTORY ON MY COMPUTER                #
#                                                                   #
#     MAKE SURE YOU SET THE WORKING DIRECTORY TO THE PATH           #
#       ON YOUR COMPUTER WHERE                                      #
#       YOU ARE STORING THE DATA                                    #
#                                                                   #
#####################################################################

work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)


# Remove all from previous environment

rm(list = ls())

# Packages
# These packages will help you create confidence intervals for continuous variables and for proportions.

install.packages("DescTools")
install.packages("plyr")
install.packages("PropCIs")
install.packages("readstata13")
install.packages("haven")


# Calculating confidence intervals really follows a basic formula; assuming a specific distribution, it 
# is the mean +/- 1.95*SE for a 95% interval.You can either compute this manually or by using the
# MeanCI() function, as follows, substituting the dataframe, variable name, and % level


library(DescTools)
library(plyr)
library(PropCIs)
library(readstata13)
library(haven)

heights <- read_dta("heights.dta")

# Note we also introduce a new sub-option you will frequently see, "na.rm=TRUE". This tells R in the 
# command in which it is called to set any instance of "NA", previously used as missing, to be removed.
# Otherwise some functions may not work when you have set missing or out of range
# data to NA. As an example:

x <- mean(heights$weight, na.rm=TRUE)
x

# To use the future CI of proportions, you need to obtain counts of the categorical variable.
# As with many things, R supports a number of strategies to obtain it. 
# Here We use the "counts()" function under the plyr package. As an example:
# library(plyr)
count(heights, 'sex')

# To calculate exact confidence intervals for a proportion (there are numerous ways to calculate 
# the CI of a proportion, but we will stick with exact method.)
# we use the PropCI package and the exactCI() function. You provide three arguments: 
# the frequency count of interest, the total count, and the confidence level
# library(PropCIs)
exactci(25,48,conf.level=0.95)


#===========================================================================================================
# Biostatistics Lab 5

#  Calculate the 95% confidence interval for means
#  library(DescTools)

MeanCI(heights$height, conf.level=0.95, na.rm=TRUE)


# Calculate mean of a proportion
library(plyr)
count(heights, 'sex')

# After running count, we see there are 25 males, 23 females, and 2 missing. 
# So, out of 48 participants, 25 are male. We then call the exactCI() function
library(PropCIs)

# this command provide the 95% confidence interval for a proportion:
exactci(25,48,conf.level=0.95)


# To calculate the confidence interval for height, first calculate the relevant components of the CI equation. 
# We will assign temporary variables to these calculations for the mean, sd, and number of observations. 
# However, note that the results may differ slightly due to differences in rounding and formula the mean
mu <- mean(heights$height, na.rm=TRUE)

# the standard deviation
sd <-sd(heights$height, na.rm=TRUE)

# the number of observations in the data set
n <-length(heights$height)

# the standard error of the  mean
se <-sd/sqrt(n)

# Then, calculate the z(alpha/2) for the multiplier. 
# For a 95% confidence interval, alpha=5%, so we are looking for the z-value that corresponds 
# to "one minus half of alpha". This is the z value that corresponds to 0.975 = 1-1/2(0.5)


# Remember, we are assuming two sided. One could also use the z value that corresponds to 0.025, 
# but the sign would need to be dropped. Similarly, for the 90% confidence interval, one would use 
# z(0.95) or z(.05). To return the multiplier using the z-distribution, use the qnorm() function
qnorm(.025)
# the value is -1.959964
# use this value to get the lower limit of the 95% CI 
ci_l<-x-(1.959964*se)

# here is the lower bound:
ci_l

# now use this value to get an upper limit of the 95% CI
# calculating the upper bound:
ci_u<-x+(1.959964*se)

# here is the upper bound
ci_u

# what to do if you were to estimate a 90% confidence interval
# need to change the z value.  for 95% CI, Z-value is obtained by dividing 5% by 2 (to get 2.5%), 
# and then obtaining qnorm(0.025)
.05/2
qnorm(0.025)
# for a 90% CI, Z-value is obtained by dividing 10% by 2 (to get 5%), and then obtaining qnorm(0.05)
.1/2
qnorm(0.05)
  
