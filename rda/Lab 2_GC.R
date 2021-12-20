# Biostatistics for Public Health
# Created by : George Cedeno
# New York University College of Global Public Health
# Last updated May 13th 2021
# Updated September 14, 2021 by A. Davidow

# This is a  R  file required for Lesson 2, lab activites 2
# It contains all syntax to 
# complete the modules in Lesson 2. You can execute the code all at once,
# or you can highlight part of the code and execute it.


# Data file for this exercise: "heights.csv"


# IMPORTANT:  Change the current directory to the folder on YOUR COMPUTER with datasets


work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

# import the heights .csv file 

install.packages("readstata13")
library(readstata13)
install.packages("haven")
library(haven)
height <- read_dta("heights.dta")

#  height<- read.csv(file="heights.csv", header=TRUE, sep=",")


head(height)


######### Section 2.1 Calculating measures of central tendency ########
# In this example, we calculate descriptive statistics for the variable age
# Use the "summary" command, followed by name of the dataset and  $ sign and thenvariable name, to display basic descriptive statistics.
summary(height$age)
# min: 11, max: 999, Q1: 28.5, Median: 39, Mean: 118.4, Q3: 64.5

# get standard devation, we have to remove na.rm=t option to tell R to remove missing value
# from the calculation, otherwise R returns NA 
sd(height$age, na.rm = T)
# sd: 263.1295

# to get variance 
var(height$age, na.rm = T)
# var: 69237.14

# get interquartile range 
b <-quantile(height$age,c(.75,.25),na.rm = T)
IQR <- b[1]-b[2]
IQR

# or you can use describe command from psych library
library(psych)
packageDescription("psych") # A general purpose toolbox for personality, psychometric theory and experimental psychology.
describe(height$age)

# compare describe with summary
summary(height$age)

####### Practice exercise 2.1 #########
# 1. Generate the measures of central tendency with the variable Height – what do we see?
describe(height$height)

# Now, physically change the value of "height" for the first case in the dataset  to read 450 (centimeters),
# and rerun the analysis. What happens to our measures of central tendency?
# MAKE A COPY OF THE DATA SET FIRST and then perform the above step so main dataset is unchanged
height_dup = height
height_dup$height[1] <- 450
describe(height_dup$height)

# mean is increased from 167 to 173 but median is not affected as much. Standard deviation 
# and skewness has increased as well due to the new height value of 450

########## Section 2.2: Compare variables by central tendency ###########

# In this lab section, we compute measures of central tendency for the variables
# "height" & "weight" simultaneously.

# Note than in R you can just run above describe command twice and it will show 
# summary of both variables one after another 
describe(height$height)
describe(height$weight)

# or else if you want to show them side by side you can following command 
install.packages("pastecs")
packageDescription("pastecs") # Regularisation, decomposition and analysis of space-time series.
library(pastecs)
stat.desc(height[,c("height", "weight")])

########## Section 2.3:Calculating Geometric and Harmonic mean##########
# Use the command  harmonic.mean and geometric.mean to calculate geometric 
# and harmonic mean of a continuous variable 'age' 

# arithmetic mean 
mean(height$age, na.rm = T)

# calculating harmonic and geometric mean using psych package 
library(psych)
harmonic.mean(height$age, na.rm = T) # used in averaging things like rates (e.g., the average travel speed given a duration of several trips)
geometric.mean(height$age, na.rm = T) # useful when numbers in the series are not independent of each other or if numbers tend to make large fluctuations.

############ Section 2.4:Calculating measures of variability##############
# In this section, we calculate measures of variability for the variable "age".
# Key words: standard deviation (sd), Variance (var), minimum value (min),
# and maximum value (max) 
stat.desc(height[,c("age")])   #or you can use 
summary(height$age)

####### practice exercise 2.4 #########

#####Generate the measures of dispersion with the variable Height – what do we see?
stat.desc(height[,c("height")]) 

# 0r
var(height$height, na.rm = T)
sd(height$height, na.rm = T)

# Now, physically change the value of height for the first case in the dataset to be 450 (centimeters), 
# and rerun the analysis. What happens to our measures of dispersion?

# here using the height_dup dataset from before where we changed the first case already
stat.desc(height_dup[,c("height")])
var(height_dup$height, na.rm = T) # gross increase
sd(height_dup$height, na.rm = T) # increase

# both variance and sd increased alot 

############Section 2.5: Compare variables by measures of variability###########

stat.desc(height[,c("height","weight")])

########### Section 2.6: Full variable properties #################
summary(height$height)
stat.desc(height[,c("height")])
quantile(height$height, c(0.01,.05,.10,.25,.50,.75,.90,.95,.99), na.rm = T)


########### Section 2.7: Box plot ################
## compare boxplot of the original data vs. the modified data
boxplot(height$height, 
        main="Boxplot for height", 
        ylab="Height in centimeters")
boxplot(height_dup$height, 
        main="Boxplot for height", 
        ylab="Height in centimeters")



