#Biostatistics for Public Health
#Created by: George Cedeno
#Last updated 09/10/2021
#New York University School of Global Public Health

#This is a file for R lab required for Lesson 1. 
#It contains all syntax to complete Lesson 1.
#
#Section 1.1: Changing work directory.
#Section 1.2: Read data files
#Section 1.3: Compute new variable
#Section 1.4: Selection of a subset of data
#
#
#Additional codes
#Section 1.5: Save a new data set
#Section 1.6: Recode a variable

################ Section 1.1: Changing work directory ##########
# The command "getwd()" displays your current directory in the Results window. 
# working directory is a folder on your computer where R looks for datasets or other files
getwd()

#Use the "setwd()" command, followed by a path on your computer,to change the current directory
#It points R to a folder on your computer.
#YOU NEED TO CHANGE THE PATH TO THE FOLDER ON YOUR COMPUTER

work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

#downlaoding and loading the libraries to load the .dta or . csv files
install.packages("foreign")
install.packages("readstata13")
install.packages("dplyr")
install.packages("readxl")

# To use a package, invoke the library(package) command to load it into the current Rstudio session
library(foreign)
library(readstata13)
library(dplyr)

######################################################################
############ Section 1.2 To read\open data files  #################
######################################################################

#Use the read.csv command to import csv files.
#The below command  will import heart.csv, which you can later save as heart.R
heart <- read.csv("~/MPH_EPI/Fall '21/Biostatistics/MyRCode/heart.csv", header = T, stringsAsFactors = T)
View(heart)
edit(heart)

#*Q1: How many observations does the dataset have?
str(heart) # 286
#*Q2: How many variables does the dataset have?
str(heart) # 12
#*Q3: Does the dataset include the variable, gender?
# yes it does; but as integers & not factors

# You can  also use dta files by using haven library and read_dta command 
install.packages("haven")
library(haven)
heights <- read_dta("heights.dta")

#use head function to look at the first few lines of the data
head(heights)

######################################################################
##############Section 1.3 Compute new variable ########################
######################################################################

#Recode gender, currenly coded as "1" for females and "2" for males, 
#into a new variable (new_gender) with codes "0" and "1"
heart$new_gender <- as.factor(heart$gender)
heart$new_gender <- recode(heart$new_gender, "1" = "0", "2" = "1")

#Here, we convert height--measured in centimeters in the dataset--to a 
#new variable measured in inches.
heights$height_inch = heights$height/2.54

#Here we took the natural log of the original height variable to
#create the logged height
heights$height_ln = log(heights$height)

#*Q4: How many variables does the dataset have now?
# heights has 7 variables & heart has 13
#*Q5: How many observations does the dataset have now?  
# heights has 50 & heart has 286
# assigning 1000 as missing value for cholestrol variable 
heart$chol[heart$chol==1000]<-NA

######################################################################
##############Section 1.4 Selection of a subset of data ##############
######################################################################

# assigning NA as missing value for age
heart$age[heart$age == 999]<-NA

# create a new temporary dataset, heights_subset, if one is younger than 30 yrs
heights_subset <-heights[which(heights$age<30),]

#now summarize age variable using summary() option 
summary(heights_subset$age)


######################################################################
############## Additional codes (1): Save a new R data set ###########
######################################################################

#*Recode "age", a continous variable, into a categorical one. To do this we create a 
#new variable, age_groups with values of 1, 2, 3, 4, and 5 corresponding to 0-29 years,
#30-39 years, 40-49, 50-59, and 60+, respectively. 
heights$age_groups <- cut(heights$age, c(0,30,40,50,60,90), right = FALSE, labels = c(1:5))

#check the frequency of each age group
table(heights$age_groups)

#label the values of the age_groups variable 
heights$age_groups <-ordered(heights$age_groups, levels=c(1,2,3,4,5), 
                             labels=c("below 30yrs", "30 thru 39", "40 thru 49", "50 thry 59","above 60 yrs"))


######################################################################
############## Additional codes (2): Save a new R data set ###########
######################################################################

#Export a R data set to CSV in R
write.csv(heights, "heights.csv", row.names = TRUE)

### End of Lab 1###
