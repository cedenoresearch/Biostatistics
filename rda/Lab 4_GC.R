# Biostatistics for Public Health
# George Cedeño
# New York University

# Code revised by Prof. Amy Davidow
# September 24, 2021

# Notes: The following code is to conduct R procedures contained within the Biostatistics for Public Health lab.
# Only the code and associated datasets are provided. 

# Each week, the R code files will follow the same format. First, the overall code will be introduced in it's default form.
# After all the code is introduced, the lab will then have specific R code tailored to the code performed in each lab.


#Packages used, install using the following command:
install.packages("plyr") # Tools for splitting, applying, & combining data


# R is very flexible and powerful for subsetting using the dataframes. 
# Assuming we have an active data frame called mydata. The following command options are viable:



#===========================================================================================================
# Biostatistics Lab 4


#################################################################################################################
#                                                                                                               #
#     DO NOT FORGET TO SET THE WORKING DIRECTORY TO A PATH ON your OWN COMPUTER WHERE YOU ARE STORING THE DATA  #
#     the setwd() that appears below is set to a working directory on Prof. Davidow's computer                  #                                                                                                              #
#                                                                                                               #
#################################################################################################################

work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)

mydata <- read.csv(file="heights.csv", header=TRUE, sep=",")
View(mydata)

# Create new data frame as a subset of previous frame, including all observations with height > 0.
newdata <- subset(mydata, height>0, select=c(sex, age, height, weight, spouses_height))
# This effectively drops any value of height=0



# By simply utilizing the equation for Z-scores, after which 
# we verify it works by requesting the means and standard deviations

newdata$zheight <- (newdata$height-mean(newdata$height))/sd(newdata$height)
mean(newdata$zheight)
sd(newdata$zheight)

#######################################################
#   Did you expect the mean value to be zero?         # Yes, but it was 2.1 e^-16
#   Is the mean value exactly equal to zero?          # Nope
#   Why is it not exactly equal to zero?              # Perhaps distribution is skewed
#######################################################



# there are subjects in the data set with age listed at 999.
# revise the data set to set these data values to missing (NA)
# Hint:  See Lab 2 for how to replace values with missing values.
# alternatively, you may restrict the data set to observations
# for which age is less than 999

newdata$age[newdata$age == 999]<-NA

mean(newdata$age, na.rm = T) # 41.82609
sd(newdata$age, na.rm = T) # 21.63876

newdata$zage <- (newdata$age-mean(newdata$age, na.rm = T))/sd(newdata$age, na.rm = T)
mean(newdata$zage, na.rm = T) # -1.3 e^-16
sd(newdata$zage, na.rm = T) # 1

# or, to restrict the dataset to observations for which age is < 999
newerdata <- subset(mydata, height>0 & age < 999, select=c(sex, age, height, weight, spouses_height))
##############################################################
#                                                            #      
#    check the mean and standard deviation of age, before    #
#    and after, eliminating the age values listed as 999.    #  
#    standardize the age using both versions of age          #  
#                                                            #
##############################################################

