# Biostatistics for Public Health
# George Cedeño
# New York University

# Notes: The following code is to conduct R procedures contained within the Biostatistics for 
# Public Health lab, which is taught using Stata. Only the code and associated datasets are provided. 
# No instruction in R is given nor any course support, however the R code is annotated such 
# that students can follow along. Using R is not a requirement of the course and is only for 
# interested students. Note that a '#' character indicates the proceeding text is a comment.

#============================================================================================
# Biostatistics Lab 9
#============================================================================================

#Remove all from previous environment
rm(list = ls())

#Package installation
install.packages("Hmisc") 

#Libraries
library(Hmisc) #functions for data analysis, high-level graphics, utility, etc.
library(dplyr) #grammar of data manipulation

#Load data frame, heights2.csv
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)
f<-file.choose()
heights2 <- read.csv(file=f, header=TRUE, sep=",")
View(heights2)


#Two-way scatterplot
plot(heights2$height, heights2$weight, 
     main="Scatterplot of weight and height", 
     xlab="Height in CMS",
     ylab="Weight in kilos")

#or
library(GGally)
ggpairs(heights2[, c(3,4)],
        upper = list(continuous = 'smooth', combo = 'facetdensity', discrete = 'blank') ,
        lower = list(continuous = 'cor', combo = 'box'))

# Generate correlations and covariances. These commands will generate all pairwise correlations. 
# There are three arguments-the data frame, the use option, and the correlation type. Using 
# "complete.obs" tells R to assume listwise deletion for missing data. Use "pairewise.complete.obs"
# for pairwise deletion. Pearson is our correlation method, but you can also use spearman and kendall.
cor(heights2, use="complete.obs", method="pearson") # are variables correlated?
cov(heights2, use="complete.obs") # do variables covary?


#You can also specify just two variables to correlate
cor(heights2$weight, heights2$height, use="complete.obs", method="pearson")
cor(heights2$acrylamide1, heights2$blood_cotinine, use="complete.obs")

#To generate significance tests for a single pair wise correlation, you can use cor.test()
cor.test(heights2$weight, heights2$height, method="pearson")

# You can generate a full matrix of all varibles, including correlations, Ns, and p-values, 
# using rcorr. You must tell R that your dataframe is a matrix. Note that the rcorr() function 
# assumes pair-wise correlations. Thus, if you told R to use complete.obs for cor(), the results
# will not match.
rcorr(as.matrix(heights2), type="pearson")

# One can also focus on a specific collection of variables to generate a correlation matrix of
# interest. In R, variables are in columns. Thus, below we create two variables x and y. X contains
# columns 3 and 4 from the specified dataframe, which are height and weight. Y includes columns 7 
# through 9, or blood cotinine, acrylamide1, and acrylamide2
x<-heights2[3:4]
y<-heights2[7:9]
cor(y,x, use="complete.obs", method="pearson")

# For linear regression, you use the function lm() for linear model. You then specify the model you
# are fitting and the dataframe. The basic syntax is:
# function_name <- lm(y ~ x1 + x2 + x3, data=dataframe) 
# Note the use of ~ for "=". Below call the regression model weight = height "model1"
# but you can call it whatever you like
model1 <- lm(weight ~ height, data=heights2)
summary(model1) #request summary table for model
confint(model1, level=.95) #request CIs for model parameters
anova(model1) #request anova table

