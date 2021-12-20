# Biostatistics for Public Health
# George Cedeno
# New York University

# Notes: The following code is to conduct R procedures contained within the Biostatistics for Public Health lab, which is taught using Stata.
# Only the code and associated datasets are provided. No instruction in R is given nor any course support, however the R code is annotated such 
# that students can follow along. USing R is not a requirement of the course and is only for interested students. Note that a '#' character indicates
# the proceeding text is a comment.

#===========================================================================================================
# Biostatistics Lab 12
#===========================================================================================================

#Remove all from previous environment
rm(list = ls())

#Package installation
install.packages("gmodels")
install.packages("epibasix")
install.packages("DescTools")

#Libraries
library(gmodels)
library(epibasix)
library(DescTools)

#Load data frame, hdl2.csv
work_dir <- "~/MPH_EPI/Fall '21/Biostatistics/MyRCode"
setwd(work_dir)
hdl <- read_dta("hdl2.dta")
View(hdl)

#Note, we have already recoded this dataset such that all variables are 0 or 1. Zeros are for negative or null responses, 1 for positive. 
attach(hdl) #This is a new command--previously we would use dataframe$variablename. That is still valid, but if yhou use attach(dataframe), R then
            #allows you to use varaible names without pointing them to a dataframe. Useful if just using one frame

#Frequency tables in R use the table() function. You can do two or three (or more) variables. table(x,y) and table(x,y,z) for example will create 2 and 3 way tables.
table1 <- table(alch, smk) 
table1 # Print the table 

#You can use the prop.table() function for additional output. Just using prop.table(tablename) yields cell percentages. Specifying option 1 produces
#row percentages, and option 2 produces column percentages
prop.table(table1)
prop.table(table1, 1)
prop.table(table1, 2)

#Using the gmodels package, you get additional information using the crosstable() function. Make sure you call gmodels.
#As with table(), you specify the row and column variables. 
CrossTable(hdl$alch, hdl$smk)

#Yet another way is to build your own tables using the xtabs function which allows you to constuct them using a formula. The basic structure is
#xtabs(~var1+var2+var3). If you just use two variables, you produce a standard 2x2 table. If you use 3, you produce a 2x2x2 table.
xtabs(~alch+smk)

#You can use the epibasix package to produce common statistics for epidemiologists using case contorl or cohort studies. After using the epibasix library,
#we'll use the function epi2x2(). This will give us an odds ratio and a chi-square statistic. Due to the method, the chi-square results may not match Stata
epi2x2(table1)

#You can calculate the chi square test of independence using chisq.test(tablename)--make sure you have already ran table(). 
#In this function we've included an option correct=FALSE to not have a continuity correction, which is necessary for results to match that of other stat programs
chisq.test(table1, correct=FALSE)

#For Mantel-Haenszel, use the mantelhaen.test(x) function. You must have already estimated a table of two nominal variables, 
#You must provide three arguments in the function: your two categorical variables and the third strata from which you wish to test conditional independence
#In this function we've specified correct=FALSE to not have a continuity correction, which is necessary for results to match that of other stat programs
mantelhaen.test(heavy, alch, smk, correct=FALSE)

#Finally to perform Breslow Day tests, you can use the BreslowDayTest from the DescTools package. Load the package first.
#You must also store the 2x2x2 (or whatever configuration) in R using the xtabs function. Here, we will create one and store it as x, using x in the
#BD function.
x<-xtabs(~heavy+alch+smk)
BreslowDayTest(x)

