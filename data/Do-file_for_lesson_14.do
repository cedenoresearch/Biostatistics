/*
Biostatistics for Public Health
Professor Ryan Richard Ruff
New York University College of Global Public Health

This is a do-file for Stata lab required for Lesson 14. It contains all syntax to complete the modules in
Lesson 14. You can execute the code all at once, or you can highlight part of the code and execute it.
*/

*Data file(s) for this exercise: "hip_surv.dta"

/* In this lab, we demonstrated how to generate survival curves and hazard curves using survival data, as well as calculate statistics using the Kaplan-Meir method.*/
    

*Section 14.1:Kaplan Meier Method
*Data file(s) for this section: "hip_surv.dta"


*Declare data to be survival-time data
/*The "stset" command is used to tell Stata the format of your survival data. You only have to ‘tell’
Stata once after which all survival analysis commands (the st commands) will use this
information.*/
stset time, failure(fracture==1)  
  
  
*Use the below command to obtain Kaplan Meier Curve
sts graph, by(protect)


*Log Rank Test:
sts test protect, logrank

*Life Table: 
/* The command "ltable" displays and graphs life tables for individual-level or aggregate data and optionally presents
the likelihood-ratio and log-rank tests for equivalence of groups. ltable also allows you to examine
the empirical hazard function through aggregation*/
ltable time fracture, survival by(protect) test


 
*Section 14.2: Cox Regression
*The command "stcox" fits, via maximum likelihood, proportional hazards models on survival time data.
stcox protect calcium




