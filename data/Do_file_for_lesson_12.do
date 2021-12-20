/*
Biostatistics for Public Health
Professor Ryan Richard Ruff
New York University College of Global Public Health

This is a do-file for Stata lab required for Lesson 12. It contains all syntax to complete the modules in
Lesson 12. You can execute the code all at once, or you can highlight part of the code and execute it.
*/

*Data file(s) for this exercise: "hdl.dta" and "hdl2_.dta"


*Section 12.1:Crosstabs 2X2
*Data file(s) for this section: "hdl.dta"

*We believe that alcohol use is related to smoking. We’d like to do an analysis of these two categorical variables.
/*The command "tabulate" produces a two-way table of frequency counts, along with various measures of association, including the common Pearson's chi-squared, the likelihood-ratio chi-squared, 
 and Fisher's exact test, 
.*/  
*Below is the general form of syntax 
*tabulate varname1 varname2 [if] [in] [weight] [, options]
*In this section, we generate a two-way table of variables "alch" & "smk" along with pearson's chi-square, expected cell count, row and column percent. 
tabulate alch smk, chi2 column expected row
 
 
 
*Section 12.2: Logs and Odds Ratio
*We believe that alcohol use is related to smoking. We’d like to calculate Odds Ratio of these two categorical variables.
*Method 1 
*Data file(s) for this section: "hdl.dta"
*The keyword "cci" followed by cell frequencies of cells a,b,c,& d obtained from 2x2 table in the section 12.1 would calculate Odds ratio.
cci 15 26 6 24

*Method 2
*Data file(s) for this section: "hdl2_.dta"
*The keyword "cc' followed by case variable "alch" and exposed variable "smk"
cc alch smk


*Section 12.3: Mantel Haenszel Odds Ratio
*Data file(s) for this section: "hdl2_.dta"
*We would like to look at the relationship between three categorical variables, with the third variable Subject smokes (the possible confounder) as a stratifying variable.
*The keyword "cc' followed by case variable "heavy" and exposed variable "alch". The keyword "by" is used to specify the third variable to stratify the analysis.
cc heavy alch, by(smk) bd


