# pae_hks

This file walks through the different components of the analytics for my PAE.

# Running PAE Analytics

To run this code, you just need to update the links in the different sections to point to the correct source files

* Source data (df) - I have this locally but can send the files along using Google Drive or some other secure link
  *  Daniel Y's R functions file
  *  EIP export from Crunch
  *  HT (levels for sharing)
  *  AF export from Crunch
	
* Question files (qsdf) - these files tell R how to label the charts and group the data
  *  AF - questions
  *  AF - MT
  *  AF - media
  *  HT - questions
  *  EIP - questions

* Analytics Scripts (code) - run these files to get outputs
  * PAE_Charts - creates charts with 95% confidence intervals (the report just shows bar graphs sans error bars, this is more detailed for you guys). Modify this script to add to the charts (eg: if you want to run this analysis for gender)
  * PAE_Regressions - runs regressions for each question, and it also has some formatting code that you can pull into the PAE_charts.R file if you want to expand the analysis from Race to other covariates 


* Export Files - already compiled outputs
  * PAE_Charts.html - output of the PAE_Charts script, contains all the charts I looked at in this analysis. Just use ctrl+f to find the text of the chart you want (titles are verbose)
  * All_results - a compilation of the R^2 from all the regressions
  * If desired, you could export the df dataframe at the end of the PAE_charts analysis to get a nice binary version of all the questions
