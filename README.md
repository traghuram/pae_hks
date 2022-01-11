# pae_hks

This file walks through the different components of the analytics for my PAE.

# Running PAE Analytics

To run this code, you just need to update the links in the different sections of `Analytics_PAE.R` and `Regressions_PAE.R` to point to the correct source files.

* **Source data (`df`)** - I have stored these files locally but can send the files along using Google Drive or some other secure link. They are referened in both R scripts.
  *  Daniel Y's R functions file
  *  EIP export from Crunch
  *  HT (levels for sharing)
  *  AF export from Crunch
	
* **Question files (`qsdf`)** - these files tell R how to label the charts and group the data. They are referenced in `Analytics_PAE.R`.
  *  AF - questions
  *  AF - MT
  *  AF - media
  *  HT - questions
  *  EIP - questions

* **Analytics Scripts (code)** - run these files to get outputs
  * `Analytics_PAE.R` - creates charts with 95% confidence intervals (the report just shows bar graphs sans error bars, this is more detailed for you guys). Modify this script to add to the charts (eg: if you want to run this analysis for gender)
  * `Regressions_PAE.R` - runs regressions for each question, and it also has some formatting code that you can pull into the Analytics_PAE.R file if you want to expand the analysis from Race to other covariates


* **Export Files** - already compiled outputs
  * `Analytics_PAE.html` - output of the Analytics_PAE script, contains all the charts I looked at in this analysis, though most were not included in the final report. Just use ctrl+f to find the text of the chart you want (titles are verbose). This includes ~80% of all three surveys (AF, EIP, HT).
  * `All_regressions.csv` - this file is a compilation of the R^2 from all the linear regressions and the accuracy from the logistic regressions (note the latter are tentative and not fully debugged).
  * If desired, you could export the `df` dataframe at the end of the `Analytics_PAE.html` analysis to get a nice version of all the questions collapsed as binary responses.
