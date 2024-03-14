# ElectionData
My final project in Graduate Data Mining class - STAT8480 - Predicts U.S. presidential voting behavior from demographic characteristics.

How to run:

Most of the scripts can be run in isolation.  I will briefly describe what each group does:

These files handle basic data cleaning and aggregation details for the election data:

01-Prep_Election_Data.R
02-Aggregate_Election_Data.R

These files handle linking census data to election data:

03-Census_Data_County.R
04-Census_Data_District.R

These scripts will not work unless you obtain an API key and enter it into both scripts. See link below:

https://api.census.gov/data/key_signup.html

These files provide some plots and information about the structure and distribution of the input variables:

05a-District_Analysis.R
05b-County_Analysis.R

These files handle all of the decision tree modeling and analysis:

06a-Dtree-District.R
06b-Dtree-County_Unbalanced.R
06c-Dtree-County_Balanced.R
06d-Dtree-County_Alt_Thresh.R
06e-Dtree-Summary.R

These files handle all of the logistic regression modeling and analysis:

07a-Reg-District.R
07b-Reg-County_Unbalanced.R
07c-Reg-County_Balanced.R
07d-Reg-County_Alt_Thresh.R
07e-Reg-Summary.R

These files handle all of the random forest modeling and analysis:

08a-RF-District.R
08b-RF-County_Unbalanced.R
08c-RF-County_Ext_Balanced.R
08d-RF-County_Int_Balanced.R
08e-RF-Summary.R
 
These files handle all of the ANN modeling and analysis:

09a-ANN-District.R
09b-ANN-County_Unbalanced.R
09c-ANN-County_Balanced.R
09d-ANN-Summary.R

These files compare the stability of the decision tree and random forest models:

10a-DTree-Analysis.R
10b-RF-Analysis.R

This file produces a plot of winning margin vs. accuracy:

11-Winner Analysis.R


Note:  If you obtain a census API key, you could run every script from beginning to end.  If you don’t want to obtain an API key, there are intermediate RData files included that should allow you to run from scripts 
“05a-District_Analysis.R” until the end in order.

