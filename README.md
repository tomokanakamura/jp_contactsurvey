# jp_contactsurvey
This repository includes R scripts and processed datasets to analyze the social contact surveys conducted in Osaka and Fukuoka prefectures, Japan from 2021 to 2023 for the manuscript titled, "Continuing to be Cautious: Japanese Contact Patterns during the COVID-19 Pandemic and their Association with Public Health Recommendations" that is currently  under peer-review.

All datasets required to repoduce the figures in the manuscript are saved in the CSV and RDS folders. 

- Each R script corresponds to the figure shown in the manuscript. 

- The R script labeled as “Fig2_Table3_Multivariable_Regression_Model_clean” provides the code necessary to run the multivariable regression model and the output shown on Table 3. To check the model fit, this provides the code necessary to run the residual plots also shown on Supp. Figure 5. This R script also provides the code for Figure 2 that shows the distribution of contacts reported per individual during the weekday in February 2023. 

- When running the R scripts necessary for Figure 1, please use the CSV files included in the folder named “CSV.” 

- When running the R scripts necessary for all other figures, please use the RDS files included in the folder named “RDS.” It is important to import these datasets in this format so all of the class of the variables will be retained especially when running the script for the multivariable regression model. 

For any other questions regarding the R scripts, please contact Tomoka Nakamura: Tomoka.Nakamura@lshtm.ac.uk 


