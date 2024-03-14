# Automated Mixed ANOVA Analysis in R for Longitudinal  Data 

This R script was made to offer a more efficient, comprehensive, and fast form of longitudinal data analysis for clinical researchers considering ANOVA statistical analysis. Traditional methods, while workable, open the door to transcription errors, inconsistency, and manual effort in inputting data. This program is made with the goal of having a quick, interactive process for intaking any raw data and outputting assumptions checking, ANOVA testing, post-hoc testing, and visualization. 

As of now, this script is still under development, but can easily be customized to other ANOVA variations and longitudinal tests utilizing the existing automated infrastructure. Currently, the script has been developed in accordance with the needs of the Mirzadeh lab at Barrow Neurological Institute. Below, I have detailed requirements, inputs, and the outputs and capabilities of this script. 

Note: For a demonstration, please utilize the "example.csv" file, place it in the working directory in your R program, and execute the program "initial_complete_mixedaov.R". The nature of the script is a compilation of smaller functions handling various aspects of the statistical process. You may create and run the entire function "anova_general" in the script, or execute each smaller aspect of the complete summary function to evaluate at each step of the process. 

## Requirements/Inputs:

This script requires some certain conditions for running data:
- Tidy data format (computer friendly formatting wherein each index is a unique observation or combination of values). Please see the "example.csv" file format for an idea of what tidy data looks like
- Standardized entry of variable measures
And requires input of:
-   File name
-   Variable of interest
-   Group variable
-   Time variable

## Capabilities/Outputs:

Through an interactive method, the program offers users choices for how to proceed throughout the script. 
The capabilities of the script include:
- Full Assumptions Checking for the mixed ANOVA test
- ANOVA results table
- Post Hoc test table to view detailed detection of signficance
- Publication-ready visualization of results with statistical annotation

PRINT FIGURE

Due to the flexible yet automated nature of the program, please find my contact information below and I would be happy to answer questions or work with you to integrate a program like this or slightly different in your workflow.

Please acknowledge usage of this program or collaboration in any publications

Siddhant Saxena <br>
Mirzadeh Lab at Barrow Neurological Institute <br>
siddhantsaxena@hotmail.com <br>
March 14, 2024
