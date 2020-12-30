# Plotting Normal Brain Volume Growth

This R code facilitates the production of the SSANOVA and GAMLSS Brain Volume growth curves described in the paper “Normal Human Brain Growth” [(medrxiv link)](https://www.medrxiv.org/content/10.1101/2020.05.19.20102319v1.full?versioned=true).

This code was compiled and tested with R version 3.6.1 on Mac OS. You must have R installed on your computer, and we recommend using RSTUDIO to help with running such code. You can download the entire code package from this GitHub repository by downloading as a single zip file. 

The “gamlss”, “gss”, and “mosaic” libraries will need to be installed in R as  
  * `install.packages("gamlss")`
  * `install.packages("gss")`
  * `install.packages("mosaic")`

If you leave the download in a 'Downloads" directory, then you can set the workking directory as
  * `setwd("Downloads/Brain-Volume-main/Brain_Volume")`

Within the Brain_Volume folder, there are 3 scripts that can be run.
  * `SSANOVA_Plots.R`: uses fitted models (saved as R objects in the Brain_Volume folder) to plot Smoothing Spline ANOVA models for normal brain volume data (with years on the x axis and volume in cubic centimeters or a ratio on the y-axis).
  * `GAMLSS_plots.R`: uses fitted models (saved as R objects in the Brain_Volume folder) to plot Generalized Additive Models for Location, Scale, and Shape for                                 normal brain volume data (with years on the x axis and volume in cubic centimeters or a ratio on the y-axis).
  * `New_Data_Plots_Zscores.R`: uses fitted models (saved as R objects in the Brain_Volume folder) to determine z-scores for new data and plot the new data on the normal brain volume growth curves (new data must be input as .csv file or directly into the script, with age in days and brain tissue volume in cubic centimeters). Z-score outputs can be saved as .csv file within the Brain_Volume folder. 
  
The `Supplemental_Master_File.xlsm` document contains the normal raw data used to create these models and plots.
