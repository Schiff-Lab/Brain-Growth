# Plotting Normal Brain Volume Growth

This R code facilitates the production of the SSANOVA and GAMLSS Brain Volume growth curves described in the paper “Normal childhood brain growth and a universal sex and anthropomorphic relationship to cerebrospinal fluid” [(Journal of Neurosurgery Pediatrics article link)](https://thejns.org/pediatrics/view/journals/j-neurosurg-pediatr/28/4/article-p458.xml).

This code was compiled and tested with R version 3.6.1 on Mac OS. You must have R installed on your computer, and we recommend using RSTUDIO to help with running such code. You can download the entire code package from this GitHub repository by downloading as a single zip file. 

The “gamlss”, “gss”, and “mosaic” libraries will need to be installed in R as  
  * `install.packages("gamlss")`
  * `install.packages("gss")`
  * `install.packages("mosaic")`

If you leave the download in a 'Downloads" directory, then you can set the working directory as
  * `setwd("Downloads/Brain-Volume-main/Brain_Volume")`

Within the Brain_Volume folder, there are 3 scripts that can be run.
  * `SSANOVA_Plots.R`: uses fitted models (saved as R objects in the Brain_Volume folder) to plot Smoothing Spline ANOVA models for normal brain volume data (with years on the x axis and volume in cubic centimeters or a ratio on the y-axis).
  * `GAMLSS_plots.R`: uses fitted models (saved as R objects in the Brain_Volume folder) to plot Generalized Additive Models for Location, Scale, and Shape for                                 normal brain volume data (with years on the x axis and volume in cubic centimeters or a ratio on the y-axis).
  * `New_Data_Plots_Zscores.R`: uses fitted models (saved as R objects in the Brain_Volume folder) to determine z-scores for new data and plot the new data on the normal brain volume growth curves (new data must be input as .csv file or directly into the script, with age in days and brain tissue volume in cubic centimeters). Z-score outputs can be saved as .csv file within the Brain_Volume folder. 
  
The `Supplemental_Master_File.csv` document contains the normal raw data used to create these models and plots.

If you wish to use the shiny app modality for plotting volumes and producing z-scores, the shinyappBV folder will need to be downloaded and opened. After installing the libraries below in Rstudio, the app.R file can be run to produce the graphical user interface for the application.

The “gamlss”, “shiny”, “shinythemes”, and “DT” libraries will need to be installed in R as  
  * `install.packages("gamlss")`
  * `install.packages("shiny")`
  * `install.packages("shinythemes")`
  * `install.packages("DT")`
