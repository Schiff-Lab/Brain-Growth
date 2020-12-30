# Plotting Normal Brain Volume Growth

This code facilitates the production of the SSANOVA and GAMLSS Brain Volume growth curves described in the paper “Normal Human Brain Growth” [medrxiv link](https://www.medrxiv.org/content/10.1101/2020.05.19.20102319v1.full?versioned=true).

This code was compiled and tested with R version 3.6.1 on Mac OS.
The “gamlss”, “gss”, and “mosaic” libraries will need to be installed.  For each script, the working directory will need to be set to the Brain_Volume folder.

Upon downloading the Brain Volume folder, set the working directory 

# Smoothing Splines ANOVA Plots:

Open the SSANOVA_Plots.R file, change the setwd() command to include the directory of the downloaded Brain_Volume folder, and run through the script to plot the SSANOVA figures.  The x axis shows age in years, and the y-axis shows volumes in cubic centimeters or the ratio.

The fits and the M data frame are saved in the R object, and were originally obtained using the following code segment:


# Generalized Additive Models for Location, Scale, and Shape Plots

Open the GAMLSS_plots.R file, change the setwd() command to include the directory of the downloaded Brain_Volume folder, and run through the script to plot the GAMLSS figures.  The x axis consists of the age in years, and the y-axis shows volumes in cubic centimeters or the ratio.

# Use the New_Data_Plots_Zscores.R file to obtain z-scores for new data and plot on the normal curves.

Open the New_Data_Plots_Zscores.R file, change the setwd() command to include the directory of the downloaded Brain_Volume folder, and add new data using either a csv file or direct input.  The tissue data should be in cubic centimeters and the age data should be in days.  Z-scores can be output and saved in a csv file, and the new data can be plotted on the normal brain volume growth curves.


