###### Code to plot brain volume and determine z-scores using normal brain volume GAMLSS growth curves #####

#rm(list=ls()) #uncomment for clearing environment
#dev.off() #uncomment for clearing plots

#install.packages("gamlss") #library install (uncomment the first run through)

library("gamlss") #library load


##### Load Data #####

load("Female_Brain_Tissue.Rdata") #female gamlss model 
load("Male_Brain_Tissue.Rdata")  #male gamlss model 
load("Master_Data_Female.Rdata") #female normal data 
load("Master_Data_Male.Rdata")  #male normal data 


### #Centiles ####

cent = c(3, 15, 50, 85, 97)


###### Add new female data below (uncomment below and change to new csv file) ######

#nf<-read.csv("~/New_volumes_females.csv", header = TRUE) #load new female data from csv sheet (specify path)
#agef<-nf$Age..days #load new female ages (days) using csv header
#tissuef<-nf$Tissue..cm.3 #load new female brain volumes (cc) using csv header
#idff<-factor(nf$IDD) #load new female id's using csv header


##### Another option is to just input the values below (replace example data below) #####

agef<-c(700,450) # in days
tissuef<-c(1350,950) # in cubic centimeters
idff<-factor(c(1,2)) 

agem<-c(170,220) # in days
tissuem<-c(1100,840) # in cubic centimeters
idmm<-factor(c(1,2))


##### Plot Brain Tissue #####

centiles(tissFF, ageff, cent, legend=TRUE, main = "Female Total Brain Tissue", xlim = c(0,6570), ylim = c(200,1800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
points(agef,tissuef)
grid(nx=18,ny=8)
axis(2,at=c(200,400,600,800,1000,1200,1400,1600),labels=c("200","400","600","800","1000","1200","1400","1600"),ylab="N")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)")

centiles(tissMM, agemm, cent, legend=TRUE, main = "Male Total Brain Tissue", xlim = c(0,6570), ylim = c(200,1800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = FALSE, pch = 13, cex = 0.2, col =  "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles =c(.8,.8,2.5,.8,.8))
points(agem,tissuem)
grid(nx=18,ny=8)
axis(2,at=c(200,400,600,800,1000,1200,1400,1600),labels=c("200","400","600","800","1000","1200","1400","1600"),ylab="N")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)")


###### Calculate brain tissue z-scores #####

female_zscores<-centiles.pred(tissFF, type = c("z-scores"),xname = "ageff", xvalues = agef, yval = tissuef)
female_zscores #output z-scores
male_zscores<-centiles.pred(tissMM, type = c("z-scores"),xname = "agemm", xvalues = agem, yval = tissuem)
male_zscores #output z-scores


##### Uncomment below to save csv files with z-scores in working directory #####

#write.csv(female_zscores, "fzscores.csv") #uncomment to save female z-scores to csv
#write.csv(male_zscores, "mzscores") #uncomment to save male z-scores to csv





