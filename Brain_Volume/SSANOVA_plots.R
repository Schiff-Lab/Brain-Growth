###### Code to plot normal brain SSANOVA figures #####

#rm(list=ls()) #uncomment for clearing environment
#dev.off() #uncomment for clearing plots

# install.packages("gamlss") #library install (uncomment the first run through) 
# install.packages("gss") #library install (uncomment the first run through) 
# install.packages("mosaic") #library install (uncomment the first run through) 

library(gss)
library(ggplot2)
library(mosaic)

###### set the working directory to that of the Brain_Volume Folder ###### 
setwd("Brain_Volume")

###### Male/Female SSANOVA Plots ###### 

par(mfrow=c(4,2),oma = c(2, 2, 1, 1),mar = c(1, 1, 1, 1.5))# mai = c(.2, .2, .2, .2) # all plots on one pagef 

###### Brain Volume ###### 
load("Raw_Vol.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "dodgerblue3",ylab="",xlab="", lty=1,xaxs="i",xaxt='n',cex = .01,ylim=c(400,1400))
lines(FemF,col = "firebrick3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1400))
lines(MalF,col = "dodgerblue3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1400))
lines(r,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1400))
lines(qq,col = "dodgerblue3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1400))
lines(rr,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1400))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(400,600,800,1000,1200,1400),labels=c("400","600","800","1000","1200","1400"),xlab="N")
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Brain Volume")
grid(nx=18,ny=NULL)

###### CSF ###### 
load("CSF_Vol.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "dodgerblue3",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,230))
lines(FemF,col = "firebrick3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,230))
lines(MalF,col = "dodgerblue3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,230))
lines(r,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,230))
lines(qq,col = "dodgerblue3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,230))
lines(rr,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,230))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(100,120,140,160,180,200,220),labels=c("100","120","140","160","180","200","220"),xlab="N")
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volue (cc)", xlab = "Age (years)", main="CSF Volume")
grid(nx=18,ny=NULL)

###### Grey Matter ###### 
load("GM_Vol.Rdata")
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "dodgerblue3",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(FemF,col = "firebrick3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(MalF,col = "dodgerblue3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(r,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(qq,col = "dodgerblue3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(rr,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(100,200,300,400,500,600,700,800),labels=c("100","200","300","400","500","600","700","800"),xlab="N")
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Grey Matter Volume")
grid(nx=18,ny=NULL)

###### White Matter ###### 
load("WM_Vol.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "dodgerblue3",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(FemF,col = "firebrick3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(MalF,col = "dodgerblue3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(r,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(qq,col = "dodgerblue3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
lines(rr,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(100,850))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(100,200,300,400,500,600,700,800),labels=c("100","200","300","400","500","600","700","800"),xlab="N")
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="White Matter Volume")
grid(nx=18,ny=NULL)

###### Grey/White Ratio ###### 
load("G_W_Ratio.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "dodgerblue3",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(1.00,2.30))
lines(FemF,col = "firebrick3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(1.00,2.30))
lines(MalF,col = "dodgerblue3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(1.00,2.30))
lines(r,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(1.00,2.30))
lines(qq,col = "dodgerblue3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(1.00,2.30))
lines(rr,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(1.00,2.30))
l<-which(rr>qq)
v<-min(l):max(l)
b<-qq[c(min(l):max(l))]
m<-rr[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(1.00,1.20,1.40,1.60,1.80,2.00,2.20),labels=c("1.00","1.20","1.40","1.60","1.80","2.00","2.20"),xlab="N")
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Ratio", xlab = "Age (years)", main="Grey/White Matter Ratio")
grid(nx=18,ny=NULL)

###### Tissue/CSF Ratio ###### 
load("TC_Ratio.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "dodgerblue3",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(4,8))
lines(FemF,col = "firebrick3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(4,8))
lines(MalF,col = "dodgerblue3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(4,8))
lines(r,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(4,8))
lines(qq,col = "dodgerblue3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(4,8))
lines(rr,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(4,8))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(4,5,6,7,8),labels=c("4","5","6","7","8"),xlab="N")
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Ratio", xlab = "Age (years)", main="Brain/CSF Ratio")
grid(nx=18,ny=NULL)

###### Weight for Height Normalized Tissue ###### 
load("W4H_Vol.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "dodgerblue3",ylab="",xlab="", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(FemF,col = "firebrick3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(MalF,col = "dodgerblue3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(r,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(qq,col = "dodgerblue3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(rr,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(400,600,800,1000,1200,1400,1600),labels=c("400","600","800","1000","1200","1400","1600"),xlab="N")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Normalized Volume", xlab = "Age (years)", main="Weight for Height Normalized")
grid(nx=18,ny=NULL)

###### Height for Age Normalized Tissue ###### 
load("Height_Vol.Rdata")
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "dodgerblue3",ylab="",xlab="", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(FemF,col = "firebrick3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(MalF,col = "dodgerblue3", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(r,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(qq,col = "dodgerblue3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
lines(rr,col = "firebrick3", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(400,1650))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(400,600,800,1000,1200,1400,1600),labels=c("400","600","800","1000","1200","1400","1600"),xlab="N")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Normalized Volume", xlab = "Age (years)", main="Height for Age Normalized")
grid(nx=18,ny=NULL)


###### Hemispheric SSANOVA Plots ###### 

par(mfrow=c(3,2),oma = c(2, 2, 1, 1),mar = c(1, 1, 1, 1.5))# mai = c(.2, .2, .2, .2) # all plots on one pagef 


###### Female Hemispheres ###### 
load("F_Hemi.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(200,300,400,500,600),labels=c("200","300","400","500","600"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Female Hemispheres")
grid(nx=18,ny=NULL)


###### Male Hemispheres ###### 
load("M_Hemi.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(125,600))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(200,300,400,500,600),labels=c("200","300","400","500","600"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Male Hemispheres")
grid(nx=18,ny=NULL)


###### Female Temporal Lobes ###### 
load("F_Temp.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(50,100,150),labels=c("50","100","150"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Female Temporal Lobes")
grid(nx=18,ny=NULL)


###### Male Temporal Lobes ###### 
load("M_Temp.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,150))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(50,100,150),labels=c("50","100","150"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Male Temporal Lobes")
grid(nx=18,ny=NULL)


######Female Hippocampus ###### 
load("F_Hipp.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
l<-which(rr>qq)
v<-min(l):max(l)
b<-qq[c(min(l):max(l))]
m<-rr[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(1,2,3),labels=c("1","2","3"),xlab="N")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Female Hippocampus")
grid(nx=18,ny=NULL)

###### Male Hippocampus ###### 
load("M_Hipp.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(0.5,3))
l<-which(rr>qq)
v<-min(l):max(l)
b<-qq[c(min(l):max(l))]
m<-rr[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(1,2,3),labels=c("1","2","3"),xlab="N")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Male Hippocampus")
grid(nx=18,ny=NULL)

############################################################
par(mfrow=c(4,2),oma = c(2, 2, 1, 1),mar = c(1, 1, 1, 1.5))


####### Female Frontal Lobes ###### 
load("F_Front.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
axis(2,at=c(50,100,150,200),labels=c("50","100","150","200"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Female Frontal Lobes")
grid(nx=18,ny=NULL)

####### Male Frontal Lobes ###### 
load("M_Front.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(25,225))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
axis(2,at=c(50,100,150,200),labels=c("50","100","150","200"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Male Frontal Lobes")
grid(nx=18,ny=NULL)

######Female Parietal Lobes ###### 
load("F_Par.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140),xlim=c(0,6570))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
axis(2,at=c(20,60,100,140),labels=c("20","60","100","140"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Female Parietal Lobes")
grid(nx=18,ny=NULL)

###### Male Parietal Lobes ###### 
load("M_Par.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140),xlim=c(0,6570))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(20,140))
axis(2,at=c(20,60,100,140),labels=c("20","60","100","140"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Male Parietal Lobes")
grid(nx=18,ny=NULL)

###### Female Occipital Lobes ###### 
load("F_Occ.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
axis(2,at=c(20,40,60,80),labels=c("20","40","60","80"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Female Occipital Lobes")
grid(nx=18,ny=NULL)

###### Male Occipital Lobes ###### 
load("M_Occ.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(15,80))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
polygon(c(v, rev(v)), c(b, rev(m)), col = "yellow", border = NA) 
axis(2,at=c(20,40,60,80),labels=c("20","40","60","80"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Male Occipital Lobes")
grid(nx=18,ny=NULL)

###### Female Cerebellum ###### 
load("F_Cer.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
axis(2,at=c(10,20,40,60,80),labels=c("10","20","40","60","80"),xlab="N")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Female Cerebellum")
grid(nx=18,ny=NULL)

###### Male Cerebellum ###### 
load("M_Cer.Rdata")
time<-0:6570
FemF<-M$Fitz[c(0:6571)]
MalF<-M$Fitz[c(6572:13142)]
FemS<-M$SEz[c(0:6571)]
MalS<-M$SEz[c(6572:13142)]
q<-(MalF-(1.96*MalS))
r<-(FemF+(1.96*FemS))
qq<-(MalF+(1.96*MalS))
rr<-(FemF-(1.96*FemS))
plot(q,col = "darkgreen",ylab="",xlab="",xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(FemF,col = "blueviolet", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(MalF,col = "darkgreen", lty=2,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(r,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(qq,col = "darkgreen", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
lines(rr,col = "blueviolet", lty=1,xaxs="i",yaxs="i",xaxt='n',yaxt='n',cex = .01,ylim=c(5,80))
l<-which(q>r)
v<-min(l):max(l)
b<-q[c(min(l):max(l))]
m<-r[c(min(l):max(l))]
axis(2,at=c(10,20,40,60,80),labels=c("10","20","40","60","80"),xlab="N")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)", main="Male Cerebellum")
grid(nx=18,ny=NULL)
