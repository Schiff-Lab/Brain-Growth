###### Code to plot normal brain GAMLSS growth curve figures #####

#rm(list=ls()) #uncomment for clearing environment
#dev.off() #uncomment for clearing plots

###### install.packages("gamlss") #library install (uncomment the first run through) ###### 

library("gamlss") #library load

#set the working directory to that of the Brain_Volume Folder
setwd("Brain_Volume")

###### Load Data #####

data<-read.csv("Supplemental_Master_File.csv", header = TRUE) #load data

### Female Data ###
nf<-data[data$Gender=="Female",]
agef<-nf$Age..days.
idf<-factor(nf$X.Subject)
tissuef<-nf$Brain.Tissue..cm.3.
heightf<-nf$Height..cm.
weightf<-nf$Weight..kg.
hcf<-nf$Head.Circumference..cm.
gmf<-nf$Grey.Matter..cm.3.
wmf<-nf$White.Matter..cm.3.
RHemf<-nf$Right.Hemisphere..cm.3.
LHemf<-nf$Left.Hemisphere..cm.3.
RTempf<-nf$Right.Temporal.Lobe..cm.3.
LTempf<-nf$Left.Temporal.Lobe..cm.3.
ROccf<-nf$Right.Occipital..cm.3.
LOccf<-nf$Left.Occipital..cm.3.
Rcerf<-nf$Right.Cerebellum..cm.3.
Lcerf<-nf$Left.Cerebellum..cm.3.
Rparf<-nf$Right.Parietal..cm.3.
Lparf<-nf$Left.Parietal..cm.3.
Rfrf<-nf$Right.Frontal..cm.3.
Lfrf<-nf$Left.Frontal..cm.3.
rhippf<-nf$Right.Hippocampus..cm.3.
lhippf<-nf$Left.Hippocampus..cm.3.
gwf<-nf$Grey.White.Matter
tcf<-nf$Tissue.CSF
bhf<-tissuef/heightf
hbf<-tissuef/weightf
csff<-nf$CSF..cm.3.
FV <- data.frame(rhippf,lhippf,agef,idf,tissuef,heightf,weightf,gmf,wmf,RHemf,LHemf,RTempf,ROccf,LOccf,Rcerf,Lcerf,Rparf,Lparf,Rfrf,Lfrf,csff,gwf,tcf,bhf,hbf)

### Male Data ###
n<-data[data$Gender=="Male",]
age<-n$Age..days.
id<-factor(n$X.Subject)
tissue<-n$Brain.Tissue..cm.3.
height<-n$Height..cm.
weight<-n$Weight..kg.
hc<-n$Head.Circumference..cm.
gm<-n$Grey.Matter..cm.3.
wm<-n$White.Matter..cm.3.
RHem<-n$Right.Hemisphere..cm.3.
LHem<-n$Left.Hemisphere..cm.3.
RTemp<-n$Right.Temporal.Lobe..cm.3.
LTemp<-n$Left.Temporal.Lobe..cm.3.
ROcc<-n$Right.Occipital..cm.3.
LOcc<-n$Left.Occipital..cm.3.
Rcer<-n$Right.Cerebellum..cm.3.
Lcer<-n$Left.Cerebellum..cm.3.
Rpar<-n$Right.Parietal..cm.3.
Lpar<-n$Left.Parietal..cm.3.
Rfr<-n$Right.Frontal..cm.3.
Lfr<-n$Left.Frontal..cm.3.
rhipp<-n$Right.Hippocampus..cm.3.
lhipp<-n$Left.Hippocampus..cm.3.
gw<-n$Grey.White.Matter
tc<-n$Tissue.CSF
bh<-tissue/height
hb<-tissue/weight
csf<-n$CSF..cm.3.
MV <- data.frame(hc,rhipp,lhipp,id,csf,age,tissue,height,weight,gm,wm,RHem,LHem,RTemp,ROcc,LOcc,Rcer,Lcer,Rpar,Lpar,Rfr,Lfr,gw,tc,bh,hb)

###### Plot Histograms ###### 
par(mfrow=c(1,2),mar = c(4, 4, 4, 4)) # all plots on one pagef 
hist(age,breaks=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570,6935,7300,7665,8030,8395),main="Age Distribution of Male Subjects",xaxt = 'n',ylab="Number of Subjects",xlab="Age (years)",col="dodgerblue3")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570,6935,7300,7665,8030,8395),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
hist(agef,breaks=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570,6935,7300,7665,8030,8395),main="Age Distribution of Female Subjects",xaxt = 'n',ylab="Number of Subjects",xlab="Age (years)",col="firebrick3")
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570,6935,7300,7665,8030,8395),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))

##### Female GAMLSS Fits #####
tissf <- gamlss(tissuef~fp(agef+random(idf),3), data=FV, family=BCPE) 
HCf <- gamlss(hcf~fp(agef+random(idf),3), data=FV, family=BCPE)
CSFf <- gamlss(csff~fp(agef+random(idf),3), data=FV, family=BCPE)
GMf <- gamlss(gmf~fp(agef+random(idf),3), data=FV, family=BCPE)
WMf <- gamlss(wmf~fp(agef+random(idf),3), data=FV, family=BCPE)
RHEMf <- gamlss(RHemf~fp(agef+random(idf),3), data=FV, family=BCPE)
RTEMPf <- gamlss(RTempf~fp(agef+random(idf),3), data=FV, family=BCPE)
LHEMf <- gamlss(LHemf~fp(agef+random(idf),3), data=FV, family=BCPE)
LTEMPf <- gamlss(LTempf~fp(agef+random(idf),3), data=FV, family=BCPE)
ROCCf <- gamlss(ROccf~fp(agef+random(idf),3), data=FV, family=BCPE)
LOCCf <- gamlss(LOccf~fp(agef+random(idf),3), data=FV, family=BCPE)
RCERf <- gamlss(Rcerf~fp(agef+random(idf),3), data=FV, family=BCPE)
LCERf <- gamlss(Lcerf~fp(agef+random(idf),3), data=FV, family=BCPE)
RPARf <- gamlss(Rparf~fp(agef+random(idf),3), data=FV, family=BCPE)
LPARf <- gamlss(Lparf~fp(agef+random(idf),3), data=FV, family=BCPE)
RFRf <- gamlss(Rfrf~fp(agef+random(idf),3), data=FV, family=BCPE)
LFRf <- gamlss(Lfrf~fp(agef+random(idf),3), data=FV, family=BCPE)
GWf <- gamlss(gwf~fp(agef+random(idf),3), data=FV, family=BCPE)
TCf <- gamlss(tcf~fp(agef+random(idf),3), data=FV, family=BCPE)
BHf <- gamlss(bhf~fp(agef+random(idf),3), data=FV, family=BCPE)
HBf<- gamlss(hbf~fp(agef+random(idf),3), data=FV, family=BCPE)
Hf<- gamlss(heightf~fp(agef+random(idf),3), data=FV, family=BCPE)
Wf<- gamlss(weightf~fp(agef+random(idf),3), data=FV,family=BCPE)
RHIPPf<- gamlss(rhippf~fp(agef+random(idf),3), data=FV, family=BCPE)
LHIPPf<- gamlss(lhippf~fp(agef+random(idf),3), data=FV, family=BCPE)

#### Male GAMLSS Fits ####
tiss <- gamlss(tissue~fp(age+random(id),3), data=MV, family=BCPE) #create model
HC <- gamlss(hc~fp(age+random(id),3), data=MV, family=BCPE) 
CSF <- gamlss(csf~fp(age+random(id),3), data=MV, family=BCPE)
GM <- gamlss(gm~fp(age+random(id),3), data=MV, family=BCPE)
WM <- gamlss(wm~fp(age+random(id),3), data=MV, family=BCPE)
RHEM <- gamlss(RHem~fp(age+random(id),3), data=MV, family=BCPE)
RTEMP <- gamlss(RTemp~fp(age+random(id),3), data=MV, family=BCPE)
LHEM <- gamlss(LHem~fp(age+random(id),3), data=MV, family=BCPE)
LTEMP <- gamlss(LTemp~fp(age+random(id),3), data=MV, family=BCPE)
ROCC <- gamlss(ROcc~fp(age+random(id),3), data=MV, family=BCPE)
LOCC<- gamlss(LOcc~fp(age+random(id),3), data=MV, family=BCPE)
RCER <- gamlss(Rcer~fp(age+random(id),3), data=MV, family=BCPE)
LCER <- gamlss(Rcer~fp(age+random(id),3), data=MV, family=BCPE)
RPAR<- gamlss(Lpar~fp(age+random(id),3), data=MV, family=BCPE)
LPAR <- gamlss(Lpar~fp(age+random(id),3), data=MV, family=BCPE)
RFR<- gamlss(Lfr~fp(age+random(id),3), data=MV, family=BCPE)
LFR <- gamlss(Lfr~fp(age+random(id),3), data=MV, family=BCPE)
GW <- gamlss(gw~fp(age+random(id),3), data=MV, family=BCPE)
TC <- gamlss(tc~fp(age+random(id),3), data=MV, family=BCPE)
BH <- gamlss(bh~fp(age+random(id),3), data=MV, family=BCPE)
HB<- gamlss(hb~fp(age+random(id),3), data=MV, family=BCPE)
H<- gamlss(height~fp(age+random(id),3), data=MV, family=BCPE)
W<- gamlss(weight~fp(age+random(id),3), data=MV, family=BCPE)
RHIPP<- gamlss(rhipp~fp(age+random(id),3), data=MV, family=BCPE)
LHIPP<- gamlss(lhipp~fp(age+random(id),3), data=MV, family=BCPE)

### #Centiles ####
cent = c(3, 15, 50, 85, 97)

##### Normalized tissue by Weight for Height ##### 
WH <- gamlss(weight~fp(height+random(id),3), data=MV, family=BCPE)
znewx <- centiles.pred(WH, xname="height",xvalues=height,yval=weight, type="z-scores" )
ps <- pnorm(znewx)
pz <- ps+0.5
m1<-min(pz)
m2<-max(pz)
brains <- tissue/pz
G <- gamlss(brains~fp(age+random(id),3), data=MV, family=BCPE)

WHf <- gamlss(weightf~fp(heightf+random(idf),3), data=FV, family=BCPE)
znewxf <- centiles.pred(WHf, xname="heightf",xvalues=heightf,yval=weightf, type="z-scores" )
psf <- pnorm(znewxf)
pzf <- psf+0.5
m1f<-min(pzf)
m2f<-max(pzf)
brainsf <- tissuef/pzf
Gf <- gamlss(brainsf~fp(agef+random(idf),3), data=FV, family=BCPE)

##### Normalized tissue by Height for Age ##### 
H <- gamlss(height~fp(age+random(id),3), data=MV, family=BCPE)
znewx <- centiles.pred(H, xname="age",xvalues=age,yval=height, type="z-scores" )
ps <- pnorm(znewx)
pz <- ps+0.5
m1<-min(pz)
m2<-max(pz)
brains <- tissue/pz
G2 <- gamlss(brains~fp(age+random(id),3), data=MV, family=BCPE)

Hf <- gamlss(heightf~fp(agef+random(idf),3), data=FV, family=BCPE)
znewxf <- centiles.pred(Hf, xname="agef",xvalues=agef,yval=heightf, type="z-scores" )
psf <- pnorm(znewxf)
pzf <- psf+0.5
m1f<-min(pzf)
m2f<-max(pzf)
brainsf <- tissuef/pzf
G2f <- gamlss(brainsf~fp(agef+random(idf),3), data=FV, family=BCPE)

#####  GAMLSS Plots ##### 

par(mfrow=c(5,2),oma = c(2, 3, 1, 1),mar = c(1,2, .8,.5)) # all plots on one page

##### Brain Tissue ##### 
centiles(tissf, agef, cent, legend=TRUE, main = "Total Brain Tissue", xlim = c(0,6570), ylim = c(200,1800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = TRUE, pch = 13, cex = 0.2, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=8)
axis(2,at=c(200,400,600,800,1000,1200,1400,1600),labels=c("200","400","600","800","1000","1200","1400","1600"),xlab="N")
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)")
par(new=TRUE)
centiles(tiss, age, cent, legend=TRUE, main = "Total Brain Tissue", xlim = c(0,6570), ylim = c(200,1800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save =FALSE, plot = TRUE, points = TRUE, pch = 13, cex = 0.2, col =  "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles =c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=8)
axis(2,at=c(200,400,600,800,1000,1200,1400,1600),labels=c("200","400","600","800","1000","1200","1400","1600"),xlab="N")
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)")

##### Head Circumference ##### 
centiles(HCf, agef, cent, legend=FALSE, main = "Head Circumference", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(30,70), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(30,50,70),labels=c("30","50","70"))
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Circumference (cm)", xlab = "Age (years)")
par(new=TRUE)
centiles(HC, age, cent,legend=FALSE,main = "Head Circumference", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(30,70), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(30,50,70),labels=c("30","50","70"))
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Circumference (cm)", xlab = "Age (years)")

##### CSF ##### 
centiles(CSFf, agef, cent, legend = FALSE, main = "Cerebrospinal Fluid",main.gsub = "@", xaxs="i",yaxs="i",xaxt='n',yaxt='n', xlim = c(0,6570),ylim = c(0,350), xaxt='n',yaxt='n',ann=FALSE, save = FALSE, plot = TRUE, points = TRUE, pch = 13, cex = 0.1, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
axis(2,at=c(0,100,200,300),labels=c("0","100","200","300"))
title(ylab = "Volume (cc)", xlab = "Age (years)")
par(new=TRUE)
centiles(CSF, age, cent, legend = FALSE,main = "Cerebrospinal Fluid",main.gsub = "@", xleg = min(age), yleg = c(0,350), xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,350), xaxt='n',yaxt='n', ann=FALSE, save = FALSE, plot = TRUE, points = TRUE, pch = 13, cex = 0.1, col = "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,100,200,300),labels=c("0","100","200","300"))
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Volume (cc)", xlab = "Age (years)")

##### Grey & White Matter ##### 
centiles(GMf, agef, cent, legend=FALSE, main = "Grey Matter", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(100,1100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(200,400,600,800),labels=c("200","400","600","800"))
par(new=TRUE)
centiles(GM, age, cent,legend=FALSE,main = "Grey Matter", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(100,1100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(200,400,600,800),labels=c("200","400","600","800"))
centiles(WMf, agef, cent,legend=FALSE, main = "White Matter", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,800), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,200,400,600,800),labels=c("0","200","400","600","800"))
par(new=TRUE)
centiles(WM, age, cent,legend=FALSE,main = "White Matter", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,800), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,200,400,600,800),labels=c("0","200","400","600","800"))

##### Grey & White Matter Ratio ##### 
centiles(TCf, agef, cent,legend=FALSE, main = "Brain/CSF Ratio", xaxs="i",yaxs="i", xlim = c(16,6570), ylim = c(2,12), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(2,4,6,8,10,12),labels=c("2","4","6","8","10","12"))
par(new=TRUE)
centiles(TC, age, cent,legend=FALSE,main = "", xaxs="i",yaxs="i", xlim = c(16,6570), ylim =c(2,12), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(2,4,6,8,10,12),labels=c("2","4","6","8","10","12"))
title(ylab = "Volume (cc)", xlab = "",ylab="",font.lab=2)
centiles(GWf, agef, cent, legend=FALSE, main = "Grey/White Matter Ratio", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(1,2.4), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(1,1.4,1.8,2.2),labels=c("1","1.4","1.8","2.2"))
par(new=TRUE)
centiles(GW, age, cent,legend=FALSE,main = "Grey/White Matter Ratio", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(1,2.4), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(1,1.4,1.8,2.2),labels=c("1","1.4","1.8","2.2"))

#####  Weight for Height Normalized Brain Volumes ##### 
centiles(G, age, cent, legend=FALSE, main = "Brain Tissue Normalized by Weight for Height",main.gsub = "@", xleg =150, yleg =1350, xlim = c(16,6570), ylim = c(0,2800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save = FALSE, plot = TRUE, points = TRUE, pch = 13, cex = 0.1, col =  "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles =c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,500,1000,1500,2000,2500),labels=c("0","500","1000","1500","2000","2500"))
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Brain Tissue/Weight for Height Age Percentile", xlab = "Age (years)")
par(new=TRUE)
centiles(Gf, agef, cent, legend=FALSE, main = "Brain Tissue Normalized by Weight for Height",main.gsub = "@", xleg =150, yleg =1350, xlim = c(16,6570), ylim = c(0,2800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save = FALSE, plot = TRUE, points = TRUE, pch = 13, cex = 0.1, col =  "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles =c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,500,1000,1500,2000,2500),labels=c("0","500","1000","1500","2000","2500"))
#axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Brain Tissue/Weight for Height Age Percentile", xlab = "Age (years)")

#####  Height for Age Normalized Brain Volumes ##### 
centiles(G2, age, cent, legend=FALSE, main = "Brain Tissue Normalized by Height for Age",main.gsub = "@", xleg =150, yleg =1350, xlim = c(16,6570), ylim = c(0,2800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save = FALSE, plot = TRUE, points = TRUE, pch = 13, cex = 0.1, col =  "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(2,2,1,2,2), lwd.centiles =c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,500,1000,1500,2000,2500),labels=c("0","500","1000","1500","2000","2500"))
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Brain Tissue/Height Age Percentile", xlab = "Age (years)")
par(new=TRUE)
centiles(G2f, agef, cent, legend=FALSE, main = "Brain Tissue Normalized by Height for Age",main.gsub = "@", xleg =150, yleg =1350, xlim = c(16,6570), ylim = c(0,2800),xaxs="i",yaxs="i",xaxt='n',yaxt='n', ann=FALSE, save = FALSE, plot = TRUE, points = TRUE, pch = 13, cex = 0.1, col =  "firebrick3", col.centiles = "firebrick3", lty.centiles =c(2,2,1,2,2), lwd.centiles =c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,500,1000,1500,2000,2500),labels=c("0","500","1000","1500","2000","2500"))
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
title(ylab = "Brain Tissue/Height Age Percentile", xlab = "Age (years)")

##### Tissue/CSF Ratio ##### 
centiles(TCf, agef, cent,legend=FALSE, main = "Brain/CSF Ratio", xaxs="i",yaxs="i",xaxt='n',yaxt='n', xlim = c(0,6570), ylim = c(2,12), xaxt='n',yaxt='n',plot = TRUE,points = TRUE,pch = 13, cex = 0.1, col = "firebrick3", col.centiles = "firebrick3", lty.centiles =c(1,1,1,1,1), lwd.centiles = c(.8,.8,2.5,.8,.8))
axis(2,at=c(2,4,6,8,10,12),labels=c("2","4","6","8","10","12"))
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
grid(nx=18,ny=NULL,col = "firebrick3",lwd=.6)
par(new=TRUE)
centiles(TC, age, cent,legend=FALSE,main = "Brain/CSF Ratio", xaxs="i",yaxs="i",xaxt='n',yaxt='n', xlim = c(0,6570), ylim =c(2,12), xaxt='n',yaxt='n',plot = TRUE,points = TRUE, pch = 13, cex = 0.1, col = "dodgerblue3", col.centiles = "dodgerblue3", lty.centiles =c(1,1,1,1,1), lwd.centiles = c(.8,.8,2.5,.8,.8))
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
grid(nx=18,ny=NULL,col = "dodgerblue3",lwd=.6)

#####  Left/Right Lobe Plots ##### 
par(mfrow=c(7,2),oma = c(2, 2, 1, 1),mar = c(.6,.5, .8,.5)) # all plots on one page

##### Hemispheres ##### 
centiles(RHEMf, agef, cent, legend=FALSE, main = "Female Hemispheres", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,750), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,200,400,600),labels=c("0","200","400","600"))
par(new=TRUE)
centiles(LHEMf, agef, cent,legend=FALSE,main = "Female Hemispheres", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,750), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,200,400,600),labels=c("0","200","400","600"))
centiles(RHEM, age, cent,legend=FALSE, main = "Male Hemispheres", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,750), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
par(new=TRUE)
centiles(LHEM, age, cent,legend=FALSE,main = "Male Hemispheres", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,750), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)

##### Frontal Lobes ##### 
centiles(RFRf, agef, cent, legend=FALSE, main = "Female Frontal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(25,250), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(50,100,150,200),labels=c("50","100","150","200"))
par(new=TRUE)
centiles(LFRf, agef, cent,legend=FALSE,main = "Female Frontal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(25,250), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(50,100,150,200),labels=c("50","100","150","200"))
centiles(RFR, age, cent,legend=FALSE, main = "Male Frontal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(25,250), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
par(new=TRUE)
centiles(LFR, age, cent,legend=FALSE,main = "Male Frontal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(25,250), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)

##### Temporal Lobes ##### 
centiles(RTEMPf, agef, cent, legend=FALSE, main = "Female Temporal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,160), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,50,100,150),labels=c("0","50","100","150"))
par(new=TRUE)
centiles(LTEMPf, agef, cent,legend=FALSE,main = "Female Temporal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,160), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,50,100,150),labels=c("0","50","100","150"))
centiles(RTEMP, age, cent,legend=FALSE, main = "Male Temporal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,160), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
par(new=TRUE)
centiles(LTEMP, age, cent,legend=FALSE,main = "Male Temporal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,160), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)

##### Parietal Lobes ##### 
centiles(RPARf, agef, cent, legend=FALSE, main = "Female Parietal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,160), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,50,100,150),labels=c("0","50","100","150"))
par(new=TRUE)
centiles(LPARf, agef, cent,legend=FALSE,main = "Female Parietal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,160), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(0,50,100,150),labels=c("0","50","100","150"))
centiles(RPAR, age, cent,legend=FALSE, main = "Male Parietal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,160), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
par(new=TRUE)
centiles(LPAR, age, cent,legend=FALSE,main = "Male Parietal Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,160), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)

##### Occipital Lobes ##### 
centiles(ROCCf, agef, cent, legend=FALSE, main = "Female Occipital Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(20,40,60,80,100),labels=c("20","40","60","80","100"))
par(new=TRUE)
centiles(LOCCf, agef, cent,legend=FALSE,main = "Female Occipital Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(20,40,60,80,100),labels=c("20","40","60","80","100"))
centiles(ROCC, age, cent,legend=FALSE, main = "Male Occipital Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
par(new=TRUE)
centiles(LOCC, age, cent,legend=FALSE,main = "Male Occipital Lobes", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)

##### Cerebellum ##### 
centiles(RCERf, agef, cent, legend=FALSE, main = "Female Cerebellum", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(20,40,60,80,100),labels=c("20","40","60","80","100"))
par(new=TRUE)
centiles(LCERf, agef, cent,legend=FALSE,main = "Female Cerebellum", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
axis(2,at=c(20,40,60,80,100),labels=c("20","40","60","80","100"))
centiles(RCER, age, cent,legend=FALSE, main = "Male Cerebellum", xaxs="i",yaxs="i", xlim = c(0,6570), ylim = c(0,100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)
par(new=TRUE)
centiles(LCER, age, cent,legend=FALSE,main = "Male Cerebellum", xaxs="i",yaxs="i", xlim = c(0,6570), ylim =c(0,100), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=NULL)

##### Hippocampus ##### 
centiles(RHIPPf, agef, cent, legend=FALSE, main = "Female Hippocampus", xaxs="i",yaxs="i", xlim = c(16,6570), ylim = c(0,4), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=4)
axis(2,at=c(0,1,2,3,4),labels=c("0","1","2","3","4"))
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
par(new=TRUE)
centiles(LHIPPf, agef, cent,legend=FALSE,main = "Female Hippocampus", xaxs="i",yaxs="i", xlim = c(16,6570), ylim =c(0,4), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=4)
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
axis(2,at=c(0,1,2,3,4),labels=c("0","1","2","3","4"))
centiles(RHIPP, age, cent,legend=FALSE, main = "Male Hippocampus", xaxs="i",yaxs="i", xlim = c(16,6570), ylim = c(0,3.5), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "blueviolet", col.centiles = "blueviolet", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=4)
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")
par(new=TRUE)
centiles(LHIPP, age, cent,legend=FALSE,main = "Male Hippocampus", xaxs="i",yaxs="i", xlim = c(16,6570), ylim =c(0,3.5), xaxt='n',yaxt='n', pch = 13, cex = 0.1, col = "darkgreen", col.centiles = "darkgreen", lty.centiles =c(2,2,1,2,2), lwd.centiles = c(.8,.8,2.5,.8,.8))
grid(nx=18,ny=4)
axis(1,at=c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,4015,4380,4745,5110,5475,5840,6205,6570),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),xlab="N")

