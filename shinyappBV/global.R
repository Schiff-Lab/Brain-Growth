
library(shiny)
library(gamlss) 
library(DT)
library(shinythemes)

load("./data/Female_Brain_Tissue.Rdata") #female gamlss model 
load("./data/Male_Brain_Tissue.Rdata")  #male gamlss model 
load("./data/Master_Data_Female.Rdata") #female normal data 
load("./data/Master_Data_Male.Rdata")  #male normal data 
load("./data/csfF.Rdata")  #male normal data 
load("./data/csfM.Rdata")  #male normal data 
load("./data/ratioF.Rdata")  #male normal data 
load("./data/ratioM.Rdata")  #male normal data


cent<-c(3, 15, 50, 85, 97)
idff<-factor(c(1,2))
idmm<-factor(c(1,2))
