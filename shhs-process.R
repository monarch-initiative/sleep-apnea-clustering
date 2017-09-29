#main function to load data and initially process before cleaning

library(tidyverse)
library(stringr)

setwd("~/Documents/MSthesis/gitdir") #change this line to location of cloned repo

source("lib/pull.R")
source("lib/colsToLower.R")
source("lib/removeCols.R")
source("lib/subsetCols.R")
source("lib/pullTerms.R")
source("lib/pruneSHHS.R")
source("lib/lookUpFolderWithString.R")
source("lib/lookUpDispname.R")


#load data
#make sure you have all these files (or comment out files you don't have)
#these will be from NSRR

shhs1<-read_csv("datasets/shhs1-dataset-0.11.0.csv")
shhs2<-read_csv("datasets/shhs2-dataset-0.11.0.csv")
cvdevents<-read_csv("datasets/shhs-cvd-events-dataset-0.11.0.csv")
cvdsum<-read_csv("datasets/shhs-cvd-summary-dataset-0.11.0.csv")
shhs1.hrv<-read_csv("datasets/shhs1-hrv-summary-0.11.0.csv")
shhs2.hrv<-read_csv("datasets/shhs2-hrv-summary-0.11.0.csv")
interim<-read_csv("datasets/shhs-interim-followup-dataset-0.11.0.csv")

#data dictionary, make sure you have this file
datadict<-read_csv("datasets/shhs-data-dictionary-0.11.0-variables.csv")


#process

shhs1.pruned<-pruneSHHS(shhs1, datadict)
shhs2.pruned<-pruneSHHS(shhs2, datadict)
cvd.pruned<-pruneSHHS(cvdsum, datadict)
shhs1.hrv.pruned<-pruneSHHS(shhs1.hrv, datadict)
shhs2.hrv.pruned<-pruneSHHS(shhs2.hrv, datadict)
interim.pruned<-pruneSHHS(interim, datadict)








