library(tidyverse)
library(stringr)

setwd("~/Documents/MSthesis/gitdir")
source("lib/pull.R")
source("lib/colsToLower.R")
source("lib/removeCols.R")
source("lib/subsetCols.R")
source("lib/pullTerms.R")
source("lib/pruneSHHS.R")
source("lib/lookUpFolderWithString.R")
source("lib/lookUpDispname.R")


#load data

shhs1<-read_csv("datasets/shhs1-dataset-0.11.0.csv")
shhs2<-read_csv("datasets/shhs2-dataset-0.11.0.csv")

datadict<-read_csv("datasets/shhs-data-dictionary-0.11.0-variables.csv")

#process

shhs1.pruned<-pruneSHHS(shhs1, datadict)
shhs2.pruned<-pruneSHHS(shhs2, datadict)








