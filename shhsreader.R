library(tidyverse)

setwd("~/Documents/MSthesis/datasets")
shhs2<-read_csv("shhs2-dataset-0.11.0.csv")
datadict<-read_csv("shhs-data-dictionary-0.11.0-variables.csv")

Admin<-datadict %>%
  filter(folder=="Administrative")

shhs2 %>%
  select(sh322a,sh323a)

ids<-Admin %>%
  select(id)
shhs2 %>%
  select(shhs2_hi, shhs2_ms)