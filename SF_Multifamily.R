###### Settings
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
setwd("C:/Github/RealEstate")
options(scipen=10)

###### Reading data
data_sf<-read_excel("MultifamilyHistoricalInfo.xlsx",sheet=1,skip=1)
data_ca<-read_excel("MultifamilyHistoricalInfo.xlsx",sheet=2,skip=1)
