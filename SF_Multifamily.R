###### Settings
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(lubridate)
library(tidyr)
setwd("C:/Github/RealEstate")
options(scipen=10)

thous1<-function(x) {
  x<-x/1000
  str_c(x,"K")
}

###### Reading data
data_sf<-read_excel("MultifamilyHistoricalInfo.xlsx",sheet=1,skip=1)
data_ca<-read_excel("MultifamilyHistoricalInfo.xlsx",sheet=2,skip=1)

###### Formatting 
colnames(data_sf)<-c("Quarter","InventoryBldgs","InventoryUnits","InventoryAvgSF","AskingRentPerUnit","AskingRentPerSF",
                     "AskingRentGrowth","EffectiveRentPerUnit","EffectiveRentPerSF","EffectiveRentGrowth","EffectiveRentConcessions",
                     "VacancyUnits","VacancyPercent","VancanyGrowth","OccupancyUnits","OccupancyPercent","OccupancyGrowth",
                     "NetAbsoroptionUnits","NetAbsorptionPercent","UnderConstructionBldgs","UnderConstructionUnits","UnderConstructionPercent",
                     "DeliveriesBldgs","DeliveriesUnits","DeliveriesPercent")

data_sf<-data_sf[-1,]
data_sf$YEAR<-as.numeric(substr(data_sf$Quarter,0,4))
data_sf$QUART<-substr(data_sf$Quarter,6,8)
data_sf$MMDD<-NA
data_sf$MMDD[data_sf$QUART=="Q1"]<-"01-01"
data_sf$MMDD[data_sf$QUART=="Q2"]<-"04-01"
data_sf$MMDD[data_sf$QUART=="Q3"]<-"07-01"
data_sf$MMDD[data_sf$QUART=="Q4"]<-"10-01"
data_sf$DATE<-as.Date(paste0(data_sf$YEAR,"-",data_sf$MMDD))
  
data_sf_long<-gather(as.data.frame(data_sf[,c("DATE","InventoryUnits","VacancyUnits","OccupancyUnits","UnderConstructionUnits","NetAbsoroptionUnits","DeliveriesUnits")]),condition,units,InventoryUnits,VacancyUnits,OccupancyUnits,UnderConstructionUnits,NetAbsoroptionUnits,DeliveriesUnits)
data_sf_long$units<-as.numeric(data_sf_long$units)
data_sf_long$condition<-ordered(data_sf_long$condition,levels=c("InventoryUnits","OccupancyUnits","VacancyUnits","UnderConstructionUnits","NetAbsoroptionUnits","DeliveriesUnits"))

###### Plotting
theme_plot2 <- theme(
  axis.text.x  = element_text(size=16,color='black',face='bold'),
  panel.background = element_rect(fill='white',colour='gray50'),
  panel.grid.minor = element_line('gray90'),
  panel.grid.major = element_line(colour = 'gray90'),
  axis.text.y = element_text(size=16,color='black',face='bold'),
  axis.title.x = element_text(size=16,face='bold'),
  axis.title.y = element_text(size=16,face='bold'),
  legend.title=element_blank(),
  legend.text = element_text(size=16,face='bold'),
  legend.position = "bottom",
  plot.title=element_text(size=20,face='bold'),
  strip.text.x = element_text(size=18,face='bold')
)

###### Basic EDA
ggplot(data=data_sf_long,aes(x=DATE,y=units,group=condition,colour=condition)) +
  geom_line(size=2) + theme_plot2 +
  facet_wrap(~condition) +
  ggtitle("SF Units") + xlab("") + ylab("") +
  scale_y_continuous(labels=thous1) +
  theme(legend.position="none")

ggplot(data=data_sf_long,aes(x=DATE,y=units,group=condition,colour=condition)) +
  geom_line(size=2) + theme_plot2 +
  facet_wrap(~condition,scales="free_y") +
  ggtitle("SF Units") + xlab("") + ylab("") +
  scale_y_continuous(labels=thous1) +
  theme(legend.position="none")


#### YoY Calcs
data_sf2<-data_sf %>% group_by(month=month(DATE)) %>%
  arrange(DATE) %>%
  mutate(InventoryUnits_YOY=InventoryUnits/lag(InventoryUnits,1))

data_sf2<-as.data.frame(data_sf2)
  