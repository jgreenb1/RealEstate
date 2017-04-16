###### Settings
library(ggplot2)
setwd("C:/Github/RealEstate")

###### Loading data
data<-read.csv("OaklandMultiTenant.csv")

###### Stripping $ and ,
strip_dollar_comma<-function(x){
  gsub(",","",gsub("\\$","",as.character(x)))
}

###### Formatting data
data$Closing.Date<-as.Date(as.character(data$Closing.Date),format="%m/%d/%Y")
data$Sold.Price<-strip_dollar_comma(data$Sold.Price)
data$LP<-strip_dollar_comma(data$LP)

## Total Beds/Baths
data$Bathrooms.1<-as.numeric(as.character(data$Bathrooms.1))
data$Bathrooms.2<-as.numeric(as.character(data$Bathrooms.2))
data$Bathrooms.3<-as.numeric(as.character(data$Bathrooms.3))
data$Bathrooms.4<-as.numeric(as.character(data$Bathrooms.4))
data$Bedrooms.1<-as.numeric(as.character(data$Bedrooms.1))
data$Bedrooms.2<-as.numeric(as.character(data$Bedrooms.2))
data$Bedrooms.3<-as.numeric(as.character(data$Bedrooms.3))
data$Bedrooms.4<-as.numeric(as.character(data$Bedrooms.4))
data$Bathrooms.1[is.na(data$Bathrooms.1)]<-0
data$Bathrooms.2[is.na(data$Bathrooms.2)]<-0
data$Bathrooms.3[is.na(data$Bathrooms.3)]<-0
data$Bathrooms.4[is.na(data$Bathrooms.4)]<-0
data$Bedrooms.1[is.na(data$Bedrooms.1)]<-0
data$Bedrooms.2[is.na(data$Bedrooms.2)]<-0
data$Bedrooms.3[is.na(data$Bedrooms.3)]<-0
data$Bedrooms.4[is.na(data$Bedrooms.4)]<-0
