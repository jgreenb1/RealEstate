###### Settings
library(ggplot2)
library(dplyr)
setwd("C:/Github/RealEstate")



###### Loading data
data<-read.csv("OaklandMultiTenant.csv")

###### Stripping $ and ,
strip_dollar_comma<-function(x){
  as.numeric(gsub(",","",gsub("\\$","",as.character(x))))
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
data$TotBath<-data$Bathrooms.1+data$Bathrooms.2+data$Bathrooms.3+data$Bathrooms.4
data$TotBed<-data$Bedrooms.1+data$Bedrooms.2+data$Bedrooms.3+data$Bedrooms.4

###### NEED TO ADD THEME

###### Number of multi-tenant units sold by month
data$MONTH<-as.Date(paste0(substr(data$Closing.Date,0,7),"-01"))
data<-data[data$Closing.Date<="2017-03-31",]

month1<-data %>% group_by(MONTH) %>% summarise(LEN=length(Sold.Price))

ggplot(data=month1,aes(x=MONTH,y=LEN)) +
  geom_line(size=2) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") +
  scale_y_continuous(breaks=seq(0,100,by=20)) +
  ggtitle("Oakland MultiTenant Sales per Month") + xlab("") + ylab("") +
  expand_limits(y=0)

###### Boxplot for Monthly Sale Price
ggplot(data=data,aes(x=MONTH,y=Sold.Price,group=MONTH)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") +
  ggtitle("Oakland MultiTenant Sold Price by Month") + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(0,150000,by=25000),labels=thous) +
  coord_cartesian(ylim=c(0,150000))


