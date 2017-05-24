###### Settings
library(ggplot2)
library(dplyr)
library(stringr)
setwd("C:/Github/RealEstate")
options(scipen=10)

thous<-function(x) {
  x<-x/1000
  str_c("$",x,"K")
}

mills<-function(x) {
  x<-x/1000000
  str_c("$",x,"M")
}

percent<-function(x) {
  x<-x*100
  str_c(x,"%")
}

today<-gsub("-","",as.Date(Sys.time()))

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
data$Days.On.Market<-as.numeric(as.character(data$Days.On.Market))

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

###### Theme
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

###### Number of multi-tenant units sold by month
data$MONTH<-as.Date(paste0(substr(data$Closing.Date,0,7),"-01"))
data<-data[data$Closing.Date<="2017-03-31",]

month1<-data %>% group_by(MONTH) %>% summarise(LEN=length(Sold.Price))

filename1<-paste0("OaklandSalesCount_",today,".jpeg")
jpeg(filename=filename1,width=1200,height=800,quality=100)
ggplot(data=month1,aes(x=MONTH,y=LEN)) +
  geom_line(size=2) + theme_plot2 +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") +
  scale_y_continuous(breaks=seq(0,100,by=20)) +
  ggtitle("Oakland MultiTenant Sales per Month") + xlab("") + ylab("") +
  expand_limits(y=0) 
dev.off()

###### Boxplot for Monthly Sale Price
filename2<-paste0("OaklandSalePriceBoxplot_",today,".jpeg")
jpeg(filename=filename2,width=1200,height=800,quality=100)
ggplot(data=data,aes(x=MONTH,y=Sold.Price,group=MONTH)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) + theme_plot2 +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") +
  ggtitle("Oakland MultiTenant Sold Price by Month") + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(0,1500000,by=250000),labels=mills) +
  coord_cartesian(ylim=c(0,1250000)) +
  stat_summary(fun.y=mean,geom="line",size=2,aes(group=1,col="red")) +
  theme(legend.position="none")
dev.off()

## Sale Price by # of Units
filename3<-paste0("OaklandSalePriceBoxplot_ByUnits_",today,".jpeg")
jpeg(filename=filename3,width=1800,height=1200,quality=100)
ggplot(data=data,aes(x=MONTH,y=Sold.Price,group=MONTH)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) + theme_plot2 +
  facet_wrap(~Units) +
  scale_x_date(date_breaks="24 months",date_labels="%b\n%Y") +
  ggtitle("Oakland MultiTenant Sold Price by Month") + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(0,1500000,by=250000),labels=mills) +
  coord_cartesian(ylim=c(0,1250000)) +
  stat_summary(fun.y=mean,geom="line",size=2,aes(group=1,col="red")) +
  theme(legend.position="none")
dev.off()

###### Does month of year matter?
data$month_only<-format(data$MONTH,"%b")
data$month_only<-factor(data$month_only,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
data$year_only<-format(data$MONTH,"%Y")

month<-data %>% group_by(month_only,year_only) %>% summarise(LEN=length(Sold.Price),MED=median(Sold.Price))

filename4<-paste0("OaklandMonthSalesBoxplot_",today,".jpeg")
jpeg(filename=filename4,width=1200,height=800,quality=100)
ggplot(data=month,aes(x=month_only,y=LEN,group=month_only)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) + theme_plot2 +
  ggtitle("Oakland MultiTenant Sales by Month of Year") + xlab("") + ylab("") +
  expand_limits(y=c(0)) 
dev.off()

filename5<-paste0("OaklandMonthMedianPriceBoxplot_",today,".jpeg")
jpeg(filename=filename5,width=1200,height=800,quality=100)
ggplot(data=month,aes(x=month_only,y=MED,group=month_only)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) + theme_plot2 +
  scale_y_continuous(breaks=seq(0,1500000,by=100000),labels=mills) +
  ggtitle("Oakland MultiTenant Median Sale Price by Month of Year") + xlab("") + ylab("") +
  expand_limits(y=c(0)) 
dev.off()

###### Check LP vs. Sales Price by month and year and see if it's varied in amount or percentage terms
data$DIFF<-data$Sold.Price-data$LP
data$DIFF_PERC<-data$DIFF/data$LP

filename6<-paste0("OaklandSalesListDiffBoxplot_",today,".jpeg")
jpeg(filename=filename6,width=1200,height=800,quality=100)
ggplot(data=data,aes(x=MONTH,y=DIFF,group=MONTH)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) + theme_plot2 +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") +
  ggtitle("Oakland MultiTenant Difference between Sales Price and List Price by Month") + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(-100000,150000,by=25000),labels=thous) +
  coord_cartesian(ylim=c(-100000,150000)) +
  stat_summary(fun.y=mean,geom="line",size=2,aes(group=1,col="red")) +
  theme(legend.position="none")
dev.off()

filename7<-paste0("OaklandSalesListDiffPercBoxplot_",today,".jpeg")
jpeg(filename=filename7,width=1200,height=800,quality=100)
ggplot(data=data,aes(x=MONTH,y=DIFF_PERC,group=MONTH)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) + theme_plot2 +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") +
  ggtitle("Oakland MultiTenant Percent Difference between Sales Price and List Price by Month") + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(-0.25,0.3,by=0.05),labels=percent) +
  coord_cartesian(ylim=c(-0.25,0.3)) +
  stat_summary(fun.y=mean,geom="line",size=2,aes(group=1,col="red")) +
  theme(legend.position="none")
dev.off()

###### What has been happening with days on market?
filename8<-paste0("OaklandDaysOnMarketBoxplot_",today,".jpeg")
jpeg(filename=filename8,width=1200,height=800,quality=100)
ggplot(data=data,aes(x=MONTH,y=Days.On.Market,group=MONTH)) +
  geom_boxplot(fill="lightblue",outlier.color=NA) + theme_plot2 +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") +
  ggtitle("Oakland MultiTenant Days on Market by Month") + xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(0,100,by=10)) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.y=median,geom="line",size=2,aes(group=1,col="red")) +
  theme(legend.position="none")
dev.off()


