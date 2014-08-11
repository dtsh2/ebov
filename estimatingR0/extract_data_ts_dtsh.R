## import data

all <- read.csv("EbolaDataDTSH.csv")
all <- read.csv("EbolaDataDTSHepi.csv")
all <- read.csv("EbolaDataDTSHwho.csv")

all$Date <- as.Date(all$Date,format="%d/%m/%Y")
str(all)
library(xts)

#ts.week<- apply.weekly(as.xts(all),FUN=colSums)
#df.2 <- expand.grid(Date = unique(all$Date),Group = unique(all$Cases))
#df <- merge(all,df.2,all=TRUE)
#aggregate(Cases ~ Outbreak + Date, FUN=sum,  data=df, na.action=na.pass)
#summary(df)
#df$Reports[is.na(df$Reports)]<-0
#is.na(df$Reports)
#head(df)
#str(df)
#plot(df$Date,df$Reports,pch=16)

# columes that indicate an outbreak
outbreak_col      <- "Outbreak"

# column for report date
date_col          <- "Date"
date_format       <- "%d/%m/%Y"

# output folder to create
output_dir        <- "outbreaks"

# survey weeks start on a saturday, so just find the previous saturday
surv_week_start <- function(date)
{
  # 1. Find the previous saturday
  prev_sat <- date
  while (weekdays(prev_sat) != "Saturday")
    prev_sat <- prev_sat - 1;
  return(as.character(prev_sat))
}

notification_dates <- as.Date(all[,date_col], format=date_format)
notification_weeks <- sapply(notification_dates, surv_week_start)
df<-cbind(all,notification_weeks)

tt<-aggregate( cbind( Cases ) ~ notification_weeks + Outbreak, 
                 data = df , FUN=sum,na.action(0))
str(tt)
################

# plot time series of total cases
plot(NULL, type="l", ylab="Cases per week", xlim=c(0,length(tt[,1])-1), ylim=c(0, max(tt[,3])*1.05), xlab="", xaxt="n", xaxs="i", yaxs="i")
x <- c(0, 1:length(tt[,1])-1, length(tt[,1])-2)
y <- c(0, tt[,3], 0)
polygon(x, y, col="lightblue", border="lightblue")

barplot(tt[,3])
tt$notification_week <- as.Date(tt$notification_week,format="%Y-%m-%d")

# export these outbreaks for R0 analyses

# create output directory
dir.create(file.path(output_dir), showWarnings = FALSE)

# for each outbreak write to file...
head(tt)

tt<-tt[,1:3]
names(tt)<-c("date","outbreak","incidence")
tt$outbreak<-as.factor(tt$outbreak)
library(plyr)
tt<-tt[order(tt$date),]
# for the epi work only - double check the #s
 oldob=c("0","16","18","17","1","8","15")
 newob=c("1","2","3","4","5","6","7")
 tt$outbreak <- newob[ match(tt$outbreak, oldob) ]

tt$date<-as.Date(tt[,1])#, format=date_format)
sapply(tt$outbreak, function(x) length(unique(x)))
barplot(tt[,3],col=c(as.numeric(tt$outbreak)),ylab="Cases per week",
        xlab="Outbreak week")
test<-format(tt$date,format="%Y")
labels<-unique(test)
legend("topleft",c(labels),bty="n",fill=c(as.numeric(unique(tt$outbreak))))

for (i in 1:length(unique(tt$outbreak)))
{
  #f<-function(outbreak){
  outbreak<-tt[tt$outbreak %in% i,]
  outbreak_data <- data.frame(outbreak)
  outbreak_file <- file.path(output_dir, sprintf("outbreak%02d.csv", i))
  write.csv(outbreak_data, outbreak_file, row.names=F)
  #f(outbreak)
} 

library(ggplot2)
library(scales) # to access breaks/formatting functions

cols<- rainbow(length(unique(tt$outbreak)))
## nb notification_weeks is a factor and the below plot works with that, but not a time series
#d<-list() # this doesn't work - do by hand
#for (i in 1:length(unique(tt$outbreak))){
#d[i]<-ggplot(subset(tt,(outbreak==i)), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[i])+
#  geom_bar(stat="identity")+guides(fill=F)
#d[i] <-d[i] + scale_x_date()
#}
max(tt$incidence)
library(scales) # to access breaks/formatting functions

d1<-ggplot(subset(tt,(outbreak=="1")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[1])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d1 <-d1 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d2<-ggplot(subset(tt,(outbreak=="2")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[2])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d2 <-d2 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d3<-ggplot(subset(tt,(outbreak=="3")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[3])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d3 <-d3 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d4<-ggplot(subset(tt,(outbreak=="4")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[4])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d4 <-d4 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d5<-ggplot(subset(tt,(outbreak=="5")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[5])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d5 <-d5 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d6<-ggplot(subset(tt,(outbreak=="6")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[6])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d6 <-d6 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d7<-ggplot(subset(tt,(outbreak=="7")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[7])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d7 <-d7+ scale_x_date(breaks = date_breaks("months"),
                      labels = date_format("%b"))+ggtitle("")

d8<-ggplot(subset(tt,(outbreak=="8")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[8])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d8 <-d8 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d9<-ggplot(subset(tt,(outbreak=="9")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[9])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d9 <-d9 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d10<-ggplot(subset(tt,(outbreak=="10")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[10])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d10 <-d10 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d11<-ggplot(subset(tt,(outbreak=="11")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[11])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d11 <-d11 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d12<-ggplot(subset(tt,(outbreak=="12")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[12])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d12 <-d12 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d13<-ggplot(subset(tt,(outbreak=="13")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[13])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d13 <-d13 + scale_x_date(breaks = date_breaks("months"),
                       labels = date_format("%b"))+ggtitle("")

d14<-ggplot(subset(tt,(outbreak=="14")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[14])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d14 <-d14+ scale_x_date(breaks = date_breaks("months"),
                      labels = date_format("%b"))+ggtitle("")

d15<-ggplot(subset(tt,(outbreak=="15")), aes(x=date, y=incidence,fill=factor(outbreak))) +scale_fill_manual(values=cols[15])+
  geom_bar(stat="identity")+guides(fill=F)+ylim(c(0,max(tt$incidence)))
d15 <-d15+ scale_x_date(breaks = date_breaks("months"),
                        labels = date_format("%b"))+ggtitle("2014")
library(ggplot2)
library(grid)
library(gridExtra)
height<-as.numeric(max(tt$incidence))

width<-as.numeric(colSums(table(tt$date,tt$outbreak)))
## for the epi
## width[4]<-9
width/sum(width)
## adjust for each data set
# grid.arrange(d1, d2, d3, d4, d5 ,d6 ,d7,ncol=7,widths=width)
grid.arrange(d1, d2, d3, d4, ncol=4,widths=width[1:4])
grid.arrange(d5 ,d6 ,d7, d8, ncol=4,widths=width[5:8])
grid.arrange(d9, d10, d11, d12, ncol=4,widths=width[9:12])
grid.arrange(d13, d14, d15, ncol=4,widths=width[13:15])

ggplot(tt, aes(x=date, y=incidence,fill=outbreak)
               ) + geom_bar(stat="identity")+
  theme(legend.position="none")

