# Reads in raw data source, extracts data for each epidemic
# and generates a new .csv file with the data from all epidemics

# assumes the variable 'data_file' contains the data file to use
##
## set wd etc
##
# output folder to create
output_dir        <- "outbreaks"
# columes that indicate an outbreak
outbreak_col      <- "Outbrk"
outbreak_code_col <- "OutbreakCode"

# column for report date
date_col          <- "reportdate"
date_format       <- "%d-%B-%y"

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

surv_date <- function(date)
{
  # Find the month
  return(format.Date(date, "%y-%m-%d"))
}

# read in data
all <- read.csv("EbolaData.csv")

notification_dates <- as.Date(all[,date_col], format=date_format)
notification_weeks <- sapply(notification_dates, surv_week_start)
notification_date <- sapply(notification_weeks, surv_date)

# now the total incidence through time, with outbreak no's as well
allt<-cbind(as.Date(notification_weeks),all)
epis<-aggregate( cbind(New.Cases) ~ as.Date(notification_weeks) + Outbrk, 
                 data = allt , FUN=sum)
head(epis)
colnames(epis)<-c("Week","Outbreak","Cases")

plot(epis$Week,epis$Cases)

barplot(epis$Cases,cex.names=0.8)

#ast<-as.numeric(epis$Week)
#epis<-cbind(ast,epis)

rows <- notification_weeks > "2000-10-14"

weeks_2000 <- notification_weeks[rows]
#outbreak_col      <- "Outbreak"
#outbreaks_2000 <- epis[rows,outbreak_col]
ob_range <- range(as.Date(weeks_2000))
ob_weeks <- matrix(0, length(unique(epis$Outbreak)), diff(ob_range)/7+1)
colnames(ob_weeks) <- as.character(seq(ob_range[1], ob_range[2], by=7))

rownames(ob_weeks) <- sort(unique(epis$Outbreak))


library(reshape)
library(reshape2)
library(plyr)

# JM's working code
#popn$NZDep <- as.numeric(popn$NZDep)
#popn$merge <- paste(popn$NZDep, popn$Age, popn$Ethnicity)
#testtable$merge <- paste(testtable$NZDep, testtable$Age, testtable$Ethnicity)
#testtable <- testtable[testtable$Ethnicity!="None",]

#popn$cases <- 0
#cases <- matrix(0, length(popn$merge),1)
#rownames(cases) <- popn$merge
#cases[testtable$merge,] <- testtable$Cases
#popn$cases <- cases

allwks<-as.Date(colnames(ob_weeks))
allwks<-as.data.frame(allwks)
#allwks$cases <- 0
#allwks$Outbreaks <- NA
colnames(allwks)<-c("Week")

m1<-merge(allwks,epis,by= c("Week"),all=T,incomparables = NA)
             
plot(m1)

barplot(m1$Cases, col="black", border=NA, space=0, xaxt="n", ylim=c(0,max(m1$Cases,na.rm=T)), ylab="Cases per week")
# figure out years...
years <- as.numeric((as.Date(paste(2000:2015, "-01-01", sep="")) - ob_range[1]) / 7)
axis(1, at=years, labels=rep("", length(years)), line=0.5)
mtext(2000:2014, side=1, at = years[-length(years)] + diff(years)/2, line=1.2,cex=0.8)

library(ggplot2)

f=ggplot(m1, aes(Week, Cases))
(f1=f+geom_line(aes(group=Outbreak, color=factor(Outbreak)))+scale_color_discrete(guide="none"))

## fill minor gaps..
for (i in 2:length(m1$Cases)){
ifelse(#(
  m1$Cases[i-1]>0 #& m1$Cases[i+1]>0)
        ,m1$Cases[i]<-0,m1$Cases[i]<-m1$Cases[i])
}

for (i in 2:length(m1$Cases)){
  ifelse(#(
    m1$Cases[i+1]>0 #& m1$Cases[i+1]>0)
    ,m1$Cases[i]<-0,m1$Cases[i]<-m1$Cases[i])
}

for (i in 2:length(m1$Outbreak)){
  ifelse(#(
    m1$Cases[i]==0 && m1$Outbreak[i]==NA
    ,m1$Outbreak[i]<-m1$Outbreak[i-1],m1$Outbreak[i]<-m1$Outbreak[i])
}

for (i in 2:length(m1$Outbreak)){
  ifelse(m1$Cases[i]==0 & m1$Outbreak[i]==NA
    ,m1$Outbreak[i]<-m1$Outbreak[i+1],m1$Outbreak[i]<-m1$Outbreak[i])
}

f=ggplot(m1, aes(Week, Cases))
(f1=f+geom_line(aes(group=Outbreak, color=factor(Outbreak)))+scale_color_discrete(guide="none"))
(max(!is.na(m1$Cases)))
summary(m1)

### m1<-m1[-c(361),]
m1 <- m1[order(m1$Outbreak),]


m1$Outbreak<-as.factor(m1$Outbreak)

m1[m1$Outbreak %in% 5,]
na.omit(m1)
names(m1)<-c("date","outbreak","incidence")
for (i in 2:length(unique(m1$outbreak))-1)
{
  #f<-function(outbreak){
    outbreak<-m1[m1$outbreak %in% i,]
    week=length(outbreak)
    outbreak_data <- data.frame(outbreak, week)
    outbreak_file <- file.path(output_dir, sprintf("outbreak%02d.csv", i))
    write.csv(outbreak_data, outbreak_file, row.names=F)
  #f(outbreak)
} 

