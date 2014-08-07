## import data

all <- read.csv("EbolaDataDTSHcases.csv")
all$Date <- as.Date(all$Date,format="%d/%m/%Y")
str(all)
library(xts)

ts.week<- apply.weekly(as.xts(all),FUN=colSums)
df.2 <- expand.grid(Date = unique(all$Date),Group = unique(all$Outbreaks))
df <- merge(all,df.2,all=TRUE)
aggregate(Reports ~ Outbreak + Date, FUN=sum,  data=df, na.action=na.pass)
summary(df)
df$Reports[is.na(df$Reports)]<-0
#is.na(df$Reports)
head(df)
str(df)
plot(df$Date,df$Reports,pch=16)


epi <- read.csv("EbolaDataDTSHcurves.csv")
epi$Date <- as.Date(epi$Date,format="%d/%m/%Y")
str(epi)

plot(epi$Date,epi$Cases,pch=16)

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

tt<-aggregate( cbind( Reports ) ~ notification_weeks + Outbreak, 
                 data = df , FUN=sum)
str(tt)

################
ob_weeks <- matrix(0, length(unique(all[,outbreak_code_col])), num_months)
colnames(ob_weeks) <- 1:num_months
for (i in 1:num_months)
  colnames(ob_weeks)[i] <- sprintf("%4d-%02d-01", 1997 + floor((i-1)/12), (i-1) %% 12 + 1)
rownames(ob_weeks) <- unique(all[,outbreak_code_col])
t <- table(all[,outbreak_code_col], notification_months)
ob_weeks[rownames(t), colnames(t)] <- t

# plot time series of total cases
ts <- colSums(ob_weeks)
pdf("incidence_1997_2014.pdf", width=10, height=5)
plot(NULL, type="l", ylab="Cases per month", xlim=c(0, num_months-1), ylim=c(0, max(ts)*1.05), xlab="", xaxt="n", xaxs="i", yaxs="i")
x <- c(0, 0:(num_months-1), num_months-1)
y <- c(0, ts, 0)
polygon(x, y, col="lightblue", border="lightblue")

axis(1, seq(0,floor(num_months/12)*12,by=12), labels=rep("", floor(num_months/12)+1))
mtext(1996+1:floor(num_months/12), side=1, at = (1:floor(num_months/12))*12 - 6, line=0.5, cex=0.8)
box()
dev.off()

barplot(ob_weeks)
dev.off()

# plot finer-grained total cases per week since 2009
rows <- notification_weeks > "2008-12-31"

weeks_2007 <- notification_weeks[rows]
outbreaks_2007 <- all[rows,outbreak_code_col]

pdf("outbreaks_2009_2014.pdf", width=10, height=5)
ob_range <- range(as.Date(weeks_2007))
ob_weeks <- matrix(0, length(unique(outbreaks_2007)), diff(ob_range)/7+1)
colnames(ob_weeks) <- as.character(seq(ob_range[1], ob_range[2], by=7))
rownames(ob_weeks) <- sort(unique(outbreaks_2007))
t <- table(outbreaks_2007, weeks_2007)
ob_weeks[rownames(t), colnames(t)] <- t
cols <- c("grey20", rainbow(nrow(ob_weeks)-1))
barplot(ob_weeks, col=cols, border=NA, space=0, xaxt="n", ylim=c(0,60), ylab="Cases per week")

# figure out years...
years <- as.numeric((as.Date(paste(2009:2015, "-01-01", sep="")) - ob_range[1]) / 7)
axis(1, at=years, labels=rep("", length(years)), line=0.5)
mtext(2009:2014, side=1, at = years[-length(years)] + diff(years)/2, line=1.2)
dev.off()

# plot cases by DHB
rows <- notification_weeks > "2008-12-31"

weeks_2007     <- notification_weeks[rows]
dhb_2007       <- all[rows,"DHB"]

pdf("cases_by_dhb_2009_2014.pdf", width=10, height=5)
ob_range <- range(as.Date(weeks_2007))
ob_weeks <- matrix(0, length(unique(dhb_2007)), diff(ob_range)/7+1)
colnames(ob_weeks) <- as.character(seq(ob_range[1], ob_range[2], by=7))

# sort regions so they're north to south (ish)
order <- c("Northland", "Waitemata", "Auckland",
           "Counties Manukau", "Waikato", "Bay of Plenty",
           "Lakes", "Tairawhiti", "Taranaki", "Whanganui",
           "Hawke's Bay", "MidCentral", "Capital and Coast",
           "Hutt Valley", "Wairarapa", "Nelson Marlborough",
           "West Coast", "Canterbury", "South Canterbury",
           "Southern")

rownames(ob_weeks) <- order
t <- table(dhb_2007, weeks_2007)
ob_weeks[rownames(t), colnames(t)] <- t
cols <- rainbow(nrow(ob_weeks))


barplot(ob_weeks, col=cols, border=NA, space=0, xaxt="n", ylim=c(0,60), ylab="Cases per week")
legend(200,60, legend=rownames(ob_weeks), fill=cols, cex=0.7)

# figure out years...
years <- as.numeric((as.Date(paste(2009:2015, "-01-01", sep="")) - ob_range[1]) / 7)
axis(1, at=years, labels=rep("", length(years)), line=0.5)
mtext(2009:2014, side=1, at = years[-length(years)] + diff(years)/2, line=1.2)
dev.off()


# combine 'outbreaks' with fewer than 10 cases into one semi-sporadic column
weeks_2009 <- notification_weeks[rows]
outbreaks_2009 <- all[rows,outbreak_code_col]
ob_range <- range(as.Date(weeks_2009))
ob_weeks <- matrix(0, length(unique(outbreaks_2009)), diff(ob_range)/7+1)
colnames(ob_weeks) <- as.character(seq(ob_range[1], ob_range[2], by=7))
rownames(ob_weeks) <- sort(unique(outbreaks_2009))
t <- table(outbreaks_2009, weeks_2009)
ob_weeks[rownames(t), colnames(t)] <- t

sporadic <- ob_weeks[1,]
epidemic <- ob_weeks[-1,]
ob_sizes <- rowSums(epidemic)

# find our main outbreaks.  We grab the big ones, then merge in sporadic cases in the same time period.
# at worse this will overestimate R0 as the epidemic is slightly larger than it really is.

ob_sizes <- ob_sizes[order(-ob_sizes)]
epidemics_to_use <- as.numeric(names(ob_sizes)[ob_sizes > 20])

# now for each, find all nearby cases
weeks_2009 <- as.Date(weeks_2009)
outbreaks_R0 <- rep(0, length(outbreaks_2009))
outbreaks_R0[outbreaks_2009 %in% epidemics_to_use] <- outbreaks_2009[outbreaks_2009 %in% epidemics_to_use]

for (epi in epidemics_to_use)
{
  current_epi_rows <- outbreaks_R0 == epi
  # expand the week range by week_fudge weeks and look for more cases.  Keep going until we find none.
  week_fudge <- 3*7
  while (TRUE)
  {
    week_range <- range(weeks_2009[current_epi_rows])
    sporadics_to_add <- weeks_2009 >= week_range[1]-week_fudge & weeks_2009 <= week_range[2]+week_fudge & !current_epi_rows
    if (sum(sporadics_to_add) == 0)
      break
    current_epi_rows <- current_epi_rows |  sporadics_to_add
  }
  # update
  outbreaks_R0[current_epi_rows] <- epi
}

# add in the extra, unidentified epidemic
extra_outbreak_weeks <- c("2010-02-27", "2010-05-15")
sporadics_to_add <- weeks_2009 >= extra_outbreak_weeks[1] & weeks_2009 <= extra_outbreak_weeks[2]
outbreaks_R0[sporadics_to_add] <- 99

# reorder the outbreaks based on their end date
outbreak_nos <- unique(outbreaks_R0[outbreaks_R0 > 0])
max_dates <- rep("", length(outbreak_nos))
for (i in 1:length(outbreak_nos))
{
  max_dates[i] <- max(weeks_2009[outbreaks_R0 == outbreak_nos[i]])
}
epidemic_order <- order(max_dates)

# plot

pdf("outbreaks_for_R0.pdf", width=10, height=5)
ob_weeks <- matrix(0, length(unique(outbreaks_R0)), diff(ob_range)/7+1)
colnames(ob_weeks) <- as.character(seq(ob_range[1], ob_range[2], by=7))
rownames(ob_weeks) <- c(0, outbreak_nos[epidemic_order])
t <- table(outbreaks_R0, weeks_2009)
ob_weeks[rownames(t), colnames(t)] <- t
cols <- c("grey30", rainbow(length(epidemic_order)))
barplot(ob_weeks, col=cols, border=NA, space=0, xaxt="n", ylim=c(0,60), ylab="Cases per week")

# figure out years...
years <- as.numeric((as.Date(paste(2009:2015, "-01-01", sep="")) - ob_range[1]) / 7)
axis(1, at=years, labels=rep("", length(years)), line=0.5)
mtext(2009:2014, side=1, at = years[-length(years)] + diff(years)/2, line=1.2)

dev.off()

# export these outbreaks for R0 analyses

# create output directory
dir.create(file.path(output_dir), showWarnings = FALSE)

# for each outbreak, compute the incidence
for (ob_num in outbreak_nos)
{
  # find the rows
  ob_rows  <- outbreaks_R0 == ob_num
  
  # create a date range
  ob_range <- range(weeks_2009[ob_rows])
  ob_weeks <- rep(0, length=diff(ob_range)/7+1)
  names(ob_weeks) <- seq(ob_range[1], ob_range[2], by=7)
  
  # compute the incidence by tabling up
  t <- table(weeks_2009[ob_rows])
  ob_weeks[names(t)] <- t
  
  # write out an outbreak file
  outbreak_data <- data.frame(date=names(ob_weeks), week=1:length(ob_weeks), incidence=ob_weeks)
  outbreak_file <- file.path(output_dir, sprintf("outbreak%02d.csv", ob_num))
  write.csv(outbreak_data, outbreak_file, row.names=F)
}
