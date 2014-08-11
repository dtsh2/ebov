# rm(list=ls())
## Daves notes
# compute generation time.  We're wanting a lognormal distribution with mean 12.0 and sd 3.5 from
# http://www.sciencedirect.com/science/article/pii/S0022519311003146
mu    <- 12
sigma <- 3.5
sigma_logn <- sqrt(log(1 + (sigma/mu)^2))
mu_logn    <- log(mu) - log(1 + (sigma/mu)^2) / 2

# then exp(rnorm(n, mu_logn, sigma_logn)) simulates from lognormal with the given mean and sd.

# the R0 library can estimate our distribution from incidence data
require(R0)
source("estR0.R")

# generation time in weeks
# if like measles
genTime <- generation.time(type="lognormal", val=c(6, 3.5)/7)
# if using dave's dodgy estimates from ebov data
k =gamfit$estimate[1] # shape
 theta = gamfit$estimate[2] # rate
 beta = 1/theta # scale
gmean =k/beta # mean
# variancegam<- k * beta^2
# sdgam<-variancegam^2
#genTime<-generation.time(type="gamma",c(mean=gmean,sd=k^2))
genTime<-generation.time(type="gamma",c(a=k,s=beta))
plot(genTime,xlim=c(0,20))
## each seems wrong - discuss with JM

sigma_logn <-lnfit$estimate[2]
mu_logn    <-lnfit$estimate[1]

genTime <- generation.time(type="lognormal", val=c(mean=mu_logn,sd=sigma_logn))
plot(genTime)
                           
# read out outbreak folder
outbreak_folder <- "outbreaks"

# read in and reorder by date
outbreak_files <- list.files(path=outbreak_folder, pattern="*.csv")

max_dates <- rep("", length(outbreak_files))
for (i in 1:length(outbreak_files))
{
  incidence <- read.csv(file.path(outbreak_folder, outbreak_files[i]), stringsAsFactors=F)
  max_dates[i] <- max(incidence$date)
}

outbreak_files <- outbreak_files[order(max_dates)]

# function for modifying colours to make them transparent
alpha <- function(col, a)
{
  if (nchar(col) == 9)
    rgb(t(col2rgb(substr(col,1,7))/255), alpha=a)
  else
    rgb(t(col2rgb(col)/255), alpha=a)
}

# do the separate analyses
average_R0 <- list()
cols <- rainbow(length(average_R0))

for (i in 1:length(outbreak_files))
{
  ob_file <- outbreak_files[i]
  incidence <- read.csv(file.path(outbreak_folder, ob_file))

  # convert our incidence data to something we can use
  counts <- incidence$incidence
  names(counts) <- incidence$date

  # if we have too long gaps in the data we'll need to strip it out

  # add dates....
  estR0<-jm_estR0(counts, genTime, t=1:length(counts), end=length(counts), methods=c("TD"), nsim=1000)

#  months <- as.Date(as.vector(t(outer(2009:2015,1:12,function(x,y) { sprintf("%04d-%02d-01", x, y) }))))
#  month_lab_short <- c("J","F","M","A","M","J","J","A","S","O","N","D")
#  month_lab_long  <- months(as.Date(sprintf("2005-%02d-01", 1:12)), T)
#  month_lab <- rep(1:12,6)
#  years  <- as.Date(sprintf("%04d-01-01", 2009:2015))

#  date_range <- as.Date(as.character(incidence$date))
  # plot...
#  plot(NULL, xlim=range(date_range), ylim=range(estR0$conf.int), ylab="R0", xaxt="n", xlab="")
#  polygon(c(date_range,rev(date_range)), c(estR0$conf.int[,1], rev(estR0$conf.int[,2])), col=alpha(cols[i], 0.5), border=NA)
#  lines(date_range, estR0$R, lwd=2, col=cols[i])
#  abline(h=1)
#  axis(1, at=months, labels=rep("",length(months)))
#  axis(1, at=years, labels=rep("",length(years)), tcl=-2.5, lwd.ticks=1.5)

#  incl_month <- months+10 >= min(date_range) & months+20 < max(date_range)
#  incl_year <- years >= min(date_range) & years < max(date_range)
  
  average_R0[[length(average_R0)+1]] <- estR0$R0
  names(average_R0)[[i]]<-i
}

cols<- rainbow(length(average_R0))
# plot R0 averages

my_vioplot <- function(dat, bw, border, col, at)
{
  e <- density(dat, bw)
  m <- 0.3/max(e$y)
  incl <- e$y > max(e$y)/1000
  polygon(c(at-e$y[incl]*m,rev(at+e$y[incl]*m)), c(e$x[incl], rev(e$x[incl])), col=col, border=border)
}

#pdf("averageR0.pdf", width=8, height=6)
#range_R0 <- range(sapply(average_R0, range))
#plot(NULL, xlim=c(0.5,length(average_R0)+0.5), ylim=range_R0 + diff(range_R0)*0.05*c(-1,1), ylab="Average R0", xlab="", xaxt="n", yaxs="i")
# alter as necessary...
plot(NULL, xlim=c(0.5,length(average_R0)+0.5), ylim=c(0,2+0.5), ylab="Average R-effective", xlab="First recorded case date", xaxt="n", yaxs="i")

for (i in 1:length(average_R0))
  my_vioplot(average_R0[[i]], bw=0.015, border=1:7, col=cols[i], at=i)
abline(h=1, col="black")

min_dates <- rep("", length(outbreak_files))
for (i in 1:length(outbreak_files))
{
  incidence <- read.csv(file.path(outbreak_folder, outbreak_files[i]), stringsAsFactors=F)
  min_dates[i] <- min(incidence$date)
}
min_dates

labs <- as.Date(min_dates)
labs<-format(labs,
       "%b %y")
labels <- c(as.character(labs))
axis(1,at=seq(1, length(average_R0), by=1), labels = labels)

##############
# for 15 ...
axis(1,at=seq(1, length(average_R0), by=1), labels = F)
text(1:length(average_R0), par("usr")[1], labels=labels, srt=45, pos=1, xpd=TRUE,cex=0.5)

