# rm(list=ls())

# the R0 library can estimate our distribution from incidence data
require(R0)
source("estR0.R")

# generation time in weeks
# if like measles
# genTime <- generation.time(type="lognormal", val=c(6, 3.5)/7)
# if using dave's dodgy estimates from ebov data
k =gamfit$estimate[1] # shape
 theta = gamfit$estimate[2] # rate
 beta = 1/theta # scale
gmean =k/beta # mean

genTime<-generation.time(type="gamma",c(a=k,s=beta))
plot(genTime,xlim=c(0,20))

##### alternative gen time
generation.time.dtsh <- function (type = c("empirical", "gamma", "weibull", "lognormal"), 
                                  val = NULL, truncate = NULL, step = 1, first.half = TRUE, 
                                  p0 = TRUE) 
{
  type = match.arg(type)
  if (type == "empirical") {
    GT = val
    if (any(GT < 0)) 
      stop("Values in 'val' must be positive")
    if (sum(GT) > 1) 
      warning("Values will be standardized to sum to 1")
    if (!is.null(truncate)) {
      if (truncate < length(val)) {
        warning(paste("Empirical distribution truncated at length ", 
                      truncate))
        GT = GT[1:truncate]
      }
    }
  }
  else {
    if (length(val) < 2) 
      stop("val= c(mean,sd) must be provided for parametric GT")
    mean = val[1]
    sd = val[2]
    if (any(c(mean, sd) <= 0)) 
      stop("'mean' and 'sd' must be positive")
    if (is.null(truncate)) {
      tmax = ceiling(mean + 10 * sd)
    }
    else {
      tmax = truncate
    }
    if (first.half) {
      t.scale = c(0, 0.5 + seq(from=0, to=tmax,by=7))
    }
    else {
      t.scale = seq(from=0, to=tmax,by=7)
    }
    if (type == "gamma") {
      a = val[1]
      s = val[2]
      GT = diff(pgamma(t.scale, shape = a, scale = s))
    }
    else if (type == "lognormal") {
      meanlog = log(mean^2/sqrt(mean^2 + sd^2))
      sdlog = sqrt(2) * sqrt(log(sqrt(mean^2 + sd^2)/mean))
      GT = diff(plnorm(t.scale, meanlog = meanlog, sdlog = sdlog))
    }
    else if (type == "weibull") {
      cv <- sd/(mean)
      if (cv < 1e-06) {
        nu <- cv/(sqrt(trigamma(1)) - cv * digamma(1))
        shape <- 1/nu
        scale <- (mean)/(1 + nu * digamma(1))
      }
      else {
        aa <- log(cv^2 + 1)
        nu <- 2 * cv/(1 + cv)
        repeat {
          gb <- (lgamma(1 + 2 * nu) - 2 * lgamma(1 + 
                                                   nu) - aa)/(2 * (digamma(1 + 2 * nu) - digamma(1 + 
                                                                                                   nu)))
          nu <- nu - gb
          if (abs(gb) < 1e-12) 
            break
        }
        shape <- 1/nu
        scale <- exp(log(mean) - lgamma(1 + nu))
      }
      GT = diff(pweibull(t.scale, shape = shape, scale = scale))
    }
    if (is.null(truncate)) {
      GT.cum = cumsum(GT)
      if (length(GT.cum[GT.cum > 0.9999]) != 0) {
        truncate = (GT.cum > 0.9999) * (1:length(GT.cum))
        truncate = min(truncate[truncate > 0])
        if (truncate == 0) 
          warning(paste("provide truncate larger than ", 
                        mean + 10 * sd))
        GT = GT[1:truncate]
      }
    }
  }
  if (p0 == TRUE) 
    GT[1] = 0
  time = 0:(length(GT) - 1)
  GT = GT/sum(GT)
  mu = sum(GT * time)
  sigma = sqrt(sum(GT * time^2) - mu^2)
  return(structure(list(GT = GT, time = time, mean = mu, sd = sigma), 
                   class = "R0.GT"))
}

genTime<-generation.time.dtsh(type="gamma",c(a=k,s=beta))
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
plot(NULL, xlim=c(0.5,length(average_R0)+0.5), ylim=c(0,max(sapply(average_R0, max)) +0.5), ylab="Average R-effective", xlab="First recorded case date", xaxt="n", yaxs="i")

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

