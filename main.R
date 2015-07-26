library(ggplot2)
library(gridExtra)
library(extrafont)
library(xkcd)

source('nike.R')

# ------------------------------------------------------------------------------
# Set basic info
# ------------------------------------------------------------------------------
accessToken  <- getNikeAccessToken()      # default is 'Data/accessToken.txt'
count        <- 159                       # total Nike+ runs 
trainstart   <- as.POSIXct('2015-04-26')  # start day for plots
raceday      <- as.POSIXct('2015-07-19')  # finish day for plots
runstatsfile <- 'Data/allrunStats.txt'    # file to hold all basic run stats
plotfile     <- 'runs.png'                # output png file for plot

# ------------------------------------------------------------------------------
# Set some default flags for whatever you want to do
# ------------------------------------------------------------------------------
get.run.list = FALSE
get.run.stats = FALSE
get.run.details = FALSE

# ------------------------------------------------------------------------------
# Read in existing data from file and figure out how many new runs to download
# ------------------------------------------------------------------------------
allrunStats <- read.csv(runstatsfile,header=T,colClasses=c('character','character','integer','integer','integer','numeric','numeric','character','numeric','numeric','numeric','numeric','numeric','integer','character','character'))
allrunStats$startTime <- as.POSIXct(allrunStats$startTime)
allrunStats$duration <- as.character(allrunStats$duration)
allrunStats$dates <- as.integer(allrunStats$dates)

newRuns <- count - nrow(allrunStats) 
if (newRuns > 0) {
  get.run.list = TRUE
  get.run.stats= TRUE
  get.run.details = FALSE
}

# ------------------------------------------------------------------------------
# Get list of run IDs and dates
# ------------------------------------------------------------------------------
if (get.run.list ==TRUE){
  #runs <- listNikeRuns(count, accessToken)
  newruns <- listNikeRuns(newRuns, accessToken)
}

# ------------------------------------------------------------------------------
# Get basic stats for list of runs and update csv.
# ------------------------------------------------------------------------------
if (get.run.stats == TRUE){
  mylist <- list()
  i <- 1
    for (run in newruns$activityId) {
        mylist[[i]] <- getNikeSingleRunStat(run, accessToken)
        i <- i+1 
	  }

  # Merge stats, this is sort of dumb
  newrunStats <- Reduce(function(...)merge(...,all=T),mylist)
  newrunStats$cumMileage <- cumsum(newrunStats$distancemi)
  newrunStats$dates <- as.numeric(newrunStats$startTime)
  newrunStats$cumMileage <- cumsum(newrunStats$distancemi)
  newrunStats$weekdays <- weekdays(newrunStats$startTime)
  # My long/easy runs are generally always on Saturday
  newrunStats$runtype[newrunStats$weekdays=='Saturday'] <- 'long'
  newrunStats$runtype[newrunStats$weekdays!='Saturday'] <- 'regular'
  newrunStats$runtype <- as.factor(newrunStats$runtype)
  allrunStats <- rbind(allrunStats,newrunStats)
  allrunStats$cumMileage <- cumsum(allrunStats$distancemi)
  allrunStats$weekdays <- weekdays(allrunStats$startTime)
  allrunStats$runtype[allrunStats$weekdays=='Saturday'] <- 'long'
  allrunStats$runtype[allrunStats$weekdays!='Saturday'] <- 'regular'
  allrunStats$runtype <- as.factor(allrunStats$runtype)
  write.csv(allrunStats,runstatsfile,quote=F, row.names=F) 
}

# ------------------------------------------------------------------------------
# Get run details
# ------------------------------------------------------------------------------
if (get.run.details == TRUE){
  mylist2 <- list()
  i <- 1
    for (run in runs$activityId) {
        mylist2[[i]] <- getNikeSingleRun(run, accessToken)
        i <- i+1 
	  }

  # Merged stats
  runDetails <- Reduce(function(...)merge(...,all=T),mylist2)
  runDetails <- arrange(runDetails,desc(startTime),incredist10sec)
}

# ------------------------------------------------------------------------------
# Plots
# ------------------------------------------------------------------------------
# Subset runs for plotting by restricting to date range
runStats <- subset(allrunStats, startTime > trainstart)

# Plot 1 : avepace vs time 
p1 <- ggplot(runStats, aes(x=startTime, y=avepace, colour=runtype)) + geom_point() +theme_xkcd() + theme(axis.line = element_line(colour = "black"))
p1 <- p1 + labs(x='Date', y='Pace (mins/mile)',title='Pace over time')
p1 <- p1 + geom_smooth(method='glm',se=TRUE)
p1fit <- glm(avepace ~ dates, data=runStats)
# Predict race day (but this is linear, so wait until closer)
# p1pred <- predict(p1fit, newdata = data.frame(dates=as.numeric(raceday)))

# Plot 2 : avepace vs run distance
p2 <- ggplot(runStats, aes(x=distancemi, y=avepace)) + geom_point() +theme_xkcd()+ theme(axis.line = element_line(colour = "black"))
p2 <- p2 + labs(x='Run distance (miles)', y='Pace (mins/mile)', title='Pace vs Run Distance')
p2 <- p2 + geom_smooth(method='glm',se=TRUE)
p2fit <- glm(avepace ~ distancemi + cadence + runtype, data=runStats)
# Predict pace by run distance and cadence and add estimate to plot
p2pred <- predict(p2fit, newdata= data.frame(distancemi=13.1,cadence=175, runtype='regular'))
p2 <- p2 + geom_point(aes(x=13.1,y=p2pred),color='red',shape=8)
p2 <- p2 + geom_text(aes(x=13.1,y=p2pred+.1), label=paste0(as.character(floor(p2pred)) , "'", as.character( round((round(p2pred,2)-floor(p2pred))*60),0 )))

# Plot 3 : avepace vs cadence
p3 <- ggplot(runStats[runStats$cadence > 150,], aes(x=cadence, y=avepace)) + geom_point()+theme_xkcd()+ theme(axis.line = element_line(colour = "black"))
p3 <- p3 + labs(x='Step cadence (bpm)', y='Pace (mins/mile)', title='Pace vs Cadence')
p3 <- p3 + geom_smooth(method='glm',se=TRUE)
p3 <- p3 + stat_smooth(color='firebrick')
p3fit <- glm(avepace ~ cadence, data=runStats[runStats$cadence > 0,])
# Fit cadence
p3pred <- predict(p3fit, newdata=data.frame(cadence=175))
#p3 <- p3 + geom_point(aes(x=173,y=p3pred),color='red',shape=8)

# Plot 4 : avepace vs cumulative run mileage
p4 <- ggplot(runStats, aes(x=cumMileage, y=avepace)) + geom_point() +theme_xkcd()+ theme(axis.line = element_line(colour = "black"))
p4 <- p4 + labs(x='Total mileage', y='Pace (mins/mile)', title='Pace vs Cumulative Run Mileage')
p4 <- p4 + geom_smooth(method='glm',se=TRUE)
p4 <- p4 + stat_smooth(color='firebrick')
p4fit <- glm(avepace ~ cumMileage, data=runStats)
# Predict pace by cumulative mileage (only extrapolate to 1.25* max)
p4pred <- predict(p4fit, newdata=data.frame(cumMileage=23+max(runStats$cumMileage)))
p4 <- p4 + geom_point(aes(x=23+max(runStats$cumMileage),y=p4pred),color='red',shape=8)
p4 <- p4 + geom_text(aes(x=23+max(runStats$cumMileage),y=p4pred+.1), label=paste0(as.character(floor(p4pred)) , "'", as.character( round((round(p4pred,2)-floor(p4pred))*60),0 )))


allfit <- glm(avepace ~ distancemi + cumMileage + runtype, data=runStats)
allpred <- predict(allfit, newdata=data.frame(cumMileage=27+max(runStats$cumMileage),distancemi=9, runtype='regular'))

grid.arrange(p1,p2,p3,p4, nrow=2)
g <- arrangeGrob(p1,p2,p3,p4, nrow=2)
ggsave(plotfile,g,width=12,height=6,units='in')
