library(RCurl)
library(rjson)
library(plyr)
library(scales)

accTokenFnm <- "Data/accessToken.txt"
baseAddr <- "https://api.nike.com/"
header <- c(Accept="application/json", "Content-Type"="application/json", appid="fuelband")

# ------------------------------------------------------------------------------
# Get user's access token
# ------------------------------------------------------------------------------
getNikeAccessToken <- function(fnm=accTokenFnm){
  accessTokenFile <- file(fnm, "rt", blocking=T)
  accessToken <- readLines(accessTokenFile, 1, ok=T, warn=F)
  close(accessTokenFile)
  return(accessToken)
}

# ------------------------------------------------------------------------------
# Download single run data
# Nike returns some overall data and waypoints.
# ------------------------------------------------------------------------------
getNikeSingleRun <- function(activityId, accessToken){
  message(paste("Downloading run", activityId, "..."))
  address <- paste(baseAddr, "me/sport/activities/", sep="")
  url <- paste(address, activityId, "?access_token=", accessToken, sep="")
  json <- getURL(url, httpheader=header)
  data <- fromJSON(json)
  
  df <- as.data.frame(data$metrics[[3]][[4]])
  df$activityId <- activityId
  names(df) <- c('totaldist10sec','activityId')
  df$startTime <- gsub("T"," ", data$startTime)
  df$startTime <- gsub("Z","", df$startTime)
  df$startTime <- as.POSIXct(strptime(df$startTime, "%Y-%m-%d %H:%M:%S"))
  df$totaldist10sec <- as.numeric(as.character(df$totaldist10sec))
  #rescale for corrected total distance bc nike+ sometimes sucks
  df$totaldist10sec <- rescale(df$totaldist10sec, c(0.000,data$metricSummary[3]$distance[1]))
  df$incredist10sec[1] <- 0
  df$incredist10sec[2:length(df$incredist10sec)] <- diff(df$totaldist10sec)
  df$pace <- 1/((df$incredist10sec)/10 *.621371 *60 )
  #get rid of weird outliers, replace with median
  #df$pace[df$pace %in% boxplot.stats(df$pace)$out] <- median(df$pace)
  return(df)
}


# ------------------------------------------------------------------------------
# Download single run stats
# Nike returns some overall data.
# ------------------------------------------------------------------------------
getNikeSingleRunStat <- function(activityId, accessToken){
  message(paste("Downloading run", activityId, "..."))
  address <- paste(baseAddr, "me/sport/activities/", sep="")
  url <- paste(address, activityId, "?access_token=", accessToken, sep="")
  json <- getURL(url, httpheader=header)
  data <- fromJSON(json)
 
  df <- as.data.frame(data$startTime)
  df$activityId <- activityId
  names(df) <- c('startTime','activityId')
  df$startTime <- gsub("T"," ", df$startTime)
  df$startTime <- gsub("Z","", df$startTime)
  df$startTime <- as.POSIXct(strptime(df$startTime, "%Y-%m-%d %H:%M:%S"))
  df$calories <- data$metricSummary$calories
  df$fuel <- data$metricSummary$fuel
  df$steps <- data$metricSummary$steps
  df$distancekm <- data$metricSummary$distance
  df$distancemi <- df$distancekm*.621371
  df$duration <- data$metricSummary$duration
  time <- as.numeric(strsplit(df$duration,':')[[1]])
  df$totalmins <- time[1]*60 + time[2] + time[3]/60
  df$avepace <- df$totalmins/df$distancemi
  df$avepacemph <- df$distancemi /df$totalmins*60 
  df$cadence <- df$steps/df$totalmins
  return(df)
}

# ------------------------------------------------------------------------------
# Get list of all running activities
# ------------------------------------------------------------------------------
listNikeRuns <- function(count, accessToken){
  address <- paste(baseAddr, "me/sport/activities/RUNNING/", sep="")
  num <- as.character(count) 
  url <- paste(address, "?access_token=", accessToken, "&count=", num, sep="")
  json <- getURL(url, httpheader=header)
  data <- fromJSON(json)
  
  # Extract only interesting data for each run (returns list of list)
  vars <- c("activityId", "startTime")
  extracted <- lapply(data$data, function(d) d[vars]) 
  
  # Now bind into df
  df <- as.data.frame(t(sapply(extracted, function(x) rbind(x))))
  names(df) <- names(extracted[[1]])

  #df <- df[- grep('-', df$activityId),] # used this to get rid of non-run weirdness before i restricted activities to only 'running'
  rownames(df) <- NULL 
  df$startTime <- gsub("T"," ", df$startTime)
  df$startTime <- gsub("Z","", df$startTime)
  df$startTime <- as.POSIXct(strptime(df$startTime, "%Y-%m-%d %H:%M:%S"))
  return(head(df,count))
}

# ------------------------------------------------------------------------------
# Download elevation data
# Not sure if this really works yet
# ------------------------------------------------------------------------------
getNikeSingleRunElev <- function(activityId, accessToken){
  message(paste("Downloading run", activityId, "..."))
  address <- paste(baseAddr, "me/sport/activities/", sep="")
  url <- paste(address, activityId, "/gps/?access_token=", accessToken, sep="")
  json <- getURL(url, httpheader=header)
  data <- fromJSON(json)
 
  df$activityId <- activityId
  df$elevationLoss <- data$elevationLoss
  df$elevationGain <- data$elevationGain
  df$elevationMax <- data$elevationMax
  df$elevationMin <- data$elevationMin
  
  df
}



