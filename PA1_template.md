Coursera MOOC_Reproducible Research_Course Project 1
Loading and preprocessing the data
setwd("F:/Personal/Coursera/Data Science/Reproducible Rsrch/W1/Project")
unzip(zipfile="activity.zip")
ActivityData <- read.csv("activity.csv", header = TRUE)
Load relevant libraries.
library(ggplot2)
library(Hmisc)
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
What is mean total number of steps taken per day?
stepsByDay <- tapply(ActivityData$steps, ActivityData$date, sum, na.rm = TRUE)
qplot(stepsByDay, xlab = "Total Steps per day", ylab = "Frequency using binwidth 500", binwidth = 500)


mean(stepsByDay, na.rm = TRUE)
## [1] 9354.23
median(stepsByDay,  na.rm = TRUE)
## [1] 10395
What is the average daily activity pattern?
averageTimeperBlock <- aggregate(x = list(meanSteps = ActivityData$steps), by = list(interval = ActivityData$interval), FUN = mean, na.rm = TRUE)
ggplot(data = averageTimeperBlock, aes(x=interval, y=meanSteps)) +
 geom_line() +
 xlab("5 minutes interval") +
 ylab("average number of steps taken")


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
averageTimeperBlock[which.max(averageTimeperBlock$meanSteps),]
##     interval meanSteps
## 104      835  206.1698
Imputing missing values
ImputedActivityData <- ActivityData
ImputedActivityData$steps <- impute(ActivityData$steps, fun = mean)
stepsByDayImputed <- tapply(ImputedActivityData$steps, ImputedActivityData$date, sum)
qplot(stepsByDayImputed, xlab= "Total steps per day (Imputed)", ylab = "Frequency using binwidth 500", binwidth = 500)


stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
Are there differences in activity patterns between weekdays and weekends?
weekday_or_weekend <- function(date) { 
 day <- weekdays(date)
 if(day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
 return("weekday")
 else if (day %in% c("Sunday", "Saturday"))
 return("weekend")
 else
 stop("invalid date")
 }

ImputedActivityData$date <- as.Date(ImputedActivityData$date)
ImputedActivityData$day <- sapply(ImputedActivityData$date, FUN = weekday_or_weekend)
AveragedImputedActivityData <- aggregate(steps ~ interval + day, data = ImputedActivityData, mean)
ggplot(AveragedImputedActivityData, aes(interval, steps)) + geom_line() + xlab("5 minute interval") + ylab("Number of steps")
