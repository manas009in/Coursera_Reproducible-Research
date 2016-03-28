#Unzip the original data file and create a data frame. Load relevant libaries.
library(ggplot2)
library(Hmisc)


unzip(zipfile="activity.zip")
ActivityData <- read.csv("activity.csv", header = TRUE)


#Plot the histogram of total steps taken per day. Also find out the mean and median of the number of steps. 
stepsByDay <- tapply(ActivityData$steps, ActivityData$date, sum, na.rm = TRUE)
qplot(stepsByDay, xlab = "Total Steps per day", ylab = "Frequency using binwidth 500", binwidth = 500)
mean(stepsByDay, na.rm = TRUE)
median(stepsByDay,  na.rm = TRUE)


#Time series plot of the average number of steps taken
averageTimeperBlock <- aggregate(x = list(meanSteps = ActivityData$steps), by = list(interval = ActivityData$interval), FUN = mean, na.rm = TRUE)
ggplot(data = averageTimeperBlock, aes(x=interval, y=meanSteps)) +
+ geom_line() +
+ xlab("5 minutes interval") +
+ ylab("average number of steps taken")


#5-minute interval that, on average, contains the maximum number of steps
averageTimeperBlock[which.max(averageTimeperBlock$meanSteps),]


#Strategy for imputing missing data. Use Hmisc package for impute function.
ImputedActivityData <- ActivityData
ImputedActivityData$steps <- impute(ActivityData$steps, fun = mean)


#Histogram of the total number of steps taken each day after missing values are imputed
stepsByDayImputed <- tapply(ImputedActivityData$steps, ImputedActivityData$date, sum)
qplot(stepsByDayImputed, xlab= "Total steps per day (Imputed)", ylab = "Frequency using binwidth 500", binwidth = 500)
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)



#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
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
