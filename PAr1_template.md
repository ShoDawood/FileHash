Reproducible Research: Peer Assessment 1

echo = TRUE

Loading and preprocessing the data

if (!file.exists("activity.csv") )
    {
     dlurl <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
     download.file(dlurl,destfile='repdata%2Fdata%2Factivity.zip',mode='wb')  
     unzip('repdata%2Fdata%2Factivity.zip')
    }

data = read.csv("activity.csv")  
What is mean total number of steps taken per day?

# calculate steps/day
stepsperday = aggregate(steps ~ date, data, sum)
# histogram
hist(stepsperday$steps, main = paste("Steps per day"), xlab="Steps")


# mean per day
meansteps = mean(stepsperday$steps)
meansteps
## [1] 10766.19
# median
mediansteps = median(stepsperday$steps)
mediansteps
## [1] 10765
What is the average daily activity pattern?

# Calculate the total number of steps taken per day
intervals = aggregate(steps ~ interval, data, mean)
# Plot
plot(intervals$interval,intervals$steps, type="l", xlab="Interval", ylab="Steps",main="Average Steps per Day by Interval")


# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxsteps = intervals[which.max(intervals$steps),1]
maxsteps
## [1] 835
Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
countNA = sum(!complete.cases(data))
countNA
## [1] 2304
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
avesteps = aggregate(steps ~ date, data, mean)
filldata = data
totalmean = mean(data[,1],na.rm=TRUE)
for(i in 1:nrow(filldata)){
  if(is.na(filldata[i,1])){
    filldata[i,1]=totalmean
  }
}
filledperday = aggregate(steps ~ date, filldata, sum)
# histogram
hist(filledperday$steps, main = paste("Steps per day"), xlab="Steps")


# mean per day
meansteps = mean(filledperday$steps)
meansteps
## [1] 10766.19
# median
mediansteps = median(filledperday$steps)
mediansteps
## [1] 10766.19
Are there differences in activity patterns between weekdays and weekends?

data$day = weekdays(as.Date(data$date))
data$daytype = "Placeholder"

for(i in 1:nrow(data)){
  if(data[i,4] %in% c("Saturday","Sunday")){
    data[i,]$daytype = "Weekend"
  }
  else {
  data[i,]$daytype = "Weekday"
  }
}

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(ggplot2)
## Warning: package 'ggplot2' was built under R version 3.3.3
bydaytype = data %>% group_by(interval, daytype) %>% summarise(meansteps = mean(steps, na.rm = TRUE))

dayplot = ggplot(data = bydaytype, mapping = aes(x = interval, y = meansteps)) + geom_line() + facet_grid(daytype ~ .) + scale_x_continuous("Interval", breaks = seq(min(bydaytype$interval), max(bydaytype$interval), 100)) + scale_y_continuous("Ave Steps") + ggtitle("Ave Steps by Interval")
dayplot
