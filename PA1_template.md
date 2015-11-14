---
title: "Reproducible Research: Peer Assessment 1"
author: "Kevin Bailey"
date: "November 12, 2015"
output: html_document
keep_md: true
---


## Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis


```r
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', './tmp.zip')
activity <- read.csv(unz('./tmp.zip','activity.csv'))
unlink('./tmp.zip')
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

```r
total_steps_per_day <-by(activity$steps, activity$date, sum, na.rm=TRUE)
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(total_steps_per_day)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


Calculate and report the mean and median of the total number of steps taken per day


```r
mean(total_steps_per_day)
```

```
## [1] 9354.23
```

```r
median(total_steps_per_day)[[1]]
```

```
## [1] 10395
```



## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval_average <- by(activity$steps, activity$interval, mean, na.rm=TRUE)

plot(names(interval_average), interval_average, type='l')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
names(interval_average[interval_average == max(interval_average)])
```

```
## [1] "835"
```



## Inputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
library(plyr)
grouped_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity2 <- ddply(activity, ~ interval, transform, steps = grouped_mean(steps))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
cleaned_steps_per_day <- by(activity2$steps, activity2$date, sum, na.rm=TRUE)
hist(cleaned_steps_per_day)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(cleaned_steps_per_day)
```

```
## [1] 10766.19
```

```r
median(cleaned_steps_per_day)[[1]]
```

```
## [1] 10766.19
```



## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
test_weekend <- function(x) { 
    if (weekdays(as.Date(x)) %in% c("Sunday","Saturday")) { 
        'weekend' 
    } else { 
        'weekday' 
    }
}
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
library(lattice)   
result <- ddply(activity2, .(interval, sapply(date, test_weekend)), summarize, steps=mean(steps))
names(result)[2] <- 'weekday_test'
xyplot(steps ~ interval | weekday_test , result, type='l', layout=c(1,2))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
