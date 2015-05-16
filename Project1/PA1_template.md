---
title: "PeerAssessment1"
output: html_document
---


**Loading the data**


```r
data = read.csv("c:/temp/activity.csv")
```

**What is mean total number of steps taken per day?**

Calculate the total number of steps taken per day


```r
totalSteps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
```

Make a histogram of the total number of steps taken each day

```r
hist(totalSteps$steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```


**What is the average daily activity pattern?**

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
totalSteps <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
plot(steps ~ interval, data = totalSteps, type ="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
totalSteps[which.max(totalSteps$steps), ]$interval
```

```
## [1] 835
```

**Imputing missing values**

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
filler <- function(steps,interval) {
        filled <- NA
        if (!is.na(steps))
                filled <-c(steps)
        else
                filled <- (totalSteps[totalSteps$interval == interval, "steps"])
        return(filled)
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filled.data <-data
filled.data$steps<- mapply(filler, filled.data$steps, filled.data$interval)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
totalSteps2 <- aggregate(steps ~ date, data = filled.data, sum)
hist(totalSteps2$steps)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
mean(totalSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps2$steps)
```

```
## [1] 10766.19
```

Median values slighly changed after imputing. Mean value remaind same. Since we imputed with mean value, differnce is very minimial.

**Are there differences in activity patterns between weekdays and weekends?**

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
filled.data$day = ifelse(as.POSIXlt(as.Date(filled.data$date))$wday%%6 == 0, "weekend", "weekday")
filled.data$day = factor(filled.data$day, levels = c("weekday", "weekend"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
interval_steps = aggregate(steps ~ interval + day, filled.data, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = interval_steps, aspect = 1/2, type = "l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

