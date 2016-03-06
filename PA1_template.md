# Reproducible Research: Peer Assessment 1

**
Loading and preprocessing the data
**

```r
# Load the activity csv file from my working directory
activityData <- read.csv("activity.csv",header=TRUE,na.strings="NA")
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# Convert the date column
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.3
```

```r
activityData$Rdate <- ymd(activityData$date)
```


**
What is mean total number of steps taken per day?
**

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day

```r
# Calculate the total number of steps taken per day
# Make a histogram of the total number of steps taken each day
sums <- tapply(activityData$steps,as.Date(activityData$Rdate),sum,na.rm=T)
barplot(sums,xlab="day",ylab="Total Number of Steps",main="Histogram of Total Number of Steps Taken Each Day",col="gray")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
# Calculate and report the mean and median of the total number of steps taken per day
mean(sums, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(sums, na.rm=TRUE)
```

```
## [1] 10395
```

**
What is the average daily activity pattern?
**

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
#  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
actDataNoNA <- na.omit(activityData)
str(actDataNoNA)
```

```
## 'data.frame':	15264 obs. of  4 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ Rdate   : POSIXct, format: "2012-10-02" "2012-10-02" ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...
##   .. ..- attr(*, "names")= chr [1:2304] "1" "2" "3" "4" ...
```

```r
avgs <- aggregate(actDataNoNA[,c(1,3)],by=list(interval=actDataNoNA$interval),FUN="mean")
names(avgs)[2]="avgSteps"
plot(avgs$interval,avgs$avgSteps,type="l",main="Time Series Plot of the 5-minute Interval",xlab="5-minute interval",ylab="Mean of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
avgs[which.max(avgs$avgSteps),c(1,2)]
```

```
##     interval avgSteps
## 104      835 206.1698
```

**
Imputing missing values
**

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Calculate and report the total number of missing values in the dataset
sum(is.na(activityData))
```

```
## [1] 2304
```

```r
# Devise a strategy for filling in all of the missing values in the dataset.
library("Hmisc")
```

```
## Warning: package 'Hmisc' was built under R version 3.2.3
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
# impute {Hmisc} : http://www.inside-r.org/packages/cran/Hmisc/docs/impute
newactivityData <- activityData
newactivityData$steps <- with(newactivityData, impute(steps, mean))

newSums <- tapply(newactivityData$steps,newactivityData$Rdate, sum)
mean(sums)
```

```
## [1] 9354.23
```

```r
mean(newSums)
```

```
## [1] 10766.19
```

```r
median(sums)
```

```
## [1] 10395
```

```r
median(newSums)
```

```
## [1] 10766.19
```

```r
barplot(newSums,xlab="day",ylab="Total Number of Steps",main="Histogram of Total Number of Steps Taken Each Day",col="gray")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)
There is no much change after use the impute in the Hmisc package

**
Are there differences in activity patterns between weekdays and weekends?
**

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
newactivityData$RdateType <-  ifelse(as.POSIXlt(newactivityData$Rdate)$wday %in% c(0,6), 'weekend', 'weekday')

library(ggplot2)
newMeans <- aggregate(steps ~ interval + RdateType, data=newactivityData, mean)

ggplot(newMeans, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(RdateType ~ .) +
    xlab("5-minute interval") + 
    ylab("Mean of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)



