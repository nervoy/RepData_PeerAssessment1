# Project 1



## Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
data <- read.csv(file="activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis.


```r
#convert the character date into Date format.
data$date <- as.Date(data$date) 
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day:

```r
stepsPerDay <- with(data,aggregate(steps,list(date=date),sum,na.rm=FALSE))
#We ignored 'NA' values here.

stepsPerDay$steps <- stepsPerDay$x #rename a column
stepsPerDay$x <- NULL
head(stepsPerDay)
```

```
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

2. Histogram of the total number of steps taken per day:

```r
hist(stepsPerDay$steps, breaks=10, col=rgb(1,0,0,0.5))
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

3. Mean and median of the total number of steps taken per day:

Mean:

```r
meanSteps <- mean(stepsPerDay$steps)
print(meanSteps)
```

```
## [1] NA
```
Median:

```r
medianSteps <- median(stepsPerDay$steps)
print(medianSteps)
```

```
## [1] NA
```
##What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
stepsPerIntervalAvgByDay <- with(data,aggregate(steps,list(interval=interval),mean,na.rm=TRUE))
stepsPerIntervalAvgByDay$steps <- stepsPerIntervalAvgByDay$x
stepsPerIntervalAvgByDay$x <- NULL
with(stepsPerIntervalAvgByDay,plot(interval,steps,type='l'))
```

![](PA1_template_files/figure-html/intervalPlot-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxStepsInterval <- stepsPerIntervalAvgByDay[stepsPerIntervalAvgByDay$steps==max(stepsPerIntervalAvgByDay$steps),"interval"]
print(maxStepsInterval)
```

```
## [1] 835
```
##Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
narows<-nrow(data[is.na(data$steps),])
print(narows)
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. We use the mean for that 5-minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
cleanData <- merge(data, stepsPerIntervalAvgByDay, by = "interval")
cleanData$steps <- ifelse(is.na(cleanData$steps.x), cleanData$steps.y, cleanData$steps.x)
cleanData$steps.x<-NULL
cleanData$steps.y<-NULL
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Total number daily number of steps has increased.

```r
cleanStepsPerDay <- with(cleanData,aggregate(steps,list(date=date),sum,na.rm=TRUE))
cleanStepsPerDay$steps <- cleanStepsPerDay$x #rename a column
cleanStepsPerDay$x <- NULL
hist(cleanStepsPerDay$steps, breaks=10, col=rgb(1,0,0,0.5))
hist(stepsPerDay$steps, breaks=10, add=T, col=rgb(0,1,0,0.5))
```

![](PA1_template_files/figure-html/CleanStepsPerDay-1.png)<!-- -->

Mean:

```r
cleanMeanSteps <- mean(cleanStepsPerDay$steps)
print(cleanMeanSteps)
```

```
## [1] 10766.19
```
Median:

```r
cleanMedianSteps <- median(cleanStepsPerDay$steps)
print(cleanMedianSteps)
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library(chron)
```

```
## Warning: package 'chron' was built under R version 3.3.1
```

```r
cleanData$weekday <- ifelse(is.weekend(cleanData$date),"weekend","weekday")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data. 

```r
weekdayStepsPerIntervalAvgByDay <- with(cleanData,aggregate(steps,list(interval=interval,weekday=weekday),mean,na.rm=TRUE))
weekdayStepsPerIntervalAvgByDay$steps <- weekdayStepsPerIntervalAvgByDay$x
weekdayStepsPerIntervalAvgByDay$x <- NULL
par(mfrow=c(2,1))
with(weekdayStepsPerIntervalAvgByDay[weekdayStepsPerIntervalAvgByDay$weekday=="weekday",],plot(interval,steps,type='l',main ="Weekdays"))
with(weekdayStepsPerIntervalAvgByDay[weekdayStepsPerIntervalAvgByDay$weekday=="weekend",],plot(interval,steps,type='l', main = "Weekends"))
```

![](PA1_template_files/figure-html/plotDifference-1.png)<!-- -->
