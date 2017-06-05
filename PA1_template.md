# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lattice)
data <- read.csv("./activity.csv", header = TRUE, sep=",", stringsAsFactors = FALSE,colClasses = c("numeric","Date","numeric"))
```

## What is mean total number of steps taken per day?
##### 1. Calculate the total number of steps taken per day

```r
steppd <- aggregate(data$steps, list(date=data$date), sum, na.rm=TRUE)
```

##### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(steppd$x, main="steps per day", xlab="steps", ylab="days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

##### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(steppd$x)
median_steps <- median(steppd$x)
```

## What is the average daily activity pattern?
##### 1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steppi <- aggregate(x=list(meanSteps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)

ggplot(data=steppi, aes(x=interval, y=meanSteps)) +
   geom_line() +
   xlab("five min interval") +
   ylab("average steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steppi[which.max(steppi$meanSteps), 1]
```

```
## [1] 835
```
## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
num_miss_val <- length(which(is.na(data$steps)))
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
data_na_rm <- data
avg_interval <- tapply(data_na_rm$steps, data_na_rm$interval, mean, na.rm=TRUE, simplify=TRUE)
data_na_rm$steps[is.na(data_na_rm$steps)] <- avg_interval[as.character(data_na_rm$interval[is.na(data_na_rm$steps)])]
```

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_na_rm$steps[is.na(data_na_rm$steps)] <- avg_interval[as.character(data_na_rm$interval[is.na(data_na_rm$steps)])]
```

##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

```r
steppd_na_rm <- aggregate(data_na_rm$steps, list(date=data$date), sum, na.rm=TRUE)

hist(steppd_na_rm$x, main="steps per day", xlab="steps", ylab="days")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean_steps_na_rm <- mean(steppd_na_rm$x)
median_steps_na_rm <- median(steppd_na_rm$x)
```

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data_na_rm$date <- as.Date(data_na_rm$date)
data_weekday <- data_na_rm
data_weekday <- mutate(data_weekday, weektype = ifelse(weekdays(data_weekday$date) == "Saturday" | weekdays(data_weekday$date) == "Sunday", "weekend", "weekday"))
```

##### 2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
stepwd <- aggregate(data_weekday$steps, by=list(data_weekday$date, 
                                            data_weekday$weektype, data_weekday$interval), mean)
names(stepwd) <- c("date", "weekday", "interval", "mean_step")
xyplot(mean_step ~ interval | weekday, stepwd, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
