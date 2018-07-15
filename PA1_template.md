---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = TRUE)
```


## What is mean total number of steps taken per day?

```r
stepsPerDay <- sapply(split(data$steps, data$date), sum)
hist(stepsPerDay, 
     col = "green", 
     main = "Number of steps per day", 
     xlab = "Number of steps",
     ylab = "Days",
     breaks = 20,
     ylim = c(0, 11),
     xlim = c(0, 23000))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(stepsPerDay, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
avgStepsPerInterval <- sapply(split(data$steps, data$interval), mean, na.rm = TRUE)
plot(avgStepsPerInterval, 
     col = "red",
     type = "l", 
     main = "Number of steps per 5min Interval",
     xlab = "5min interval", 
     ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
names(avgStepsPerInterval)[avgStepsPerInterval == max(avgStepsPerInterval)]
```

```
## [1] "835"
```


## Imputing missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
dataFilled <- data
for (i in 1:nrow(dataFilled)) {
  if (is.na(dataFilled[i, "steps"])) {
    interval = as.character(dataFilled[i, "interval"])
    dataFilled[i, "steps"] = avgStepsPerInterval[interval]
  }
}

totalStepsPerDay <- sapply(split(dataFilled$steps, dataFilled$date), sum)
hist(totalStepsPerDay, 
     col = "blue", 
     main = "Number of steps per day (NAs filled)", 
     xlab = "Number of steps",
     ylab = "Days",
     breaks = 20,
     ylim = c(0, 12),
     xlim = c(0, 23000))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(totalStepsPerDay, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalStepsPerDay, na.rm = TRUE)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
dataFilled$weekday <- weekdays(as.Date(dataFilled$date, format = "%Y-%m-%d"))
dataFilled$dayType <- factor(ifelse(dataFilled$weekday == "Sunday" | dataFilled$weekday == 
                                           "Saturday", "weekend", "weekday"), levels = c("weekday", "weekend"))

dataFilledWeekdays  <- dataFilled[dataFilled$dayType == "weekday", ]
dataFilledWeekend   <- dataFilled[dataFilled$dayType == "weekend", ]
avStepsPerIntWeekdays <- sapply(split(dataFilledWeekdays$steps, dataFilledWeekdays$interval), mean)
avStepsPerIntWeekend  <- sapply(split(dataFilledWeekend$steps, dataFilledWeekend$interval), mean)

par(mfrow = c(2, 1), mar = c(4, 5, 2, 2))
plot(avStepsPerIntWeekend, 
     type = "l", 
     col = "blue",
     xlab = "",
     ylab = "Avg. steps",
     main = "weekend")

plot(avStepsPerIntWeekdays, 
     type = "l", 
     col = "red",
     xlab = "5min interval",
     ylab = "Avg. steps",
     main = "weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
