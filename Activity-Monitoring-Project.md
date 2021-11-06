---
title: "Reproducible Research - Project I"
author: "Aleksandra Cvetanovska"
date: "11/4/2021"
output: 
  html_document: 
    keep_md: yes
---



## Data processing & simple analysis

Firstly, we read the data in, and create a new object that has NA values removed. Then we use the cleaned data to create a histogram of total amount of steps per day


```r
amd <- read.csv("Activity Monitoring/activity.csv")
clean_amd <- amd[!is.na(amd$steps),]
aggregated_daily_steps <- aggregate(. ~ date, data=clean_amd, FUN=sum)

hist(
  aggregated_daily_steps$steps,
  col='lightblue',
  xlab='Total Number of Steps Per Day',
  main='Histogram of Total Number of Steps Per Day'
)
```

![](Activity-Monitoring-Project_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

The mean for steps per day is 1.0766189\times 10^{4}, and the median is 10765.


```r
interval_mean_steps <- aggregate(steps ~ interval, clean_amd, mean)

plot(
  ts(interval_mean_steps$steps),
  ylab='Total Number of Steps Per Interval',
  main='Time Series of Steps Per Interval',
  col='darkgreen'
)
```

![](Activity-Monitoring-Project_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The interval with the highest amount of steps is interval #835.

## Imputation of Missing Data

The number of missing values in the data set is 2304. The data imputation approach we'll take in this analysis is to calculate the mean of the steps per interval, and the use that value instead of the NA. The reason we're not using mean per day, is that in the data we have days have NA values for each interval.


```r
interval_mean_steps <- aggregate(steps ~ interval, data=clean_amd, FUN=mean)

imputed_amd <- amd;
for (i in 1:nrow(amd)) {
  if (is.na(amd[i,]$steps)) {
    imputed_amd[i,]$steps <- interval_mean_steps[which(interval_mean_steps$interval == amd[i,]$interval),]$steps
  }
}

head(imputed_amd)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Now that we've imputed values we can re-create the histogram of steps per day

```r
imputed_aggregated_daily_steps <- aggregate(. ~ date, data=imputed_amd, FUN=sum)

hist(
  aggregated_daily_steps$steps,
  col='lightblue',
  border=F,
  xlab='Total Number of Steps Per Day',
  main='Histogram of Total Number of Steps Per Day'
)
hist(
  imputed_aggregated_daily_steps$steps,
  add=T,
  col=scales::alpha('pink', .4),
  border=F
)
```

![](Activity-Monitoring-Project_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

As we can see, due to the new, imputed values we get higher values for total number of steps per day. The mean for steps per day is 10766.19, as well as the median - 10766.19. As we can notice, the mean doesn't change due to the type of imputation we performed (adding mean values). But, the median does change.


```r
imputed_amd$day_type <- ifelse(wday(imputed_amd$date) %in% c(1, 7), 'Weekend', 'Weekday')
split_by_interval_day_type <- aggregate(steps ~ interval + day_type, imputed_amd, mean)

xyplot(
  split_by_interval_day_type$steps ~ split_by_interval_day_type$interval|split_by_interval_day_type$day_type,
  layout=c(1,2),
  type="l",
  main="Average Number of Steps per Day by Interval",
  xlab="Interval",
  ylab="Average Number of Steps"
)
```

![](Activity-Monitoring-Project_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
