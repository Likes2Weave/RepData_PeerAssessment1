---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
setwd("~/GitHub/datasciencecoursera/Course5/RepData_PeerAssessment1")
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

## Loading and preprocessing the data

```r
readLines("activity.csv", n=5)
```

```
## [1] "\"steps\",\"date\",\"interval\"" "NA,\"2012-10-01\",0"            
## [3] "NA,\"2012-10-01\",5"             "NA,\"2012-10-01\",10"           
## [5] "NA,\"2012-10-01\",15"
```

When viewing the first few lines of the raw data, you see that activity.csv includes a header with three columns (steps, date, interval), dates are strings in the form year-month-day (%Y-%m-%d) and missing values are tracked as NA. Set header to true to preserve column names.

To be able to organize data by time intervals such as day or month, convert the character representation of date to date class.


```r
## Read
activity <- read.csv("activity.csv", header=TRUE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
## Convert characters to date
activity <- transform(activity, date=as.Date(date))
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
Some intervals and days display NA values for steps. For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps per day. Use the total steps per day to create the histogram of the total steps taken per day.


```r
SummarySteps <- select (activity, date, steps) %>%
    group_by(date) %>%
    summarise(total_steps=sum(steps, na.rm = TRUE))

with (SummarySteps, hist(total_steps, 
                         main="Histogram of Steps per day",
                         xlab = "Total Steps each Day"))
```

![](PA1_template_files/figure-html/Histogram-1.png)<!-- -->

Calculate the mean and median of the total number of steps taken per day. Display the results.


```r
SummarySteps <- summarise(SummarySteps, 
                           mean_steps=mean(total_steps),
                           median_steps=median(total_steps))

print(SummarySteps)
```

```
## # A tibble: 1 x 2
##   mean_steps median_steps
##        <dbl>        <int>
## 1      9354.        10395
```

## What is the average daily activity pattern?

Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
SummaryInterval <- activity %>%
    group_by(interval) %>%
    summarise(mean_steps=mean(steps, na.rm=TRUE))

with(SummaryInterval, 
     plot(interval, mean_steps, type="l", 
          main = "Average Daily Activity Pattern"))
```

![](PA1_template_files/figure-html/PlotByInterval-1.png)<!-- -->
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
WhichIntervalMax <- which.max(SummaryInterval$mean_steps)
```

Interval 104 contains the maximum number of steps. 

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
