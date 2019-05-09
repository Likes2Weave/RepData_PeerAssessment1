---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

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
## Check for a header and delimiters without reading the whole file
readLines("activity.csv", n=3)
```

```
## [1] "\"steps\",\"date\",\"interval\"" "NA,\"2012-10-01\",0"            
## [3] "NA,\"2012-10-01\",5"
```

When viewing the first few lines of the raw data, you see that activity.csv includes a header with three columns (steps, date, interval), dates are strings in the form year-month-day (%Y-%m-%d) and missing values are tracked as NA. Set header to true to preserve column names.


```r
## Read file and review data.frame
activity <- read.csv("activity.csv", header=TRUE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

With the factor date's 61 levels, can assume that the data covers 61 days. To be able to organize data by time intervals such as day or month, convert the character representation of date to date class.


```r
## Convert characters to date and verify
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

Use the total steps per day to create the histogram of the total steps taken per day.


```r
## summarize (sum) steps and plot histogram
SummarySteps <- select (activity, date, steps) %>%
    group_by(date) %>%
    summarise(total_steps=sum(steps, na.rm = TRUE))

with (SummarySteps, hist(total_steps, 
                         main="Histogram of Steps per day",
                         xlab = "Total Steps each Day"))
```

![](PA1_template_files/figure-html/Histogram-1.png)<!-- -->

Calculate the mean and median of the total number of steps taken per day. 


```r
MeanAndMedian <- summarise(SummarySteps, 
                           mean_steps=mean(total_steps),
                           median_steps=median(total_steps))

print(MeanAndMedian)
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
## summarize (mean) and plot.
## calculate median, save for later when needed to impute NAs
SummaryInterval <- activity %>%
    group_by(interval) %>%
    summarise(mean_steps=mean(steps, na.rm=TRUE),
              median_steps=median(steps, na.rm=TRUE))

with(SummaryInterval, 
     plot(interval, mean_steps, type="l", 
          main = "Average Daily Activity Pattern"))
```

![](PA1_template_files/figure-html/PlotByInterval-1.png)<!-- -->

```r
## Find interval with highest averge steps
WhichInterval <- which.max(SummaryInterval$mean_steps)
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? Interval 104 contains the maximum number of steps. 

## Imputing missing values

As mentioned earlier, there are a number of days/intervals where there are missing values coded as NA. The presence of missing days may introduce bias into some calculations or summaries of the data.

Previously, showed that just steps has NA values.Total number of rows with missing values is 2304.

Fill in missing data with the mean steps for the given interval. Doing this was not the most highly recommended, but it is simple.(Based on a quick scan of the articles available at https://github.com/lgreski/datasciencectacontent/blob/master/markdown/gen-handlingMissingValues.md)

Curious? Also try filling missing data with the median steps for the given interval.

Create a new dataset that is equal to the original dataset but with the missing data replaced by the mean (or median) for the given interval.


```r
## Fill with mean
activityNoNA_mean <- activity %>% 
    mutate(steps = ifelse(is.na(steps),
                SummaryInterval$mean_steps[
                    SummaryInterval$interval==activity$interval],steps))

## Fill with median
activityNoNA_median <- activity %>% 
    mutate(steps = ifelse(is.na(steps),
                SummaryInterval$median_steps[
                    SummaryInterval$interval==activity$interval],steps))
```

Make a histogram of the total number of steps taken each day. Recalculate the number of steps per day, first.  Standardize the y-axis range to make comparisons easier to see.


```r
## summarize NA replaced by mean
SummaryStepsNoNA_mean <- select (activityNoNA_mean, date, steps) %>%
    group_by(date) %>%
    summarise(total_steps=sum(steps, na.rm = TRUE))

## summarize NA replaced by median
SummaryStepsNoNA_median <- select (activityNoNA_median, date, steps) %>%
    group_by(date) %>%
    summarise(total_steps=sum(steps, na.rm = TRUE))

## print histograms side by side for comparison
par(mfrow=c(1,3))

with (SummarySteps, hist(total_steps, col="blue", 
                         ylim = c(0,30),
                         main="Steps/day (Original)"))

with (SummaryStepsNoNA_mean, hist(total_steps, 
                                  ylim = c(0,30),
                                  main="Steps/day (NA to mean)"))

with (SummaryStepsNoNA_median, hist(total_steps, 
                                    ylim = c(0,30),
                                    main="Steps/day (NA to median)"))
```

![](PA1_template_files/figure-html/Histogram-2-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day. 


```r
MeanAndMedianNoNA_mean <- summarise(SummaryStepsNoNA_mean, 
                           mean_steps=mean(total_steps),
                           median_steps=median(total_steps))
MeanAndMedianNoNA_median <- summarise(SummaryStepsNoNA_median, 
                           mean_steps=mean(total_steps),
                           median_steps=median(total_steps))

print(MeanAndMedianNoNA_mean)
```

```
## # A tibble: 1 x 2
##   mean_steps median_steps
##        <dbl>        <dbl>
## 1      9531.        10439
```

```r
print(MeanAndMedianNoNA_median)
```

```
## # A tibble: 1 x 2
##   mean_steps median_steps
##        <dbl>        <int>
## 1      9373.        10395
```

```r
print(MeanAndMedian)
```

```
## # A tibble: 1 x 2
##   mean_steps median_steps
##        <dbl>        <int>
## 1      9354.        10395
```

In both cases, replacing NAs with the mean or median results in new data sets with higher means. Replacing NAs with the mean also increased the resulting median. Hovever, replacing the NAs with the median had no change on the median of the new data set.

Imputing missing data using the mean seems to have had a more pronounced impact on the data in the lower step count range. However, based on the shape of the histogram, imputing with the median had less impact.

## Are there differences in activity patterns between weekdays and weekends?

Use weekdays() to create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. Use the activity dataset created by imputing NAs with the median.


```r
Weekend <- c("Sat","Sun")
# weekday or weekend?
activityByDay <- activityNoNA_median %>% 
     mutate(the_week = ifelse((weekdays(date,abbreviate=TRUE) %in% Weekend),
                              "weekend","weekday"))

## Make Factor, define levels so that "weekend" intervals will appear as the top panel
activityByDay$the_week <- factor(activityByDay$the_week, levels = c("weekend","weekday"))

## Double check
str(activityByDay)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ the_week: Factor w/ 2 levels "weekend","weekday": 2 2 2 2 2 2 2 2 2 2 ...
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). Since we could choose any plotting system, decided to use ggplot with facet_wrap.


```r
## Summarize (mean) and Plot
SummaryByDay <- activityByDay %>%
    group_by(the_week, interval) %>%
    summarise(mean_steps=mean(steps, na.rm=TRUE))

g = qplot(interval, mean_steps, data =SummaryByDay, 
          geom="path", xlab = "Interval", ylab="Number of Steps")
g + facet_wrap(~ the_week, nrow=2)
```

![](PA1_template_files/figure-html/ActivityByDay-1.png)<!-- -->

From the graphs, can see that there is a different pattern of activity on weekends and weekdays. On weekdays, there is a more pronounced spike in the number of steps between interval 500 and 100.

To really see if there is a difference between weekday and weekend activity levels, compare the total mean steps.


```r
SummaryByWeek <- SummaryByDay %>%
    group_by(the_week) %>%
    summarise(total_mean_steps=sum(mean_steps, na.rm=TRUE))
 
print(SummaryByWeek)
```

```
## # A tibble: 2 x 2
##   the_week total_mean_steps
##   <fct>               <dbl>
## 1 weekend            12407.
## 2 weekday             9951.
```

More steps on the weekends (approximately 17% more).  


## MISC
In case of MacEmergency, review:
https://github.com/lgreski/datasciencectacontent/blob/master/markdown/repData-configuringKnitrWithMarkdownOutput.md
