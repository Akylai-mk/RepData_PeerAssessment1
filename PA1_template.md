---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: true
---

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

## Loading and preprocessing the data


```r
library(readr)
activity <- readr::read_csv(unzip("activity.zip", "activity.csv"))
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
activityData <- transform(activity,day=as.Date(as.Date(date),"%Y%m%d"))
```

## What is mean total number of steps taken per day?
### 2.1. Make a histogram of the total number of steps taken each day


```r
daily <- aggregate(steps~day, data=activityData,FUN=sum)
Sys.setlocale("LC_TIME", "English") ##system language is Russian, need to switch it to English
```

```
## [1] "English_United States.1252"
```

```r
library(ggplot2)
ggplot(daily, aes(day, steps)) + 
        geom_histogram(stat="identity") + 
        labs(x = "day", y = "Total number of steps", title = "Total number of steps taken each day")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### 2.2. Calculate and report the **mean** and **median** total number of steps taken per day
First, let's calculate **mean** total number of steps taken per day


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
activityData %>% group_by (day) %>% 
        summarise(steps = sum(steps, na.rm=TRUE)) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 1 x 1
##   steps
##   <dbl>
## 1 9354.
```

And now let's calculate **median** total number of steps taken per day


```r
activityData %>% group_by (day) %>% 
        summarise(steps = sum(steps, na.rm=TRUE)) %>%
        summarise(steps = median(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 1 x 1
##   steps
##   <dbl>
## 1 10395
```

## What is the average daily activity pattern?


```r
intervalData <- activityData %>% group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(intervalData, plot(interval, steps, type = "l", pch=20, main = "average daily activity pattern"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5-minute interval contains the maximum number of steps?

```r
intervalData[order(intervalData$steps, decreasing = TRUE),]
```

```
## # A tibble: 288 x 2
##    interval steps
##       <dbl> <dbl>
##  1      835  206.
##  2      840  196.
##  3      850  183.
##  4      845  180.
##  5      830  177.
##  6      820  171.
##  7      855  167.
##  8      815  158.
##  9      825  155.
## 10      900  143.
## # ... with 278 more rows
```

ANSWER: 835 (8:35)

## Imputing missing values
Calculate and report the total number of missing values


```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset.
*strategy: let's take mean for that 5-minute interval for all other days*

Create a new dataset with the missing data filled in.


```r
imputed.value <- function(steps, interval) {
        inputed <- NA
        if (!is.na(steps))
                inputed <- c(steps)
        else
                inputed <- (intervalData[intervalData$interval==interval, "steps"])
        return(inputed)
}
activityDataNew <- activityData
activityDataNew$steps <- mapply(imputed.value, activityDataNew$steps, activityDataNew$interval)
activityDataNew$steps <- as.numeric(activityDataNew$steps)
```

Make a histogram of the total number of steps taken each day


```r
dailyNew <- aggregate(steps~day, data=activityDataNew,FUN=sum)
ggplot(dailyNew, aes(day, steps)) + 
        geom_histogram(stat="identity") + 
        labs(x = "day", y = "Total number of steps", title = "Total number of steps taken each day")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

**mean** total number of steps taken per day   


```r
activityDataNew %>% group_by (day) %>% 
        summarise(steps = sum(steps, na.rm=TRUE)) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 1 x 1
##    steps
##    <dbl>
## 1 10766.
```

**median** total number of steps taken per day


```r
activityDataNew %>% group_by (day) %>% 
        summarise(steps = sum(steps, na.rm=TRUE)) %>%
        summarise(steps = median(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 1 x 1
##    steps
##    <dbl>
## 1 10766.
```

**mean** and **median** are higher than the estimates from the first part of the assignment.
Imputing missing data is increasing the estimates of the total daily number of steps

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels, "weekday" and "weekend"


```r
activityDataNew$week <- weekdays(activityDataNew$date)
activityDataNew$factor <- ifelse(activityDataNew$week == "Saturday" | 
                                         activityDataNew$week == "Sunday",
                                 "weekend", "weekday")
```

Make a panel plot 


```r
intervalDataWeekend <- activityDataNew %>% 
        subset(factor=="weekend") %>%
        group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
intervalDataWeekday <- activityDataNew %>% 
        subset(factor=="weekday") %>%
        group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
par(mfrow = c(2,1), mar=c(2,2,2,1))
with(intervalDataWeekend, plot(interval, steps, type = "l", pch=20, col="blue", main = "weekend"))
with(intervalDataWeekday, plot(interval, steps, type = "l", pch=20, col="blue", main = "weekday"))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
