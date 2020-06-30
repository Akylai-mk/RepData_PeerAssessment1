# R script for RepData_PeerAssignment1
## data is taken from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
## and unzipped into current working directory

### 1.Loading and preprocessing the data
library(readr)
activity <- readr::read_csv(unzip("activity.zip", "activity.csv"))
names(activity)
dim(activity)  ## 17568 rows of 3 columns
### process data if needed
activityData <- transform(activity,day=as.Date(as.Date(date),"%Y%m%d"))

### 2. What is mean total number of steps taken per day? (IGNORE NAs)
### 2.1. Make a histogram of the total number of steps taken each day
daily <- aggregate(steps~day, data=activityData,FUN=sum)
Sys.setlocale("LC_TIME", "English") ##system language is Russian, need to switch it to English
library(ggplot2)
ggplot(daily, aes(day, steps)) + 
        geom_histogram(stat="identity") + 
        labs(x = "day", y = "Total number of steps", title = "Total number of steps taken each day")
### 2.2. Calculate and report the **mean** and **median** total number of steps taken per day
### **mean** total number of steps taken per day
library(dplyr)
activityData %>% group_by (day) %>% 
        summarise(steps = sum(steps, na.rm=TRUE)) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
### **median** total number of steps taken per day
activityData %>% group_by (day) %>% 
        summarise(steps = sum(steps, na.rm=TRUE)) %>%
        summarise(steps = median(steps, na.rm = TRUE))

### 3. What is the average daily activity pattern?
### 3.1. Make a time series plot of the 5-minute interval and the average number of steps taken
intervalData <- activityData %>% group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
with(intervalData, plot(interval, steps, type = "l", pch=20))
###3.2. Which 5-minute interval contains the maximum number of steps?
intervalData[order(intervalData$steps, decreasing = TRUE),]
### ANSWER: 835 (8:35)

### 4. Imputing missing values
### 4.1. Calculate and report the total number of missing values
sum(is.na(activityData$steps)) ##get number of NA in activity dataset
### 4.2.Devise a strategy for filling in all of the missing values in the dataset.
## strategy: let's take mean for that 5-minute interval 
### 4.3. Create a new dataset with the missing data filled in.
inputed.value <- function(steps, interval) {
        inputed <- NA
        if (!is.na(steps))
                inputed <- c(steps)
        else
                inputed <- (intervalData[intervalData$interval==interval, "steps"])
        return(inputed)
}
activityDataNew <- activityData
activityDataNew$steps <- mapply(inputed.value, activityDataNew$steps, activityDataNew$interval)
activityDataNew$steps <- as.numeric(activityDataNew$steps)
### 4.4. Make a histogram of the total number of steps taken each day
dailyNew <- aggregate(steps~day, data=activityDataNew,FUN=sum)
ggplot(dailyNew, aes(day, steps)) + 
        geom_histogram(stat="identity") + 
        labs(x = "day", y = "Total number of steps", title = "Total number of steps taken each day")


library(tidyr)
daily$type <- "old"
dailyNew$type <- "new"
dailyMerged <- rbind(daily, dailyNew)
dailyMerged %>% 
        ggplot(aes(day, steps,fill=type)) + 
        geom_histogram(stat="identity", position="dodge") +
        labs(x = "day", y = "Total number of steps", title = "Total number of steps taken each day")
### 4.5 **mean** and **median** total number of steps taken per day   
activityDataNew %>% group_by (day) %>% 
        summarise(steps = sum(steps, na.rm=TRUE)) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
### **median** total number of steps taken per day
activityDataNew %>% group_by (day) %>% 
        summarise(steps = sum(steps, na.rm=TRUE)) %>%
        summarise(steps = median(steps, na.rm = TRUE))
##**mean** and **median** are higher than the estimates from the first part of the assignment.
##imputing missing data is increasing the estimates of the total daily number of steps

### 5. Are there differences in activity patterns between weekdays and weekends?
###5.1. Create a new factor variable in the dataset with two levels, "weekday" and "weekend"
activityDataNew$week <- weekdays(activityDataNew$date)
activityDataNew$factor <- ifelse(activityDataNew$week == "Saturday" | 
                                         activityDataNew$week == "Sunday",
                                 "weekend", "weekday")
###5.2 Make a panel plot 
intervalDataWeekend <- activityDataNew %>% 
        subset(factor=="weekend") %>%
        group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))

intervalDataWeekday <- activityDataNew %>% 
        subset(factor=="weekday") %>%
        group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
par(mfrow = c(2,1), mar=c(2,2,2,1))
with(intervalDataWeekend, plot(interval, steps, type = "l", pch=20, col="blue", main = "weekend"))
with(intervalDataWeekday, plot(interval, steps, type = "l", pch=20, col="blue", main = "weekday"))
