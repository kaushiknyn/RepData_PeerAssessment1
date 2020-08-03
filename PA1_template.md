---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
#Unzip the file
unzip(zipfile = "activity.zip")

#Read in the csv file
dat <- read.csv(file = "activity.csv",header = TRUE)
```

## What is mean total number of steps taken per day?


```r
#Install the dplyr package
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
#Create a data frame with total steps per day
steps_per_day <- steps_per_day <- dat %>% group_by(date) %>% summarise(total_steps = sum(steps))

#Create a histogram of the total number of steps taken per dat

hist(steps_per_day$total_steps, main = "Histogram of total steps per day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/calculatesteps-1.png)<!-- -->
<br>

###Calculate the mean and median of the total steps per day

<br>

### Mean total steps:

```r
mean(steps_per_day$total_steps, na.rm = T)
```

```
## [1] 10766.19
```

<br>

### Meadian total steps:

```r
median(steps_per_day$total_steps, na.rm = T)
```

```
## [1] 10765
```

<br>
<br>

## What is the average daily activity pattern?


```r
##Create a data frame with average steps per by time interval (after removing the NA values)

time_series <- dat %>% group_by(interval) %>% summarise(avg_steps_per_day = mean(steps, na.rm = T))

#Creating the time series plot
library(lattice)
xyplot(time_series$avg_steps_per_day ~ time_series$interval, type = "l", main = "Average daily pattern",ylab = "Average steps per day",xlab = "5-minute interval")
```

![](PA1_template_files/figure-html/avgdailypattern-1.png)<!-- -->

```r
#Finding the interval that on average across all the days in the dataset, contains the maximum number of steps

max_interval <- time_series$interval[which.max(time_series$avg_steps_per_day)]
```
<br>

#### The 5-minute interval, which on average across all the days in the dataset, contains the maximum number of steps is **835**

<br>
<br>

## Inputing missing values


```r
#Calculating total number of rows with missing values

missing_values <- sum(is.na(dat))
```
<br>

#### There are **2304** rows with missing values

<br>


```r
#Creating a loop to replace NA's and creating a new dataset out of it

dat2 <- dat
i = nrow(dat)
val = 1
while(val <= i){
     if(is.na(dat2[val, "steps"]) == TRUE)
       dat2[val, "steps"] <- time_series$avg_steps_per_day[which(dat2[val, "interval"] == time_series$interval)]
     val = val + 1
}

#Create a data frame from the new dataset with total steps per day
steps_per_day_2 <- dat2 %>% group_by(date) %>% summarise(total_steps = sum(steps))

#Create a histogram of the total number of steps taken per dat

hist(steps_per_day_2$total_steps, main = "Histogram of total steps per day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/replacena-1.png)<!-- -->
<br>
###Calculate the mean and median of the total steps per day

<br>

### Mean total steps:

```r
mean(steps_per_day_2$total_steps, na.rm = T)
```

```
## [1] 10766.19
```
<br>

### Meadian total steps:


```r
median(steps_per_day_2$total_steps, na.rm = T)
```

```
## [1] 10766.19
```
<br>

### *"Even though the frequency of the steps per day has increased after replacing the NA values with the mean values, there is no impact on the mean and the median of the total nbumber of steps itself."*

<br>
<br>

## Are there differences in activity patterns between weekdays and weekends?


```r
#Adding a new factor variable to dat2 to differentiate between weekdays and weekends

dat2$date <- as.Date(dat$date)

#Create a vector for weekdays
weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

dat2$WeekDay <- factor((weekdays(dat2$date) %in% weekdays1), levels = c(FALSE, TRUE), labels = c("Weekend", "Weekday"))

#Creating a new data frame to have the average steps by weekday/weekend and 5 minute intervale

time_series_2 <- dat2 %>% group_by(interval, WeekDay) %>% summarise(average_steps = mean(steps))

#Creating a panel plot
library(lattice)
xyplot(average_steps ~ interval | WeekDay, data = time_series_2, layout = c(1, 2), type = "l", ylab = "Number of steps", xlab = "Interval")
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->
<br>

### Based on the time series analysis, there does not seem to be a major difference of the average number of steps between weekdays and weekends
