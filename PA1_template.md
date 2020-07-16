---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
setwd("~/desktop/Coursera/Reproducible Research/Course Project 1")
rm(list=ls())

library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
## ✓ tibble  3.0.2     ✓ dplyr   1.0.0
## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```



## Loading and preprocessing the data
1. Load the data

```r
activity <- read.csv("activity.csv")
glimpse(activity)
```

```
## Rows: 17,568
## Columns: 3
## $ steps    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ date     <chr> "2012-10-01", "2012-10-01", "2012-10-01", "2012-10-01", "201…
## $ interval <int> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 105, 110,…
```

```r
head(activity)
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
For this part of the assignment, ignore the missing values in the dataset

```r
newactivity <- na.omit(activity)

head(newactivity)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
tail(newactivity)
```

```
##       steps       date interval
## 17275     0 2012-11-29     2330
## 17276     0 2012-11-29     2335
## 17277     0 2012-11-29     2340
## 17278     0 2012-11-29     2345
## 17279     0 2012-11-29     2350
## 17280     0 2012-11-29     2355
```

```r
summary(newactivity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:15264       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0
```

1. Calculate the total number of steps taken per day.

```r
stepsdata <- newactivity %>% 
    group_by(date) %>% 
    summarise(
        dailySum = sum(steps)
    )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

2. Make a histogram of the total number of steps taken each day

```r
ggplot(stepsdata, aes(x=dailySum)) +
    geom_histogram(color="darkred", fill="lightpink") + 
    labs(x = "Total Number of Steps",
         y = "Count")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median total number of steps taken per day

```r
stepsdata <- newactivity %>% 
    group_by(date) %>% 
    summarise(
        dailySum = sum(steps)
    )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
stepmean <- mean(stepsdata$dailySum)
stepmedian <- median(stepsdata$dailySum)
```
The mean total number of steps taken per day is `stepmean`.
The median total number of steps taken per day is `stepmedian`.


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5 minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)

```r
adap <- newactivity %>% 
    group_by(interval) %>% 
    summarise(
        stepsTaken = mean(steps)
    )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(adap, aes(x=interval, y=stepsTaken)) + 
    geom_line()+ 
    labs(x = "Time Interval",
         y = "Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

```r
by_stepsTaken <- adap %>% 
    arrange(desc(stepsTaken))

head(by_stepsTaken)
```

```
## Warning: `...` is not empty.
## 
## We detected these problematic arguments:
## * `needs_dots`
## 
## These dots only exist to allow future extensions and should be empty.
## Did you misspecify an argument?
```

```
## # A tibble: 6 x 2
##   interval stepsTaken
##      <int>      <dbl>
## 1      835       206.
## 2      840       196.
## 3      850       183.
## 4      845       180.
## 5      830       177.
## 6      820       171.
```

```r
maxinterval <- by_stepsTaken[1,1]
maxsteps <- by_stepsTaken[1,2]
```
The 5-minute interval that contains the maximum number of steps is interval 
`maxinterval` which cotains `maxsteps` steps.



## Imputing missing values
1. Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)

```r
totalna <- sum(is.na(activity$steps))
```
There are a total of `totalna` missing values in the data set.

2. Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could 
use the mean/median for that day, or the mean for that 5-minute interval, etc.

I used the k nearest neighbors algorithm can be used for imputing missing data 
by finding the k closest neighbors to the observation with missing data and then
imputing them based on the the non-missing values in the neighbors. 
There are several possible approaches to this. You can use 1NN schema, where 
you find the most similar neighbor and then use its value as a missing data 
replacement. Alternatively you can use kNN, with k neighbors and take mean of
the neighbors, or weighted mean, where the distances to neighbors are used as
weights, so the closer neighbor is, the more weight it has when taking the 
mean. Using weighted mean seems to be used most commonly.
[More information can be found here.]( https://stats.stackexchange.com/questions/327074/k-nearest-neighbor-imputation-explanation
)


3. Create a new dataset that is equal to the original dataset but with the 
missing data filled in.

```r
library(VIM)
```

```
## Loading required package: colorspace
```

```
## Loading required package: grid
```

```
## VIM is ready to use.
```

```
## Suggestions and bug-reports can be submitted at: https://github.com/statistikat/VIM/issues
```

```
## 
## Attaching package: 'VIM'
```

```
## The following object is masked from 'package:datasets':
## 
##     sleep
```

```r
imputedActivity <- kNN(activity, variable = "steps")
imputedActivity <- imputedActivity[, 1:3]

summary(imputedActivity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 36.59                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0
```

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
imputedStepsData <- imputedActivity %>% 
    group_by(date) %>% 
    summarise(
        imputedDailySum = sum(steps)
    )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(imputedStepsData, aes(x = imputedDailySum)) + 
    geom_histogram(color = "darkblue", fill = "lightblue") + 
    labs(x = "Total Number of Steps", 
         y = "Count")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Mean and Median

```r
mean(imputedStepsData$imputedDailySum)
```

```
## [1] 10537.97
```

```r
median(imputedStepsData$imputedDailySum)
```

```
## [1] 10600
```

```r
mean(stepsdata$dailySum)
```

```
## [1] 10766.19
```

```r
median(stepsdata$dailySum)
```

```
## [1] 10765
```
The imputed data mean and median valuse do differ from the values found in the first part of the assignment. When imputing the missing values using the knn method, both the mean and median are lower that the valuse found in the first part of this assignment. I believe this is from adding in more data it has caused a shift to a lower mean and median


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” 
and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityDay <- activity %>% 
    mutate(day = weekdays(as.Date(date)), 
           week = as.factor(
               ifelse(day %in% c("Saturday", "Sunday"),"weekend", "weekday")))

glimpse(activityDay)
```

```
## Rows: 17,568
## Columns: 5
## $ steps    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
## $ date     <chr> "2012-10-01", "2012-10-01", "2012-10-01", "2012-10-01", "201…
## $ interval <int> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 105, 110,…
## $ day      <chr> "Monday", "Monday", "Monday", "Monday", "Monday", "Monday", …
## $ week     <fct> weekday, weekday, weekday, weekday, weekday, weekday, weekda…
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

To complete this task, I sorted the data by interval and by week and then calculated the mean number of steps (and also removed the NA values)

```r
adap2 <- activityDay %>% 
    group_by(interval, week) %>% 
    summarise(
        stepsTaken = mean(steps, na.rm = TRUE)
    )
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
head(adap2)
```

```
## Warning: `...` is not empty.
## 
## We detected these problematic arguments:
## * `needs_dots`
## 
## These dots only exist to allow future extensions and should be empty.
## Did you misspecify an argument?
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval week    stepsTaken
##      <int> <fct>        <dbl>
## 1        0 weekday      2.33 
## 2        0 weekend      0    
## 3        5 weekday      0.462
## 4        5 weekend      0    
## 5       10 weekday      0.179
## 6       10 weekend      0
```

```r
tail(adap2)
```

```
## Warning: `...` is not empty.
## 
## We detected these problematic arguments:
## * `needs_dots`
## 
## These dots only exist to allow future extensions and should be empty.
## Did you misspecify an argument?
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval week    stepsTaken
##      <int> <fct>        <dbl>
## 1     2345 weekday      0.205
## 2     2345 weekend      1.86 
## 3     2350 weekday      0.308
## 4     2350 weekend      0    
## 5     2355 weekday      1.46 
## 6     2355 weekend      0
```

Created time series plots of the 5 minute intervals (x-axis) and the average
number of steps taken (y-axis) for each the weekday and weekend

```r
ggplot(adap2, aes(x=interval, y=stepsTaken, group = week, color = week)) + 
    geom_line() + 
    facet_grid(week ~.) + 
    theme(legend.position = "none") + 
    labs(x = "5-minute Intervals",
         y = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->




