---
output: 
  html_document:
    keep_md: true
---
#Project 1 of the Coursera course on Reproducible Research

##Loading libraries and data

```r
rm(list=ls())
library(ggplot2)
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
library(tidyr)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "./activity.zip")
data <- read.csv(unz("activity.zip", "activity.csv"))
```

##Investigating the data

```r
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

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

##Mean total number of steps taken per day
###The total number of steps taken per day

```r
sumOfSteps <- data %>% group_by(date) %>% summarize(totalSumOfSteps = sum(steps))
sumOfSteps
```

```
## # A tibble: 61 x 2
##    date       totalSumOfSteps
##    <fct>                <int>
##  1 2012-10-01              NA
##  2 2012-10-02             126
##  3 2012-10-03           11352
##  4 2012-10-04           12116
##  5 2012-10-05           13294
##  6 2012-10-06           15420
##  7 2012-10-07           11015
##  8 2012-10-08              NA
##  9 2012-10-09           12811
## 10 2012-10-10            9900
## # ... with 51 more rows
```

###A histogram of the total number of steps taken each day

```r
hist(sumOfSteps$totalSumOfSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

###The mean and median of the total number of steps taken per day

```r
mean(sumOfSteps$totalSumOfSteps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(sumOfSteps$totalSumOfSteps, na.rm = T)
```

```
## [1] 10765
```

###The average daily activity pattern

```r
meanStepsPerInterval <- aggregate(steps ~ interval, data, mean)

plot(meanStepsPerInterval$interval, meanStepsPerInterval$steps, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

###The 5-minute interval, which contains on average across all the days in the dataset, the maximum number of steps

```r
max(meanStepsPerInterval$steps) #206.1698 steps
```

```
## [1] 206.1698
```

```r
meanStepsPerInterval[meanStepsPerInterval$steps > 206,]
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values
###The total number of missing values in the dataset

```r
sum(is.na(data))
```

```
## [1] 2304
```

###Filling in all of the missing values in the dataset

```r
for (i in 1:nrow(data)) {
    if(is.na(data$steps[i])) {
        a <- meanStepsPerInterval$steps[which(meanStepsPerInterval$interval == data$interval[i])]
        data$steps[i] <- a
    }
}
```

###A new dataset that is equal to the original dataset but with the missing data filled in

```r
meanStepsPerInterval2 <- aggregate(steps ~ interval, data, mean)
```


###A histogram of the total number of steps taken each day and Calculate and the mean and median total number of steps taken per day. 

```r
plot(meanStepsPerInterval2$interval, meanStepsPerInterval2$steps, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
max(meanStepsPerInterval2$steps) #206.1698 steps
```

```
## [1] 206.1698
```

```r
meanStepsPerInterval2[meanStepsPerInterval2$steps > 206,]
```

```
##     interval    steps
## 104      835 206.1698
```

##The mean and median have not changed due to the imputation


##Differences in activity patterns between weekdays and weekends

```r
dataWeek <- data %>% 
    mutate(day = weekdays(as.Date(date, '%Y-%m-%d'))) %>% 
    mutate(kindOfDay = case_when(day == "zaterdag" | day == "zondag" ~ "weekend",
                                 day != "zaterdag" & day != "zondag" ~ "weekday"))
dataWeek <- aggregate(steps ~ interval+kindOfDay, dataWeek, mean)


ggplot(dataWeek, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = kindOfDay)) +
    facet_grid(kindOfDay ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("Steps")) +
    ggtitle("Steps per Interval by weekday/weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

##There are minor differences between weekdays and weekends. It seems that the maximum number of steps is more evenly spread out over the intevals. 
