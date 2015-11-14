# PA1_template.Rmd
Smita Kakaraddi  
November 9, 2015  
**This is R Markdown document for Reproducible Research Course - Peer Assignment 1.**

## Load and Process data

####Download data(activity.csv) required for this assignment to working directory. 


```r
library(dplyr)
library(plyr)
library(data.table)
library(tidyr)
library(ggplot2)
```


```r
act_data<-read.csv("activity.csv")
act_data<-mutate(act_data,date=as.Date(date)) # change date field type
head(act_data)
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

##  Mean total number of steps taken per day

####1. Calculate summary of the total number of steps taken each day and then make a histogram of the results 


```r
total_steps <- with(act_data, aggregate(steps, by = list(date), sum,na.rm = FALSE))
setnames(total_steps, names(total_steps), c( "Date", "Steps"))
hist(total_steps$Steps, xlab="Total number of steps per day", main=NULL, col="pink")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

***
####2. Calculate and report mean and median total number of steps taken per day


```r
mean_total_steps <- mean(total_steps$Steps, na.rm = TRUE)
mean_total_steps
```

```
## [1] 10766.19
```

```r
median_total_steps <- median(total_steps$Steps, na.rm = TRUE)
median_total_steps
```

```
## [1] 10765
```
***
## Average daily activity pattern

####1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval_steps <- with(act_data, aggregate(steps, by = list(interval), FUN="mean",na.rm = TRUE))
setnames(interval_steps, names(interval_steps), c( "Interval", "Steps"))
plot(interval_steps$Interval,interval_steps$Steps, ylab="Average number of steps per interval",xlab="Interval", main=NULL, type="l", lwd=2, col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

***
####2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_interval_steps <- max(interval_steps$Steps)
max_interval <- interval_steps[interval_steps$Steps==max_interval_steps,1]
```
   Maximum number of steps/interval on average is **206.1698113** and corresponding Interval is **835**.

***
## Imputing missing values

####1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
na<-sum(is.na(act_data))
```
There are **2304**  rows with NAs in this dataset

***
####2.Devise a strategy for filling in all of the missing values in the dataset.Strategy used here is the mean for that 5-minute interval.Make a histogram of the result


```r
setnames(interval_steps, names(interval_steps), c( "interval", "steps")) # renaming the columns names ( remoed caps)
newdata<- inner_join(act_data,interval_steps,by=c("interval"="interval"))
newdata<-mutate(newdata,steps.x=ifelse(is.na(steps.x),steps.y,steps.x)) 
newdata<-select(newdata,-steps.y)
setnames(newdata,names(newdata),c("steps","date","interval"))
newdata1 <- with(newdata, aggregate(steps, by = list(date), sum))
setnames(newdata1,names(newdata1),c("date","steps"))
hist(newdata1$steps, xlab="Total number of steps per day", main=NULL, col="orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

***
####3.Calculate and report the mean and median total number of steps taken per day of newdata and comapre it to first part of assignment


```r
mean_newdata1 <- mean(newdata1$steps)
mean_newdata1
```

```
## [1] 10766.19
```

```r
median_newdata1 <- median(newdata1$steps)
median_newdata1
```

```
## [1] 10766.19
```
There is no difference in the both mean values.
Difference between new median value previous median value( **10765** )is very small. Hence the conclusion is that there were very few missing values and the impact of imputing missing data is minimal.

***
## Are there differences in activity patterns between weekdays and weekends?

**Use whether a given day is weekday or weekend**

```r
newdata$date <- strptime(newdata$date, "%Y-%m-%d")
newdata <- mutate(newdata, day = weekdays(date))

for (i in 1:length(newdata$day)) 
  {
    if (newdata[i, 4] == "Saturday" | newdata[i, 4] == "Sunday") {
        newdata[i, 4] <- "weekend"
        
    } else {
        newdata[i, 4] <- "weekday"
        
    }
}

head(summary)
```

```
##                          
## 1 function (object, ...) 
## 2 UseMethod("summary")
```

**Calcute average number of steps taken, averaged across all weekday days or weekend days and create time series plot (type = "l" )**


```r
summary <- aggregate(newdata$steps, list(interval = newdata$interval, day = newdata$day), 
    mean)
names(summary) <- c("interval", "day", "steps")

ggplot(data=summary, aes(x=interval, y=steps, group=day)) + geom_line(aes(color=day))+ facet_wrap(~ day, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 


We can conclude from above plots that there is more activity on weekends than weekdays.


