---
title: "Reproducible Research: Peer Assessment 1"
author: "Robert M Henke"
date: "Saturday, December 13, 2014"

output:
        html_document:
                keep_md: true
---

### Loading and preprocessing the data

Reads data from file, `activity.csv` (located in folder `repdata_data_activity` of the working directory) into a data.table.

```r
library(data.table)
```

```
## data.table 1.9.4  For help type: ?data.table
## *** NB: by=.EACHI is now explicit. See README to restore previous behaviour.
```

```r
file_path <- paste0(getwd(),"/repdata_data_activity/activity.csv")

activity_DT <- fread(file_path, stringsAsFactors=FALSE)
```

Converts column named "date"" to Date data-type and adds a column named `dayOweek` cooresponding to the day of the week defined in column `date`. 

```r
activity_DT <- activity_DT[,date:= as.Date(date, "%Y-%m-%d")][,dayOweek:=weekdays(date)]
```

###What is mean total number of steps taken per day?

Calculates total number of steps taken per day and graphs the results as a histogram.

```r
total_steps_daily <- activity_DT[, list(total_steps=sum(steps)), by="date"]

hist(total_steps_daily$total_steps, main="Daily Activity",breaks=6, xlab="Total Steps each Day", 
     ylab="Number of Days", col="grey"  , xlim = c(0,25000),ylim = c(0,35))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculates the mean and median of the total steps taken per day from the supplied dataset           

```r
mean_step_day <- as.integer(total_steps_daily[, mean(total_steps,na.rm = TRUE )])
                                 
median_step_day <- as.integer(total_steps_daily[, median(total_steps,na.rm = TRUE)])
```
**Mean = 10766**  

**Median = 10765**

###What is the average daily activity pattern?

Calculates the mean number of steps taken in each 5-minute interval across all days of the study.  

```r
mean_step_5m_interval <- activity_DT[,list(mean_steps=mean(steps,na.rm = TRUE)), by="interval"]

max_row <- mean_step_5m_interval[,which.max(mean_steps)]

max_step <- mean_step_5m_interval[max_row, interval]
```
**The maximum steps occur at the 104th 5-minute interval coresponding to interval label 835**


Plots mean steps taken for each 5-minute interval across all days vs. each 5-minute interval expressed as the time of day

```r
plot(0:287, mean_step_5m_interval$mean_steps, type = "l", main = "Mean steps per 5-minute interval", 
        ylab = "Steps", xlab = "Time of day", col="black",xaxt = "n")
        axis(side = 1, at = c(0,73,145,215,288), labels = c("00:00", "06:00", "12:00", "18:00","23:55"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

###Imputing missing values

Calculates the number of missing values.

```r
na_count <- nrow(activity_DT)- nrow(na.omit(activity_DT)) 
```
**Number of missing values= 2304**

The missing data is associated with missing weekdays. The missing data is imputated by replacing the missing weekdays with the corresponding weekday average.
A weekday (Monday,Tuesday...) average is calculated and results are added to a new column, `mean_steps_dayOweek`, to the data.table. Missing `steps` column values are replaced with the corresponding value in the `mean_steps_dayOweek` column. 

```r
mean_total_step_per_dayOweek <- activity_DT[,list(steps,date,interval,mean_steps_dayOweek=mean(steps,na.rm = TRUE)),by="dayOweek"]
                                                
mean_total_step_per_dayOweek <- mean_total_step_per_dayOweek[, steps:=ifelse(is.na(steps), mean_steps_dayOweek,steps)]
```

The columns are reorganized to match those in the source dataset.

```r
imputed_activity_DT <- mean_total_step_per_dayOweek[,list(steps,date,interval)]
```
 
Calculates total number of steps taken per day from the imputated dataset and graphs the results as a histogram.       

```r
imputed_total_steps_daily <- imputed_activity_DT[, list(total_steps=sum(steps)), by="date"]

hist(imputed_total_steps_daily$total_steps, main="Daily Activity",breaks=6, xlab="Total Steps Each Day", 
     ylab="Number of Days", col="purple"  , xlim = c(0,25000),ylim = c(0,35))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Calculates the mean and median of the total steps taken per day from the imputated dataset    

```r
Imean_step_per_day <- as.integer(imputed_total_steps_daily[, mean(total_steps, na.rm = TRUE)])

Imedian_step_per_day <- as.integer(imputed_total_steps_daily[, median(total_steps,na.rm = TRUE)])
```

**Imputed Mean= 10821**

**Imputed Median= 11015**



####**SUMMARY**####

|**Dataset**| **Mean** |**Median** |
|:----------|:----:|:--------:|
|Source |10766|10765|
|Imputed|10821|11015|
|Difference|55|250|

###Are there differences in activity patterns between weekdays and weekends?

Adds two column. 1. `wk_type` denoting if `dayOweek` value is a weekday or weekend and then converts `wk_type` into a factor with 2 levels (weekday & weekend) and  2. `interval_num` representing the 5-minute interval number.

```r
 mean_total_step_per_dayOweek <- mean_total_step_per_dayOweek[, day_type:=ifelse( (dayOweek=="Saturday") | (dayOweek=="Sunday") , "weekend", "weekday")]

mean_total_step_per_dayOweek$day_type <- as.factor(mean_total_step_per_dayOweek$day_type )

mean_total_step_per_dayOweek$interval_num <- 0:287
```
Plots the activity difference between weekdays and weekends.

```r
wk_df <- mean_total_step_per_dayOweek[,list(mean_steps=mean(steps)), by="interval_num,day_type"]

library(lattice)
xyplot(mean_steps~interval_num | day_type,
       layout = c(1, 2),
       main="Mean steps per 5-minute interval",
       xlab="Time of Day",
       ylab="Number of steps",
       type="l",
       lty=1,
       group=day_type,
       col=c("red","blue"),
       scales=list(
                x=list(
                at=c(0,73,145,215,287),
                labels=c("00:00", "06:00", "12:00", "18:00","23:55") )),               
       data=wk_df)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

