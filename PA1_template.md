---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```




## What is mean total number of steps taken per day?

```r
dailyData <- aggregate(steps~date, data, sum, na.rm=TRUE)
hist(dailyData$steps, main="Initial Distribution", xlab="Steps", ylab = "Commonality", col="orange")
```

![](PA1_template_files/figure-html/dingo2-1.png)<!-- -->

```r
a <- mean(dailyData$steps, na.rm=TRUE)
b <- median(dailyData$steps, na.rm=TRUE)
```

Mean is 10766, median is 10765.





## What is the average daily activity pattern?


```r
intervalAgg <- aggregate(steps~interval, data, sum, na.rm=TRUE)

plot(intervalAgg$interval,  intervalAgg$steps, type="l")
```

![](PA1_template_files/figure-html/dingo3-1.png)<!-- -->

```r
intervalue<- subset(intervalAgg, intervalAgg$steps == max(intervalAgg$steps, na.rm=TRUE))$interval

intervalue
```

```
## [1] 835
```

The largest interval is 835.




## Imputing missing values


```r
navals <- sum(is.na(data$steps))
navals
```

```
## [1] 2304
```

```r
newData <- data

interVals <- unique(newData$interval)


#sorry for this garbage, recovering java programmer

for(i in 1:length(interVals)){
        subByInt <- subset(newData, newData$interval == interVals[i])
        mean <- mean(subByInt$steps, na.rm=TRUE)
        if(is.nan(mean)){mean<-0} #all na values inthis interval
        for(j in 1:nrow(subByInt)){
               if(is.na(newData$steps[j])){
                   newData$steps[j] <- mean #replaces NA with interval mean
                
               } 
        subByInt <- NA        
        }
} #end of cleaning NA steps

newDailyData <- aggregate(steps~date, newData, sum, na.rm=TRUE)
hist(newDailyData$steps, main="Mean with NA", xlab="Steps", ylab = "Commonality", col="blue")
```

![](PA1_template_files/figure-html/dingo4-1.png)<!-- -->

```r
c <- mean(dailyData$steps, na.rm=TRUE)
d <- median(dailyData$steps, na.rm=TRUE)

print(c(c,d))
```

```
## [1] 10766.19 10765.00
```
There were 2304 missing values.
The number of steps increases  significantly, and somewhat universally.



## Are there differences in activity patterns between weekdays and weekends?

```r
newData$fiveTwo <- weekdays(as.Date(newData$date))
newData[newData$fiveTwo %in% c("Saturday", "Sunday"),]$fiveTwo <- "WE"
newData[newData$fiveTwo != "WE",]$fiveTwo <- "WD"
newData$fiveTwo <- as.factor(newData$fiveTwo)

WD <- subset(newData, newData$fiveTwo =="WD")
r <- aggregate(steps~interval, WD, FUN = function(x) mean(as.numeric(as.character(x))))

WE <- subset(newData, newData$fiveTwo =="WE")
n <- aggregate(steps~interval, WE, FUN = function(x) mean(as.numeric(as.character(x))))



par(mfrow=c(2,1), mar=c(4,4,2,1), oma=c(0,0,2,0))

with(newData, {
plot(r$steps~r$interval,data=WD,type="l", main="Weekdays", xlab="Time", col="green")
plot(n$steps~n$interval,data=WE,type="l", main="Weekends", xlab="Time", col="purple")

})
```

![](PA1_template_files/figure-html/dingo5-1.png)<!-- -->

People work out before work and on the weekend they don't. Groundbreaking.
