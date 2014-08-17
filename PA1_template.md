# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
<ol>
<li>
Extracting data from the compressed file and reading it to a data frame:


```r
zipfile <- "activity.zip"
flatdata <- unzip(zipfile, list = TRUE)[1]["Name"]
data <- read.table(unz(zipfile, flatdata), header = TRUE, sep = ",")
```
</li>

<li>
Excluding rows with undefined *'date'* or *'interval'*

```r
data <- data[!is.na(data$date) & !is.na(data$interval),]
```
</li>

<li>
Converting date format from *'factor'* to  *'date'*

```r
data$date <- as.Date(strptime(x = data$date,format = "%Y-%m-%d"))
```
</li>
<li>
Adding columns necassary for further analises:

- **day_number** - numbering days from the beginning of the study

- **missing** - logical value which assigns TRUE to the rows with missing *'steps'* value


```r
data$day_number <- as.integer(data$date - data$date[1] + 1)
data$missing <- is.na(data$steps)
```
</li>

<li>
Data types overview:


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps     : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date      : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day_number: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ missing   : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
```
</li>
</ol>

## What is mean total number of steps taken per day?
<ol>
<li>
Creating new data frame *'sum_steps_by_day'* by aggregating the *'data'* data frame on the column *'day_number'*. For each day a sum of steps is calculted.

```r
sum_steps_by_day <- aggregate(steps ~ as.factor(day_number), data = data, sum)
```
</li>
<li>
Calculating the **mean** and **median** of the number of steps per day:

```r
mean_steps <- mean(sum_steps_by_day$steps)
median_steps <- median(sum_steps_by_day$steps)
```
- **mean**: 1.0766 &times; 10<sup>4</sup>
- **median**: 10765

</li>
<li>
The total number of steps taken each day is shown in the histogram below. Each bar represents an interval of the length 1000. On the *y* axis the frequencies are measured in the number of days in which steps value falls into the given interval. The *mean* and *median* values are shown as vertical lines in the plot.

```r
hist(sum_steps_by_day$steps
     ,breaks = seq(0,25000,by=1000)
     ,main = "Histogram of the total number of steps taken each day"
     ,xlab = "Number of steps taken per day"
     ,col = "red")
abline(v = mean_steps, col = "blue", lwd = 2)
abline(v = median_steps, col = "green", lwd = 2, lty=2)
legend("topright", lty = c(1,2), bty = "n", col = c("blue", "green"), legend = c(paste("mean ~= ",round(mean_steps)), paste("median = ", median_steps)))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 
</li>
</ol>

## What is the average daily activity pattern?
<ol>
<li>
Creating new data frame *'mean_steps_by_interval'* by aggregating the *'data'* data frame on the column *'interval'*. For each time interval an average number of steps is calculted.

```r
mean_steps_by_interval <- aggregate(steps~interval, data=data, mean, na.rm=TRUE)
```
</li>
<li>
Finding the interval with the highiest value of mean steps:

```r
max_steps <- max(mean_steps_by_interval$steps)
max_steps_interval <- mean_steps_by_interval[mean_steps_by_interval$steps == max_steps,][["interval"]]
```
</li>
<li>
In the plot the average daily pattern is shown. On the *x* axis there are subsequent 5-minutes intervals,  the *y* axis represents the corresponding average number of steps. The interval with highiest value is marked with the vertical line.

```r
with(mean_steps_by_interval, plot(steps~interval
                                  ,type = "l"
                                  ,ylim = c(0,250)
                                  ,xlim = c(0,2500)
                                  )
     )
abline(v = max_steps_interval, col = "blue", lty = 2)
text(pos=4,max_steps_interval + 10, max_steps+20,paste("max steps interval =",max_steps_interval, "\nmean number of steps ~=", round(max_steps)))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
</li>
</ol>

## Imputing missing values
<ol>
<li>
Calculating the total number of missing values in the dataset (i.e. the total number of rows with NAs in the column 'steps'):

```r
n_missing <- sum(data$missing)
```
There are **2304** rows with missing values.
</li>
<li>
The missing values have been substituted with mean values for a given 5-minute interval and the resulting data are saved in a new data frame ***'filledData'***

```r
filledData <- data
for (i in 1:nrow(filledData)){
        if(is.na(filledData$steps[i])) {
                current_interval <- filledData$interval[i]
                filledData$steps[i] <- mean_steps_by_interval[mean_steps_by_interval$interval == current_interval,]$steps
        }
}
```
</li>
<li>
The new data set is aggregated to obtain new data frame with sums of steps in subsequent days. Also the **mean** and **median** are calculated:

```r
filled_sum_steps_by_day <- aggregate(steps ~ date, data = filledData, sum)
filled_mean_steps <- mean(filled_sum_steps_by_day$steps)
filled_median_steps <- median(filled_sum_steps_by_day$steps)
```
</li>
<li>
The new data set is visualised in a histogram plot, showing the frequencies of given mean numbers of steps per day. Also the **mean** and **median** are included in the form of vertical lines.


```r
hist(filled_sum_steps_by_day$steps, 
     breaks = seq(0,25000,by=1000),
     main = "Histogram of the total number of steps taken each day (filled NAs)", 
     xlab = "Number of steps taken per day",
     col = "red")
abline(v = filled_mean_steps, col = "blue", lwd = 2)
abline(v = filled_median_steps, col = "green", lwd = 2, lty=2)
legend("topright", lty = c(1,2), bty = "n", col = c("blue", "green"), legend = c(paste("mean ~= ",round(filled_mean_steps)), paste("median = ", filled_median_steps)))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

There are almost no differences at all between this historam and the one from the first part of the report.
</li>
</ol>
## Are there differences in activity patterns between weekdays and weekends?
<ol>
<li>
Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data$weekend <- factor(as.numeric(as.POSIXlt(data$date)$wday)>5,labels = c("weekday","weekend"))
```
</li>
<li>
Aggregating the steps data by interval, separately for weekdays and for weekends

```r
steps_by_interval_by_day <- aggregate(steps~interval+weekend, data=data, mean, na.rm = TRUE)
```
</li>
<li>
Importing the *'lattice'* package. Preparing panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
xyplot(steps~interval|weekend
       ,data=steps_by_interval_by_day
       ,type="l"
       ,layout = c(1,2)
       ,xlab = "Interval"
      
       ,ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 
There are visible differences in the activity patterns between weekdays and weekends. While in both cases the highiest activity can be observed in the first part of the day, on weekends it stays high and on weekdays it drops significantly.
</li>
</ol>
