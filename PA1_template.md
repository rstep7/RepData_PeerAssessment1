# Reproducible Research: Peer Assessment 1

***

## Loading and preprocessing the data

```r
# Load the libraries
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
suppressMessages(library(lubridate))
```

```
## Warning: package 'lubridate' was built under R version 3.2.3
```

```r
suppressMessages(library(timeDate))

# Load data into activity data frame
activity <- tbl_df(read.csv("activity.csv",header=TRUE,na.strings="NA"))
```

***

## What is mean total number of steps taken per day?


```r
# Calculate steps per day
steps_day <- activity %>% group_by(date) %>% summarize(total_steps=sum(steps))

# Histogram of steps per day
hist(steps_day$total_steps, main="Histogram of Steps Per Day", xlab="Steps Per Day", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
## Mean and median number of steps taken each day
# Calculate and print mean and median steps per day
mean_median_steps_day <- summary(steps_day$total_steps)
mean_median_steps_day["Mean"]
```

```
##  Mean 
## 10770
```

```r
mean_median_steps_day["Median"]
```

```
## Median 
##  10760
```

***

## What is the average daily activity pattern?


```r
# Calculate steps per interval
steps_interval <- activity %>% na.omit() %>% group_by(interval) %>% summarize(total_steps=sum(steps))

# Plot steps per interval
plot(steps_interval$interval,steps_interval$total_steps,
     type="l",
     main="Average Steps Taken Per Interval",
     ylab="Steps",
     xlab="Interval"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
## The 5-minute interval that, on average, contains the maximum number of steps
max_steps <- filter(steps_interval,total_steps == max(total_steps))
max_steps[["interval"]]
```

```
## [1] 835
```

***

## Imputing missing values


```r
# Calculate rows with missing data
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# Replace NA values with their respective rownumber mod 5
# Also creating new data set
a2 <- mutate(activity,steps=replace(steps,is.na(steps),row_number()%%5))
```

```
## Warning in replace(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, :
## number of items to replace is not a multiple of replacement length
```

```r
# Calculate steps per day with changed data
steps_day_a2 <- a2 %>% group_by(date) %>% summarize(total_steps=sum(steps))

# Histogram of steps per day with changed data
hist(steps_day_a2$total_steps, main="Histogram of Steps Per Day After Imputing", xlab="Steps Per Day", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

```r
# Calculate and print mean and median steps per day
mean_median_steps_day_a2 <- summary(steps_day_a2$total_steps)
mean_median_steps_day_a2["Mean"]
```

```
## Mean 
## 9430
```

```r
mean_median_steps_day_a2["Median"]
```

```
## Median 
##  10400
```
The values differ from the original data.  Missing data can have a significant impact on the results.


***

## Are there differences in activity patterns between weekdays and weekends?


```r
# Create new factor variable that includes Weekdays and Weekends
a3 <- mutate(a2,dayofweek=weekdays(ymd(date)))
a3 <- mutate(a3,day_type=as.factor(ifelse(isWeekday(date, wday=1:5) == TRUE,"Weekday", "Weekend")))

# Calculate steps per interval
steps_interval_weekday <- a3 %>% na.omit() %>% group_by(day_type, interval) %>% summarize(total_steps=sum(steps))

# Plot steps per interval
ggplot(data=steps_interval_weekday, aes(x=interval, y=total_steps, colour=day_type)) +
        geom_line() +
        theme(legend.position="none") +
        facet_wrap(~day_type, nrow=2) +
        labs(x="Interval",y="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

We can conclude that there are differences in activity patterns between weekdays and weekends.
