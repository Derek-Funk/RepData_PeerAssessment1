# Reproducible Research - Week 2 Course Project
Derek Funk  
August 19, 2017  



### Part 1: Loading & Pre-processing data
We will first import our data file into R and format the date variable as a date data type.


```r
data_raw <- read.csv("activity.csv")
data_raw$date <- as.Date(data_raw$date)
```

### Part 2: Mean number of steps per day

Next, we create a data frame of daily steps per day. Its first few rows are shown below.

```r
library(dplyr)
daily_steps <- data_raw %>%
                    group_by(date) %>%
                    summarise(daily_count = sum(steps, na.rm = TRUE))
head(daily_steps)
```

```
## # A tibble: 6 × 2
##         date daily_count
##       <date>       <int>
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

Here is a visualization of these daily counts.


```r
hist(daily_steps$daily_count, breaks = 10, main = "Frequency of Total Steps per day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

We find that the mean number of daily steps is about 9,354.
The median is 10,395 steps.


```r
mean(daily_steps$daily_count)
```

```
## [1] 9354.23
```

```r
median(daily_steps$daily_count)
```

```
## [1] 10395
```

### Part 3: Average daily activity pattern

We now look at what time of day the most activity occurs. We do this by looking at the number of steps in each interval, averaged over all days. Below is a plot showing the means for each interval.


```r
mean_steps <- data_raw %>%
                    group_by(interval) %>%
                    summarise(mean_steps = mean(steps, na.rm = TRUE))
plot(mean_steps ~ interval, data = mean_steps, type = "l", xlab = "Interval", ylab = "Mean Number of Steps", main = "Number of Steps per Interval, averaged over all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The plot shows that, on average, the most steps occur a bit before interval 1000. The calculation below tells us that it is in fact interval 835.


```r
mean_steps[which.max(mean_steps$mean_steps), 1]
```

```
## # A tibble: 1 × 1
##   interval
##      <int>
## 1      835
```

### Part 4: Imputing missing values

Let's first get an idea of how many missing values we have.


```r
sum(is.na(data_raw))
```

```
## [1] 2304
```

It appears there are 2,304 missing values.

We will impute each missing interval with the mean for that 5-minute interval. The code below creates a new such dataset by looping through all 17,568 measurements and replacing any NAs with the mean steps for the corresponding interval.


```r
dim(data_raw)[1]
```

```
## [1] 17568
```

```r
data_imputed <- data_raw
for(i in 1:17568){
    if(is.na(data_imputed[i,1]) == TRUE){
        data_imputed[i,1] <- mean_steps[which(mean_steps$interval == data_raw[i,3]), 2]
    }
}
```

We will perform a few of the same calculations on this imputed dataset as we did with the original dataset.


```r
daily_steps_imputed <- data_imputed %>%
                            group_by(date) %>%
                            summarise(daily_count = sum(steps))
hist(daily_steps_imputed$daily_count, breaks = 10, main = "Frequency of Total Steps per day (imputed data)", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(daily_steps_imputed$daily_count)
```

```
## [1] 10766.19
```

```r
median(daily_steps_imputed$daily_count)
```

```
## [1] 10766.19
```

The resulting mean and median are about 10,766 steps.
The effect of our imputation strategy has been to overall increase the distribution of total steps per day.

### Part 5: Weekdays vs. Weekends

We'll create a new column in this imputed dataset that denotes whether it is a weekday or a weekend. This is done by looping through each row and using the weekdays() function to see if the date is a Saturday or Sunday.


```r
data_imputed$day_type <- "weekday"
for(i in 1:17568){
    if(weekdays(data_imputed$date[i]) %in% c("Saturday","Sunday")){
        data_imputed$day_type[i] <- "weekend"    
    }
}
```

We again look at the average daily activity pattern, but now weekdays vs. weekends. We split the imputed dataset accordingly.


```r
data_imputed_weekday <- data_imputed[data_imputed$day_type == "weekday",]
data_imputed_weekend <- data_imputed[data_imputed$day_type == "weekend",]

mean_steps_weekday <- data_imputed_weekday %>%
                            group_by(interval) %>%
                            summarise(mean_steps = mean(steps))
mean_steps_weekend <- data_imputed_weekend %>%
                            group_by(interval) %>%
                            summarise(mean_steps = mean(steps))

par(mfrow=c(1,2))
plot(mean_steps ~ interval, data = mean_steps_weekday, type = "l", xlab = "Interval", ylab = "Mean Number of Steps")
mtext("Number of Steps per Interval,", line = 2)
mtext("averaged over all weekdays", line = 1)
plot(mean_steps ~ interval, data = mean_steps_weekend, type = "l", xlab = "Interval", ylab = "Mean Number of Steps")
mtext("Number of Steps per Interval,", line = 2)
mtext("averaged over all weekends", line = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
