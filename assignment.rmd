Report on Activity Monitoring
========================================================

This report contain multiple analysis of activity data thats recorded for an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

```{r}

activityData <- read.csv("activity.csv");


```

## Mean total number of steps taken per day

Let us see the mean of the total number of steps taken per day. The below histogram shows the total number of steps taken per day.

```{r meanSteps}
totalStepsPerDay <- aggregate(activityData$steps,by=list(activityData$date), FUN=sum, na.rm=TRUE)
hist(totalStepsPerDay$x, col = "green", xlab = "Total Steps per Day", ylab = "Frequency", main = "Total Steps taken per day")

meanSteps <- mean(totalStepsPerDay$x)
medianSteps <- median(totalStepsPerDay$x)

```
The mean of steps taken per day is `r meanSteps` and median of steps taken per day is `r medianSteps`


## Average daily activity pattern

Now let us look at the daily activity pattern. We will first average the steps taken per 5 minute slot across days and plot it as a line graph

```{r }
avgStepsPerInterval <- aggregate(activityData$steps,by=list(activityData$interval), FUN=mean, na.rm=TRUE)
plot(avgStepsPerInterval, type="l", ylab = "Avg. Steps", xlab = "Interval", main = "Average Steps Taken by Intervals", col="red")

```

Let us find out which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.
```{r }
maxStepInterval <- avgStepsPerInterval[which.max(avgStepsPerInterval$x), ]$Group.1

```

With this we can see the interval `r maxStepInterval` has the maximum number of steps on an average.

## Imputing missing values
If we inspect the data we can see there are a number of days/intervals where there are missing values, that may introduce bias into some calculations or summaries of the data. 

First let us figure out how many records has missing data

``` {r}
numMissingRows <- sum(is.na(activityData$steps))

```

We find there are `r numMissingRows` rows with missing data. 

We will replace the missing values with the mean of the number of steps for that interval.

``` {r}


for(rowIndex in 1:nrow(activityData)) { 
  row <- activityData[rowIndex,]
  if(is.na(row$steps)){
    activityData[rowIndex,]$steps <- avgStepsPerInterval[avgStepsPerInterval$Group.1==row$interval,]$x
  }
}


```

Let us do the mean calculation again

``` {r ref.label='meanSteps'}

```

Now the new mean of steps taken per day is `r meanSteps` and new median of steps taken per day is `r medianSteps`. We can see the mean and median has changed

## Differences in activity patterns between weekdays and weekends

Now let us check if the activity pattern is different on weekends and weekdays

Let us start by grouping data into weekdays and weekends first and then running an average of steps taken by time interval and do a panel plot with each panel representing weekday or weekend

```{r}

isWeekday <- function(date) {
    day <- weekdays(as.POSIXct(date))
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("Weekday") else 
        return("Weekend") 
}

activityData$isWeekDay <- sapply(activityData$date, FUN = isWeekday)

totalStepsPerDayByWeekday <- aggregate(activityData$steps,by=list(activityData$isWeekDay, activityData$interval), FUN=mean, na.rm=TRUE)

library(ggplot2)
ggplot(totalStepsPerDayByWeekday, aes(Group.2, x)) + geom_line() + facet_grid(Group.1 ~ .) + 
    xlab("Interval") + ylab("Avg. Number of Steps")


```


