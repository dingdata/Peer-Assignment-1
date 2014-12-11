## ----loaddata------------------------------------------------------------
data <- read.csv("activity.csv")


## ------Make a Histogram---------------------------------------------------
hist(tapply(data$steps, data$date, sum), xlab = "Total daily steps", breaks = 20, 
    main = "Total of steps taken per day")


## -------Calculate the mean and median--------------------------------------
total.daily.steps <- as.numeric(tapply(data$steps, data$date, sum))
step.mean <- mean(total.daily.steps, na.rm = TRUE)
step.median <- median(total.daily.steps, na.rm = TRUE)

step.mean
step.median

## -----Average Daily Activity Pattern----------------------------------------
data$interval <- as.factor(as.character(data$interval))
interval.mean <- as.numeric(tapply(data$steps, data$interval, mean, na.rm = TRUE))
intervals <- data.frame(intervals = as.numeric(levels(data$interval)), interval.mean)
intervals <- intervals[order(intervals$intervals), ]

labels <- c("00:00", "05:00", "10:00", "15:00", "20:00")
labels.at <- seq(0, 2000, 500)
plot(intervals$intervals, intervals$interval.mean, type = "l", main = "Average steps 5-minute interval", 
    ylab = "Average steps", xlab = "Time of day", xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)

intervals.sorted <- intervals[order(intervals$interval.mean, decreasing = TRUE), 
    ]
head(intervals.sorted)

max.interval <- intervals.sorted$intervals[1[1]]
max.interval

## ----how_many_missing----------------------------------------------------
# How many missing
dim(data[is.na(data$steps), ])[1]



## ------------------------------------------------------------------------
# Replace each missing value with the mean value of its 5-minute interval
steps <- vector()
for (i in 1:dim(data)[1]) {
  if (is.na(data$steps[i])) {
    steps <- c(steps, intervals$interval.mean[intervals$intervals == data$interval[i]])
  } else {
    steps <- c(steps, data$steps[i])
  }
}


activity_without_missing_data <- data.frame(steps = steps, date = data$date, 
                                            interval = data$interval)

## ------------------------------------------------------------------------
hist(tapply(activity_without_missing_data$steps, activity_without_missing_data, 
            sum), xlab = "Total daily steps", breaks = 20, main = "Total of steps taken per day")


## ------------------------------------------------------------------------
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
activity_without_missing_data$date <- as.Date(activity_without_missing_data$date)
activity_without_missing_data$day <- sapply(activity_without_missing_data$date, FUN=weekday.or.weekend)


## Classify by weekend and weekday and repeat the previous process. Create a new factor variable with two factor levels in the dataset - 
## weekday and weekend - indicating whether a given date is a weekday or weekend day.
activity_without_missing_data$day.type <- c("weekend", "weekday", "weekday", 
                                            "weekday", "weekday", "weekday", "weekend")[as.POSIXlt(activity_without_missing_data$date)$wday + 
                                                                                          1]
activity_without_missing_data$day.type <- as.factor(activity_without_missing_data$day.type)

weekday <- activity_without_missing_data[activity_without_missing_data$day.type == 
                                           "weekday", ]
weekend <- activity_without_missing_data[activity_without_missing_data$day.type == 
                                           "weekend", ]
weekday.means <- as.numeric(tapply(weekday$steps, weekday$interval, mean))
weekend.means <- as.numeric(tapply(weekend$steps, weekend$interval, mean))

intervals.day.type <- data.frame(intervals = as.numeric(levels(data$interval)), 
                                 weekday.means, weekend.means)
intervals.day.type <- intervals.day.type[order(intervals.day.type$intervals), 
                                        ]
##Plot two time series - weekdays and weekends - of the 5-minute intervals and average number of steps taken.

par <- par(mfrow = c(2, 1))
plot(intervals.day.type$intervals, intervals.day.type$weekday.means, type = "l", 
     col = "red", ylab = "Average steps", xlab = "Time of day", main = "Average steps 5-minute interval at weekday", 
     xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)
plot(intervals.day.type$intervals, intervals.day.type$weekend.means, type = "l", 
     col = "blue", ylab = "Average steps", xlab = "Time of day", main = "Average steps 5-minute interval at weekend", 
     xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)