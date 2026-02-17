#1. Code for reading in the dataset and/or processing the data

activity<-read.csv("activity.csv")

install.packages(ggplot2)
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("timeDate") 
library(timeDate)

#2. Histogram of the total number of steps taken each day

totalsteps <- aggregate(steps ~ date, activity, sum)
print(totalsteps) 

hist(totalsteps$steps, breaks = 30, xlab = "Total Steps per Day", ylab = "Number of Days", main = "Histogram of Total Daily Steps")

#3. Mean and median number of steps taken each day

summary(totalsteps)

#The mean number of steps is 10766. The median number of steps is 10765.

#4. Time series plot of the average number of steps taken

activity_per_interval <- aggregate(steps~interval, activity, mean)
ggplot(data = activity_per_interval) + 
  geom_line(aes(interval,steps)) + 
  xlab("5-Min Interval") +
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Taken per 5-min Interval")

#5. The 5-minute interval that, on average, contains the maximum number of steps

activity_per_interval[which.max(activity_per_interval$steps),]

#The 835th 5-min interval contains the maximum number of steps 

#6. Code to describe and show a strategy for imputing missing data

sum(is.na(activity$steps))
# The total number of missing values in the dataset is 2304. 

# My strategy for filling in missing values (NAs) is to substitute the missing values with the mean of that day. 

dailymeansteps <- activity %>% 
  group_by(date) %>%
  summarise(daily_mean = mean(steps, na.rm = TRUE))

dailymeansteps$daily_mean[is.nan(dailymeansteps$daily_mean)] <- 0

imputed_activity <- activity %>%
  left_join(dailymeansteps, by = "date")

imputed_activity$steps <- ifelse(
  is.na(imputed_activity$steps),
  imputed_activity$daily_mean,
  imputed_activity$steps
)

imputed_activity$daily_mean <- NULL
finaldata <- imputed_activity

#7. Histogram of the total number of steps taken each day after missing values are imputed

totalstepsimputed <- aggregate(steps ~ date, finaldata, sum)
print(totalstepsimputed) 

hist(totalstepsimputed$steps, breaks = 30, xlab = "Total Steps per Day", ylab = "Number of Days", main = "Histogram of Total Daily Steps after Imputing Missing Data")

summary(totalstepsimputed)

#The mean is now 9354. The median is now 10395. Previously, the mean and median were 10766 and 10765 respectively.Thus, both the mean and median decreased after imputing the missing data with the daily mean. 

#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

finaldata$date <- as.Date(strptime(finaldata$date, format="%Y-%m-%d"))
finaldata$dayofweek <- sapply(finaldata$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"}
  else {y <- "Weekday"}
  y
})

activityByDay <-  aggregate(steps ~ interval + dayofweek, finaldata, mean, na.rm = TRUE)

weekvsweekendplot <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayofweek)) + 
  geom_line() + ggtitle("Average Number of Steps by Type of Day") + 
  xlab("Interval") + 
  ylab("Average Number of Steps Taken") +
  facet_wrap(~dayofweek, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Type of Day") 
 print(weekvsweekendplot) 


