library(dplyr)
library(ggplot2) 

# Read the activity CSV file to dataframe
activityMonitorData <- read.csv(file="activity.csv", header=TRUE, sep=",")

# ________________________________________________________________________________________________________________________
# Answer to question "What is mean total number of steps taken per day?"
# ________________________________________________________________________________________________________________________
totalNumberSteps <- aggregate(activityMonitorData[c("steps")], by=activityMonitorData[c("date")], FUN=sum, na.rm=TRUE)  

# Calculate and report the mean and median of the total number of steps taken per day
totalMean <- mean(totalNumberSteps$steps)
totalMedian <- median(totalNumberSteps$steps)

# Make a histogram of the total number of steps taken each day
ggplot(totalNumberSteps, aes(x = steps)) + 
        geom_histogram(stat = "bin", binwidth = 500, bins = 100, fill = "light green", color = "black") + 
        geom_vline(xintercept = totalMean, color = "blue", linetype = "solid", size = 1) + 
        geom_vline(xintercept = totalMedian, color = "red", linetype = "solid", size = 1) + 
        labs(title = "Histogram of total number of steps \n taken for each day") + 
        labs(x = "", y = "")

# Save plot ttlStepsTakenEachDayPlot
ggsave(filename="ttlStepsTakenEachDayPlot.png", height=4, width=4)

# ________________________________________________________________________________________________________________________
# Answer to question "What is the average daily activity pattern?"
# ________________________________________________________________________________________________________________________
activityPattern <- aggregate(activityMonitorData[c("steps")], by=activityMonitorData[c("interval")], FUN=mean, na.rm=TRUE) 

#  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxNbrStepsInterval <- activityPattern$interval[activityPattern$steps==max(activityPattern$steps)]

#  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of 
#  steps taken, averaged across all days (y-axis)
ggplot(activityPattern, aes(interval,steps)) + 
        geom_line() + 
        geom_vline(xintercept = maxNbrStepsInterval, color = "red", linetype = "solid", size = .5) + 
        labs(title = "Time series of the 5-minute interval and \n the average number of steps taken, \n averaged across all days") + 
        labs(x = "5-minute Intervals", y = "Number of Steps Taken ")

# Save plot timeSeriesStepIntrvlPlot.png
ggsave(filename="timeSeriesStepIntrvlPlot.png", height=4, width=4)

# ________________________________________________________________________________________________________________________
# Imputing missing values
# ________________________________________________________________________________________________________________________

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
totalNbrRowMissingSteps <- sum(is.na(activityMonitorData$steps))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
seedValue <- totalMean/totalMedian

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
imputedActivityMonitorData <- activityMonitorData
imputedActivityMonitorData$steps <- ifelse(is.na(activityMonitorData$steps), seedValue, activityMonitorData$steps)

# Calculate and report the mean and median total number of steps taken per day.
imputedTotalNumberSteps <- aggregate(imputedActivityMonitorData[c("steps")], by=imputedActivityMonitorData[c("date")], FUN=sum, na.rm=TRUE)  
imputedTotalMean <- mean(imputedTotalNumberSteps$steps)
imputedTotalMedian <- median(imputedTotalNumberSteps$steps)

# Make a histogram of the total number of steps taken each day 
ggplot(imputedTotalNumberSteps, aes(x = steps)) + 
        geom_histogram(stat = "bin", binwidth = 500, bins = 100, fill = "yellow", color = "black") + 
        geom_vline(xintercept = imputedTotalMean, color = "dark green", linetype = "solid", size = 1) + 
        geom_vline(xintercept = imputedTotalMedian, color = "purple", linetype = "solid", size = 1) + 
        labs(title = "Histogram of total number of steps \n taken for each day \n (Imputed data values)") + 
        labs(x = "", y = "")

# Save plot imputedTtlStepsTakenEachDayPlot
ggsave(filename="imputedTtlStepsTakenEachDayPlot.png", height=4, width=4)


# ________________________________________________________________________________________________________________________
# Answer to question "Are there differences in activity patterns between weekdays and weekends?"
# ________________________________________________________________________________________________________________________

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
dayTypeActivityPattern <- imputedActivityMonitorData
dayTypeActivityPattern$daytype <- ifelse(weekdays(as.Date(dayTypeActivityPattern$date)) == "Saturday" | 
                                                 weekdays(as.Date(dayTypeActivityPattern$date)) == "Sunday",
                                         "weekend","weekday")
dayTypeActivityPattern <- aggregate(dayTypeActivityPattern[c("steps")], by=dayTypeActivityPattern[c("interval","daytype")], FUN=mean, na.rm=TRUE) 

# Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps t
# taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example 
# of what this plot should look like using simulated data.
ggplot(dayTypeActivityPattern, aes(interval, steps),) + 
        geom_line(color="blue") +
        facet_grid(daytype~., scales="free") +
        theme(strip.text.y = element_text(color="dark blue", size=10, face="bold"),
              strip.background = element_rect(color="dark blue", fill="light blue")) +
        labs(title = "Time Series of 5-minute Interval \n and Average Number of Steps Taken") +
        labs(x = "Interval", y = "Number of Steps")

# Save plot dayTypeActivityPatternPlot
ggsave(filename="dayTypeActivityPatternPlot.png", height=4, width=4)
