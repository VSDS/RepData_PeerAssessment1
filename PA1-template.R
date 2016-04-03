#Load libraries
library(ggplot2)
library(lattice)

# Get the data files
if(!file.exists("activity.csv"))
{
	unzip(zipfile="repdata-data-activity.zip") 
}

#Read the data
stepsData <- read.csv("activity.csv")


# Summarize the steps data by day and plot histogram
stepsDataByDate <- aggregate(steps ~ date, data=stepsData, FUN=sum,na.rm=TRUE)
png("stepsDataByDate.png", , height=480, width=960)
hist(stepsDataByDate$steps,col = "blue",main = "Histogram of the total number of daily steps",xlab = "Total number of steps taken each day",breaks = 20)
dev.off()

#Mean and median
stepsMean <- mean(stepsDataByDate$steps)
stepsMedian <- median(stepsDataByDate$steps)
stepsMean
stepsMedian

#Average Activity Pattern
#Time series Plot
stepsIntervalData <- aggregate(steps ~ interval, data=stepsData, FUN = mean, na.rm=TRUE)
png("timeSeriesPlot.png", , height=480, width=960)
plot2 <- ggplot(stepsIntervalData, aes(interval, steps)) + geom_line(color = "blue") + labs(title = "Time Series Plot of the 5-minute Intervals", x = "Intervals (5 minute)", y = "Average number of steps taken")
print(plot2)
dev.off()

#Interval with maximum steps
stepsIntervalData$interval[which.max(stepsIntervalData$steps)]

#Calculate number of missing values
sum(is.na(stepsData))

#Fill missing values with interval mean
stepsDataImputed <- stepsData
for (i in 1:nrow(stepsDataImputed)) 
{
    if (is.na(stepsDataImputed$steps[i])) 
        stepsDataImputed$steps[i] <- stepsIntervalData[which(stepsDataImputed$interval[i] == stepsIntervalData$interval), ]$steps
}


#create new dataset
write.csv(stepsDataImputed, "activity1.csv")

# Summarize the steps data by day and plot histogram with new data (imputed)
stepsDataByDateNew <- aggregate(steps ~ date, data=stepsDataImputed, FUN=sum, na.rm=TRUE)
png("stepsDataByDateNew.png", , height=480, width=960)
hist(stepsDataByDateNew$steps,col = "blue",main = "Histogram of the total number of daily steps (Imputed)",xlab = "Total number of steps taken each day",breaks = 20)
dev.off()

#Mean and median with new data (imputed)
stepsMeanNew <- mean(stepsDataByDateNew$steps)
stepsMedianNew <- median(stepsDataByDateNew$steps)
stepsMeanNew 
stepsMedianNew 

#Differences between Weekday and Weekends
#Function to identify daytype
daytype <- function(inputDate) 
{
    if (weekdays(as.Date(inputDate)) %in% c("Saturday", "Sunday")) 
       "Weekend"
    else 
       "Weekday"
    
}
#Add Weekday/Weekend information to data
stepsDataImputed$daytype <- sapply(stepsDataImputed$date, daytype)


# Summarize the steps data by day and plot with new data (imputed)
stepsDataByDayType <- aggregate(stepsDataImputed$steps, list(stepsDataImputed$interval, stepsDataImputed$daytype),FUN = "mean")
names(stepsDataByDayType) <- c("interval","daytype","mean.steps")
png("panelWeekdayWeekend.png", , height=480, width=960)
plot4 <- xyplot(stepsDataByDayType$mean.steps ~ stepsDataByDayType$interval | stepsDataByDayType$daytype, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Mean number of steps")
print(plot4)
dev.off()

