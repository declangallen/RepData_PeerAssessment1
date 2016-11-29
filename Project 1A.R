##set the name of the zip folder
destfile="./ActiveMonitorData/data.zip"

##the web address of the data from the Coursera website
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"   

##if the destfile doesn't already exist in the local dir, begin download and unzipping
if (!file.exists(destfile)) {
        dir.create('ActiveMonitorData')
        download.file(fileURL ,destfile) 
        unzip("./ActiveMonitorData/data.zip", exdir = "./ActiveMonitorData")
}

##set local directory to the newly created data directory
setwd("./ActiveMonitorData")

##Read Activity file
data <- read.csv("activity.csv")

##install load necessary packages
install.packages("ggplot2")

install.packages("dplyr")

library(ggplot2)

library(dplyr)

library(lubridate)

library(gridExtra)

##edit data frame
ActData <- tbl_df(mutate(data, popDay = as.numeric(data$date)))

ActDate <- as.POSIXlt(ActData$date, format = "%Y-%m-%d")

ActData$weekday <- weekdays(ActDate)

ActData$weekend <- ifelse(ActData$weekday == "Saturday" | ActData$weekday == "Sunday", "Weekend", "Weekday")

ActData$month <- as.factor(month(ActData$date))

ActData$Day_of_Month <- as.numeric(format(as.Date(ActData$date, format = "%Y-%m-%d"), "%d"))

levels(ActData$month) <- c("October, 2012", "November, 2012")



##2. Histogram of the total number of steps taken each day.

byday <- group_by(ActData, popDay, month, Day_of_Month) %>% filter(!is.na(steps))

sumDay <- summarise(byday, steps = sum(steps))

plot1 <- ggplot(sumDay,aes(x=steps))+geom_histogram(binwidth = 500, fill = "blue")+ggtitle("Total Steps per Day")+theme(plot.title = element_text(hjust = 0.5))+ylab("frequency")

plot2 <- ggplot(sumDay,aes(x=steps))+geom_histogram(binwidth = 500, fill = "blue")+facet_grid(.~month) +ylab("frequency")

grid.arrange(plot1, plot2, nrow=2, ncol=1)

##3. Mean and median steps

meanSteps <- mean(sumDay$steps, na.rm = T)

meanSteps

medianSteps <- median(sumDay$steps, na.rm = T)

medianSteps

##4. Time Series plot of the average number of steps taken

byInt <- group_by(ActData, interval)%>% filter(!is.na(steps))

avgInt <- summarise(byInt, avgSteps = mean(steps, na.rm = T))

ggplot(avgInt, aes(x=interval, y=avgSteps))+
        geom_line()+
        ggtitle("Average Steps per Interval")+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Average Steps")


##5. The 5-minute interval that, on average, contains the maximum number of steps

top6 <- head(arrange(avgInt, desc(avgSteps)))

top6

##6. Code to describe and show a strategy for imputing missing data

ActDataFill <- merge(ActData, avgInt, by = "interval")

ActDataFill$fillsteps <- ifelse(is.na(ActDataFill$steps), ActDataFill$avgSteps, ActDataFill$steps)

ActDataFill <- ActDataFill[order(ActDataFill$date),]

##7. Histogram of the total number of steps taken each day after missing values are imputed

bydayNoNA <- group_by(ActDataFill, popDay, month, Day_of_Month)

sumDayNoNA <- summarise(bydayNoNA, steps = sum(fillsteps))

plot1 <- ggplot(sumDayNoNA,aes(x=steps))+geom_histogram(binwidth = 500, fill = "blue")

plot2 <- ggplot(sumDayNoNA,aes(x=steps))+geom_histogram(binwidth = 500, fill = "blue")+facet_grid(.~month)

grid.arrange(plot1, plot2, nrow=2, ncol=1)

##8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

byIntW <- group_by(ActData, interval, weekend)%>% filter(!is.na(steps))

avgIntW <- summarise(byIntW, avgSteps = mean(steps, na.rm = T))

plot <- ggplot(avgIntW, aes(x=interval, y=avgSteps))+geom_line()+
        facet_wrap(~weekend, ncol = 1, nrow = 2)+
        ggtitle("Average Steps per Interval, Weekday vs. Weekend")+
        theme(plot.title = element_text(hjust = 0.5))+
        ylab("Average Steps")
print(plot)


knit("PA1_template.Rmd")
