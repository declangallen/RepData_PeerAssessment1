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

##edit data frame
ActData <- tbl_df(mutate(data, popDay = as.numeric(data$date)))

ActDate <- as.POSIXlt(ActData$date, format = "%Y-%m-%d")

ActData$weekday <- weekdays(ActDate)

ActData$month <- as.factor(month(ActData$date))

ActData$Day_of_Month <- as.numeric(format(as.Date(ActData$date, format = "%Y-%m-%d"), "%d"))

levels(ActData$month) <- c("October, 2012", "November, 2012")

byday <- group_by(ActData, popDay, month, Day_of_Month)

##2. Histogram of the total number of steps taken each day.
sumDay <- summarise(byday, steps = sum(steps))

a <- ggplot(sumDay,aes(x=Day_of_Month, y=steps))

a+geom_histogram(stat = "identity")+facet_grid(.~month)

##3. mean and median steps per day
##mean
meanDay <- summarise(byday, steps = mean(steps))

##fix median
medianDay <- summarise(byday, steps = median(steps, na.rm = FALSE))

##remove nas
meanDay1 <- meanDay[!is.na(meanDay$steps),]

#4. Time series plot of the average  number of steps taken
b <- ggplot(meanDay1, aes(x=Day_of_Month, y=steps))

b +geom_point()+geom_line()+facet_grid(.~month)

##IFELSE!!!!!!! good stuff
dataMerged$filledSteps <- ifelse(is.na(dataMerged$steps), dataMerged$., dataMerged$steps)




##5. The 5-minute interval that, on average, contains the maximum number of steps
dataNoNA <- data[!is.na(data$steps),]

byInterval <- group_by(dataNoNA, interval)

meanInterval <- summarise(byInterval, steps = sum(steps))

top10 <- arrange(meanInterval, desc(steps))

top10 <- top10[1:10,]

ylim <- c(0, 1.1*max(top10$steps))

c <- barplot(top10$steps , names.arg = top10$interval[1:10], xlab = "Interval", width = 1, ylim = ylim,
              main = "Top 10 Intervals by Steps", 
              ylab = "Steps")

text(x = c, y = top10$steps, label = top10$steps, pos = 3, cex = .75, col = "blue")

##6. Missing Information replacement

ActDatanoNA <- mutate(ActData, meanInterval$steps[match(ActData$interval, meanInterval$interval)])

ACtDatanoNA <- ActDatanoNA$steps == "NA"