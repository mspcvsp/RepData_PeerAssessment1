# Reproducible Research: Peer Assessment 1

[Link 1][1]
[Link 2][2]
[Link 3][3]
[Link 4][4]
[Link 5][5]


```r
# Verify required packages are installed
required.packages <- c("lubridate",
                       "data.table",
                       "ggplot2",
                       "xtable")

new.packages <- required.packages[!(required.packages %in% 
                                    installed.packages()[,"Package"])]

if(length(new.packages)) {
  install.packages(new.packages)
} 

# Load required packages
library(data.table)
library(ggplot2)
library(lubridate)
library(xtable)
```

## Loading and preprocessing the data
There are 12*24 = 288 5 minute intervals in a day. However, the 5-minute 
interval values range from 0 to 2355. The purpose of the following section
of code is to:
1. Convert the 5-minute interval variable into a factor variable in order to 
facilaite computing the average daily activity  
2. Set the 5-minute intervals to correspond to the number of 5 minute intervals
in one twenty-four hour period 


```r
if (!file.exists("./Data")) {
    dir.create("./Data")    
    unzip("./activity.zip", exdir="Data")
}

activityData <- read.csv("./Data/activity.csv",
                         header=TRUE,
                         stringsAsFactors=FALSE)

activityData$date <- ymd(activityData$date)

originalIntervals <- unique(activityData$interval)

activityData$interval <- as.factor(activityData$interval)

levels(activityData$interval) <- seq(1,length(originalIntervals))

activityData <- data.table(activityData)
```


```r
kable(as.data.frame.matrix(t(as.matrix(summary(originalIntervals)))),
      format="markdown")
```



| Min.| 1st Qu.| Median| Mean| 3rd Qu.| Max.|
|----:|-------:|------:|----:|-------:|----:|
|    0|     589|   1180| 1180|    1770| 2360|

## What is mean total number of steps taken per day?

```r
computeTotalNumberStepsPerDay <- function(activityData) {
    totalNumberStepsPerDay <- 
        as.data.frame(activityData[complete.cases(activityData),
                                   sum(steps),by=date])
    
    colnames(totalNumberStepsPerDay) <- c("date","totalnumberofsteps")
    
    return(totalNumberStepsPerDay)
}

totalNumberStepsPerDay <- computeTotalNumberStepsPerDay(activityData)

ggplot(totalNumberStepsPerDay, aes(x=totalnumberofsteps)) +
    geom_histogram(binwidth=1000,fill="white",colour="black") + 
    xlab("Total Number of Steps / Day")
```

![plot of chunk totalNumberOfSteps](figure/totalNumberOfSteps.png) 

```r
ggsave("./figure/totalNumberOfSteps.png")

cat(sprintf("Mean total number of steps taken per day: %.1f", 
            mean(totalNumberStepsPerDay$totalnumberofsteps)),
    sprintf("Median total number of steps taken per day: %.1f", 
            median(totalNumberStepsPerDay$totalnumberofsteps)),
    fill=TRUE,
    sep="\n")
```

```
## Mean total number of steps taken per day: 10766.2
## 
## Median total number of steps taken per day: 10765.0
```

## What is the average daily activity pattern?

```r
dailyActivty <- as.data.frame(activityData[complete.cases(activityData),
                                           mean(steps),
                                           by=interval])

dailyActivty$interval <- as.numeric(dailyActivty$interval)
colnames(dailyActivty) <- c("interval","averagenumberofsteps")

ggplot(dailyActivty, aes(x=interval,y=averagenumberofsteps)) + 
    geom_line(size=1) +
    coord_cartesian(xlim=c(min(dailyActivty$interval),
                           max(dailyActivty$interval)),
                    ylim=c(min(dailyActivty$averagenumberofsteps),
                           max(dailyActivty$averagenumberofsteps))) +
    theme_gray(base_size=14) + 
    xlab("5 minute interval") + 
    ylab("Average Number of Steps") +
    ggtitle("Average Daily Activity Pattern") +
    scale_x_continuous(breaks=c(1,50,100,150,200,250))
```

![plot of chunk dailyActivity](figure/dailyActivity.png) 

```r
maxInterval <- 
    as.integer(dailyActivty[which(dailyActivty$averagenumberofsteps == 
                                  max(dailyActivty$averagenumberofsteps)),
                            "interval"])

maxIntervalTime <- as.POSIXlt(paste(activityData[1,date],"00:00:00"))
maxIntervalTime$min <- maxIntervalTime$min + maxInterval*5
maxIntervalTime <- strftime(maxIntervalTime,"%H:%M (24 hour)")

ggsave("./figure/dailyActivity.png")
```

On average across all the days in the dataset, the five minute interval that 
contains the maximum number steps is #104 which corresponds to   
08:40 (24 hour).

## Imputing missing values

```r
totalNumberMissingValues <- nrow(activityData) - 
                            sum(complete.cases(activityData))
```

The total number of missing values is 2304, which is 
computed as by subtracting the number of rows where all of the variables (i.e. 
columns) are specified from the total number of rows.


```r
dailyActivty$interval <- as.factor(dailyActivty$interval)

for (n in seq(1,nrow(dailyActivty))) {
    intervalIndex <-
        which(activityData$interval == dailyActivty[n,"interval"])
    
    naIndex <- 
        intervalIndex[is.na(activityData[intervalIndex,steps])]
    
    avgIntegerNumberOfSteps <- 
        as.integer(round(dailyActivty[n,"averagenumberofsteps"]))
    
    activityData[naIndex,steps:=avgIntegerNumberOfSteps]
}

totalNumberStepsPerDay <- computeTotalNumberStepsPerDay(activityData)

ggplot(totalNumberStepsPerDay, aes(x=totalnumberofsteps)) +
    geom_histogram(binwidth=1000,fill="white",colour="black") + 
    xlab("Total Number of Steps / Day") + 
    ggtitle("NA's set to mean value")
```

![plot of chunk imputeMissingValues](figure/imputeMissingValues.png) 

```r
ggsave("./figure/imputeMissingValues.png")

cat(sprintf("Mean total number of steps taken per day: %.1f", 
            mean(totalNumberStepsPerDay$totalnumberofsteps)),
    sprintf("Median total number of steps taken per day: %.1f", 
            median(totalNumberStepsPerDay$totalnumberofsteps)),
    fill=TRUE,
    sep="\n")
```

```
## Mean total number of steps taken per day: 10765.6
## 
## Median total number of steps taken per day: 10762.0
```

## Are there differences in activity patterns between weekdays and weekends?


```r
assign("depthtrigger", 60, data.table:::.global)
activityData[,daytype:=weekdays(date)]

weekday <- c("Monday",
             "Tuesday",
             "Wednesday",
             "Thursday",
             "Friday")

for (curDay in weekday) {
    activityData[which(activityData$daytype == curDay),daytype:="weekday"]
}

weekendDay <- c("Saturday",
                "Sunday")

for (curDay in weekendDay) {
    activityData[which(activityData$daytype == curDay),daytype:="weekend"]
}

activityData$daytype <- as.factor(activityData$daytype)

activityData$interval <- as.numeric(activityData$interval)

activityPattern <- as.data.frame(activityData[,mean(steps),by=list(daytype,interval)])

colnames(activityPattern) <- c("daytype","interval","avgnumberofsteps")

ggplot(activityPattern,aes(x=interval,y=avgnumberofsteps)) + 
    geom_line(size=1) + 
    theme_gray(base_size=14) +
    facet_grid(daytype ~ .) + 
    facet_wrap(~daytype,nrow=2) + 
    xlab("Interval") + ylab("Number of Steps")
```

![plot of chunk activityPatternDifferences](figure/activityPatternDifferences.png) 
[1]: http://tex.stackexchange.com/questions/152488/suppress-library-comments-from-ouput-with-knitr
[2]: http://stackoverflow.com/questions/14187048/r-language-clean-variables-and-close-connections
[3]: http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
[4]: http://stackoverflow.com/questions/15267018/knitr-gets-tricked-by-data-table-assignment
[5]: http://www.r-bloggers.com/read-compressed-zip-files-in-r/
[6]: http://stackoverflow.com/questions/18799901/data-frame-group-by-column
[7]: http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame
[9]: http://stat.ethz.ch/R-manual/R-devel/library/base/html/cat.html
[10]: http://stackoverflow.com/questions/14733732/cant-change-fonts-in-ggplot-geom-text
[11]: http://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots
[12]: http://www.r-bloggers.com/how-to-calculate-with-dates-and-hours-in-r/
[13]: http://jeromyanglim.blogspot.com/2012/05/getting-started-with-r-markdown-knitr.html
[14]: http://www.londonr.org/LondonR-20090331/data.table.LondonR.pdf
[15]: http://datatable.r-forge.r-project.org/
[16]: http://stackoverflow.com/questions/8857287/how-to-add-subtract-time-from-a-posixlt-time-while-keeping-its-class-in-r
[17]: http://stackoverflow.com/questions/11308754/add-multiple-columns-to-r-data-table-in-one-function-call
[18]: http://rprogramming.net/aggregate-data-in-r-using-data-table/
[19]: http://blog.revolutionanalytics.com/2010/02/making-publicationready-tables-with-xtable.html
[20]: http://stackoverflow.com/questions/10758961/how-to-convert-a-table-to-a-data-frame-in-r
[21]: http://stackoverflow.com/questions/5430338/remove-data-frame-row-names-when-using-xtable
