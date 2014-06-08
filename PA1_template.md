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
                       "RColorBrewer",
                       "ggplot2")

new.packages <- required.packages[!(required.packages %in% 
                                    installed.packages()[,"Package"])]

if(length(new.packages)) {
  install.packages(new.packages)
} 

# Load required packages
library(lubridate)
library(data.table)
library(ggplot2)
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
 
activityData$interval <- as.factor(activityData$interval)
levels(activityData$interval) <- seq(1,length(unique(activityData$interval)))

activityData <- data.table(activityData)
```

## What is mean total number of steps taken per day?

```r
totalNumberStepsPerDay <- 
    as.data.frame(activityData[complete.cases(activityData),
                               sum(steps),by=date])

colnames(totalNumberStepsPerDay) <- c("date","totalnumberofsteps")

ggplot(totalNumberStepsPerDay, aes(x=totalnumberofsteps)) +
    geom_histogram(binwidth=1000,fill="white",colour="black") + 
    xlab("Total Number of Steps / Day")
```

![plot of chunk totalNumberOfSteps](figure/totalNumberOfSteps.png) 

```r
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
print(sprintf())
```

```
## Error: argument "fmt" is missing, with no default
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
[1]: http://tex.stackexchange.com/questions/152488/suppress-library-comments-from-ouput-with-knitr
[2]: http://stackoverflow.com/questions/14187048/r-language-clean-variables-and-close-connections
[3]: http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
[4]: http://www.r-bloggers.com/read-compressed-zip-files-in-r/
[5]: http://stackoverflow.com/questions/18799901/data-frame-group-by-column
[6]: http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame
[7]: http://stat.ethz.ch/R-manual/R-devel/library/base/html/cat.html
[8]: http://stackoverflow.com/questions/14733732/cant-change-fonts-in-ggplot-geom-text
[9]: http://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots
[10]: http://www.r-bloggers.com/how-to-calculate-with-dates-and-hours-in-r/