Reproducible Research: Assessment 1 
========================================================
  prepared by WhitefishDontJump, March 2015

### General Note to Peer Reviewer:
I will be weaving text from the assignment's text, including the introduction, data source, and data description into this report.

My responses to questions, including the required R code, are after each question.

Thank you for taking a look at my work and completing the peer assessment.
I hope that you have comments, either on the github commit, or on the assessment form.


### Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. 

These type of devices are part of the quantified self movement  a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and
interpreting the data. 

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Data

The data for this assignment can be downloaded from the course web site:

  <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>
    

The variables included in this dataset are:

* **steps** : Number of steps taking in a 5-minute interval (missing values are coded as NA)
* **date** : The date on which the measurement was taken in YYYY-MM-DD format
* **interval** : Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())


2. Process/transform the data (if necessary) into a format suitable for your analysis

--------------------------------------------------------------------------------


```r
if (!file.exists("activity.zip")) {
    
    fileurl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileurl, destfile = "activity.zip")
    unzip("activity.zip")
    
}

activityraw <- read.csv("activity.csv", colClasses = rep("character", 3))
```

*Comment: The raw interval data are strings of length 1 to 4, with implicit
hours and minutes.*


```r
## The packages I am using are

    library("stringr")
    library("lubridate")
    library("reshape2")
    library("ggplot2")
    library("scales")
```



```r
##  pad the interval string to 4 characters with leading zeros and
##  add a column that combines date and interval values to a POSIXct variable

    activityraw$interval <- str_pad(activityraw$interval, width = 4, pad = "0") 
    activityraw$iposixct <- paste(activityraw$date, activityraw$interval)          
    activityraw$iposixct <- parse_date_time(activityraw$iposixct, "ymd HM")    

    activityraw$steps <- as.numeric(activityraw$steps)
    activityraw$date <- as.Date (activityraw$date) 

    activity <- activityraw[complete.cases(activityraw),]  ## rm NAs
```



### What are the mean and median total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day



```r
##  Recast the data by days and calculate sum for each day

    actmelt <- melt(activity, id.vars = "date", measure.vars="steps" ) 
    actcast <- dcast(actmelt, date ~ variable, fun.aggregate = sum)

## create the histogram
    par(bg = "#F4F4F7")

    hist(actcast$steps, main = "Plot 1: Frequency of Total Steps/Day",
         xlab = "Steps Per Day", breaks = 40, col = "gray",
         ylab= "Frequency (per 2 month period)")
```

![plot of chunk Plot_1_DailyStepTotal](figure/Plot_1_DailyStepTotal.png) 

*Comment: There are 2 days in the set with 0 steps, in addition to 8 days of NAs which are excluded from this plot.*

**Answer to Question 2 in this section:**


```r
    meancast <- dcast(actmelt, date ~ variable, fun.aggregate = mean)

    mediancast <- dcast(actmelt, date ~ variable, fun.aggregate = median)
```

The series of daily medians are all zero, because on each and every day, more than half of the intervals have zero steps. The summary() on the series of medians
confirms that all are zero:


```r
summary(mediancast$steps)   ### they are all zero, each and every day
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0       0       0       0       0
```

The series of daily means, however, have substantial variation across the set:


```r
summary(meancast$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.14   30.70   37.40   37.40   46.20   73.60
```

```r
meancast$steps  ## prints a vector of daily means, in chronological order
```

```
##  [1]  0.4375 39.4167 42.0694 46.1597 53.5417 38.2465 44.4826 34.3750
##  [9] 35.7778 60.3542 43.1458 52.4236 35.2049 52.3750 46.7083 34.9167
## [17] 41.0729 36.0938 30.6285 46.7361 30.9653 29.0104  8.6528 23.5347
## [25] 35.1354 39.7847 17.4236 34.0938 53.5208 36.8056 36.7049 36.2465
## [33] 28.9375 44.7326 11.1771 43.7778 37.3785 25.4722  0.1424 18.8924
## [41] 49.7882 52.4653 30.6979 15.5278 44.3993 70.9271 73.5903 50.2708
## [49] 41.0903 38.7569 47.3819 35.3576 24.4688
```

As indicated by the histogram, there is also substantial variation in the daily totals


```r
  mean(actcast$steps) ; median(actcast$steps) ## median & mean of daily totals
```

```
## [1] 10766
```

```
## [1] 10765
```

Across the daily totals in the histogram, Plot1, the **mean is 10,766 total steps/day** and the **median is 10,765 total steps/day** for the
two month period ending November 30, 2012.


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## recast the data by 5 min. interval, then calculate mean for each interval

        stepmelt <- melt(activity, id.vars = c("date", "interval"), 
                         measure.vars= "steps" )

        stepcast <- dcast(stepmelt, interval ~ variable, fun.aggregate = mean)


###     add a column with POSIXct interval for X axis then plot.

        stepcast$intervalct <- parse_date_time (stepcast$interval, "HM")

        par(bg = "#F4F4F7")

        plot(x = stepcast$intervalct, y = stepcast$steps, type="l",  
             ylab = "Mean Steps per Interval", 
             xlab = "Time of Day (288 intervals/day)",
             main = "Plot 2: Average Steps per 5 Minute Interval"
             )
```

![plot of chunk Plot_2_IntervalStepPlot](figure/Plot_2_IntervalStepPlot.png) 

**Answer to Question 2 in this section:**


```r
###     determine the maximum average interval

        stepmax <- stepcast[(max(stepcast$steps)==stepcast$steps),]
        
        stepmax
```

```
##     interval steps          intervalct
## 104     0835 206.2 0000-01-01 08:35:00
```



The time interval with the highest (or maximum) average steps occurs at  **08:35** with a mean of **~ 206** steps.

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

----------------------------------------------------------------------------


```r
##  get count of NAs, % of NAs

    sum(!complete.cases(activityraw)); mean(is.na(activityraw$steps))
```

```
## [1] 2304
```

```
## [1] 0.1311
```

```r
##  get number of days with and without NAs

    length(unique(activityraw$date)); length(unique(activity$date))
```

```
## [1] 61
```

```
## [1] 53
```

**Response to Question 1 in this Section:**

There are **2,304** rows with missing 'steps' data.

In other words, about **13 %** of the step data is NA.


-------------------------------------------------------------------------------


**Question 2: Explanation of Strategy for filling missing data**

All of the NAs occur on 8 dates out of 61 dates, which is **13%** of the dates, consistent with total NA count.

As a result, there are only **53** observed values for each interval.

***Strategy:***

1. For each interval with an NA, copy the steps from the same interval on another date with good data.

2. By randomly and uniformly sampling from the existing set of good dates, the sampled values will trend toward matching the distribution in the existing data, whatever its characteristics may be.

**Executing the Strategy in R:**

1. Using a uniform distribution (runif() function), seeded, with range, 0.5 to 53.5, and subsequently rounded to give an integer value (1 to 53),  this random result will specify which date to select from the 53 dates with good data. 

2. A vector "rdate" will contain the uniformly random dates from runif(), and have length equal to the number of NAs, 2304.  A vector "intbad" will contain the sequence of intervals in the data frame of NA data, "actbad".

3. For each NA row(i) in "actbad", the variable "steps" will be imputed by copying the value from the row of clean data that contains the date "rdate(i)" AND the interval  "intbad(i)"

**Verifying the Results**

1.  Review the summary statistics for the random date vector, "rdate." The mean median and quartiles should be uniformly distributed.

2.  Prepare a histogram of daily totals for the complete imputed dataset. The shape should be similar to that of Plot 1, the histogram of totals before distribution.

3. Compare the summary statistics before and after NA replacements, including the net change in statistics, in a table.




```r
##  segregate good and bad data

    actbad <- activityraw[!complete.cases(activityraw),]  ## df of NAs
    
    datelist <- unique(activity$date)  ## unique non-NA dates, 10/02 - 11/29

##  create a uniformly distributed vector of good dates with
##  length equal to the rows of NA data (actbad) and
##  a vector of intervals extracted from the NA data frame.
##  Both of these vectors will have length of 2304.

    set.seed(42)

    rdate <- datelist[round(runif(n = nrow(actbad), min = 0.5, 
                        max = (length(datelist + 0.5))))]   

    intbad <- actbad[,"interval"]  ## for convenience & legibility only

##  verify good distribution of dates

    summary(rdate)
```

```
##         Min.      1st Qu.       Median         Mean      3rd Qu. 
## "2012-10-02" "2012-10-15" "2012-10-28" "2012-10-30" "2012-11-16" 
##         Max. 
## "2012-11-29"
```

```r
##  replace the NAs steps in actbad with selected values from activity
    
    for(i in seq_along(rdate)) {
        
            actbad[i, "steps"] <- 
                    activity[ (rdate[i] == activity$date) & 
                              (intbad[i] == activity$interval),
                              "steps"]

    }

## combine data to make the new dataset (Question 3 in this section)

    newact <- rbind(activity, actbad)

    newact <- newact[order(newact$iposixct),]   ## ordered by date & time
```

-------------------------------------------------------------------------------

**Responses to Question 4 for this section of the assignment**



```r
## prep histogram

    newmelt <- melt(newact, id.vars = "date", measure.vars= "steps" )

    newcast <- dcast(newmelt, date ~ variable, fun.aggregate = sum)

    par(bg = "#F4F4F7")

    hist(newcast$steps, main = "Plot 3: Daily Total Steps after NAs Replaced",
         xlab = "Steps per day", breaks = 40, col = "gray",
         ylab= "Frequency (days in 2 Month period)")
```

![plot of chunk Plot_3_ImputedHistogram](figure/Plot_3_ImputedHistogram.png) 

```r
### examining changes in mean, median and totals steps after NAs replaced

    mean(newcast$steps) ; median(newcast$steps)
```

```
## [1] 10826
```

```
## [1] 10714
```

```r
    (mean(newcast$steps) - mean(actcast$steps))/mean(actcast$steps)
```

```
## [1] 0.005571
```

```r
    (median(newcast$steps) - median(actcast$steps))/median(actcast$steps)
```

```
## [1] -0.004738
```

```r
    sum(newact$steps) ; sum(activity$steps)
```

```
## [1] 660396
```

```
## [1] 570608
```

**Remarks (#4 in 'Imputing missing values section)**


The replacement of NAs **changed the daily steps mean by 
0.56%
and the median by -0.47%.**

Imputing data to the NAs changed the **total steps to 660,396 from 570,608.**  This change in totals is consistent with adding 8 days of missing data to the 53 days of good data to create the 61 day dataset.

Here is a comparison of summary statistics before & after imputing values to NAs:

```r
## examine the replacement set summary stats for steps per interval    

    OriginalSteps <- summary(activity$steps)
    ImputedSteps <- summary(newact$steps)
    NetChangeSteps <- OriginalSteps - ImputedSteps
    rbind(ImputedSteps, OriginalSteps, NetChangeSteps)
```

```
##                Min. 1st Qu. Median Mean 3rd Qu. Max.
## ImputedSteps      0       0      0 37.6      12  806
## OriginalSteps     0       0      0 37.4      12  806
## NetChangeSteps    0       0      0 -0.2       0    0
```

```r
##  Generate a table comparing daily totals
##  before and after imputing data to NAs

    OriginalSet <- summary(actcast$steps, digits=5)
    ImputedSet <- summary(newcast$steps, digits=5)
    NetChanges <- ImputedSet - OriginalSet
    rbind(ImputedSet, OriginalSet, NetChanges)
```

```
##             Min. 1st Qu. Median  Mean 3rd Qu.  Max.
## ImputedSet    41    8918  10714 10826   12883 21194
## OriginalSet   41    8841  10765 10766   13294 21194
## NetChanges     0      77    -51    60    -411     0
```
Imputing the missing data did not significantly alter the summary statistics.  

**In summary, the uniform random data and interval matching strategy worked as expected:**  
The NAs were replaced with representative values which did not substantially alter the character of the data.

-------------------------------------------------------------------------------

### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels  weekday and weekend indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

-------------------------------------------------------------------------------

**Responses to Questions in this Section:**


```r
## part 1: building a two level factor per #1 using wday() function which
## returns a day of week number and a character vector to set 'weekday' for
## 2:5 and 'weekend' for 1,7 - and then bind to df as factor.

weeker <- c("weekend", rep("weekday", 5), "weekend")

newact$dayo <- as.factor(weeker[wday(newact$date)])
```

*Comment: While the assignment suggested the weekdays() function,  I chose the wday() function. It returns an integer value for day and required less typing. ;)*


```r
## Prepare the panel plot per Question 2 in this section

## recast the data by 5 min. interval, then calculate mean for each interval

        daymelt <- melt(newact, id.vars = c("dayo", "interval"), 
                         measure.vars= "steps")

        daycast <- dcast(daymelt, dayo + interval ~ variable, 
                         fun.aggregate = mean)

##     add a column with POSIXct interval for X axis and then plot.

        daycast$intervalct <- parse_date_time (daycast$interval, "HM")

        x = ggplot(daycast, aes(x = intervalct, y = steps))
        x = x + geom_line() + facet_grid(dayo~.)
        x = x + labs(x = "Time of Day", 
                     y = "Steps per 5 Minute Interval" )
        x = x + ggtitle("Plot 4: Mean Steps per Interval ~ Weekdays vs. Weekends")
        x = x + scale_x_datetime(labels = date_format("%H:%M"))
        print(x)
```

![plot of chunk Plot_4_WdayWkendPlotMean](figure/Plot_4_WdayWkendPlotMean.png) 


*Comments:*

*1. Weekends seem to be characterized by sleeping in, followed by more changes in activity levels throughout the day, and staying up later.*

*2, The maximum weekday value at 8:35 AM is higher than any weekend peak. This peak may be associated with an exercise routine or commuting.* 

----------------------------------------------------------------------------------

**Thank you for reviewing my work.**

I hope that you will leave comments, both appreciative and constructively critical, with suggestions for improvements.



--------- ***this is the end of the required assignment*** --------------------




### Supplementary Analysis ###
----------------------------------------------------------------------------------

Here are additional code and plots that I created. 
They are not required by the assignment.

*A line plot of the maximum value per interval, Weekdays vs. Weekends.*
 

```r
## Prepare the panel plot for maximum values weekday and weekend intervals

## recast the data by 5 min. interval, then calculate max for each interval

    maxcast <- dcast(daymelt, dayo + interval ~ variable, 
                         fun.aggregate = max, "")  ## needed dummy arg

    maxcast$steps <- as.numeric(maxcast$steps) ## dcast made it "character"

## add a column with POSIXct interval for X axis and then plot.

    maxcast$intervalct <- parse_date_time (maxcast$interval, "HM")

    x = ggplot(maxcast, aes(x = intervalct, y = steps))
    x = x + geom_line() + facet_grid(dayo~.)
    x = x + labs(x = "Time of Day", 
                 y = "Steps per 5 Minute Interval" )
    x = x + ggtitle("Plot A: Maximum Steps per Interval ~ Weekdays vs. Weekends")
    x = x + scale_x_datetime(labels = date_format("%H:%M"))
    print(x)
```

![plot of chunk Plot_A_Supplementary](figure/Plot_A_Supplementary.png) 

*Comments:*

*1. The plot of maximum steps per interval provides a better context for the maximum mean value at 8:35 AM.*
    
*2. There are multiple intervals on both weekdays and weekends with maxima that are 700 to 800 steps per interval. This rate is more than 2 steps per second. Perhaps an exercise routine that is done on different times (but usually in the early morning) is the cause of the higher maxima.*

------------------------------------------------------------------------------------


end of document.
