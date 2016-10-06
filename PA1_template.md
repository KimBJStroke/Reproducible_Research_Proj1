Step 1: Reading in the dataset and manipulating it
--------------------------------------------------

    data <- read.csv ("activity.csv", header=TRUE)
    data$date <- as.Date (data$date)

Step 2A: Histogram of the total number of steps taken each day
--------------------------------------------------------------

    Step <- aggregate (steps ~ date, data=data, sum, na.rm=TRUE)
    hist (Step$steps, breaks=10, main = "Total number of steps taken per day", xlab = "steps", ylab = "frequencies")

![](PA1_template_files/figure-markdown_strict/Histogram-1.png)

Step 2B: mean and median of steps
---------------------------------

    StepsMean <- mean (data$steps, na.rm=TRUE)
    StepsMedian <- median (data$steps, na.rm=TRUE)

    print (paste ("Mean value of steps per day is: ", StepsMean))

    ## [1] "Mean value of steps per day is:  37.3825995807128"

    print (paste ("Median value of steps per day is: ", StepsMedian))

    ## [1] "Median value of steps per day is:  0"

Step 3: Daily activity pattern
------------------------------

    StepsInterval <- aggregate (steps ~ interval, data=data, mean, na.rm=TRUE)
    plot (StepsInterval$interval, StepsInterval$steps, type = "l", 
          main = "Average Steps per Five-Minute Interval Across All Day", 
          xlab = "Interval", ylab = "Number of Steps")

![](PA1_template_files/figure-markdown_strict/time%20series%20plot%20across%20all%20day-1.png)

    MaxStep <- max(StepsInterval$steps)
    print (paste ("The maximal number of steps in a five-minute interval is: ", MaxStep))

    ## [1] "The maximal number of steps in a five-minute interval is:  206.169811320755"

Step 4: Imputing missing values
-------------------------------

    # basic statistics about missingness
    SumMissing <- sum (is.na(data$steps))
    print (paste ("The number of missing values in steps is :", SumMissing))

    ## [1] "The number of missing values in steps is : 2304"

    # Imputing median value into missingness
    ImputedData <- data
    ImputedData$steps[is.na(ImputedData$steps)] <- median(data$steps, na.rm=TRUE)

    ImputedStep <- aggregate (steps ~ date, data=ImputedData, sum, na.rm=TRUE)
    hist (ImputedStep$steps, breaks=10, 
          main = "Total number of steps taken per day (Imputed dataset)", 
          xlab = "steps", ylab = "frequencies")

![](PA1_template_files/figure-markdown_strict/missing%20values-1.png)

    ImputedStepsMean <- mean (ImputedData$steps, na.rm=TRUE)
    ImputedStepsMedian <- median (ImputedData$steps, na.rm=TRUE)

    print (paste ("Mean value of steps per day (imputed dataset) is: ", ImputedStepsMean))

    ## [1] "Mean value of steps per day (imputed dataset) is:  32.4799635701275"

    print (paste ("Median value of steps per day (imputed dataset) is: ", ImputedStepsMedian))

    ## [1] "Median value of steps per day (imputed dataset) is:  0"

Step 5: Differences between weekdays and weekend (using imputed dataset)
------------------------------------------------------------------------

    # assigning weekday and weekend
    ImputedData$Day <- weekdays(ImputedData$date)
    ImputedData$Weekend <- as.factor (ifelse(ImputedData$Day == "토요일" | ImputedData$Day == "일요일", "weekend", "weekday"))

    # plotting
    library (lattice)
    plotting <- aggregate (steps~interval + Weekend, ImputedData, mean)
    xyplot (steps~interval | factor (Weekend), data=plotting, aspect = 1/3, type="l")

![](PA1_template_files/figure-markdown_strict/weekday%20versus%20weekend-1.png)
