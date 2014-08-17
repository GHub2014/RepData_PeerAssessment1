---
title: "PA1_template"
author: "GHub2014"
date: "August 16, 2014"
output: html_document
---

1. Load the data.
The data file has already been saved and unzipped to the working directory.


```r
setwd("~/Documents/Coursera/Class5RepResearch/Assign1RepResearch/RepData_PeerAssessment1")
activity <- read.csv("~/Documents/Coursera/Class5RepResearch/Assign1RepResearch/RepData_PeerAssessment1/activity.csv", 
               sep=",", na.strings="NA", stringsAsFactors=FALSE)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis
For now, the data file is ready as is.

##What is mean total number of steps taken per day?

1.  Make a histogram of the total number of steps taken each day


```r
activityNA <- na.omit(activity)
NAactivity <- data.frame(activityNA)
ActivitySum <- aggregate(cbind(steps) ~ date, data=NAactivity, FUN=sum)
x <- as.numeric(ActivitySum$steps)
hist(x, col="blue", xlab="Steps",
   main="Histogram of total steps taken each day")              
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
2. Calculate and report the mean and median total number of steps taken per day


```r
mean <- mean(x)
mean
```

```
## [1] 10766
```

```r
median <- median(x)
median
```

```
## [1] 10765
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
IntervalSteps <- aggregate(cbind(steps) ~ interval, data=NAactivity, FUN="mean")
y <- as.numeric(IntervalSteps$steps)
z <- IntervalSteps$interval
plot(z, y, type="l", xlab="5-minute interval",ylab="Average number of steps", main="Average steps taken during each \n interval across all days", xlim=range(0:2355))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
which.max(y)
```

```
## [1] 104
```

```r
IntervalSteps[104,]
```

```
##     interval steps
## 104      835 206.2
```

##Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NArows <- sum(is.na(activity))
NArows
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
NAframe <- as.data.frame(activity)
NAframe[is.na(NAframe)] <- y
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
head(NAframe)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
NAframeActivitySum <- aggregate(cbind(steps) ~ date, data=NAframe, FUN=sum)
q <- as.numeric(NAframeActivitySum$steps)
hist(q, col="blue", xlab="Steps",
   main="Histogram of total steps taken each day")  
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
mean <- mean(q)
mean
```

```
## [1] 10766
```

```r
median <- median(q)
median
```

```
## [1] 10766
```
Explanation of results. I replaced all NAs with the mean of how may steps were taken during each interval across all days, object y. With the NA's omitted, the mean was 10766.19 and the median was
10765.

When the NAs are replaced with object y, the new mean is 9354.23 and the new median is 10395. By inputting the averages, both the mean and median decreased.

##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
Dates <- as.POSIXlt(NAframe$date)
Datesbind <- cbind(Dates, NAframe)
Weekdays <- weekdays(Datesbind$Dates)
Weekbind <- cbind(Datesbind,Weekdays)

DayEnd <- ifelse(Weekdays  %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"), 'weekday', 'weekend')
DayEndFactor <- factor(DayEnd)
DayEndFactor
```

```
##     [1] weekday weekday weekday weekday weekday weekday weekday weekday
##     [9] weekday weekday weekday weekday weekday weekday weekday weekday
##    [17] weekday weekday weekday weekday weekday weekday weekday weekday
##    [25] weekday weekday weekday weekday weekday weekday weekday weekday
##    [33] weekday weekday weekday weekday weekday weekday weekday weekday
##    [41] weekday weekday weekday weekday weekday weekday weekday weekday
##    [49] weekday weekday weekday weekday weekday weekday weekday weekday
##    [57] weekday weekday weekday weekday weekday weekday weekday weekday
##    [65] weekday weekday weekday weekday weekday weekday weekday weekday
##    [73] weekday weekday weekday weekday weekday weekday weekday weekday
##    [81] weekday weekday weekday weekday weekday weekday weekday weekday
##    [89] weekday weekday weekday weekday weekday weekday weekday weekday
##    [97] weekday weekday weekday weekday weekday weekday weekday weekday
##   [105] weekday weekday weekday weekday weekday weekday weekday weekday
##   [113] weekday weekday weekday weekday weekday weekday weekday weekday
##   [121] weekday weekday weekday weekday weekday weekday weekday weekday
##   [129] weekday weekday weekday weekday weekday weekday weekday weekday
##   [137] weekday weekday weekday weekday weekday weekday weekday weekday
##   [145] weekday weekday weekday weekday weekday weekday weekday weekday
##   [153] weekday weekday weekday weekday weekday weekday weekday weekday
##   [161] weekday weekday weekday weekday weekday weekday weekday weekday
##   [169] weekday weekday weekday weekday weekday weekday weekday weekday
##   [177] weekday weekday weekday weekday weekday weekday weekday weekday
##   [185] weekday weekday weekday weekday weekday weekday weekday weekday
##   [193] weekday weekday weekday weekday weekday weekday weekday weekday
##   [201] weekday weekday weekday weekday weekday weekday weekday weekday
##   [209] weekday weekday weekday weekday weekday weekday weekday weekday
##   [217] weekday weekday weekday weekday weekday weekday weekday weekday
##   [225] weekday weekday weekday weekday weekday weekday weekday weekday
##   [233] weekday weekday weekday weekday weekday weekday weekday weekday
##   [241] weekday weekday weekday weekday weekday weekday weekday weekday
##   [249] weekday weekday weekday weekday weekday weekday weekday weekday
##   [257] weekday weekday weekday weekday weekday weekday weekday weekday
##   [265] weekday weekday weekday weekday weekday weekday weekday weekday
##   [273] weekday weekday weekday weekday weekday weekday weekday weekday
##   [281] weekday weekday weekday weekday weekday weekday weekday weekday
##   [289] weekday weekday weekday weekday weekday weekday weekday weekday
##   [297] weekday weekday weekday weekday weekday weekday weekday weekday
##   [305] weekday weekday weekday weekday weekday weekday weekday weekday
##   [313] weekday weekday weekday weekday weekday weekday weekday weekday
##   [321] weekday weekday weekday weekday weekday weekday weekday weekday
##   [329] weekday weekday weekday weekday weekday weekday weekday weekday
##   [337] weekday weekday weekday weekday weekday weekday weekday weekday
##   [345] weekday weekday weekday weekday weekday weekday weekday weekday
##   [353] weekday weekday weekday weekday weekday weekday weekday weekday
##   [361] weekday weekday weekday weekday weekday weekday weekday weekday
##   [369] weekday weekday weekday weekday weekday weekday weekday weekday
##   [377] weekday weekday weekday weekday weekday weekday weekday weekday
##   [385] weekday weekday weekday weekday weekday weekday weekday weekday
##   [393] weekday weekday weekday weekday weekday weekday weekday weekday
##   [401] weekday weekday weekday weekday weekday weekday weekday weekday
##   [409] weekday weekday weekday weekday weekday weekday weekday weekday
##   [417] weekday weekday weekday weekday weekday weekday weekday weekday
##   [425] weekday weekday weekday weekday weekday weekday weekday weekday
##   [433] weekday weekday weekday weekday weekday weekday weekday weekday
##   [441] weekday weekday weekday weekday weekday weekday weekday weekday
##   [449] weekday weekday weekday weekday weekday weekday weekday weekday
##   [457] weekday weekday weekday weekday weekday weekday weekday weekday
##   [465] weekday weekday weekday weekday weekday weekday weekday weekday
##   [473] weekday weekday weekday weekday weekday weekday weekday weekday
##   [481] weekday weekday weekday weekday weekday weekday weekday weekday
##   [489] weekday weekday weekday weekday weekday weekday weekday weekday
##   [497] weekday weekday weekday weekday weekday weekday weekday weekday
##   [505] weekday weekday weekday weekday weekday weekday weekday weekday
##   [513] weekday weekday weekday weekday weekday weekday weekday weekday
##   [521] weekday weekday weekday weekday weekday weekday weekday weekday
##   [529] weekday weekday weekday weekday weekday weekday weekday weekday
##   [537] weekday weekday weekday weekday weekday weekday weekday weekday
##   [545] weekday weekday weekday weekday weekday weekday weekday weekday
##   [553] weekday weekday weekday weekday weekday weekday weekday weekday
##   [561] weekday weekday weekday weekday weekday weekday weekday weekday
##   [569] weekday weekday weekday weekday weekday weekday weekday weekday
##   [577] weekday weekday weekday weekday weekday weekday weekday weekday
##   [585] weekday weekday weekday weekday weekday weekday weekday weekday
##   [593] weekday weekday weekday weekday weekday weekday weekday weekday
##   [601] weekday weekday weekday weekday weekday weekday weekday weekday
##   [609] weekday weekday weekday weekday weekday weekday weekday weekday
##   [617] weekday weekday weekday weekday weekday weekday weekday weekday
##   [625] weekday weekday weekday weekday weekday weekday weekday weekday
##   [633] weekday weekday weekday weekday weekday weekday weekday weekday
##   [641] weekday weekday weekday weekday weekday weekday weekday weekday
##   [649] weekday weekday weekday weekday weekday weekday weekday weekday
##   [657] weekday weekday weekday weekday weekday weekday weekday weekday
##   [665] weekday weekday weekday weekday weekday weekday weekday weekday
##   [673] weekday weekday weekday weekday weekday weekday weekday weekday
##   [681] weekday weekday weekday weekday weekday weekday weekday weekday
##   [689] weekday weekday weekday weekday weekday weekday weekday weekday
##   [697] weekday weekday weekday weekday weekday weekday weekday weekday
##   [705] weekday weekday weekday weekday weekday weekday weekday weekday
##   [713] weekday weekday weekday weekday weekday weekday weekday weekday
##   [721] weekday weekday weekday weekday weekday weekday weekday weekday
##   [729] weekday weekday weekday weekday weekday weekday weekday weekday
##   [737] weekday weekday weekday weekday weekday weekday weekday weekday
##   [745] weekday weekday weekday weekday weekday weekday weekday weekday
##   [753] weekday weekday weekday weekday weekday weekday weekday weekday
##   [761] weekday weekday weekday weekday weekday weekday weekday weekday
##   [769] weekday weekday weekday weekday weekday weekday weekday weekday
##   [777] weekday weekday weekday weekday weekday weekday weekday weekday
##   [785] weekday weekday weekday weekday weekday weekday weekday weekday
##   [793] weekday weekday weekday weekday weekday weekday weekday weekday
##   [801] weekday weekday weekday weekday weekday weekday weekday weekday
##   [809] weekday weekday weekday weekday weekday weekday weekday weekday
##   [817] weekday weekday weekday weekday weekday weekday weekday weekday
##   [825] weekday weekday weekday weekday weekday weekday weekday weekday
##   [833] weekday weekday weekday weekday weekday weekday weekday weekday
##   [841] weekday weekday weekday weekday weekday weekday weekday weekday
##   [849] weekday weekday weekday weekday weekday weekday weekday weekday
##   [857] weekday weekday weekday weekday weekday weekday weekday weekday
##   [865] weekday weekday weekday weekday weekday weekday weekday weekday
##   [873] weekday weekday weekday weekday weekday weekday weekday weekday
##   [881] weekday weekday weekday weekday weekday weekday weekday weekday
##   [889] weekday weekday weekday weekday weekday weekday weekday weekday
##   [897] weekday weekday weekday weekday weekday weekday weekday weekday
##   [905] weekday weekday weekday weekday weekday weekday weekday weekday
##   [913] weekday weekday weekday weekday weekday weekday weekday weekday
##   [921] weekday weekday weekday weekday weekday weekday weekday weekday
##   [929] weekday weekday weekday weekday weekday weekday weekday weekday
##   [937] weekday weekday weekday weekday weekday weekday weekday weekday
##   [945] weekday weekday weekday weekday weekday weekday weekday weekday
##   [953] weekday weekday weekday weekday weekday weekday weekday weekday
##   [961] weekday weekday weekday weekday weekday weekday weekday weekday
##   [969] weekday weekday weekday weekday weekday weekday weekday weekday
##   [977] weekday weekday weekday weekday weekday weekday weekday weekday
##   [985] weekday weekday weekday weekday weekday weekday weekday weekday
##   [993] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1001] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1009] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1017] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1025] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1033] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1041] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1049] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1057] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [1441] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1449] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1457] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1465] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1473] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1481] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1489] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1497] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1505] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1513] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1521] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1529] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1537] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1545] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1553] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1561] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1569] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1577] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1585] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1593] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1601] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1609] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1617] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1625] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1633] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1641] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1649] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1657] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1665] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1673] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1681] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1689] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1697] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1705] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1713] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1721] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1729] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1737] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1745] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1753] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1761] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1769] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1777] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1785] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1793] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1801] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1809] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1817] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1825] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1833] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1841] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1849] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1857] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1865] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1873] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1881] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1889] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1897] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1905] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1913] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1921] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1929] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1937] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1945] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1953] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1961] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1969] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1977] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1985] weekend weekend weekend weekend weekend weekend weekend weekend
##  [1993] weekend weekend weekend weekend weekend weekend weekend weekend
##  [2001] weekend weekend weekend weekend weekend weekend weekend weekend
##  [2009] weekend weekend weekend weekend weekend weekend weekend weekend
##  [2017] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2025] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2033] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2041] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2049] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2057] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2441] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2449] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2457] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2465] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2473] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2481] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2489] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2497] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2505] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2513] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2521] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2529] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2537] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2545] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2553] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2561] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2569] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2577] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2585] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2593] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2601] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2609] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2617] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2625] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2633] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2641] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2649] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2657] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2665] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2673] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2681] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2689] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2697] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2705] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2713] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2721] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2729] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2737] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2745] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2753] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2761] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2769] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2777] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2785] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2793] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2801] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2809] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2817] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2825] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2833] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2841] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2849] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2857] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2865] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2873] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2881] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2889] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2897] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2905] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2913] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2921] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2929] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2937] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2945] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2953] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2961] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2969] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2977] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2985] weekday weekday weekday weekday weekday weekday weekday weekday
##  [2993] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3001] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3009] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3017] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3025] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3033] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3041] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3049] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3057] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3441] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3449] weekday weekday weekday weekday weekday weekday weekday weekday
##  [3457] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3465] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3473] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3481] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3489] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3497] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3505] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3513] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3521] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3529] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3537] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3545] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3553] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3561] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3569] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3577] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3585] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3593] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3601] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3609] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3617] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3625] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3633] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3641] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3649] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3657] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3665] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3673] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3681] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3689] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3697] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3705] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3713] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3721] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3729] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3737] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3745] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3753] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3761] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3769] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3777] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3785] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3793] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3801] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3809] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3817] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3825] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3833] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3841] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3849] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3857] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3865] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3873] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3881] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3889] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3897] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3905] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3913] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3921] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3929] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3937] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3945] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3953] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3961] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3969] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3977] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3985] weekend weekend weekend weekend weekend weekend weekend weekend
##  [3993] weekend weekend weekend weekend weekend weekend weekend weekend
##  [4001] weekend weekend weekend weekend weekend weekend weekend weekend
##  [4009] weekend weekend weekend weekend weekend weekend weekend weekend
##  [4017] weekend weekend weekend weekend weekend weekend weekend weekend
##  [4025] weekend weekend weekend weekend weekend weekend weekend weekend
##  [4033] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4041] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4049] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4057] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4441] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4449] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4457] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4465] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4473] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4481] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4489] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4497] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4505] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4513] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4521] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4529] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4537] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4545] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4553] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4561] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4569] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4577] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4585] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4593] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4601] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4609] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4617] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4625] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4633] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4641] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4649] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4657] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4665] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4673] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4681] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4689] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4697] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4705] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4713] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4721] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4729] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4737] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4745] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4753] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4761] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4769] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4777] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4785] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4793] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4801] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4809] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4817] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4825] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4833] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4841] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4849] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4857] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4865] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4873] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4881] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4889] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4897] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4905] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4913] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4921] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4929] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4937] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4945] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4953] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4961] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4969] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4977] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4985] weekday weekday weekday weekday weekday weekday weekday weekday
##  [4993] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5001] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5009] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5017] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5025] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5033] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5041] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5049] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5057] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5441] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5449] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5457] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5465] weekday weekday weekday weekday weekday weekday weekday weekday
##  [5473] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5481] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5489] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5497] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5505] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5513] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5521] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5529] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5537] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5545] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5553] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5561] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5569] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5577] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5585] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5593] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5601] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5609] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5617] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5625] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5633] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5641] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5649] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5657] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5665] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5673] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5681] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5689] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5697] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5705] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5713] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5721] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5729] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5737] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5745] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5753] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5761] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5769] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5777] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5785] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5793] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5801] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5809] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5817] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5825] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5833] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5841] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5849] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5857] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5865] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5873] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5881] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5889] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5897] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5905] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5913] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5921] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5929] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5937] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5945] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5953] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5961] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5969] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5977] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5985] weekend weekend weekend weekend weekend weekend weekend weekend
##  [5993] weekend weekend weekend weekend weekend weekend weekend weekend
##  [6001] weekend weekend weekend weekend weekend weekend weekend weekend
##  [6009] weekend weekend weekend weekend weekend weekend weekend weekend
##  [6017] weekend weekend weekend weekend weekend weekend weekend weekend
##  [6025] weekend weekend weekend weekend weekend weekend weekend weekend
##  [6033] weekend weekend weekend weekend weekend weekend weekend weekend
##  [6041] weekend weekend weekend weekend weekend weekend weekend weekend
##  [6049] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6057] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6441] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6449] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6457] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6465] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6473] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6481] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6489] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6497] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6505] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6513] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6521] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6529] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6537] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6545] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6553] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6561] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6569] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6577] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6585] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6593] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6601] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6609] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6617] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6625] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6633] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6641] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6649] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6657] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6665] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6673] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6681] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6689] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6697] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6705] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6713] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6721] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6729] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6737] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6745] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6753] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6761] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6769] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6777] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6785] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6793] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6801] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6809] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6817] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6825] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6833] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6841] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6849] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6857] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6865] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6873] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6881] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6889] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6897] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6905] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6913] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6921] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6929] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6937] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6945] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6953] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6961] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6969] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6977] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6985] weekday weekday weekday weekday weekday weekday weekday weekday
##  [6993] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7001] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7009] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7017] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7025] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7033] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7041] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7049] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7057] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7441] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7449] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7457] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7465] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7473] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7481] weekday weekday weekday weekday weekday weekday weekday weekday
##  [7489] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7497] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7505] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7513] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7521] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7529] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7537] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7545] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7553] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7561] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7569] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7577] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7585] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7593] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7601] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7609] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7617] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7625] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7633] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7641] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7649] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7657] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7665] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7673] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7681] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7689] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7697] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7705] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7713] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7721] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7729] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7737] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7745] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7753] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7761] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7769] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7777] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7785] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7793] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7801] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7809] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7817] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7825] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7833] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7841] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7849] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7857] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7865] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7873] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7881] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7889] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7897] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7905] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7913] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7921] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7929] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7937] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7945] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7953] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7961] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7969] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7977] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7985] weekend weekend weekend weekend weekend weekend weekend weekend
##  [7993] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8001] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8009] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8017] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8025] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8033] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8041] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8049] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8057] weekend weekend weekend weekend weekend weekend weekend weekend
##  [8065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8441] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8449] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8457] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8465] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8473] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8481] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8489] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8497] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8505] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8513] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8521] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8529] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8537] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8545] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8553] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8561] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8569] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8577] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8585] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8593] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8601] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8609] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8617] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8625] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8633] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8641] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8649] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8657] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8665] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8673] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8681] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8689] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8697] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8705] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8713] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8721] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8729] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8737] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8745] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8753] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8761] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8769] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8777] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8785] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8793] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8801] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8809] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8817] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8825] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8833] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8841] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8849] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8857] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8865] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8873] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8881] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8889] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8897] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8905] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8913] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8921] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8929] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8937] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8945] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8953] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8961] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8969] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8977] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8985] weekday weekday weekday weekday weekday weekday weekday weekday
##  [8993] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9001] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9009] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9017] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9025] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9033] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9041] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9049] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9057] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9065] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9073] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9081] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9089] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9097] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9105] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9113] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9121] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9129] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9137] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9145] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9153] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9161] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9169] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9177] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9185] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9193] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9201] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9209] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9217] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9225] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9233] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9241] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9249] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9257] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9265] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9273] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9281] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9289] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9297] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9305] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9313] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9321] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9329] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9337] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9345] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9353] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9361] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9369] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9377] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9385] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9393] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9401] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9409] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9417] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9425] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9433] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9441] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9449] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9457] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9465] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9473] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9481] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9489] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9497] weekday weekday weekday weekday weekday weekday weekday weekday
##  [9505] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9513] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9521] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9529] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9537] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9545] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9553] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9561] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9569] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9577] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9585] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9593] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9601] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9609] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9617] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9625] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9633] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9641] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9649] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9657] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9665] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9673] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9681] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9689] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9697] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9705] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9713] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9721] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9729] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9737] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9745] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9753] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9761] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9769] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9777] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9785] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9793] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9801] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9809] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9817] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9825] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9833] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9841] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9849] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9857] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9865] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9873] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9881] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9889] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9897] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9905] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9913] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9921] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9929] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9937] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9945] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9953] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9961] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9969] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9977] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9985] weekend weekend weekend weekend weekend weekend weekend weekend
##  [9993] weekend weekend weekend weekend weekend weekend weekend weekend
##  [ reached getOption("max.print") -- omitted 7568 entries ]
## Levels: weekday weekend
```
DayEndFactor represents the two levels of the factor: weekday weekend

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


