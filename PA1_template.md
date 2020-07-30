---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
one set is original data set and one more is data set without NA values

```r
actdt = read.csv("activity.csv")
#head(actdt)
actvlt = !is.na(actdt[,1])
actvldt = actdt[actvlt,][]
#head(actvldt)
actdt$date = as.Date(actdt$date,"%Y-%m-%d")
str(actdt)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```




## What is mean total number of steps taken per day?
let total number of steps taken per day be totstp

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
totalstp = actdt %>% group_by(date) %>% summarize(meanstp = mean(steps,na.rm = TRUE),totstp = sum(steps,na.rm = TRUE),medstp = median(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(totalstp)
```

```
## # A tibble: 6 x 4
##   date       meanstp totstp medstp
##   <date>       <dbl>  <int>  <dbl>
## 1 2012-10-01 NaN          0     NA
## 2 2012-10-02   0.438    126      0
## 3 2012-10-03  39.4    11352      0
## 4 2012-10-04  42.1    12116      0
## 5 2012-10-05  46.2    13294      0
## 6 2012-10-06  53.5    15420      0
```

```r
hist(totalstp$totstp)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
median(totalstp$medstp,na.rm = TRUE)
```

```
## [1] 0
```

```r
mean(totalstp$meanstp,na.rm = TRUE)
```

```
## [1] 37.3826
```




## What is the average daily activity pattern?

```r
actbyint = actdt %>% group_by(interval) %>% summarize(avgstp = mean(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(actbyint)
```

```
## # A tibble: 6 x 2
##   interval avgstp
##      <int>  <dbl>
## 1        0 1.72  
## 2        5 0.340 
## 3       10 0.132 
## 4       15 0.151 
## 5       20 0.0755
## 6       25 2.09
```

```r
plot(actbyint$interval,actbyint$avgstp,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
actbyint[which.max(actbyint$avgstp),]
```

```
## # A tibble: 1 x 2
##   interval avgstp
##      <int>  <dbl>
## 1      835   206.
```



## Imputing missing values


```r
missval = is.na(actdt[,1])
indexvect = actdt[missval,3]
sum(missval)
```

```
## [1] 2304
```

```r
for (i in 1:nrow(actdt)) {
  for(k in 0:471){
  if((is.na(actdt[i,1]) == TRUE) & (actdt[i,3] == 5*k) ){
    actdt[i,1] = actbyint[actbyint[,1] == 5*k,][,2]
  }
  }
}
```

```
## Warning: The `i` argument of ``[`()` can't be a matrix as of tibble 3.0.0.
## Convert to a vector.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

```r
actnewdt = actdt
totalstpn = actnewdt %>% group_by(date) %>% summarize(meanstp = mean(steps,na.rm = TRUE),totstp = sum(steps,na.rm = TRUE),medstp = median(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(totalstpn)
```

```
## # A tibble: 6 x 4
##   date       meanstp totstp medstp
##   <date>       <dbl>  <dbl>  <dbl>
## 1 2012-10-01  37.4   10766.   34.1
## 2 2012-10-02   0.438   126     0  
## 3 2012-10-03  39.4   11352     0  
## 4 2012-10-04  42.1   12116     0  
## 5 2012-10-05  46.2   13294     0  
## 6 2012-10-06  53.5   15420     0
```

```r
hist(totalstpn$totstp)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
median(totalstpn$medstp,na.rm = TRUE)
```

```
## [1] 0
```

```r
mean(totalstpn$meanstp,na.rm = TRUE)
```

```
## [1] 37.3826
```


## Are there differences in activity patterns between weekdays and weekends?

```r
actdtwk = actdt
actdtwk$day = weekdays(actdtwk$date)
weekenddtlg = filter(actdtwk,day == c("Saturday","Sunday"))
head(weekenddtlg)
```

```
##   steps       date interval      day
## 1     0 2012-10-06        0 Saturday
## 2     0 2012-10-06       10 Saturday
## 3     0 2012-10-06       20 Saturday
## 4     0 2012-10-06       30 Saturday
## 5     0 2012-10-06       40 Saturday
## 6     0 2012-10-06       50 Saturday
```

```r
weekdtlg = filter(actdtwk,day == c("Monday","Tuesday","Wednesday","Thursday","Friday"))
```

```
## Warning in day == c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"):
## longer object length is not a multiple of shorter object length
```

```r
head(weekdtlg)
```

```
##       steps       date interval    day
## 1 1.7169811 2012-10-01        0 Monday
## 2 2.0943396 2012-10-01       25 Monday
## 3 0.3018868 2012-10-01       50 Monday
## 4 0.3396226 2012-10-01      115 Monday
## 5 0.1698113 2012-10-01      140 Monday
## 6 0.0000000 2012-10-01      205 Monday
```

```r
actbyintwk = weekdtlg %>% group_by(interval) %>% summarize(avgstp = mean(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(actbyintwk)
```

```
## # A tibble: 6 x 2
##   interval avgstp
##      <int>  <dbl>
## 1        0 4.35  
## 2        5 0     
## 3       10 0.0147
## 4       15 0.889 
## 5       20 0.0168
## 6       25 0.698
```

```r
plot(actbyintwk$interval,actbyintwk$avgstp,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
actbyintwnd = weekenddtlg %>% group_by(interval) %>% summarize(avgstp = mean(steps,na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(actbyintwnd)
```

```
## # A tibble: 6 x 2
##   interval  avgstp
##      <int>   <dbl>
## 1        0 0.215  
## 2        5 0.0425 
## 3       10 0.0165 
## 4       15 0.0189 
## 5       20 0.00943
## 6       25 6.76
```

```r
plot(actbyintwnd$interval,actbyintwnd$avgstp,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

