---
title: "Reproducible Research: Assignment 1"
output: html_document
keep_md: true
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(ggplot2)
```


##    Code for reading in the dataset and/or processing the data
```{r}
actna<-read.csv("activity.csv")
actna$date<-as.Date(actna$date)
act<-actna[complete.cases(actna),]

```

##    Histogram of the total number of steps taken each day
```{r Hist-1}
byday<-tapply(act$steps,act$date,sum)
hist(byday,30, col="gray",main="Histogram of the numbers of steps taken each day", xlab="Steps by day")
```

##    Mean and median number of steps taken each day
```{r}
mean<-round(mean(byday),0)
median<-median(byday)
```
Each day, the average number of steps taken is `r mean` and the median `r median`.

##    Time series plot of the average number of steps taken
```{r Time}
bydate<-group_by(act,date)  %>% mutate("meandate"=mean(steps))
plot(bydate$date,bydate$meandate,type='l', main="Average of steps taken by day",xlab="Date",ylab="Average number of Steps")
```

##    The 5-minute interval that, on average, contains the maximum number of steps
```{r}
act<-group_by(act,interval) %>% mutate("meanint"=mean(steps))
max<-act[which.max(act$meanint),3][[1]]
```
The 5-minute interval that contains on average the maximum number of steps is: `r max`.


##    Code to describe and show a strategy for imputing missing data
To replace missing data with useful values, we first need to get the rows with missing values (in our dataset, only the "steps" colum has missing data). Then, we crate a temporary dataset with the matching intervals and mean number of steps per interval.  
This allows to match to each missing value the average number of steps for the corresponding interval.
```{r}
summary(actna)
ind<-which(is.na(actna[,1]))
agg<-act[which(!duplicated(act$interval)),c(3,4)]   
actna[ind,1]<-merge(actna,agg)[ind,4]
```

##    Histogram of the total number of steps taken each day after missing values are imputed
```{r Hist-no-na}
hist(tapply(actna$steps,actna$date,sum),30, col="gray",main="Histogram of the numbers of steps taken each day", xlab="Steps by day")
```

##    Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r comparison}
actna$day<-weekdays(actna$date)
actna$weekday = ifelse(actna$day %in% c('samedi', 'dimanche'), 'Weekend', 'Weekday')
actna<-group_by(actna,weekday,interval) %>% mutate("meanint"=mean(steps))

ggplot(actna, aes(x=interval, y=meanint, color=weekday)) + geom_line() + facet_grid(rows=vars(weekday)) + labs(title="Avg. Daily Steps by Day Type")
```