###Cleaning the enviroment###
rm(list=ls())
options(scipen = 999)

#Uploading the data 
setwd("C:/Users/Omer Gralnik/Desktop/Economics in Big Data World/Final Project")
list.files()
july <- read.csv("uber-raw-data-jul14.csv",header = TRUE)

#install.packages("data.table")
library(data.table)
#calculating the distance between coordinates
#Geosphere pachage is needed
#install.packages("geosphere")
library(geosphere)

#setting the 'NY Stock exchange' coordinate
x<-40.706913 #The latitude coordinate
y<--74.011322 #The longtitude coordinate
xy <- cbind(y,x)
xy

#generate the distance variable
ju <- data.frame(july$Lon,july$Lat)
july$distance <- distm(ju,xy,fun = distHaversine)



#creating a new data frame of coordinates situated 1 km from NY stock exchange
july1 <- subset(july, july$distance<= 1000)      

#install.packages("lubridate")
library(lubridate)

#creating a new data frame with all the dates of July month by 15 minutes intervals
time_interval <- seq(ISOdate(2014,7,1), ISOdate(2014,8,1), by="15 mins")
time_interval <- data.frame(time_interval)
time_interval$time <- hour(time_interval$time_interval)
time_interval <- subset(time_interval,time >= 17)
time_interval$time <- NULL

#setting the dates as the same format of the test's format
july1$dates <- as.POSIXct(july1$Date.Time,format ='%m/%d/%Y %H:%M:%S', tz='GMT') 

#creating a new variable of hours
july1$hours <- hour(july1$dates)

#subset the data to only the relevant hours
july1 <- subset(july1,hours >= 17)

for (i in 1:length(time_interval$time_interval)) {
  for (j in 1:length(july1$dates)) {
    if (difftime(july1$dates[j],time_interval$time_interval[i], units = 'min')<0) {
      next
    }
    if (difftime(july1$dates[j],time_interval$time_interval[i], units = 'min')<15) {
      july1$dates[j] <- time_interval$time_interval[i]
    }
  }
}

#due to the long time that takes to the loops to run
#after running the loops, we saved the new data frame as a csv file, so that we won't need to run the loops again
#write.csv(july1,"C:/Users/Omer Gralnik/Desktop/Economics in Big Data World/Final Project/July1.csv", row.names = FALSE)
#july1 <- read.csv('July1.csv', header = TRUE)

#checking the new data
summary(july1)

#we wanted to make separate variables to date and time, so that we could work on each variable alone
july1$dates1 <- july1$dates
#install.packages("tidyr")
library(tidyr)
july1 <- tidyr::separate(july1, dates, c("date", "time"), sep = " ")
july1$Date.Time <- NULL

#generating a numeric variable for the pickup in order to calculate the sum of the pickups 
july1$count <- 1

#generating a new vector of all the dates in july month
date_vec <- july1$date
date_vec
dates <- unique(date_vec)
dates

#now we want to count how many pickups were made on each day
x1 <- by(july1$count, july1$date, sum)
x1

#for the next steps we need a function that returns the value it gets
x2 <- function(x){
  return (x)
}

#the next command combine each date with the sum of pickups made 
x3 <- sapply(x1,x2 )
x3

#this is a variable that includes all the dates in the month
x4 <- unique(july1$date)
x4

#now we can create a new data frame that inculdes all the dates and sum of pickups made in those dates
ab <- data.frame(x3,x4)

#the same thing we have done to the time variable
x5 <- by(july1$count, july1$time, sum)
x6 <- sort(unique(july1$time))
x7 <- sapply(x5,x2)
bc <- data.frame(x6,x7)

#to make things clear for us, we created a new factorial variable that determine how far was 
#the distance of the pickup from the NY Stock exchange by intervals of 250 meters
july1$distance1 <- NA
july1$distance1 <- ifelse(july1$distance<250, 'very close', july1$distance1)
july1$distance1 <- ifelse(july1$distance>=250&july1$distance<500, 'close', july1$distance1)
july1$distance1 <- ifelse(july1$distance>=500&july1$distance<750, 'far', july1$distance1)
july1$distance1 <- ifelse(july1$distance>=750, 'very far', july1$distance1)

#checking the data again
summary(july1$distance1)

#install.packages("ggplot2")
library(ggplot2)

#Descriptive statistics
plot(by(july1$count, july1$date, sum),
     ylab = 'uber pickups',
     xlab = 'day in month')


plot(by(july1$count, july1$time, sum)/31,
     ylab = 'uber pickups',
     xlab = 'time intervals')

plot(by(july1$count, july1$day, sum),
     ylab = 'uber pickups',
     xlab = 'time intervals')

ggplot(aes(x =x4, y= x3), data =ab) +
  geom_bar(stat="identity")+
  ylab ('Uber calls')+
  xlab ('Day in month')+
  ggtitle('Uber calls by day')

ggplot(aes(x =x6, y= x7), data =bc) +
  geom_bar(stat="identity")+
  ylab ('Uber calls')+
  xlab ('Day in month')+
  ggtitle('Uber calls by day')

ggplot(aes(x = time, y=count), data =july1) +
  geom_bar(stat="identity")+
  facet_wrap(~date)+
  ylab ('Uber calls')+
  ggtitle('Uber calls by date and time')

ggplot(aes(x = time, y=count), data =july1) +
  geom_bar(stat="identity")+
  facet_wrap(~distance1)+
  ylab ('Uber calls')+
  ggtitle('Uber calls by date and time')

table(july1$count,july1$time)

prop.table(table(july1$count,july1$time))

#the same steps we have made to create the 'ab' and 'bc' data frames
vec2 <- by(july1$count, july1$distance1, sum)
vec2
vec3 <- sapply(vec2, x2)
vec3
cd <- data.frame(vec3)

#we wanted to check the correlation between the factorial distance we created
#to the real distance of the pickup from the NY Stock exchange
cd$distance <- c(2,3,1,4)
cor(cd$distance,cd$vec3)  


#Now is the part where we include the S&P 500

#loading the data
snp <- read.csv("S&P 500 Historical Data.csv", header = TRUE)

#formatting the dates 
snp$date <- as.POSIXct(snp$ï..Date,format ='%d-%b-%y', tz='GMT') 

#subsetting the relevant dates
snp_july <- subset(snp, snp$date>='2014-07-01'&snp$date<'2014-08-1')

#making the date variable as a characteristic one
snp_july$date <- as.character(snp_july$date)
july1$dates <- as.character(july1$dates)

#now we create a new data frame contains both uber's and S&P's
total <- merge(july1,snp_july,by.x = 'date',by.y = 'date',all = TRUE, no.dups=FALSE)

#removing the NA's from the data
total1 <- subset(total, is.na(Change..)==FALSE)

#transforming the percents into characters for the next step
total1$Change.. <- as.character(total1$Change..)

#generating a new dummy variable that determines whether the change was possitive or negative
total1$up_down <- ifelse(total1$Change..>0,'up','down')

#removing non-relevant variables from the data
total1$ï..Date <- NULL
total1$Vol. <- NULL
total1$Date.Time <- NULL

#transform the change variable into a numeric so that we could anlyze it
total1$Change.. = as.numeric(gsub("[\\%,]", "", total1$Change..))

#the same steps we have made to create the 'ab' and 'bc' data frames
y1 <- by(total1$count,total1$Change..,length)
y1
y3 <- sapply(y1,x2)
y3
y4 <- unique(total1$Change..)

#checking the correlation between the change variable to the count one
cor(y3,y4)

#generating new vectors that determines the mean of pickups by each time 
b1<- by(total1$count,total1$time,sum)/21
b2 <- by(july1$count,july1$time,sum)/31
b1
b2

#now we can compare between the two data frames
cbind(b1,b2)

#Descriptive statistics
ggplot(aes(x = time, y=count), data =total1) +
  geom_bar(stat="identity")+
  facet_wrap(~Change..)+
  ylab ('Uber calls')+
  ggtitle('Uber calls by change rate and time')

ggplot(aes(x = Change.., y=count), data =total1) +
  geom_bar(stat="identity")+
  facet_wrap(~distance1)+
  ylab ('Uber calls')+
  ggtitle('Uber calls by change rate and time')

#creating the Up_Down variable in the data frame of snp_july
snp_july$Change.. = as.numeric(gsub("[\\%,]", "", snp_july$Change..))
snp_july$up_down <- ifelse(snp_july$Change..>0 , 'Up', 'Down')
snp_july$up_down <- factor(snp_july$up_down)

#creating a vector of the number of days in which there was a possitive/negative change
f <- summary(snp_july$up_down)
f

#creating a vector of the sum of pickups made on either possitive/negative day
a <- by(total1$count, total1$up_down, length)
a

#now we can calculate the mean of the pickups by possitive or negative change
down_mean <- a[1]/f[1]
up_mean <- a[2]/f[2]
down_mean
up_mean
#it seems that there are more pickups on a negative change-interesting

#creating a new variable of the name of the day in the week (sunday/monday etc..) in each data frame
total1$day <- wday(total1$date, label = TRUE)
july1$day <- wday(july1$date, label = TRUE)

#removing the non-trading days (saturday,sunday)
levels(total1$day)[1] <- NA
levels(total1$day)[6] <- NA
levels(total1$day)

#the same steps we have made to create the 'ab' and 'bc' data frames
z5 <- by(total1$count, total1$day, sum)
z6 <- sort(unique(total1$day))
z7 <- sapply(z5,x2)
ba <- data.frame(z6,z7) #how many pickups were made in each day on the week

#Descriptive statistics
plot(by(ba$z7, ba$z6, sum),
     ylab = 'uber pickups',
     xlab = 'Moday-Friday')

ggplot(aes(x = time, y=count), data =total1) +
  geom_bar(stat="identity")+
  facet_wrap(~day)+
  ylab ('Uber calls')+
  ggtitle('Uber calls by day in week and time (without Saturday and Sunday)')

ggplot(aes(x = time, y=count), data =july1) +
  geom_bar(stat="identity")+
  facet_wrap(~day)+
  ylab ('Uber calls')+
  ggtitle('Uber calls by day in week and time (with Saturday and Sunday)')

ggplot(aes(x = time, y=count), data =total1) +
  geom_bar(stat="identity")+
  facet_wrap(~up_down)+
  ylab ('Uber calls')+
  ggtitle('Was the change rate Positive/Negative')

total1$dates1 <- as.character(total1$dates1)
w1 <- by(total1$Base,total1$dates1,length)
w1 #includes all the time intervals and how many pickups made in each one
w2 <- sort(unique(total1$dates1))
w2 #a variable of all the time intervals
w3 <- sapply(w1, x2)
dc <- data.frame(w2,w3) #data frame of each time interval and the pickups made

#a new data frame contains all the relevant variables
total2 <- merge(total1,dc,by.x = 'dates1',by.y = 'w2',all = TRUE, no.dups=TRUE)
total2$day <- factor(total2$day)

#now we created new data frame that we could work with and not change the original one
b10 <- sort(unique(july1$dates1))
b11 <- data.frame(b10,q3)
b11$date_time <- b11$b10
b11 <- tidyr::separate(b11, b10, c("date", "time"), sep = " ")
b11$date_time <- as.character(b11$date_time)
b10
b11 <- merge(b11,snp_july,by.x = 'date',by.y = 'date',all = TRUE, no.dups=FALSE)
b11$up_down <- ifelse(is.na(b11$up_down),'not',ifelse(b11$Change..>0,'up','down'))#not means that it was no trading day
b11$day <- wday(b11$date, label = TRUE)
b11$date <- factor(b11$date)

#this is the model that we estimated for the non-S&P 500 data (first part of the project)
fit_old <- lm(q3~day+time+time*day, data = b11)

#this are some of the models we have estimated and compared (there were more)
fit <- lm(q3~time+day+up_down+time*day+time*up_down+day*up_down+time*day*up_down , data = b11)
fit_w <- lm(w3~time+day+up_down+time*day+time*up_down+day*up_down+time*day*up_down , data = total2)
fit_log <- lm(log(q3)~time+day+up_down+time*day+time*up_down+day*up_down+time*day*up_down , data = b11)
fit_log_w <- lm(log(w3)~time+day+up_down+time*day+time*up_down+day*up_down+time*day*up_down , data = total2)

#the summary of all the models
summary(fit)
summary(fit_w)
summary(fit_old)
summary(fit_log)
summary(fit_log_w)


#The test data

#loading the data
uber_test <- read.csv("uber_test.csv", header = TRUE)

#formatting the dates
uber_test$Time_Interval <- as.POSIXct(uber_test$Time_Interval,format ='%d-%m-%y %H:%M', tz='GMT') 

#setting the day's name variable
uber_test$day <- wday(uber_test$Time_Interval, label = TRUE)
uber_test$Time_Interval <- as.character(uber_test$Time_Interval)

#subsetting the relevant dates for the prediction
snp_sep <-  subset(snp, snp$date>='2014-09-01'&snp$date<'2014-10-01')
snp_sep$date <- as.character(snp_sep$date)

#transforming the change variable to a numeric one
snp_sep$Change.. = as.numeric(gsub("[\\%,]", "", snp_sep$Change..))

#creating the up&down variable
snp_sep$up_down <- ifelse(snp_sep$Change..>0,'up','down')

#generating the dates variable
uber_test$date_time <- uber_test$Time_Interval
uber_test <- tidyr::separate(uber_test, Time_Interval, c("date", "time"), sep = " ")
uber_test$date_time <- as.character(uber_test$date_time)

#merging the test data with the S&P data and performing the same ammends
uber_test1 <- merge(uber_test,snp_sep,by.x = 'date',by.y = 'date',all = TRUE, no.dups=FALSE)
uber_test1$up_down <- ifelse(is.na(uber_test1$up_down),'not',ifelse(uber_test1$Change..>0,'up','down'))
uber_test1_yes <- subset(uber_test1,uber_test1$up_down!='not')

#creating the predictions
b11$count <- predict(fit)
uber_test1$count <- predict(fit_log,newdata = uber_test1)
uber_test1$count_without_snp <- predict(fit_old,newdata = uber_test1)
uber_test1$count <- exp(uber_test1$count)

#creating the new data frame and making it look at the specified format 
uber_test <- data.frame(uber_test1$date_time,uber_test1$count,uber_test1$count_without_snp)
uber_test$Time_Interval <- uber_test$uber_test1.date_time
uber_test$pick_num_withoutSP <- uber_test$uber_test1.count_without_snp
uber_test$pick_num_withSP <- uber_test$uber_test1.count
uber_test$uber_test1.date_time <- NULL
uber_test$uber_test1.count_without_snp <- NULL
uber_test$uber_test1.count <- NULL

#writing the csv file with the predictions
write.csv(uber_test,"C:/Users/Omer Gralnik/Desktop/Economics in Big Data World/Final Project/uber_test.csv", row.names = FALSE)



