library(lubridate)
library(data.table)
library(DataCombine)
library(plyr)
library(dplyr)
library(broom)
kickstarter <- read.csv("kickstarter.csv")
kickstarter$page_num <- factor(rep(c(rep(1,12), rep(2,12), rep(3,12), rep(4,12), rep(5,12), rep(6,12), rep(7,12), rep(8,12), rep(9,12), rep(10,12)),1164))
kickstarter$name <- factor(kickstarter$name)
kickstarter$category_name <- factor(kickstarter$category_name)
kickstarter$country <- factor(kickstarter$country)
kickstarter$currency <- factor(kickstarter$currency)
kickstarter$sort_by <- as.factor(kickstarter$sort_by)
kickstarter$state_changed_at <- as.Date(as.POSIXct(kickstarter$state_changed_at, origin="1970-01-01"))
kickstarter$created_at <- as.Date(as.POSIXct(kickstarter$created_at, origin="1970-01-01"))
kickstarter$launched_at <- as.Date(as.POSIXct(kickstarter$launched_at, origin="1970-01-01"))
kickstarter$deadline <- as.Date(as.POSIXct(kickstarter$deadline, origin="1970-01-01"))
kickstarter$campaign_duration <- as.numeric(kickstarter$deadline - kickstarter$launched_at)
kickstarter$running_day <- as.numeric(mdy(kickstarter$Date) - kickstarter$launched_at)
kickstarter$days_to_end <- as.numeric(kickstarter$deadline - mdy(kickstarter$Date))
kickstarter$Time <- ymd_hms(paste(mdy(kickstarter$Date), kickstarter$TimeStamp), tz = "EST")
kickstarter$time_diff <- ave(unclass(kickstarter$Time), kickstarter$name, FUN=function(x) c(NA, diff(x)))/3600
kickstarter$backer_arrival <- ave(kickstarter$backers_count, kickstarter$name, FUN=function(x) c(NA,diff(x)))
kickstarter$arrival_rate <- kickstarter$backer_arrival/ kickstarter$time_diff
kickstarter$revn_increase <- ave(kickstarter$converted_pledged_amount, kickstarter$name, FUN=function(x) c(NA,diff(x)))
kickstarter$avg_pledge <- kickstarter$revn_increase/ kickstarter$backer_arrival
kickstarter <- kickstarter[order(kickstarter$name),]
##Creating a function to lag backer_count
lg <- function(x){if(length(x)>1){c(NA, x[1:(length(x)- 1)])} else{NA}}
##Creating a lagged variable for backer_count
kickstarter.lag <- tapply(kickstarter$backers_count, kickstarter$name, lg)
kickstarter$backers_count.1 <- as.integer(unlist(kickstarter.lag))
kickstarter$perc_funded_lag <- as.numeric(unlist(tapply(kickstarter$percent_funded, kickstarter$name, lg)))
kickstarter$increase_in_perc_funded <- kickstarter$percent_funded - kickstarter$perc_funded_lag
kickstarter$lagged_total_pledge <- as.integer(unlist(tapply(kickstarter$pledged, kickstarter$name, lg)))
kickstarter$lagged_avg_pledge <- as.integer(unlist(tapply(kickstarter$avg_pledge, kickstarter$name, lg)))
kickstarter$lagged_rank <- as.integer(unlist(tapply(kickstarter$rank, kickstarter$name, lg)))
kickstarter$fully_funded_factor <- as.factor(unlist(lapply(kickstarter$percent_funded, function(x) {ifelse(x>=300,1,0)})))
#Removing negative arrival rates
kickstarter <- kickstarter[kickstarter$arrival_rate>0,]
kickstarter <- kickstarter[is.na(kickstarter$name)==FALSE,]
kickstarter <- kickstarter[kickstarter$revn_increase>=0,]
kickstarter <- kickstarter[kickstarter$arrival_rate!=Inf,]

##Regression equation in online appendix
j <- lm(rank ~ campaign_duration + days_to_end + perc_funded_lag + goal + fully_funded_factor + sort_by + name + category_name, data = kickstarter)
summary(j)$coefficients[1:9, 1:4]
glance(j)
summary(j)$coefficients[1:9, 1:4]
k <- summary(j)
k$coefficients[1:9]
confint(j)[1:9]
k$coefficients[1:9, 1:2]
k$coefficients[1:9, 1:4]
confint(j)[1:9, 1:2]
glance(j)
k$df
