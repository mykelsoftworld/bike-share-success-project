setwd("E:/datasets/Trip date set")
if(!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require("lubridate")){
  install.packages("lubridate")                                       #helps in date formats
  library(lubridate)
}
if(!require("ggplot2")){
  install.packages("ggplot2")
  library(ggplot2) 
}                                                   #for visualizing the data
if(!require("dplyr")){
  install.packages("dplyr")
}
library(dplyr)                                                      #for cleaning the data
if(!require("tidyr"))
{install.packages("tidyr")
  library(tidyr) 
}
if(!require("geosphere")){
  install.packages("geosphere")                                         ## helps in finding distance between geographic coordinates
  library(geosphere)
}

#import trip data
tripdata01<- read.csv("202101-divvy-tripdata.csv")
tripdata02<- read.csv("202102-divvy-tripdata.csv")
tripdata03<- read.csv("202103-divvy-tripdata.csv")
tripdata04<- read.csv("202104-divvy-tripdata.csv")
tripdata05<- read.csv("202105-divvy-tripdata.csv")
tripdata06<- read.csv("202106-divvy-tripdata.csv")
tripdata07<- read.csv("202107-divvy-tripdata.csv")
tripdata08<- read.csv("202108-divvy-tripdata.csv")
tripdata09<- read.csv("202109-divvy-tripdata.csv")
tripdata10<- read.csv("202110-divvy-tripdata.csv")
tripdata11<- read.csv("202111-divvy-tripdata.csv")
tripdata12<- read.csv("202112-divvy-tripdata.csv")


tripdata01v2<-tripdata01%>%na_if("")%>%na.omit()
tripdata02v2<-tripdata01%>%na_if("")%>%na.omit()
tripdata03v2<-tripdata03%>%na_if("")%>%na.omit()
tripdata04v2<-tripdata04%>%na_if("")%>%na.omit()
tripdata05v2<-tripdata05%>%na_if("")%>%na.omit()
tripdata06v2<-tripdata06%>%na_if("")%>%na.omit()
tripdata07v2<-tripdata07%>%na_if("")%>%na.omit()
tripdata08v2<-tripdata08%>%na_if("")%>%na.omit()
tripdata09v2<-tripdata09%>%na_if("")%>%na.omit()
tripdata10v2<-tripdata10%>%na_if("")%>%na.omit()
tripdata11v2<-tripdata11%>%na_if("")%>%na.omit()
tripdata12v2<-tripdata12%>%na_if("")%>%na.omit()

str(tripdata07)
str(tripdata07v2)

#Combine all the data sets, we have data from Jan, 2021 to Dec, 2021

combined_data<-rbind(tripdata01v2,tripdata02v2,tripdata03v2,tripdata04v2,tripdata05v2,tripdata06v2,tripdata07v2,tripdata08v2,tripdata09v2,tripdata10v2,tripdata11v2,tripdata12v2)
str(combined_data)

##checking consistency in ride id column

combined_data$start_station_id <- trimws(combined_data$start_station_id, which = c("both"))
combined_data$end_station_id <- trimws(combined_data$end_station_id, which = c("both"))
#process the data for analysis
##categorical representation of rideeable type and the frequency in the form of a table.
table(combined_data$rideable_type)   #classic > electric > docked
##this will show us occurences of each rideable_type
str(combined_data)

##categorical representation of membership status and the frequency in the form of a table.
table(combined_data$member_casual)                                   ##no typos

##converting date strings to datetime object and making a consistent format
combined_data$started_at <- ymd_hms(combined_data$started_at)                ##changes to YYYY-MM-DD HH:MM:SS
combined_data$ended_at <- ymd_hms(combined_data$ended_at) 

##confirming
str(combined_data)

##dropping rows with all nulls 
dim(combined_data)
combined_data <- drop_na(combined_data)
dim(combined_data)

## Adding columns that list the date, month, day, and year of each ride, we will need it for analysis later
combined_data$hour_of_day <-as.numeric(format(as.POSIXct(combined_data$started_at), format = "%H"))
combined_data$date <- as.Date(combined_data$started_at)                      #The default format is yyyy-mm-dd
combined_data$month <- format(as.Date(combined_data$date), "%m")             #stores month from the date
combined_data$day <- format(as.Date(combined_data$date), "%d")               #stores the day from the date
combined_data$year <- format(as.Date(combined_data$date), "%Y")              #stores the year from the date
combined_data$day_of_week <- format(as.Date(combined_data$date), "%A")       #stores the day ofthe week from the date

# Adding a day_period column that will be used for analysis 
combined_data <- combined_data%>%
  mutate(day_period = case_when(hour_of_day >= 0 & hour_of_day <=6 ~ "night",
                                hour_of_day >6 & hour_of_day <=12 ~ "morning",
                                hour_of_day > 12 & hour_of_day <= 18 ~ "afternoon",
                                hour_of_day > 18 & hour_of_day <=23  ~ "evening"))

str(combined_data_v3)
## finding time length of ride(in seconds) to check if there are any negatives and for future analysis need
combined_data$ride_length <- difftime(combined_data$ended_at,combined_data$started_at)
glimpse(combined_data)
combined_data$ride_length <- as.numeric(as.character(combined_data$ride_length)) ##changing to a numeric format for using it in analysis

##checking if we correctly changed ride_length to numeric format
glimpse(combined_data)


##filter outliers by filtering out data with negative ride lenght and ride lenght above 24hrs(24x60x60 = 86400)
##  filter(ride_length<0|ride_length>86400)
##yes we have a few such rows, we will create a new table to store the needed data
combined_data_v2 <- combined_data%>% filter(ride_length >0 && ride_length<86400) ##or (length of ride >0 && length of ride <86400)

##checking how the rows number changes
str(combined_data_v2)

##checking to see if we have any duplicate ride_ids
table_0 <- combined_data_v2[duplicated(combined_data_v2$ride_id),]
table_0 %>% print(n=40) 
##we dont have any duplicate ride_id, can remove table_0

##We want to find distance between the end and start locations of the ride for data given as geographic coordinates

##Adding column ride_distance for finding dist. between coordinates with column headers start_lng, start_lat, end_lang, end_lat
combined_data_v2$ride_distance <- distGeo(matrix(c(combined_data_v2$start_lng, combined_data_v2$start_lat), ncol = 2), matrix(c(combined_data_v2$end_lng, combined_data_v2$end_lat), ncol = 2))

##Adding column ride_route 
combined_data_v2$ride_route<-paste(combined_data_v2$start_station_id,"--",combined_data_v2$end_station_id)

str(combined_data_v2)
##  arrange(-ride_distance)
#write.csv(combined_data_v2,"combine_data.csv", row.names=FALSE)
#Analysis of data
table(combined_data_v2$day_of_week)

##in order to sort in a predefined order  arranging, we need to input the levels for  days 
combined_data_v2 $day_of_week <- 
  ordered(combined_data_v2$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

combined_data_v2 $day_period <- 
  ordered(combined_data_v2$day_period, levels = c('morning', 'afternoon', 'evening', 'night'))


##Data agreegation for analysis

table_1<-combined_data_v2  %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(day_of_week)%>%
  print(n=20)

table_2<- combined_data_v2  %>%
  group_by(member_casual, day_of_week) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(day_of_week)%>%
  print(n=30)

table_3<-combined_data_v2  %>%
  group_by(member_casual, day_period) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(day_period)%>%
  print(n=10)
# table_14<-combined_data_v2  %>%
#   group_by(member_casual,day_of_week, day_period) %>%
#   summarise(number_of_ride = n(), .groups = 'drop') %>%
#   arrange(day_of_week)%>%
#   print(n=60)
table_4<-combined_data_v2  %>%
  group_by(member_casual, ride_route) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(desc(number_of_ride))%>%
  print(n=100)

table_5<-combined_data_v2  %>%
  group_by(member_casual, start_station_name) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(desc(number_of_ride),start_station_name)%>%
  print(n=100)

##in order to sort in a predefined order  arranging, we need to input the levels for months, days as our analysis results will be arranged by months and days
combined_data_v2 $month <-
  ordered(combined_data_v2 $month, levels = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11','12'))


##for analysing number of rides for every month in a summary for both casual riders and members
##here, I wanted to see all rows of table instead of a tibble( shows 10 rows), so I took values in a temporary table
table_6<-combined_data_v2  %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(month) %>%##for analysing AVERAGE TIME LENGTH of rides through each day of week in a summary for both casual riders and members
  
  
  ##aggregating for length of ride, for each week for both casual riders and members
  aggregate(combined_data_v2$ride_length + combined_data_v2$member_casual + combined_data_v2$day_of_week, FUN=mean)

##using a temporary table to see all rows
table_7 <-combined_data_v2  %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  arrange(month)%>%
  print(n=30)

##for analysing AVERAGE ride distance through each day of week in a summary for both casual riders and members
table_8<-combined_data_v2  %>%
  group_by(member_casual, day_of_week) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(day_of_week)%>%
  print(n=30)

table_9 <-combined_data_v2  %>%
  group_by(member_casual, month) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(month)%>%
  print(n=30)
##
# table_4<-combined_data_v2  %>%
#   group_by(member_casual, day_of_week) %>%
#   summarise(number_of_ride = n(), .groups = 'drop') %>%
#   arrange(day_of_week)%>%
#   print(n=20)
# 
# table_5<-combined_data_v2  %>%
#   group_by(member_casual, day_period) %>%
#   summarise(number_of_ride = n(), .groups = 'drop') %>%
#   arrange(day_period)%>%
#   print(n=10)
# # table_14<-combined_data_v2  %>%
# #   group_by(member_casual,day_of_week, day_period) %>%
# #   summarise(number_of_ride = n(), .groups = 'drop') %>%
# #   arrange(day_of_week)%>%
# #   print(n=60)
# table_6<-combined_data_v2  %>%
#   group_by(member_casual, ride_route) %>%
#   summarise(number_of_ride = n(), .groups = 'drop') %>%
#   arrange(desc(number_of_ride))%>%
#   print(n=100)
# 
# table_14<-combined_data_v2  %>%
#   group_by(member_casual, start_station_name) %>%
#   summarise(number_of_ride = n(), .groups = 'drop') %>%
#   arrange(desc(number_of_ride),start_station_name)%>%
#   print(n=100)



table_10<-combined_data_v2  %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n() , .groups = 'drop')

table_11 <-combined_data_v2  %>%
  group_by(rideable_type) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(rideable_type)%>%
  print()

table_12<- combined_data_v2  %>%
  group_by(member_casual, rideable_type) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(rideable_type)%>%
  print(n=30)

table_13<-combined_data_v2  %>%
  group_by(rideable_type, start_station_name) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(desc(rideable_type),desc(number_of_ride))%>%
  print(n=100)

#Data visualisation


##plotting for Comparing number of rides on days for casual riders and members
combined_data_v2  %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")+
  labs(tag="1.", title="Comparing number of rides on days for casual riders and members", subtitle = "Data for Dec, 2020 - Nov, 2021")+
  labs(x="Days of week", y="Number of rides")

##plotting for Comparing number of rides monthly for casual riders and members
combined_data_v2  %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")+
  labs(tag="2.",title="Comparing number of rides monthly for casual riders and members", subtitle = "Data for Dec, 2020 - Nov, 2021")+
  labs(x="Months", y="Number of Rides")

##plotting for Comparing average ride time by days for casual riders and members
combined_data_v2  %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = (average_ride_length/60), fill =  member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")+
  labs(tag="3.",title="Comparing average ride time by days for casual riders and members", subtitle = "Data for Dec, 2020 - Nov, 2021")+
  labs(x="Days of week", y="Average time of ride (Minutes)")

##plotting for Comparing average ride time by months for casual riders and members
combined_data_v2  %>%
  group_by( member_casual, month) %>%
  summarise(average_ride_length = mean(length_of_ride), .groups = 'drop') %>%
  ggplot(aes(x = month, y = (average_ride_length/60), fill =  member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")+
  labs(tag="4.",title="Comparing average ride time by months for casual riders and members", subtitle = "Data for Dec, 2020 - Nov, 2021")+
  labs(x="Months", y="Average time of ride (Minutes)")

##plotting for Variation in rides number vs rides distance (metres) for casual riders and members
combined_data_v2  %>%
  group_by(member_casual) %>%
  filter(ride_distance < 10000) %>% #Remove outliers
  ggplot(aes(x = ride_distance, fill = member_casual)) + 
  geom_histogram()+#Disregard binwidth
  labs(tag="5.",title="Variation in rides number vs rides distance (metres) for casual riders and members", subtitle = "Data for jan, 2021 - Dec, 2021")+
  labs(x="Ride distance (metres)", y="Number of Rides")

##Variation in average riding distance (metres) through days for casual riders and members
combined_data_v2  %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_distance = mean(ride_distance), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = average_ride_distance, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")+
  labs(tag="6.",title="Variation in average riding distance (metres) through day for casual riders and members", subtitle = "Data for Dec, 2020 - Nov, 2021")+
  labs(x="Days", y="Ride distance (metres)")

##Variation in average riding distance (metres) for each month for casual riders and members 
combined_data_v2  %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_distance = mean(ride_distance), .groups = 'drop') %>%
  ggplot(aes(x = month, y = average_ride_distance, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")+
  labs(tag="7.",title="Variation in average riding distance (metres) through months for casual riders and members", subtitle = "Data for Dec, 2020 - Nov, 2021")+
  labs(x="Months", y="Ride distance (metres)")

##plot for frequency of each rideable type for both casual riders and members
combined_data_v2  %>%
  group_by(member_casual) %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) + 
  geom_histogram(position = "dodge", stat = "count")+
  labs(tag="8.",title="Plot for the frequency of each rideable_type ", subtitle = "Data for Dec, 2020 - Nov, 2021")+
  labs(x="rideable_type", y="Rides count")


##end_of_code