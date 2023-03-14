#Installed packages for data manipulation
install.packages('tidyverse')
install.packages('lubridate')
install.packages('data.table')

#Load packages
library(tidyverse)
library(lubridate)
library(data.table)


#Load Divy CSV files
jan22 <- read.csv('202201-divvy-tripdata.csv')
feb22 <- read.csv('202202-divvy-tripdata.csv')
mar22 <- read.csv('202203-divvy-tripdata.csv')
apr22 <- read.csv('202204-divvy-tripdata.csv')
may22 <- read.csv('202205-divvy-tripdata.csv')
jun22 <- read.csv('202206-divvy-tripdata.csv')
july22 <- read.csv('202207-divvy-tripdata.csv')
aug22 <- read.csv('202208-divvy-tripdata.csv')
sept22 <- read.csv('202209-divvy-publictripdata.csv')
oct22 <- read.csv('202210-divvy-tripdata.csv')
nov22 <- read.csv('202211-divvy-tripdata.csv')
dec22 <- read.csv('202212-divvy-tripdata.csv')

#Combind data into one dataframe
bike_df <- rbind(jan22,feb22,mar22,apr22,may22,jun22,july22,aug22,sept22,oct22,nov22,dec22)

#Remove unnecessary dataframes to create space in Environment
remove(jan22,feb22,mar22,apr22,may22,jun22,july22,aug22,sept22,oct22,nov22,dec22)

bike_df$ride_length <- difftime(bike_df$ended_at, bike_df$started_at, units = "mins") #Calculate ride legnth column
bike_df$date <- as.Date(bike_df$started_at) ###Calculate date column
bike_df$day_of_week <- wday(bike_df$started_at) ###Calculate day of week column
bike_df$month <- format(as.Date(bike_df$date), "%m") ###Calculate month column
bike_df$day <- format(as.Date(bike_df$date), "%d") ###Calculate day column
bike_df$year <- format(as.Date(bike_df$date), "%y") ###Calculate year column
bike_df$start_time <- format(as.POSIXct(bike_df$started_at), format= "%H:%M:%S") ##Calculate start time column
bike_df$start_hour <- hour(bike_df$started_at) ###Calculate hour column

#Create columns for different seasons of the year
bike_df <- bike_df %>% mutate(season= case_when(month == "01" ~ "Winter",
                                                month == "02" ~ "Winter",
                                                month == "03" ~ "Spring",
                                                month == "04" ~ "Spring",
                                                month == "05" ~ "Spring",
                                                month == "06" ~ "Summer",
                                                month == "07" ~ "Summer",
                                                month == "08" ~ "Summer",
                                                month == "09" ~ "Fall",
                                                month == "10" ~ "Fall",
                                                month == "11" ~ "Fall",
                                                month == "12" ~ "Winter"))

#Creat columns for different times of day
bike_df <- bike_df %>% mutate(time_of_day = case_when(start_hour == "0" ~ "Night",
                                                      start_hour == "1" ~ "Night",
                                                      start_hour == "2" ~ "Night",
                                                      start_hour == "3" ~ "Night",
                                                      start_hour == "4" ~ "Night",
                                                      start_hour == "5" ~ "Night",
                                                      start_hour == "6" ~ "Morning",
                                                      start_hour == "7" ~ "Morning",
                                                      start_hour == "8" ~ "Morning",
                                                      start_hour == "9" ~ "Morning",
                                                      start_hour == "10" ~ "Morning",
                                                      start_hour == "11" ~ "Morning",
                                                      start_hour == "12" ~ "Afternoon",
                                                      start_hour == "13" ~ "Afternoon",
                                                      start_hour == "14" ~ "Afternoon",
                                                      start_hour == "15" ~ "Afternoon",
                                                      start_hour == "16" ~ "Afternoon",
                                                      start_hour == "17" ~ "Afternoon",
                                                      start_hour == "18" ~ "Evening",
                                                      start_hour == "19" ~ "Evening",
                                                      start_hour == "20" ~ "Evening",
                                                      start_hour == "21" ~ "Evening",
                                                      start_hour == "22" ~ "Evening",
                                                      start_hour == "23" ~ "Evening"))

#Create column for month's name
bike_df <- bike_df %>% mutate(month_name = case_when(month == "01" ~ "Janurary",
                                                month == "02" ~ "Feburary",
                                                month == "03" ~ "March",
                                                month == "04" ~ "April",
                                                month == "05" ~ "May",
                                                month == "06" ~ "June",
                                                month == "07" ~ "July",
                                                month == "08" ~ "August",
                                                month == "09" ~ "September",
                                                month == "10" ~ "Ocotber",
                                                month == "11" ~ "November",
                                                month == "12" ~ "December"))
#Clean Data
bike_df <- bike_df %>% select(-c(ride_id,started_at,ended_at,start_station_id, start_station_name,end_station_id,end_station_name,
                                 start_lat,start_lng,end_lat,end_lng))#Delete unneeded rows
bike_df <- distinct(bike_df) #Remove duplicate rows
bike_df <- na.omit(bike_df) #Remove NA values
bike_df  <- bike_df[!(bike_df$ride_length <=0),] #Remove 0 and negative values

#--------------------Total Rides--------------------
nrow(bike_df)

#----------By Type of Bike----------
bike_df %>% group_by(member_casual,rideable_type) %>% count(rideable_type) #Total Rides by Member Type
bike_df %>% group_by(rideable_type) %>% count(rideable_type) #Total Rides by Bike Type

#----------By Type of Member----------
bike_df %>% group_by(member_casual) %>% count(member_casual) #Rides by member type

#----------By Day of Week----------
bike_df %>% group_by(member_casual) %>% count(day_of_week) #Total rides by member type
bike_df %>% count(day_of_week) #Total ride

#----------By Month----------
bike_df %>% group_by(member_casual) %>% count(month) %>% print(n=24) #Total rides by member type
bike_df %>% count(month) #Total Rides

#----------By Day of month----------
bike_df %>% group_by(member_casual) %>% count(day) %>% print(n=62) #Total rides by member type
bike_df %>% count(day) #Total rides

#----------By Hour----------
bike_df %>% group_by(member_casual) %>% count(start_hour) %>% print(n=48) #Total rides by member type
bike_df %>% count(start_hour) #Total rides

#----------By Time of Day----------

#-----Morning-----
bike_df %>% group_by(member_casual) %>% filter(time_of_day == "Morning") %>% count(time_of_day) #Total rides by member
bike_df %>% filter(time_of_day == "Morning") %>% count(time_of_day) #Total rides

#-----Afternoon-----
bike_df %>% group_by(member_casual) %>% filter(time_of_day == "Afternoon") %>% count(time_of_day) #Total rides by member
bike_df %>% filter(time_of_day == "Afternoon") %>% count(time_of_day) #Total rides

#-----Evening-----
bike_df %>% group_by(member_casual) %>% filter(time_of_day == "Evening") %>% count(time_of_day) #Total rides by member
bike_df %>% filter(time_of_day == "Evening") %>% count(time_of_day) #Total rides

#-----Night-----
bike_df %>% group_by(member_casual) %>% filter(time_of_day == "Night") %>% count(time_of_day) #Total rides by member
bike_df %>% filter(time_of_day == "Night") %>% count(time_of_day) #Total rides

#-----All Times of Day-----
bike_df %>% group_by(member_casual) %>% count(time_of_day) #Total rides by member
bike_df %>% count(time_of_day) #Total rides

#----------Seasons----------

#-----Winter-----
bike_df %>% group_by(member_casual) %>% filter(season == "Winter") %>% count(season) #Total rides by member
bike_df %>% filter(season == "Winter") %>% count(season) #Total rides

#-----Spring-----
bike_df %>% group_by(member_casual) %>% filter(season == "Spring") %>% count(season) #Total rides by member
bike_df %>% filter(season == "Spring") %>% count(season) #Total rides

#-----Summer-----
bike_df %>% group_by(member_casual) %>% filter(season == "Summer") %>% count(season) #Total rides by member
bike_df %>% filter(season == "Summer") %>% count(season) #Total rides

#-----Fall-----
bike_df %>% group_by(member_casual) %>% filter(season == "Fall") %>% count(season) #Total rides by member
bike_df %>% filter(season == "Fall") %>% count(season) #Total rides

#-----All Seasons-----
bike_df %>% count(season) #Total rides by member
bike_df %>% count(season) #Total rides

#---------------Average Ride Legnth---------------

bike_avgRide <- mean(bike_df$ride_length)
print(bike_avgRide)

#----------By Type of Member----------
bike_df %>% group_by( member_casual) %>% summarise_at(vars(ride_length),list(time = mean))

#----------By Type of Bike----------
bike_df %>% group_by(member_casual,rideable_type) %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type

bike_df %>% group_by(rideable_type) %>% summarise_at(vars(ride_length), list(time=mean)) #Average by bike type

#----------By Day of Week----------
bike_df %>% group_by(member_casual,day_of_week) %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type

bike_df %>% group_by(day_of_week) %>% summarise_at(vars(ride_length), list(time=mean)) #Average by day of week

#----------By Month----------
bike_df %>% group_by(member_casual,month) %>% summarise_at(vars(ride_length), list(time=mean)) %>% print(n=24) #Average by member type

bike_df %>% group_by(month) %>% summarise_at(vars(ride_length), list(time=mean)) #Average by month

#----------By Day----------
bike_df %>% group_by(member_casual,month) %>% summarise_at(vars(ride_length), list(time=mean)) %>% print(n=24) #Average by member type

bike_df %>% group_by(month) %>% summarise_at(vars(ride_length), list(time=mean)) #Average by month

#----------By Hour----------
bike_df %>% group_by(member_casual,start_hour) %>% summarise_at(vars(ride_length), list(time=mean)) %>% print(n=48) #Average by member type

bike_df %>% group_by(start_hour) %>% summarise_at(vars(ride_length), list(time=mean)) %>% print(n=24) #Average by hour

#----------By Time of Day---------

#-----Morning-----
bike_df %>% group_by(member_casual) %>% filter(time_of_day == "Morning") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type
bike_df %>% filter(time_of_day == "Morning") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by morning

#-----Afternoon-----
bike_df %>% group_by(member_casual) %>% filter(time_of_day == "Afternoon") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type
bike_df %>% filter(time_of_day == "Afternoon") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by afternoon

#-----Evening------
bike_df %>% group_by(member_casual) %>% filter(time_of_day == "Evening") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type
bike_df %>% filter(time_of_day == "Evening") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by evening

#-----Night-----
bike_df %>% group_by(member_casual) %>% filter(time_of_day == "Night") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type
bike_df %>% filter(time_of_day == "Night") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by night

#-----All Times of Day-----
bike_df %>% group_by(member_casual) %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type
bike_df %>% summarise_at(vars(ride_length), list(time=mean)) #Average all times of day

#----------Season---------

#-----Winter-----
bike_df %>% group_by(member_casual) %>% filter(season == "Winter") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type
bike_df %>% filter(season == "Winter") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by Winter

#-----Spring-----
bike_df %>% group_by(member_casual) %>% filter(season == "Spring") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by member type
bike_df %>% filter(season == "Spring") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by Spring

#-----Summer-----
bike_df %>% group_by(member_casual) %>% filter(season == "Summer") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by Member Type
bike_df %>% filter(season == "Summer") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by Summer

#-----Fall-----
bike_df %>% group_by(member_casual) %>% filter(season == "Fall") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by Member Type
bike_df %>% filter(season == "Fall") %>% summarise_at(vars(ride_length), list(time=mean)) #Average by Fall

fwrite(bike_df,"bike_date.csv")



