
#installing the required packages
#tidyverse for data import and wrangling
#lubridate for data functions
#ggplot2 for data visualization

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle data attributes
library(ggplot2) #helps visualize directory
getwd() #displays your working directory
setwd("C:/Users/tejes/OneDrive/Documents/GDA_Project_Quartery_data") #to set directly file wher the files are located

#step1: collect data and read the csv files

q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#step2: wrangle data and combine into a single file

colnames(q2_2019)
colnames(q4_2019)
colnames(q3_2019)
colnames(q1_2020)

#rename column to make them consistent with q1_2020 

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type=bikeid
                   ,started_at=start_time
                   ,ended_at=end_time
                   ,start_station_name=from_station_name
                   ,start_station_id=from_station_id
                   ,end_station_name=to_station_name
                   ,end_station_id=to_station_id
                   ,member_casual=usertype))
(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,memeber_casual=usertype))
(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID"
                   ,started_at = "01 - Rental Details Local Start Time"
                   ,ended_at = "01 - Rental Details Local End Time"
                   ,start_station_name = "03 - Rental Start Station Name"
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name  = "02 - Rental End Station Name"
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))
#Inspect the data frames and look for incongruities

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

#convert ride_id and rideable_type to character so that they can stack corrently
q4_2019 <- mutate(q4_2019, ride_id=as.character(ride_id)
          ,rideable_type = as.character(rideable_type))

q3_2019 <- mutate(q3_2019,ride_id = as.character(ride_id)
           ,rideable_type = as.character(rideable_type))

q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id)
           ,rideable_type = as.character(rideable_type))

#stack individual quater's data frames in to one big data frame
all_trips <- bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)

#remove lat, long, birth year, and gender fields ad this data was dropped beginning in 2020
all_trips <- all_trips %>%
  select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,"01 - Rental Details Duration In Seconds Uncapped",
            "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

"STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS"
colnames(all_trips) #list all names
nrow(all_trips)#how many rows are there in the data frame
dim(all_trips)#Dimensions of the data frame
head(all_trips)#to check the first six rows of the data frame
str(all_trips)#see the list of colums and data types (number, character, etc)
summary(all_trips)#statistical summary of data. Maainly for numerics

table(all_trips $ member_casual)#to check how many observations fall under each usertype

#Reassign to the desired values(we will go with the current 2020 labels)

all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                ,"Subscriber" = "member"
                ,"Customer" = "casual"))

table(all_trips$member_casual) # to check the data

#adding columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) #the default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d%")
all_trips$year <- format(as.Date(all_trips$date), "%Y%")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A%")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#Inspect the structure of the column
str(all_trips)

#Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#Remove "bad" data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#step 4: CONDUCT DESCRIPTIVE ANALYSIS
#===========================================
  #Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length)#straight average(total ride length / rides)
median(all_trips_v2$ride_length)#midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length)#longest ride
min(all_trips_v2$ride_length)#shortest ride

summary(all_trips_v2$ride_length)

#Compare members and casual users
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=mean)
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=median)
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=max)
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=min)        

#see the average ride time by each day for members vs causal users
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual+all_trips_v2$day_of_week,FUN=mean)
                  
#day of the week are out of the orders, so below code we are fixing that
 #all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Now, lets run the average ride by each day for members vs casual users
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual+all_trips_v2$day_of_week,FUN = mean)


#analyze ridership data by type and weekday


all_trips_v2$memeber_casual = NULL

#Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual,weekday)%>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides,fill=member_casual))+
  geom_col(position = "dodge")

#Let's create a visualization for average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual))+
  geom_col(position =  "dodge")

 #STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#creating a csv file that we will visualize in excel,Tableau, or for presentation
counts<- aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual +
                     all_trips_v2$day_of_week,FUN=mean)
write.csv(counts,file = 'avg_ride_length.csv')


