library(tidyverse)
library(lubridate)
getwd()
setwd("/Users/Maahir/Desktop")
#=====================
# STEP 1: COLLECT DATA
#=====================
q1_2019 <- read_csv("Q1_2019_Final.csv")
q2_2019 <- read_csv("Q2_2019_Final.csv")
q3_2019 <- read_csv("Q3_2019_Final.csv")
q4_2019 <- read_csv("Q4_2019_Final.csv")

#====================================================
# STEP 2: COMBINE INTO A SINGLE FILE
#====================================================
all_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019)
View(all_trips)
#df <- data.frame(all_trips)
#write.csv(df, "/Users/Maahir/Desktop/2019_Full_Final.csv", row.names=FALSE)
#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)
mean(all_trips$tripduration)
median(all_trips$tripduration)
max(all_trips$tripduration)
min(all_trips$tripduration)
aggregate(all_trips$tripduration ~ all_trips$usertype, FUN = mean)
aggregate(all_trips$tripduration ~ all_trips$usertype, FUN = median)
aggregate(all_trips$tripduration ~ all_trips$usertype, FUN = max)
aggregate(all_trips$tripduration ~ all_trips$usertype, FUN = min)
aggregate(all_trips_v2$tripduration ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$tripduration ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(start_date = as.POSIXct(start_date, format = "%m/%d/%Y")) %>%
  mutate(weekday = wday(start_date, label = TRUE)) %>%
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(tripduration)) %>%
  arrange(usertype, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

all_trips_v2 %>%
  mutate(start_date = as.POSIXct(start_date, format = "%m/%d/%Y")) %>%
  mutate(weekday = wday(start_date, label = TRUE)) %>%
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(tripduration)) %>%
  arrange(usertype, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")
