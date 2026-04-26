#This will be the R document I use to do all the analysis before I make a "new" one with the R-markdown final analysis


library(tidyverse)
library(here)


#Import data

file <- "activity.csv"

file_pathway <- file.path(here(),"Data/Reproducible_Research",file)

Activity_Data <- read.csv(file_pathway)

#Convert to a date and in this case, I am not going to use lubridate package which is very helpful

Activity_Data$date <- as.Date(Activity_Data$date, format = "%Y-%m-%d")



#What is the mean number of steps taken per day per 5 minute interval (not needed but its interesting to know)

Activity_Data %>%
  group_by(date) %>%
  summarize(mean = mean(steps, na.rm = TRUE),
            median = median(steps, na.rm = TRUE))

#What is the mean and median # of steps taken each day through the entire period of time (about 2 months)?

Daily_Steps <- Activity_Data %>% 
  group_by(date) %>% 
  summarize(Daily_Steps = sum(steps, na.rm = TRUE))

paste("The mean # of steps per day taken is", round(mean(Daily_Steps$Daily_Steps), 1),
      "and the median # of steps per day taken is", round(median(Daily_Steps$Daily_Steps), 1))

Daily_Steps %>%
  ggplot(aes(x = Daily_Steps))+
  geom_histogram(bins = 50)+
  theme_bw()+
  labs(title = paste("Histogram of Total Daily Steps from",Daily_Steps$date[1], "to",Daily_Steps$date[length(Daily_Steps$date)]),
       x = "Total Daily Steps",
       y = "Occurences",
       subtitle = "The mean # of steps is in Blue and the median # of steps is in red")+
  geom_vline( xintercept = (round(mean(Daily_Steps$Daily_Steps))),
              show.legend = TRUE,
              color = "blue")+
  geom_vline( xintercept = (round(median(Daily_Steps$Daily_Steps))),
              show.legend = TRUE,
              color = "red")


#What is the average daily activity pattern?

min_to_time <-function(minutes = 60){
  
  minutes <- round(minutes, digits = 2)
  
  hours_since_midnight <- minutes / 60
  
  #adds an extra "0" to the character if the time segment isn't 2 digits so that the format is consistent
  hour <- floor(hours_since_midnight)
  
  min <- floor(minutes %% 60)
  
  sec <- ((minutes %% 60) - min)*60
  
  #adds an extra "0" to the character if the time segment isn't 2 digits so that the format is consistent
  hour <- ifelse(hour < 10, paste0("0",hour), hour)
  min <- ifelse(min < 10, paste0("0",min), min) 
  sec <- ifelse(sec < 10, paste0("0",sec), sec)
  
  
  final_time <- paste0(hour,":",min,":",sec)
  return(final_time)
}

#My code works, but because of weirdness in the data with how it jumps from 55 in the time interval to 100 (which I thought was equal to minutes but it clearly isn't, so i can't connect it to the time as well)
#I could spend time in the future figuring out how to do it, but its not worth it right now
Time_modulated_Data <- Activity_Data %>%
  mutate(hour_min_sec = min_to_time(interval),
         full_time = paste(as.character(date)),
         full_time_2 = paste(full_time, hour_min_sec), 
  full_time_3 = strptime(full_time_2, "%Y-%m-%d %H:%M:%S")) %>%
  select(steps, date, interval, hour_min_sec, final_time = full_time_3)


###### I am going to down here answer the initial question

Time_Series_Data <- Activity_Data %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE),
            median_steps = median(steps, na.rm = TRUE))

with(Time_Series_Data, plot(interval, mean_steps, type = "l", 
                            xlab = "Time interval",
                            ylab = "Mean steps taken per  period",
     col = "#311432"))


Time_Series_Data %>%
  filter(mean_steps == max(mean_steps))



#Inputting missing values and what to change them too

NA_Data <- Activity_Data[is.na(Activity_Data$steps) == TRUE,]

dim(NA_Data)

paste("The # of rows of NA data is",dim(NA_Data)[1],"and the # of columns is",dim(NA_Data)[2])

#Strategy for inputting missing data will be to take the mean # of steps for that time period each day and then graph a histograph

Filled_Data <- NA_Data %>%
  inner_join(., Time_Series_Data, by = "interval") %>%
  mutate(steps = mean_steps) %>%
  select(steps, date, interval) %>%
  bind_rows(.,Activity_Data) %>%
  drop_na()

#I asked AI to help a 2nd method to see if there is a better way to do this and there is

Activity_Data %>%
  group_by(interval) %>%
  mutate(steps = as.numeric(steps)) %>%
  mutate(steps = replace_na(steps, mean(steps, na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(Daily_Steps = sum(steps, na.rm = TRUE)) %>%
  ggplot(aes(x=Daily_Steps))+
  geom_histogram(bins = 50)+
  theme_bw()
  
  
  
#Now I am going to make the graph!
Filled_Data %>%
  group_by(date) %>% 
  summarize(Daily_Steps = sum(steps, na.rm = TRUE)) %>%
  ggplot(aes(x=Daily_Steps))+
  geom_histogram(bins = 50)+
  theme_bw()



### Are there differences in activity patterns between weekdays and weekends?


Activity_Data$day <- weekdays(Activity_Data$date)

Activity_Data$day_type <- ifelse(Activity_Data$day != c("Saturday","Sunday"), "Weekday", "Weekend")


##ggplot example

Activity_Data %>%
  group_by(interval, day_type) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE)) %>%
  ggplot(aes(x=interval, y = mean_steps, color = day_type))+
  geom_line()+
  theme_bw()+
  labs(y = "Mean # of Steps per day",
       x = "Time Interval",
       title = "Daily Mean Steps across each time interval for the dayfor Weekdays and Weekend days")

#non-ggplot graph (although I am still using dplyr rather than tapply)

weekend_data <- subset(Activity_Data, day_type == "Weekend")

weekend_data <- weekend_data %>% group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))
  
weekday_data <- subset(Activity_Data, day_type == "Weekday")

weekday_data <- weekday_data %>% group_by(interval) %>%
              summarize(mean_steps = mean(steps, na.rm = TRUE))


with(weekday_data, plot(x = interval, y = mean_steps, xlab = "Time Interval", type ="l",
                        ylab = "Mean # of Steps per day", col = "blue"))
with(weekend_data, points(x=interval, y = mean_steps, col = "red", type = "l"))
