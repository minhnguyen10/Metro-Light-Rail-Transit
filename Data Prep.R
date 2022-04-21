library(tidyverse)

processed_data <- read.csv("processed.csv")

#Adds day of the week from date as column DAY
split_dates <- strsplit(processed_data$DATE, ",")
days <- sapply(split_dates,"[[",1)
processed_data <- processed_data %>% add_column(DAY = days)

#Adds SEASON Variable
#Splits on the end of a month rather than on the 21st 
month_day <- sapply(split_dates,"[[",2)
split_month_day <- strsplit(month_day, " ")
month <- sapply(split_month_day,"[[",2)
processed_data <- processed_data %>% add_column(MONTH = month)
processed_data <- processed_data %>% 
  mutate(SEASON = case_when(
    MONTH %in% c('January', 'February', 'March') ~ 'Winter',
    MONTH %in% c('April', 'May', 'June') ~ 'Spring',
    MONTH %in% c('August', 'September', 'July') ~ 'Summer',
    MONTH %in% c('October', 'November', 'December') ~ 'Fall'
  ))

#Discretizes distance to rail/bus stop of below (0) or above (1) 1.1 miles
processed_data$Orig_Rail_Meter_Disc <- processed_data$Orig_Rail_Meter %>% 
  cut(breaks = c(-Inf,1770,Inf), labels = c(0,1))
processed_data$Dest_Rail_Meter_Disc <- processed_data$Dest_Rail_Meter %>% 
  cut(breaks = c(-Inf,1770,Inf), labels = c(0,1))
processed_data$Orig_Bus_Meter_Disc <- processed_data$Orig_Bus_Meter %>% 
  cut(breaks = c(-Inf,1770,Inf), labels = c(0,1))
processed_data$Dest_Bus_Meter_Disc <- processed_data$Dest_Bus_Meter %>% 
  cut(breaks = c(-Inf,1770,Inf), labels = c(0,1))
