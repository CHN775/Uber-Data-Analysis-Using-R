# import libraries
library(ggplot2) 
library(ggthemes) 
library(dplyr)  
library(lubridate) 
library(scales) 
library(tidyr) 
library(DT) 

# importing dataset
april<-read.csv("C:/Users/Windows/OneDrive/Desktop/uber_project/uber-raw-data-apr14.csv")
may<-read.csv("C:/Users/Windows/OneDrive/Desktop/uber_project/uber-raw-data-may14.csv")
june<-read.csv("C:/Users/Windows/OneDrive/Desktop/uber_project/uber-raw-data-jun14.csv")
july<-read.csv("C:/Users/Windows/OneDrive/Desktop/uber_project/uber-raw-data-jul14.csv")
aug<-read.csv("C:/Users/Windows/OneDrive/Desktop/uber_project/uber-raw-data-aug14.csv")
sept<-read.csv("C:/Users/Windows/OneDrive/Desktop/uber_project/uber-raw-data-sep14.csv")

# Combine the data together
data<-rbind(april,may,june,july,aug,sept)
cat("The dimensions of the data are:", dim(data))

# Print the first 6 rows of the data
head(data)

# Readable format for the DateTime
data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)
head(data)

# Create individual columns for month day and year
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))
head(data)

# Add Time variables as well
data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))
head(data)

# Data Visualisation
# Plotting the trips by hours in a day
hourly_data <- data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

# Show data in a searchable js table
datatable(hourly_data)

# Plot the data by hour
ggplot(hourly_data, aes(
  x = reorder(hour, Total),
  y = Total ,
  fill = Total )) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Trips Every Hour",
    subtitle = "Aggregated Today",
    hjust = -0.2, size = 3.5) +
  ggtitle("Trips Every Hour", subtitle = "Aggregated Today") +
  xlab("Hour of the Day") +
  ylab("Total Trips") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_gradient(low = "yellow", high = "red") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")
  )

# Plotting trips by hour and month
# Aggregate the data by month and hour
month_hour_data <- data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)

# Plotting data by trips during every day of the month
#Aggregate data by day of the month 
day_data <- data %>% group_by(day) %>% dplyr::summarize(Trips = n())
day_data

# Plot the data for the day
ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill =  "blue" , color = "black" ) +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)

# Collect data by day of the week and month
day_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())

day_month_data

# Plot the Collect data by day of the week and month
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# Number of Trips place during months in a year
month_data <- data %>% group_by(month) %>% dplyr::summarize(Total = n())

month_data

# Plot the Number of Trips place during months in a year
ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

#Heatmap visualization of day, hour and month

#Heatmap by Hour and Day
day_hour_data <- data %>% group_by(day, hour) %>% dplyr::summarize(Total = n())

datatable(day_hour_data)

custom_colors <- c("blue", "lightblue", "white", "lightcoral", "red")

ggplot(day_hour_data, aes(day, hour, fill =Total))+
  geom_tile(color = "white")+
  ggtitle("Heat Map by Hour and Day")+
  scale_fill_gradientn(colors = custom_colors)+
  theme_minimal()+
  theme(plot.title = element_text(hjust =0.5, size = 16 ,face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(size =24))

# Plot Heatmap by Day and Month
# Collect data by month and day
month_day_data <- data %>% group_by(month, day) %>% dplyr::summarize(Trips = n())

month_day_data

# Plot a heatmap by Collect data by month and day
ggplot(month_day_data, aes(day, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")

# Plot a heatmap by day of the week and month
ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")

# Creating a map visualization of rides in NYC
# Set Map Constants
min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

ggplot(data, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data, aes(x=Lon, y=Lat, color = Base)) +
    geom_point(size=1) +
    scale_x_continuous(limits=c(min_long, max_long)) +
    scale_y_continuous(limits=c(min_lat, max_lat)) +
    theme_map() +
    ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")
