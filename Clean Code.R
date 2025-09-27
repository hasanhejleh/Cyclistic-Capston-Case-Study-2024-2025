# **Setting Up The Environment**

install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("geosphere")

## 1. loading packages

library(tidyverse)
library(skimr)
library(janitor)
library(dplyr)
library(geosphere)

## 2. storing the files as dataframes

divvy2024_01_df <- read_csv("202401-divvy-tripdata.csv")
divvy2024_02_df <- read_csv("202402-divvy-tripdata.csv")
divvy2024_03_df <- read_csv("202403-divvy-tripdata.csv")
divvy2024_04_df <- read_csv("202404-divvy-tripdata.csv")
divvy2024_05_df <- read_csv("202405-divvy-tripdata.csv")
divvy2024_06_df <- read_csv("202406-divvy-tripdata.csv")
divvy2024_07_df <- read_csv("202407-divvy-tripdata.csv")
divvy2024_08_df <- read_csv("202408-divvy-tripdata.csv")
divvy2024_09_df <- read_csv("202409-divvy-tripdata.csv")
divvy2024_10_df <- read_csv("202410-divvy-tripdata.csv")
divvy2024_11_df <- read_csv("202411-divvy-tripdata.csv")
divvy2024_12_df <- read_csv("202412-divvy-tripdata.csv")
divvy2025_01_df <- read_csv("202501-divvy-tripdata.csv")

## 3. Check Data Integrity Of All Df's (do this for all so create 13 code chunks each of which is for one file)

colnames(divvy2024_01_df)
head(divvy2024_01_df)
str(divvy2024_01_df)

## 4. Aggregate the data into one data frame

# create a vector containing the names of our data files. (13 files in total)

csv_files <- list.files(path = "/kaggle/input/divvy-trips-public",
                        pattern = "*.csv",
                        full.names = TRUE)


View(as.data.frame(csv_files)) # view csv_files to check.


# combine the data files into one data frame.


divvy_combined_df <- csv_files %>% 
  lapply(function(file) { # lapply allows us to apply the anonymouse function within on all elements of the list csv_files
    
    df <- read_csv(file) # creates a data frame for each file
    df$yearmonth <- gsub(pattern = "[^0-9]", replacement = "", 
                         basename(file)) # creates a new column yearmonth to store the original date of the data for each file's records respectively
    
    return(df)
    
  }) %>% 
  bind_rows() # combines the rows of all the data frames into one dataframe inside divvy_combined_df

# **Process & Clean The Data**

## 1. Re-Format Some Columns (start_lat, start_lng, end_lat, end_lng)

divvy_combined_df$start_lat <- as.double(divvy_combined_df$start_lat)
divvy_combined_df$end_lat <- as.double(divvy_combined_df$end_lat)
divvy_combined_df$start_lng <- as.double(divvy_combined_df$start_lng)
divvy_combined_df$end_lng <- as.double(divvy_combined_df$end_lng)

## 2. Check for NA & Blank values within our data.

colSums(is.na(divvy_combined_df))

# remove NA values for end_lat & end_lng
divvy_combined_df <- divvy_combined_df %>% 
  filter(!is.na(end_lat))

# replace NA with "Unknown" for both start station name & id

divvy_combined_df$start_station_name[is.na(divvy_combined_df$start_station_name)] <- "Unknown"

divvy_combined_df$start_station_id[is.na(divvy_combined_df$start_station_id)] <- "Unknown"

# replace NA with "Unknown" for both end station name & id

divvy_combined_df$end_station_name[is.na(divvy_combined_df$end_station_name)] <- "Unknown"

divvy_combined_df$end_station_id[is.na(divvy_combined_df$end_station_id)] <- "Unknown"

## 3. Create a trip duration Column.

# Re format date-time columns to ensure a uniform format.

# Ensure both columns are in POSIXct format before doing anything else
divvy_combined_df$started_at <- as.POSIXct(divvy_combined_df$started_at, 
                                           format = "%Y-%m-%d %H:%M:%S", 
                                           tz = "America/Chicago")

divvy_combined_df$ended_at <- as.POSIXct(divvy_combined_df$ended_at, 
                                         format = "%Y-%m-%d %H:%M:%S", 
                                         tz = "America/Chicago")

# Now that the columns are correctly in POSIXct, you can safely calculate the duration
duration <- difftime(divvy_combined_df$ended_at, divvy_combined_df$started_at, units = "mins")

# Finally add the duration column
divvy_combined_df <- divvy_combined_df %>% 
  mutate(duration)

## 4. Create a trip distance Column.

divvy_combined_df <- divvy_combined_df %>% 
  mutate(distance = distHaversine(
    
    cbind(start_lng, start_lat),
    cbind(end_lng, end_lat)
    
  )
  )

## 5. Filter Out Invalid Trips

# create a data frame to store the invalid trips for possible future correction plans

invalid_trips <- divvy_combined_df %>% 
  filter(duration < 1.30 & (distance < 200 | distance > 768000))

# Update our data frame to filter out the invalid trips

divvy_combined_df <- divvy_combined_df %>% 
  filter(duration >= 1.3 & distance >= 200 & distance <= 768000)

# Checking percentage of trips <200 meters & <1.30 minutes

# calculate the mean as percentage
percent_short_trips <- mean(divvy_combined_df$distance < 200 | divvy_combined_df$duration < 1.30 | divvy_combined_df$distance >768000) * 100

#display the percentage in total
print(paste("Percentage of trips < 200 meters OR < 1.30 minutes OR > 768000 meters:", round(percent_short_trips, 2), "%")) 

## 6. Hiccup/Problem with the data

# convert distance column from meters to kilo meters.

divvy_combined_df$distance <- as.double(divvy_combined_df$distance / 1000)

# create a data frame to store the realistic & correct data

cleaned_data <- divvy_combined_df %>%
  mutate(speed = distance / (as.double(duration) / 60)) %>%  # Speed in km/h
  filter(speed >= 1 & speed <= 30)

# view the cleaned data

View(cleaned_data)

## 6.1 New Plan/Approach

# Apply duration constraints.

divvy_combined_df <- divvy_combined_df %>%
  filter(duration >= 1.30)

# Create new start_weekday column

divvy_combined_df <- divvy_combined_df %>% 
  mutate(start_weekday = weekdays(started_at))

## 7. Remove Duplicates

# Check the amount of duplicates before removal by storing them in a data frame

duplicates <- divvy_combined_df %>%
  group_by(ride_id) %>%
  filter(n() > 1) %>%
  arrange(ride_id)

View(duplicates)

# For the removal of duplicates to work we need to make sure that milisecond inaccuracies are accounted for in started_at, ended_at

divvy_combined_df$started_at <- round_date(divvy_combined_df$started_at, unit = "minute") # Round Data to nearest value
divvy_combined_df$ended_at <- round_date(divvy_combined_df$ended_at, unit = "minute") # Round Data to nearest value

# Remove the duplicates based on ride_id, started_at, ended_at columns

divvy_combined_df <- divvy_combined_df %>% distinct(ride_id, started_at, ended_at,  .keep_all = TRUE)

# Check for duplicates by filtering for one of the values

divvy_combined_df %>% 
  filter(ride_id == "01406457A85B0AFF") %>% 
  view()

# Drop duplicates df

remove(duplicates)

# Analyze Data For Trends

## 1. Figure Out The Distribution Of The Data

# Reformat duration column from timediff object to numeric
divvy_combined_df$duration <- as.numeric(divvy_combined_df$duration)

# Define bins bounds
lower_bound <- min(divvy_combined_df$duration)
upper_bound <- max(divvy_combined_df$duration)

# Define duration bins (adjust based on data distribution)
bins <- c(lower_bound, 5, 10, 30, 60, 120, 300, 600, 900, 1200, upper_bound)


# Compute percentage in each bin
duration_distribution <- divvy_combined_df %>%
  mutate(duration_bin = cut(duration, breaks = bins, include.lowest = TRUE)) %>%
  group_by(duration_bin) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(percentage))  # Sort by highest concentration

# View the distribution
print(duration_distribution)

# Visualize the distribution

ggplot(data = divvy_combined_df, aes(x = duration)) +
  geom_histogram(bins = 40, color = "black", fill = "steelblue", alpha = 0.7) +
  scale_x_log10(breaks = c(1.3, 5, 10, 30, 60 ,120))+
  labs(
    title = "Distribution Of Trips By Duration (minutes)",
    x = "Duration (minutes)",
    y = "Frequency"
    
  ) +
  theme_minimal()

## 2. Use Visualizations To Spot Patterns

### 2.1 Visualize the relationship between Membership Type & Amount of Trips

# Group trips by membership type & bike type

avg_duration <- divvy_combined_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(average_duration = mean(duration), trip_count = n(), .groups = "drop") %>%
  arrange(member_casual, rideable_type)

# Create a stacked bar chart visualization for the segments of the data

ggplot(avg_duration, aes(x = member_casual, y = trip_count, fill = rideable_type)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = scales::comma(trip_count)), 
            position = position_stack(vjust = 0.5),  
            color = "black", size = 5, fontface = "bold") +
  labs(
    title = "Number of Trips For Each Membership Type",
    x = "User Type",
    y = "Number of Trips",
    caption = "1,924,305 casual members & 3,620,854 annual members"
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 1))

### 2.2 Research the relationship between average duration for each membership type.

# Precompute summary stats (min, max, median) for each member type
summary_data <- avg_duration %>%
  group_by(member_casual) %>%
  summarise(
    min_duration = min(average_duration),
    min_rideable = rideable_type[which.min(average_duration)],
    max_duration = max(average_duration),
    max_rideable = rideable_type[which.max(average_duration)],
    median_duration = median(average_duration),
    median_rideable = rideable_type[which.min(abs(average_duration - median(average_duration)))], # Closest to median
    .groups = "drop"
  )

# Create the boxplot
ggplot(avg_duration, aes(x = member_casual, y = average_duration, fill = member_casual)) +
  geom_boxplot() +
  
  # Add min value text
  geom_text(data = summary_data, aes(x = member_casual, y = min_duration,
                                     label = paste0(round(min_duration, 2), " / ", min_rideable)),
            vjust = 1.5, color = "black", size = 3) +
  
  # Add max value text
  geom_text(data = summary_data, aes(x = member_casual, y = max_duration,
                                     label = paste0(round(max_duration, 2), " / ", max_rideable)),
            vjust = -0.5, color = "black", size = 3) +
  
  # Add median value text
  geom_text(data = summary_data, aes(x = member_casual, y = median_duration,
                                     label = paste0(round(median_duration, 2), " / ", median_rideable)),
            vjust = -0.2, color = "black", size = 3) +
  
  labs(title = "Average Duration for Each Bike Type by User Type",
       x = "User Type",
       y = "Average Duration (minutes)") +
  theme_minimal()

### 2.3 What Days do our riders prefer to take trips?

# Day of the week relationship & number of rides

divvy_combined_df %>% 
  group_by(start_weekday) %>% 
  summarise(num_of_rides = n()) %>% 
  arrange(-num_of_rides)


# Visualize the relationship using a stacked bar chart with bike type segments

ggplot(data = divvy_combined_df, mapping = aes(x = start_weekday, fill = rideable_type)) +
  geom_bar() + 
  facet_wrap(~member_casual) + 
  labs(
    x = "Week Days",
    y = "Number Of Trips",
    title = "Casual Vs Annual Members Trips Distribution On Weekdays"
  ) +
  theme_minimal() +
  theme(
    # Add margin between facets
    panel.spacing = unit(1.5, "lines"),  # Increase spacing between facets
    
    # Adjust x-axis text angle, spacing, and alignment
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, margin = margin(t = 10)),  # Rotate x-axis text and add margin
    
    # Add padding to the plot
    plot.margin = margin(1, 1, 1, 1, "cm"),  # Add margin around the plot
    
    # Adjust facet strip text
    strip.text = element_text(size = 12, face = "bold")  # Customize facet labels
  )

## 2.4 When do our riders tend to use our bikes? (hours throughout the day)

# Add a start_hour column by extracting the hour from started_at column

divvy_combined_df <- divvy_combined_df %>% 
  mutate(start_hour = format(as.POSIXct(started_at), "%H"))

# Create a data frame to store the number of trips and their relative start hours

trips_by_hour <- divvy_combined_df %>%
  group_by(start_hour) %>%
  summarise(num_of_trips = n())

# Add a new formatted column for the start_hour values in the visualization

trips_by_hour <- trips_by_hour %>%
  mutate(start_hour_numeric = as.numeric(sub(":00:00", "", start_hour)))

ggplot(trips_by_hour, aes(x = start_hour_numeric, y = num_of_trips)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = paste0(seq(0, 23))) +
  labs(title = "Number of Trips by Hour",
       x = "Hour of the Day (24-hour format)",
       y = "Number of Trips") +
  theme_minimal()

### 2.5 What are the busiest stations for each membership type?

# Store station names & trip counts & membership type in one data frame

start_station_counts <- divvy_combined_df %>%
  group_by(start_station_name, member_casual) %>%
  summarise(num_of_trips = n(), .groups = 'drop') %>%
  arrange(-num_of_trips)

# Filter stations by membership type since some stations were only used by 1 membership type

casual_stations <- start_station_counts %>% 
  filter(member_casual == "casual", start_station_name != "Unknown") %>% 
  arrange(-num_of_trips) %>% 
  top_n(10)

member_stations <- start_station_counts %>% 
  filter(member_casual == "member", start_station_name != "Unknown") %>% 
  arrange(-num_of_trips) %>% 
  top_n(10)

# Top 10 Busiest Stations for Casual Members Visualization

options(repr.plot.width = 10, repr.plot.height = 8)

ggplot(data = casual_stations, aes(x = reorder(start_station_name, num_of_trips), y = num_of_trips)) +
  geom_bar(stat = "identity", col = "steelblue", fill = "steelblue") +  # Color bars
  coord_flip() +  # Flip coordinates to make the bars horizontal
  labs(
    x = "Station Name",
    y = "Number of Trips",
    title = "Top 10 Busiest Stations for Casual Members"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, family = "sans"),  # Improve x-axis text
    axis.text.y = element_text(size = 12, family = "sans"),  # Improve y-axis text
    axis.title.x = element_text(size = 14, face = "bold", family = "sans"),  # Improve x-axis title
    axis.title.y = element_text(size = 14, face = "bold", family = "sans"),  # Improve y-axis title
    plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),  # Improve plot title
    panel.grid.major = element_line(color = "gray80"),  # Add light grid lines for readability
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # Adjust margins
  )

# Top 10 Busiest Stations for Annual Members Visualization

ggplot(data = member_stations, aes(x = reorder(start_station_name, num_of_trips), y = num_of_trips)) +
  geom_bar(stat = "identity", col = "#ff7f0e", fill = "#ff7f0e") +  # Color bars
  coord_flip() +  # Flip coordinates to make the bars horizontal
  labs(
    x = "Station Name",
    y = "Number of Trips",
    title = "Top 10 Busiest Stations for Annual Members"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, family = "sans"),  # Improve x-axis text
    axis.text.y = element_text(size = 12, family = "sans"),  # Improve y-axis text
    axis.title.x = element_text(size = 14, face = "bold", family = "sans"),  # Improve x-axis title
    axis.title.y = element_text(size = 14, face = "bold", family = "sans"),  # Improve y-axis title
    plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0.5),  # Improve plot title
    panel.grid.major = element_line(color = "gray80"),  # Add light grid lines for readability
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # Adjust margins
  )


