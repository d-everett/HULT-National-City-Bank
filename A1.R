#A1: OkCupidEDS
#Author: Dalton Lee Everett
#Date: Friday March 10th, 2023

# Im sorry i forgot which one i needed to install, so were doing them all lol
install.packages('sf')
install.packages('geosphere')
install.packages('radiant.data')
install.packages('DataExplorer')
install.packages('dplyr')
install.packages('geosphere')

library(radiant.data)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(ggmap)
library(geosphere)
library(sf)

# Set WD
setwd("~/Desktop/School/Hult/Boston/Spring/Visualize R/hult_class/personalFiles")

# Data look up on github
okcupid <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/profiles.csv')
working_data <- okcupid

# What's the overall structure  & dimensions of the data?
str(working_data   )
dim(working_data   )

# Data set class
class(working_data    )

# Classes for each column
sapply(working_data     , class)
#from here we can see the different columns and what they are

# What are the column names?
names(working_data     )


###############################################################################
# Below i am searching the columns that i will be performing my EDA on
#


# (1)
# Orientation
# 
#
#Lets see what the difference of orientations mixes look like
prop.table(table(working_data$orientation)) * 100
#86% straight
#9% gay
#4% bisexual
sum(is.na(working_data$orientation))
#no N/As

# (2)
# Drinks
#
#
#What about drinks?
prop.table(table(working_data$drinks)) * 100
#73% socially
#10% rarely
#9% often
#5% not at all
sum(is.na(working_data$drinks))
#2985 nulls
sum(!is.na(working_data$drinks)) / nrow(working_data) * 100
#95% of values are non-null

# (3)
# Job
#
#
sum(!is.na(working_data$job)) / nrow(working_data) * 100
#86% are non-null
#
sum(working_data$job == 'no', na.rm = TRUE)
# we can assume that these non-nulls are 'no', because there are 0 counts of 'jobs' = 'no'

# (4)
# Age
#
#
prop.table(table(working_data$age)) * 100
#this is ugly
#any nulls?
sum(!is.na(working_data$Age)) / nrow(working_data) * 100
#no nulls

# i split up the ages 20-40 into 4 seperate groups since that is the largest age groups
# we want to get more exact information
age_intervals <- cut(working_data$age, breaks = c(0, 20, 25, 30, 35, 40, 50, Inf), right = FALSE)
table(age_intervals)
#lets see the % mix
prop.table(table(age_intervals)) * 100
#29% 25-30
#20% 30-35
#16% 20-25
#12.2 40-45
#12.1 35-40

# (5)
# Education

# (6)
# Offspring

# (7)
# Pets
#
#
prop.table(table(working_data$pets)) * 100
#37% Likes dogs and cats
#18% Likes dogs
#10.7% Likes dogs and has cats
#10.3 % Has Dogs and likes cats
sum(is.na(working_data$pets))
#19921 nulls
sum(!is.na(working_data$pets)) / nrow(working_data) * 100
#66% of values are non-null

# (8)
# Religion
#
#
prop.table(table(working_data$religion)) * 100
#way too ugly, we need to clean it up so there is no extra 'fluff'
#lets see the nulls
sum(!is.na(working_data$religion)) / nrow(working_data) * 100
#we can assume that the people who did not put any (thus leaving a null value)
#are 'prefer not to say' 
working_data$religion_category <- ifelse(grepl("agnostic", working_data$religion), "Agnostic", 
                                    ifelse(grepl("atheism", working_data$religion), "Atheism", 
                                           ifelse(grepl("buddhism", working_data$religion), "Buddhism", 
                                                  ifelse(grepl("catholicism", working_data$religion), "Catholic", 
                                                         ifelse(grepl("christianity", working_data$religion), "Christianity", 
                                                                ifelse(grepl("hinduism", working_data$religion), "Hindu", 
                                                                       ifelse(grepl("islam", working_data$religion), "Islam", 
                                                                              ifelse(grepl("judaism", working_data$religion), "Judaism", "Prefer not to say"))))))))

table(working_data$religion_category)
prop.table(table(working_data$religion_category)) * 100
#46% perfer not to say
#14% are agnostic
#11% atheism
#9% Christianity
#7% Catholic

# Create a new column 'religion_status' to indicate whether a profile is religious or not
working_data$religion_status <- ifelse(working_data$religion_category %in% c("Agnostic", "Atheism", "Prefer not to say"), 
                                       "Non-religious", "Religious")
# Compute the percentage mix of religious vs. non-religious profiles
prop.table(table(working_data$religion_status)) * 100

# (9)
# Location
#
#
head(working_data$location)
# in order to see the exact location of our users, i will need to merge two tables
# profiles & location

# lets bring in location
location <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/LatLon.csv')
head(location)
#merge it now
profile_location <- merge(working_data, location, by= 'location')
#lets see unique location
unique(profile_location$location)

#lets split the location so we can see city and state
profile_location <- separate(profile_location, location, into = c("city", "state"), sep = ", ")
#lets see the state
unique(profile_location$state)
# percentage mix?
prop.table(table(profile_location$state)) * 100
#99.8% of data is in california
names(profile_location)

################################################################################
# 

# Insight (1)
#
#
#
#lets see the % mix
prop.table(table(age_intervals)) * 100
#
#
#What about drinks?
prop.table(table(working_data$drinks)) * 100
#
# Create a subset of the data frame that meets the criteria
subset_df <- subset(working_data, age >= 25 & age <= 30 & drinks == "socially" & !is.na(job))

# Calculate the percentage of people who meet the criteria
percentage <- nrow(subset_df) / nrow(working_data) * 100

# Print the result
cat(sprintf("%.2f%% of people are between 25-30, drink socially, and have a job.", percentage))
# 22.06% of our users are between 25-30, drinks socially, and has a job




# Insight (2)
# Of the people that are religious, which group (religion) is the largest in our age groups
# what are the top 4 religions
#
#
#
# Define the age ranges
age_ranges <- list("[0,20)" = c(0, 20),
                   "[20,25)" = c(20, 25),
                   "[25,30)" = c(25, 30),
                   "[30,35)" = c(30, 35),
                   "[35,40)" = c(35, 40),
                   "[40,50)" = c(40, 50),
                   "[50,Inf)" = c(50, Inf))

# Create a new column that specifies the age range for each individual
original_df <- working_data %>%
  mutate(age_range = cut(age, breaks = c(0, 20, 25, 30, 35, 40, 50, Inf),
                         labels = names(age_ranges), include.lowest = TRUE)) %>%
  filter(religion_category != "Agnostic" & religion_category != "Atheism" & religion_category != "Prefer not to say")
# Group the data by age range and religion, and then summarize to get the top three religions for each age range
top_three_religions <- original_df %>%
  group_by(age_range, religion_category) %>%
  summarize(count = n()) %>%
  filter(religion_category != "Agnostic" & religion_category != "Atheism" & religion_category != "Prefer not to say") %>%
  group_by(age_range) %>%
  top_n(3, count) %>%
  ungroup()
# Group the data by age range and religion, and then summarize to get the percentage mix of the top four religions for each age range
percentage_mix <- original_df %>%
  filter(religion_category %in% top_three_religions$religion_category) %>%
  group_by(age_range, religion_category) %>%
  summarize(count = n()) %>%
  group_by(age_range) %>%
  mutate(total_count = sum(count),
         percentage_mix = count / total_count * 100) %>%
  ungroup()

# Print the resulting dataframe
percentage_mix

# Create a histogram of the percentage mix by age range and religion
ggplot(percentage_mix, aes(x = age_range, y = percentage_mix, fill = religion_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percentage Mix of Top Four Religions by Age Range",
       x = "Age Range", y = "Percentage Mix") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
# Catholic and Christianity is close to each other but not the same, so thats why i left them




# Insight (3)
#
#
#
#
# Lets see the percentage mix where our profiles are at
prop.table(table(profile_location$state)) * 100
#
#since 99.8% of our profiles are in California, lets just look into California
# i want to see county lines as well
# Get the map data for California and the county lines
ca_map <- map_data("state", region = "california")
county_map <- map_data("county", region = "california")
# Set the limits of the x and y axis to display only California
xlim <- c(-125.5, -113.5)
ylim <- c(32.5, 42)
#now lets plot our profiles
ggplot() +
  geom_polygon(data = county_map, aes(x = long, y = lat, group = group), 
               color = "gray", fill = NA) +
  geom_polygon(data = ca_map, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_point(data = profile_location %>% filter(!is.na(lat) & !is.na(lon)),
             aes(x = lon, y = lat)) +
  labs(title = "OkCupid User Locations") +
  coord_map(xlim = xlim, ylim = ylim)

#since we can see the profiles are concentrated to the bay and los angeles area
#not only those cities but the surrounding 10 miles as well so we have better location understanding
#lets see which of those cities has more profiles users


# Define the coordinates of San Francisco and Los Angeles
sf_coords <- c(-122.4194, 37.7749)
la_coords <- c(-118.2437, 34.0522)
# Calculate the distances between each profile location and San Francisco/Los Angeles
profile_location$sf_dist <- distm(profile_location[, c("lon", "lat")], sf_coords, fun = distHaversine)/1609.34
profile_location$la_dist <- distm(profile_location[, c("lon", "lat")], la_coords, fun = distHaversine)/1609.34
# Count the number of profiles within 100 miles of San Francisco/Los Angeles
sf_count <- sum(profile_location$sf_dist <= 15)
la_count <- sum(profile_location$la_dist <= 15)
# Calculate the percentage of profiles within 100 miles of San Francisco/Los Angeles
sf_percent <- sf_count / nrow(profile_location) * 100
la_percent <- la_count / nrow(profile_location) * 100
# Print the results
cat(sprintf("%.2f%% of profiles are within 15 miles of San Francisco\n", sf_percent))
cat(sprintf("%.2f%% of profiles are within 15 miles of Los Angeles\n", la_percent))

#San Fran 82.38%
#LA 0.05%

# Get map data for California and San Francisco
sf_map <- map_data("county", region = "california")[map_data("county", region = "california")$subregion == "san francisco", ]
# Set the limits of the x and y axis to display only San Francisco and 50 miles around it
xlim <- c(-123.2, -121.7)
ylim <- c(36.9, 38.4)
# Plot the map with San Francisco and its surrounding areas
ggplot() +
  geom_polygon(data = ca_map, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_polygon(data = sf_map, aes(x = long, y = lat, group = group),
               color = "red", fill = NA) +
  geom_point(data = profile_location %>% filter(!is.na(lat) & !is.na(lon)),
             aes(x = lon, y = lat,)) +
  labs(title = "Map of users in Bay Area") +
  coord_map(xlim = xlim, ylim = ylim)









# Insight (4)
#
#
#
#
prop.table(table(working_data$orientation)) * 100
#86% straight
#9% gay
#4% bisexual





# Get map data for California and San Francisco
ggplot() +
  geom_polygon(data = ca_map, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_polygon(data = sf_map, aes(x = long, y = lat, group = group),
               color = "red", fill = NA) +
  geom_point(data = profile_location %>% filter(!is.na(lat) & !is.na(lon)),
             aes(x = lon, y = lat, color = orientation)) +
  scale_color_manual(values = c('powderblue', 'purple', "orange"), 
                     name = "Orientation",
                     breaks = c("straight", "gay", "bisexual"),
                     labels = c("Straight", "Gay", "Bisexual")) +
  labs(title = "Map of Bay Area and Sexual Orientation Plots") +
  coord_map(xlim = xlim, ylim = ylim)



# Filter working_data to only include profiles in California
working_data_ca <- working_data[profile_location$state == "california",]
# Compute the percentage mix of orientations for profiles in California
prop.table(table(working_data_ca$orientation)) * 100






