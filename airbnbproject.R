#I'm here
install.packages("wordcloud")
install.packages("ggmap")
install.packages("tm")
install.packages("tidytext")
install.packages("mapsapi")
install.packages("mapview")
install.packages("sf")
install.packages("crossplot")

# Load libraries

library(readxl)
library(ggplot2)
library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(wordcloud)
library(ggmap)
library(tm)
library(tidytext)
library(mapsapi)
library(mapview)
library(sf)
library(leaflet)

# Import data and assign to data frames

Montreal <- read.csv(file.choose())
New_Brunswick <- read.csv(file.choose())
Ottawa <- read.csv(file.choose())
Quebec_City <- read.csv(file.choose())
Toronto <- read.csv(file.choose())
Vancouver <- read.csv(file.choose())
Victoria <- read.csv(file.choose())

# Add city name to data frames

Montreal$City <- "Montreal"
New_Brunswick$City <- "New Brunswick"
Ottawa$City <- "Ottawa"
Quebec_City$City <- "Quebec City"
Toronto$City <- "Toronto"
Vancouver$City <- "Vancouver"
Victoria$City <- "Victoria"

# Combine data sets

All_Cities <- rbind(Montreal, New_Brunswick, Ottawa, 
                    Quebec_City, Toronto, Vancouver, Victoria)

# Inspect 

view(All_Cities)
str(All_Cities)

# Data has 74 variables. Filter required variables.

Filtered_All_Cities <- All_Cities %>% select(id, name, description, neighborhood_overview, host_id, host_name, host_since, 
                                             host_about, host_response_time, host_response_rate, host_acceptance_rate, 
                                             host_is_superhost, host_neighbourhood, host_listings_count, host_has_profile_pic, 
                                             host_identity_verified, neighbourhood_cleansed, latitude, longitude, 
                                             property_type, room_type, accommodates, bathrooms_text, bedrooms, beds, amenities,
                                             price, minimum_nights, maximum_nights, minimum_nights_avg_ntm, maximum_nights_avg_ntm,
                                             has_availability, availability_30, number_of_reviews, number_of_reviews_ltm, number_of_reviews_l30d,
                                             first_review, last_review, review_scores_rating, instant_bookable, reviews_per_month, City)

Filtered_All_Cities$City <- as.factor(Filtered_All_Cities$City)
Filtered_All_Cities$City

# Convert strings to numeric

Filtered_All_Cities$host_response_rate <- 
  as.numeric(str_remove(Filtered_All_Cities$host_response_rate, "%")) 
Filtered_All_Cities$host_response_rate

Filtered_All_Cities$host_acceptance_rate <- 
  as.numeric(str_remove(Filtered_All_Cities$host_acceptance_rate, "%"))
Filtered_All_Cities$host_acceptance_rate

Filtered_All_Cities$price <- as.numeric(str_remove(Filtered_All_Cities$price, "\\$")) 
Filtered_All_Cities$price

Filtered_All_Cities$bathrooms <- 
  as.numeric(gsub(".*?([0-9]+).*", "\\1", Filtered_All_Cities$bathrooms_text))
Filtered_All_Cities$bathrooms

# Convert "t" and "f" observations binary

Filtered_All_Cities$host_is_superhost[Filtered_All_Cities$host_is_superhost == "t"] <- 1
Filtered_All_Cities$host_is_superhost[Filtered_All_Cities$host_is_superhost == "f"] <- 0
Filtered_All_Cities$host_is_superhost <- as.numeric(Filtered_All_Cities$host_is_superhost)
Filtered_All_Cities$host_is_superhost

Filtered_All_Cities$host_has_profile_pic[Filtered_All_Cities$host_has_profile_pic == "t"] <- 1
Filtered_All_Cities$host_has_profile_pic[Filtered_All_Cities$host_has_profile_pic == "f"] <- 0
Filtered_All_Cities$host_has_profile_pic <- as.numeric(Filtered_All_Cities$host_has_profile_pic)
Filtered_All_Cities$host_has_profile_pic

Filtered_All_Cities$host_identity_verified[Filtered_All_Cities$host_identity_verified == "t"] <- 1
Filtered_All_Cities$host_identity_verified[Filtered_All_Cities$host_identity_verified == "f"] <- 0
Filtered_All_Cities$host_identity_verified <- as.numeric(Filtered_All_Cities$host_identity_verified) 
Filtered_All_Cities$host_identity_verified

Filtered_All_Cities$has_availability[Filtered_All_Cities$has_availability == "t"] <- 1
Filtered_All_Cities$has_availability[Filtered_All_Cities$has_availability == "f"] <- 0
Filtered_All_Cities$has_availability <- as.numeric(Filtered_All_Cities$has_availability)
Filtered_All_Cities$has_availability

Filtered_All_Cities$instant_bookable[Filtered_All_Cities$instant_bookable == "t"] <- 1
Filtered_All_Cities$instant_bookable[Filtered_All_Cities$instant_bookable == "f"] <- 0
Filtered_All_Cities$instant_bookable <- as.numeric(Filtered_All_Cities$instant_bookable)
Filtered_All_Cities$instant_bookable

# Create only numeric data frame

Num_All_Cities <- Filtered_All_Cities %>% 
  select(host_response_rate, host_acceptance_rate,host_is_superhost,host_listings_count, 
         host_has_profile_pic,host_identity_verified, accommodates, bedrooms, beds,
         price, minimum_nights, maximum_nights,has_availability, availability_30, 
         availability_60, availability_90, availability_365, number_of_reviews, 
         number_of_reviews_ltm, number_of_reviews_l30d, review_scores_rating, 
         review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, 
         review_scores_communication,review_scores_location, review_scores_value, 
         instant_bookable,reviews_per_month)

# Summary statistics

summary(Num_All_Cities)

pairs(Num_All_Cities)

# Visualization

OUTSTANDING

#Dummy variables for cities

Filtered_All_Cities$Montreal <- ifelse(Filtered_All_Cities$City == "Montreal",1,0)
Filtered_All_Cities$New_Brunswick <- ifelse(Filtered_All_Cities$City == "New Brunswick",1,0)
Filtered_All_Cities$Ottawa <- ifelse(Filtered_All_Cities$City == "Ottawa",1,0)
Filtered_All_Cities$Quebec_City <- ifelse(Filtered_All_Cities$City == "Quebec City",1,0)
Filtered_All_Cities$Toronto <- ifelse(Filtered_All_Cities$City == "Toronto",1,0)
Filtered_All_Cities$Vancouver <- ifelse(Filtered_All_Cities$City == "Vancouver",1,0)
Filtered_All_Cities$Victoria <- ifelse(Filtered_All_Cities$City == "Victoria",1,0)

str(Filtered_All_Cities)

# Exploratory analysis
# Listing name

airbnb_words <- Filtered_All_Cities %>%
  select(name) %>%
  unnest_tokens(word, name) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

common_words <- airbnb_words %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  top_n(n = 20, wt = count) %>%
  ggplot() +
  geom_bar(mapping = aes(x=reorder(word, count),
                         y=count),
           stat="identity", fill = "brown") +
  labs(title="Top 20 words described in listing name",
       y="Word count", x="Most common Words") +
  coord_flip() +
  theme_minimal()

print(common_words)

# Listing description

airbnb_words2 <- Filtered_All_Cities %>%
  select(description) %>%
  unnest_tokens(word, description) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

common_words2 <- airbnb_words2 %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  top_n(n = 20, wt = count) %>%
  ggplot() +
  geom_bar(mapping = aes(x=reorder(word, count),
                         y=count),
           stat="identity", fill = "brown") +
  labs(title="Top 20 words described in listing description",
       y="Word count", x="Most common Words") +
  coord_flip() +
  theme_minimal()

print(common_words2)

# Neighborhood overview

airbnb_words3 <- Filtered_All_Cities %>%
  select(neighborhood_overview) %>%
  unnest_tokens(word, neighborhood_overview) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

common_words3 <- airbnb_words3 %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  top_n(n = 20, wt = count) %>%
  ggplot() +
  geom_bar(mapping = aes(x=reorder(word, count),
                         y=count),
           stat="identity", fill = "brown") +
  labs(title="Top 20 words described in neighborhood overview",
       y="Word count", x="Most common Words") +
  coord_flip() +
  theme_minimal()

print(common_words3)

# Price visualization by region

leaflet(data = Filtered_All_Cities) %>%  addProviderTiles("Stamen.Watercolor") %>%
  addProviderTiles("Stamen.TonerHybrid") %>%
  addCircleMarkers(~longitude, ~latitude, 
                   radius = ifelse(Filtered_All_Cities$price > 100, 2, 0.2),
                   color = ifelse(Filtered_All_Cities$price > 100, "red", "green"),
                   fillOpacity = 0.4)

# Neighborhood visualization

neighcolors <- colorFactor(topo.colors(25), Filtered_All_Cities$neighbourhood_cleansed)

popup <- paste0("<strong>'hood: </strong>", Filtered_All_Cities$neighbourhood_cleansed)

leaflet(Filtered_All_Cities) %>% addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(
    color = ~neighcolors(neighbourhood_cleansed),
    stroke = FALSE, fillOpacity = 0.5, radius = 1.2,
    popup = ~popup
  )





