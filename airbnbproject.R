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

Filtered_All_Cities <- All_Cities %>% 
  select(id, name, description, neighborhood_overview, host_id, host_name, host_since,
         host_about, host_response_time, host_response_rate, host_acceptance_rate,
         host_is_superhost, host_neighbourhood, host_listings_count, host_has_profile_pic,
         host_identity_verified, neighbourhood_cleansed, latitude, longitude, 
         property_type, room_type, accommodates, bathrooms_text, bedrooms, beds, amenities,
         price, minimum_nights, maximum_nights, minimum_nights_avg_ntm, maximum_nights_avg_ntm,
         has_availability, availability_30, number_of_reviews, number_of_reviews_ltm, number_of_reviews_l30d,
         first_review, last_review, review_scores_rating, instant_bookable, reviews_per_month, City)

str(Filtered_All_Cities)

# Convert City to factor 

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

str(Filtered_All_Cities)

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

# Univariate analysis

summary(Filtered_All_Cities)

# Price
summary(Filtered_All_Cities$price) 
hist(Filtered_All_Cities$price)

# Response rate
summary(Filtered_All_Cities$host_response_rate)
hist(Filtered_All_Cities$host_response_rate)

# Acceptance rate
summary(Filtered_All_Cities$host_acceptance_rate)
hist(Filtered_All_Cities$host_acceptance_rate)

# Super host
summary(Filtered_All_Cities$host_is_superhost)

# Listings count
summary(Filtered_All_Cities$host_listings_count)
hist(Filtered_All_Cities$host_listings_count)

# Profile pic
summary(Filtered_All_Cities$host_has_profile_pic)
hist(Filtered_All_Cities$host_has_profile_pic)

# Identity verified
summary(Filtered_All_Cities$host_identity_verified)

# Accommodates
summary(Filtered_All_Cities$accommodates)
hist(Filtered_All_Cities$accommodates)

# Bathrooms
summary(Filtered_All_Cities$bathrooms)
hist(Filtered_All_Cities$bathrooms)

# Bedrooms
summary(Filtered_All_Cities$bedrooms)
hist(Filtered_All_Cities$bedrooms)

# Beds
summary(Filtered_All_Cities$beds)
hist(Filtered_All_Cities$beds)

# Price
summary(Filtered_All_Cities$price)
hist(Filtered_All_Cities$price)

# Minimum nights
summary(Filtered_All_Cities$minimum_nights)
hist(Filtered_All_Cities$minimum_nights)

# Maximum nights
# Take note of city with 999,999,999 nights - SHOULD WE REMOVE?
summary(Filtered_All_Cities$maximum_nights)
hist(Filtered_All_Cities$maximum_nights)

# Has availability
summary(Filtered_All_Cities$has_availability)

# Availability within 30 days
summary(Filtered_All_Cities$availability_30)
hist(Filtered_All_Cities$availability_30)

# Number of reviews
summary(Filtered_All_Cities$number_of_reviews)
hist(Filtered_All_Cities$number_of_reviews)

# Number of reviews last month
summary(Filtered_All_Cities$number_of_reviews_ltm)
hist(Filtered_All_Cities$number_of_reviews_ltm)

# Number of reviews last 130 days
summary(Filtered_All_Cities$number_of_reviews_l30d)
hist(Filtered_All_Cities$number_of_reviews_l30d)

# Review scores rating
summary(Filtered_All_Cities$review_scores_rating)
hist(Filtered_All_Cities$review_scores_rating)

# Instant bookable 
summary(Filtered_All_Cities$instant_bookable)

# Reviews per month
summary(Filtered_All_Cities$reviews_per_month)
hist(Filtered_All_Cities$reviews_per_month)

# City 
summary(Filtered_All_Cities$City)



###Montreal 
Montreal_Full_List$description <- as.numeric(gsub("<br>", "", Montreal_Full_List$description ))
Montreal_Full_List$description <- as.numeric(gsub("</b>", "", Montreal_Full_List$description ))
Montreal_Full_List$description <- as.numeric(gsub("!", "", Montreal_Full_List$description ))

Montreal_Full_List$neighborhood_overview <- as.numeric(gsub("<br>", "", Montreal_Full_List$neighborhood_overview ))
Montreal_Full_List$neighborhood_overview <- as.numeric(gsub("</b>", "", Montreal_Full_List$neighborhood_overview ))

###Quecbec
Quesbec_Full_List$description <- as.numeric(gsub("<br>", "", Quecbec_Full_List$description ))
Quecbec_Full_List$description <- as.numeric(gsub("</b>", "", Quecbec_Full_List$description ))
Quecbec_Full_List$description <- as.numeric(gsub("!", "", Quecbec_Full_List$description ))

Quecbec_Full_List$neighborhood_overview <- as.numeric(gsub("<br>", "", Quecbec_Full_List$neighborhood_overview ))
Quecbec_Full_List$neighborhood_overview <- as.numeric(gsub("</b>", "", Quecbec_Full_List$neighborhood_overview ))

###Toronto
Toronto_Full_List$description <- as.numeric(gsub("<br>", "", Toronto_Full_List$description ))
Toronto_Full_List$description <- as.numeric(gsub("</b>", "", Toronto_Full_List$description ))
Toronto_Full_List$description <- as.numeric(gsub("!", "", Toronto_Full_List$description ))

Toronto_Full_List$neighborhood_overview <- as.numeric(gsub("<br>", "", Toronto_Full_List$neighborhood_overview ))
Toronto_Full_List$neighborhood_overview <- as.numeric(gsub("</b>", "", TOronto_Full_List$neighborhood_overview ))

###Ottawa
Ottawa_Full_List$description <- as.numeric(gsub("<br>", "", Ottawa_Full_List$description ))
Ottawa_Full_List$description <- as.numeric(gsub("</b>", "", Ottawa_Full_List$description ))
Ottawa_Full_List$description <- as.numeric(gsub("!", "", Ottawa_Full_List$description ))

Ottawa_Full_List$neighborhood_overview <- as.numeric(gsub("<br>", "", Ottawa_Full_List$neighborhood_overview ))
Ottawa_Full_List$neighborhood_overview <- as.numeric(gsub("</b>", "", Ottawa_Full_List$neighborhood_overview ))

###NewBrunsvic
NewBrunsvic_Full_List$description <- as.numeric(gsub("<br>", "", NewBrunsvic_Full_List$description ))
NewBrunsvic_Full_List$description <- as.numeric(gsub("</b>", "", NewBrunsvic_Full_List$description ))
NewBrunsvic_Full_List$description <- as.numeric(gsub("!", "", NewBrunsvic_Full_List$description ))

NewBrunsvic_Full_List$neighborhood_overview <- as.numeric(gsub("<br>", "", NewBrunsvic_Full_List$neighborhood_overview ))
NewBrunsvic_Full_List$neighborhood_overview <- as.numeric(gsub("</b>", "", NewBrunsvic_Full_List$neighborhood_overview ))

###Vancouver
Vancouver_Full_List$description <- as.numeric(gsub("<br>", "", Vancouver_Full_List$description ))
Vancouver_Full_List$description <- as.numeric(gsub("</b>", "", Vancouver_Full_List$description ))
Vancouver_Full_List$description <- as.numeric(gsub("!", "", Vancouver_Full_List$description ))

Vancouver_Full_List$neighborhood_overview <- as.numeric(gsub("<br>", "", Vancouver_Full_List$neighborhood_overview ))
Vancouver_Full_List$neighborhood_overview <- as.numeric(gsub("</b>", "", Vancouver_Full_List$neighborhood_overview ))


###Victoria
Victoria_Full_List$description <- as.numeric(gsub("<br>", "", Victoria_Full_List$description ))
Victoria_Full_List$description <- as.numeric(gsub("</b>", "", Victoria_Full_List$description ))
Victoria_Full_List$description <- as.numeric(gsub("!", "", Victoria_Full_List$description ))





