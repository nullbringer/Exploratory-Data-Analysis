library(shiny)
library(tigris)
library(plyr)
library(dplyr)
library(leaflet)
library(maps)
library(maptools)
library(sp)
library(shinycssloaders)
library(stringr)
### Common Execution ###
########################

# Downloading the shapefiles for states at the lowest resolution
states_import <- states(cb=T)

dropdown_choices <- read.csv(file="data/dropdowns.csv", header=FALSE)$V1

# read collected tweets from csv
fluTweet <- read.csv(file="data/twitter_data.csv", header=TRUE, sep=",")

# clean data: keep data with longitude and latitude value and remove duplicates
fluTweet <- fluTweet[!(is.na(fluTweet$lat) | fluTweet$lat==""), ]
fluTweet <- unique(fluTweet, by = "status_id")

# Get state list
state_off <- data.frame(state.abb, state.name)
colnames(state_off) <- c("state", "NAME")
state_off$NAME <-tolower(state_off$NAME)



testPoints <- data.frame(x = fluTweet$lng, y = fluTweet$lat)
states <- map('state', fill=TRUE, col="transparent", plot=FALSE)

IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))

pointsSP <- SpatialPoints(testPoints, 
                          proj4string=CRS("+proj=longlat +datum=WGS84"))

indices <- over(pointsSP, states_sp)

stateNames <- sapply(states_sp@polygons, function(x) x@ID)
fluTweet$state <- stateNames[indices]
fluTweet <- fluTweet[!(is.na(fluTweet$state) | fluTweet$state==""), ]

####### End Common Execution ####

count_by_state<- fluTweet %>% count(state)

colnames(count_by_state) <- c("NAME", "TWEETS")


state_tweets <- left_join(count_by_state, state_off)



states_merged_sb <- geo_join(states_import, state_tweets, "STUSPS", "state")

# Creating a color palette based on the number range in the total column
pal <- colorNumeric("Greens", domain=states_merged_sb$TWEETS)

# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work

states_merged_sb <- subset(states_merged_sb, !is.na(TWEETS))

# Setting up the pop up text
#popup_sb <- paste0("Total: ", as.character(states_merged_sb$TWEETS))

popup_sb <- paste0("<strong>", states_merged_sb$NAME, 
                   "</strong><br />Tweets: ", states_merged_sb$TWEETS)
                   




fluTweet_filtered <- fluTweet %>% 
  filter(str_detect(text, "fever"))


kk <-CO2 %>%
  filter(str_detect(Treatment, "non"))

nrow(kk)
