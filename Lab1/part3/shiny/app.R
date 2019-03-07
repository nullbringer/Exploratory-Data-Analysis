library(shiny)
library(tigris)
library(plyr)
library(dplyr)
library(leaflet)
library(maps)
library(maptools)
library(sp)


# read collected tweets from csv
fluTweet <- read.csv(file="data/twitter_data.csv", header=TRUE, sep=",")

# clean data: keep data with longitude and latitude value and remove duplicates
fluTweet <- fluTweet[!(is.na(fluTweet$lat) | fluTweet$lat==""), ]

fluTweet <- unique(fluTweet, by = "status_id")




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

count_by_state<- fluTweet %>% count(state)

colnames(count_by_state) <- c("NAME", "TWEETS")

state_off <- data.frame(state.abb, state.name)
colnames(state_off) <- c("state", "NAME")
state_off$NAME <-tolower(state_off$NAME)


state_tweets <- left_join(count_by_state, state_off)


# Downloading the shapefiles for states at the lowest resolution
states <- states(cb=T)

states_merged_sb <- geo_join(states, state_tweets, "STUSPS", "state")


# Creating a color palette based on the number range in the total column
pal <- colorNumeric("Greens", domain=states_merged_sb$TWEETS)

# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work

states_merged_sb <- subset(states_merged_sb, !is.na(TWEETS))

# Setting up the pop up text
popup_sb <- paste0("Total: ", as.character(states_merged_sb$TWEETS))


# User interface ----
ui <- fluidPage(
  titlePanel("Data Intensive Computing: Lab 1: Part3"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select Heatmap from below list"),
      
      selectInput("var", 
                  label = "Choose:",
                  choices = c("CDC Map" = "cdc",
                              "Twitter Map" = "twt",
                              "CDC vs Twitter MAp" = "cdctwt"),
                  selected = "Percent White"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
      ),
    
    #mainPanel(plotOutput("map"))
    mainPanel(
      leafletOutput("mymap")
      )
    
  )
  )

server <- function(input, output) {

  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({

    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>% 
      addPolygons(data = states_merged_sb , 
                  fillColor = ~pal(states_merged_sb$TWEETS), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = ~popup_sb) %>%
      addLegend(pal = pal, 
                values = states_merged_sb$TWEETS, 
                position = "bottomright", 
                title = "TWEETS")
  })
  
  
  
}

# Run app ----
shinyApp(ui, server)