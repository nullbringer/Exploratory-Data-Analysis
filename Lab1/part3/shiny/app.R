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
library(tidyr)

### Common Execution ###
########################

# Downloading the shapefiles for states at the lowest resolution
states_import <- states(cb=T)


dp_dw <- read.csv(file="data/dropdowns.csv", header=FALSE, sep=",")

dropdown_choices <- setNames(as.list(dp_dw$V1), dp_dw$V2)


#dropdown_choices <- read.csv(file="data/dropdowns.csv", header=FALSE)$V1

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

####### End Common Execution #####


## CDS Starts ###


ili_activity_level = read.csv("data/cds.csv")
act_lvl <- ili_activity_level[c(1,4,5,7)]

act_lvl <-separate(data = act_lvl, col = ACTIVITY.LEVEL, into = c("leveltext", "levelvalue"), sep = " ")

act_lvl$levelvalue <- as.numeric(act_lvl$levelvalue)

level_by_state <- ddply(act_lvl, .(STATENAME), summarize,  Level=ceiling(mean(levelvalue)))
names(level_by_state) <- c("NAME","ENTRIES")

level_by_state$NAME <- tolower(level_by_state$NAME)

level_by_state <- left_join(level_by_state, state_off)

level_by_state_sb <- geo_join(states_import, level_by_state, "STUSPS", "state")

level_by_state_sb <- subset(level_by_state_sb, !is.na(ENTRIES))



### CDS Ends ###




# User interface ----
ui <- fluidPage(
  titlePanel("Data Intensive Computing: Lab 1: Part3"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select data from the dropdowns to compare!!"),
      
      selectInput("var1", 
                  label = "Data Set 1:",
                  choices = dropdown_choices,
                  selected = "cds"),
      
      selectInput("var2", 
                  label = "Data Set 2:",
                  choices = dropdown_choices,
                  selected = "twt")
      ),
    
    #mainPanel(plotOutput("map"))
    mainPanel(
      leafletOutput("mymap1") %>% withSpinner(color="#0dc5c1", type = "7"),
      leafletOutput("mymap2") %>% withSpinner(color="#0dc5c1", type = "7")
      )
    
  )
  )

server <- function(input, output) {

  observeEvent(input$var1, {
    
    print(input$var1)
    
    if(input$var1 == 'cds'){
      
      pal <- colorNumeric("Greens", domain=level_by_state_sb$ENTRIES)
      popup_sb <- paste0("<strong>", level_by_state_sb$NAME, 
                         "</strong><br />Tweets: ", level_by_state_sb$ENTRIES)
      states_merged_sb <- level_by_state_sb
      map_legend_title <- "Level"
      
    } else {
      
      
      if(input$var1 == 'twt'){
        fluTweet_filtered <- fluTweet
      } else{
        
        fluTweet_filtered <- fluTweet %>% 
          filter(str_detect(text, input$var1))
        
      }
      
      print(nrow(fluTweet_filtered))
      
      count_by_state<- fluTweet_filtered %>% count(state)
      
      colnames(count_by_state) <- c("NAME", "ENTRIES")
      
      
      state_tweets <- left_join(count_by_state, state_off)
      
      
      states_merged_sb <- geo_join(states_import, state_tweets, "STUSPS", "state")
      
      # Creating a color palette based on the number range in the total column
      pal <- colorNumeric("Greens", domain=states_merged_sb$ENTRIES)
      
      # Getting rid of rows with NA values
      # Using the Base R method of filtering subset() because we're dealing with a SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work
      
      states_merged_sb <- subset(states_merged_sb, !is.na(ENTRIES))
      
      popup_sb <- paste0("<strong>", states_merged_sb$NAME, 
                         "</strong><br />Tweets: ", states_merged_sb$ENTRIES)
      
      map_legend_title <- "Tweets"
      
    }
    
    
    output$mymap1 <- renderLeaflet({
      
      
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = states_merged_sb , 
                    fillColor = ~pal(states_merged_sb$ENTRIES), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2,
                    
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = states_merged_sb$ENTRIES, 
                  position = "bottomright", 
                  title = map_legend_title)
    })
    
  })
  
  
  
  
  observeEvent(input$var2, {
    
    print(input$var2)
    
    if(input$var2 == 'cds'){
      
      pal <- colorNumeric("Reds", domain=level_by_state_sb$ENTRIES)
      popup_sb <- paste0("<strong>", level_by_state_sb$NAME, 
                         "</strong><br />Tweets: ", level_by_state_sb$ENTRIES)
      states_merged_sb <- level_by_state_sb
      map_legend_title <- "Level"
      
      
    } else {
      
      if(input$var2 == 'twt'){
        fluTweet_filtered <- fluTweet
      } else{
        
        fluTweet_filtered <- fluTweet %>% 
          filter(str_detect(text, input$var2))
        
      }
      
      print(nrow(fluTweet_filtered))
      
      count_by_state<- fluTweet_filtered %>% count(state)
      
      colnames(count_by_state) <- c("NAME", "ENTRIES")
      
      
      state_tweets <- left_join(count_by_state, state_off)
      
      
      states_merged_sb <- geo_join(states_import, state_tweets, "STUSPS", "state")
      
      # Creating a color palette based on the number range in the total column
      pal <- colorNumeric("Reds", domain=states_merged_sb$ENTRIES)
      
      # Getting rid of rows with NA values
      # Using the Base R method of filtering subset() because we're dealing with a SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work
      
      states_merged_sb <- subset(states_merged_sb, !is.na(ENTRIES))
      
      popup_sb <- paste0("<strong>", states_merged_sb$NAME, 
                         "</strong><br />Tweets: ", states_merged_sb$ENTRIES)
      
      
      map_legend_title <- "Tweets"
    }
    output$mymap2 <- renderLeaflet({
      
      
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = states_merged_sb , 
                    fillColor = ~pal(states_merged_sb$ENTRIES), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2,
                    
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = states_merged_sb$ENTRIES, 
                  position = "bottomright", 
                  title = map_legend_title)
    })
    
  })
  
  
  
}

# Run app ----
shinyApp(ui, server)