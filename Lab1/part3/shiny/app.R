library(shiny)
library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")

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
    mainPanel(imageOutput("myImage"))
    
  )
  )

server <- function(input, output) {
#  output$map <- renderPlot({
 #   args <- switch(input$var,
#                   "Percent White" = list(counties$white, "darkgreen", "% White"),
 #                  "Percent Black" = list(counties$black, "black", "% Black"),
  #                 "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
   #                "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
#    
 #   args$min <- input$range[1]
#    args$max <- input$range[2]
 #   
#    do.call(percent_map, args)
#  })
  
  output$myImage <- renderImage({

    
    switch(input$var,
           "cdc" = list(src = 'data/tweets_by_state.png', contentType = 'image/png', width = 840, height = 840),
           "twt" = list(src = 'data/ili_activity_level.png', contentType = 'image/png', width = 840, height = 840),
           "cdctwt" = list(src = 'data/tweets_by_state.png', contentType = 'image/png', width = 840, height = 840)
           
    )
    
    
    
  }, deleteFile = FALSE)
  
  
  
}

# Run app ----
shinyApp(ui, server)