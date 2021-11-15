#
####################################################################
# ETM-540 TEAM 1 FINAL PROJECT - SHINY APPLICATION
####################################################################
#

library(shiny)
library(leaflet)

setwd("G:/My Drive/FALL-2021/ETM640/Project/Code/")

tourist_locations <- read.csv("location_data.csv") # LOAD DATA FROM FILE

# USE MEANINGFUL NAMES FOR THE DATA COLUMNS
colnames(tourist_locations) <- c("Attraction", 
                                 "InPDX", 
                                 "Cost",
                                 "Address",
                                 "DistanceFromDT",
                                 "OpenTime",
                                 "CloseTime",
                                 "latitude",
                                 "longitude",
                                 "Classification")

# JUST A TEMP LOCATION IN CASE WE NEED TO CHANGE THE SOURCE DATA MATRIX
refined_locations <- tourist_locations


# USER INTERFACE SECTION DEFINITION
ui <- fluidPage(
  
  # APPLICATION TITLE
  titlePanel("Attractions in Oregon"),
  
  # CREATE A SIDEBAR SECTION
  sidebarLayout(  
    
    # CREATE PANEL - CAN BE SEVERAL DIFFERENT TYPES
    sidebarPanel(
      
      # EXAMPLE SELECTION TYPES
      numericInput("min", "Minimum", 0),
      numericInput("max", "Maximum", 100),
      sliderInput("n", "n", min = 0, max = 100, value = 50),
      
      plotOutput("optimal_path") # TEMP AREA FOR OPTIMIZATION OUTPUT
    ),
    
    # CREATE MAIN DATA OUTPUT AREA
    # NEED FUNCTION TO UPDATE DATA ON SELECTIONS IN SIDEBARPANEL()
    mainPanel (
      leafletOutput("mymap", height=600), # MAP OF TOURIST ATTRACTION LOCATIONS
      p(),
      dataTableOutput("data"), # LIST OF DATA FROM IMPORT FILE
      p() # BE SURE TO ADD A COMMA (,) TO THE END OF THIS IF PLOT ENABLED
      #plotOutput("optimal_path") # TEMP AREA FOR OPTIMIZATION OUTPUT
      
    )
    
  ) # END sidebarLayout()
  
) # END UI


# MAIN CODE TO GENERATE DATA TO SEND TO THE USER INTERFACE SECTION
server <- function(input, output, session) {
  
  # OUTPUT CONTENTS OF LEAFLET MAP TO THE "mymap" AREA OF THE USER INTERFACE
  output$mymap <- renderLeaflet({
    leaflet() %>%
    addTiles() %>%
    addMarkers(lng = refined_locations[,9], 
               lat = tourist_locations[,8], 
               label = as.character(tourist_locations[,1]), 
               popup = as.character(tourist_locations[,4]))
  })

  # OUTPUT THE CONTENTS OF THE refined_locations MATRIX IN A TABLE
  # DEFINED AS "data" IN THE USER INTERFACE
  output$data <- renderDataTable({
    refined_locations
  })
  
  
  ############################
  # WORK IN PROGRESS
  ############################

  # ASSOCIATED WITH THE MIN SLIDER VALUE IN THE USER INTERFACE
  observeEvent(input$min, {
    updateSliderInput(inputId = "n", min = input$min)
  })
  
  # ASSOCIATED WITH THE MAX SLIDER VALUE IN THE USER INTERFACE
  observeEvent(input$max, {
    updateSliderInput(inputId = "n", max = input$max)
  })
  
  # A PLOT OF FIXED SIZE - TEMP AREA - INTO "optimal_path" AREA IN THE UI
  output$optimal_path <- renderImage({
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    png(outfile, width=400, height=400)
    hist(rnorm(input$n))
    dev.off()
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
} # END SERVER


# INIT THE SHINY APPLICATION
shinyApp(ui, server)

