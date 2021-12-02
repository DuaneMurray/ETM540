#
####################################################################
# ETM-540 TEAM 1 FINAL PROJECT - R SHINY APPLICATION
#
# Source Code Respository: https://github.com/DuaneMurray/ETM540
####################################################################
# 

library(shiny)
library(leaflet)
suppressPackageStartupMessages(library(tinytex))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ROI))
suppressPackageStartupMessages(library(ROI.plugin.glpk))
suppressPackageStartupMessages(library(ompr))
suppressPackageStartupMessages(library(ompr.roi))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyverse))
#library(DT) # FUTURE: SELECTION/MANIPULATION OF DATA FROM TABLES

#setwd("G:/My Drive/FALL-2021/ETM640/Project/Code/") # SET WORKING DIR
setwd(".")

#tourist_locations <- read.csv("portland_location_data_2.csv") # LOAD FROM FILE
tourist_locations <- read.csv("TEST_portland_location_data_2.csv")

# USE THE LAST ROW IN THE DATA FILE FOR THE STARTING LOCATION
start_location <- tail(tourist_locations, n = 1)
colnames(start_location) <- colnames(tourist_locations)

# USE MEANINGFUL NAMES FOR THE DATA COLUMNS
colnames(tourist_locations) <- c("Attraction", 
                                 "Cost",
                                 "Address",
                                 "OpenTime",
                                 "CloseTime",
                                 "Latitude",
                                 "Longitude",
                                 "Classification")

# JUST A TEMP LOCATION TO FILTER THE SOURCE DATA MATRIX BY USER SELECTIONS
refined_locations <- tourist_locations

time_avail <- 24 # TOTAL TIME IN 1 DAY THAT TOURIST HAS TO VISIT ATTRACTIONS

# USER INTERFACE SECTION DEFINITION
ui <- fluidPage(
  
  # APPLICATION TITLE
  titlePanel("Tourist Attractions in Portland Oregon"),
  
  # CREATE A SIDEBAR SECTION
  sidebarLayout(  
    
    # CREATE PANEL - CAN BE SEVERAL DIFFERENT TYPES
    sidebarPanel(
      
      # USER BUDGET
      sliderInput(inputId = "budget" ,
                  label="What is your budget per location?:",
                  value = 50, min=0, max=50),
    
      # USER CATEGORY INTERESTS - CAN SELECT MULTIPLE OPTIONS
      selectInput("interests", "Choose the areas that you are interested in:",
                  list(`Interests` = list(
                    "Art",
                    "History",
                    "Adventure",
                    "Scenic",
                    "Sports",
                    "Foodie",
                    "Music",
                    "Shopping",
                    "Festivals",
                    "Eco Tourism",
                    "Architecture",
                    "Theatre",
                    "Landmark",
                    "Recreational")), multiple = TRUE),
      
      # DYNAMIC LIST FROM DATA FILE SHOWING ALL AVAILABLE ATTRACTIONS
      selectInput("locations", "Choose Specific Locations:", 
                  refined_locations[,1], selected = NULL, multiple = TRUE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      sliderInput(inputId = "start_time" ,
                  label="Start Time (24 Hr)?",
                  value = 1, min=1, step=1, max=24, sep=""),

      sliderInput(inputId = "end_time" ,
            label="End Time (24 Hr)?",
            value = 24, min=1, step=1, max=24, sep=""),

      # RESET ALL INPUT VALUES TO DEFAULT
      actionButton("reset_input", "Reset"),
      
      # RUN THE DATA THROUGH THE OPTIMIZATION TSP MODEL
      actionButton("run_model", "RUN MODEL"),
      
      # BLANK ROW SPACE
      p(),
      
      # TEMP AREA TO OUTPUT TEXT MESSAGES
      textOutput("text"),
      
      # PLACE THE PLOT FROM THE OPTIMIZATION MODEL ONTO THE SHINY SCREEN
      plotOutput("optimal_path") # TEMP AREA FOR OPTIMIZATION OUTPUT
      
    ),
    
    # CREATE MAIN DATA OUTPUT AREA
    mainPanel (
      leafletOutput("mymap", height=600), # MAP OF TOURIST ATTRACTION LOCATIONS
      p(),
      
      dataTableOutput("data"), # LIST OF DATA FROM IMPORT FILE
      p() # BE SURE TO ADD A COMMA (,) TO THE END OF THIS IF PLOT ENABLED
    )
    
  ) # END sidebarLayout()
  
) # END UI


# MAIN CODE TO GENERATE DATA TO SEND TO THE USER INTERFACE SECTION
server <- function(input, output, session) {
  
  # ACTIONS TO TAKE WHEN THE USER PRESSES THE RUN MODEL BUTTON IN THE UI
  observeEvent(input$run_model, {
    
    output$optimal_path <- renderPlot({
      # CLEAR THE GGPLOT OUTPUT AREA
    })
    output$text <- renderText({ 
      # CLEAR THE TEXT OUTPUT AREA 
    })
    
    n <- nrow(refined_locations) # NUMBER OF TOTAL LOCATIONS TO VISIT

    # LONGITUDE = x, LATUTUDE = y
    locations <- data.frame(id = 1:n, x = refined_locations[,7], 
                            y = refined_locations[,6], 
                            loc_name = refined_locations[,1])
    
    #time_required <- data.frame(id=1:n, time_req = refined_locations[,6])
    
    starting_pt <- data.frame(id = 1:n, x = start_location[,7], 
                            y = start_location[,6])
    
    distance <- as.matrix(stats::dist(select(locations, x, y), 
                                      diag = TRUE, upper = TRUE))
    
    dist_fun <- function(i, j) {
      vapply(seq_along(i), function(k) distance[i[k], j[k]] 
             , numeric(1L))
    }
    
    model <- MIPModel() %>%
      # WE CREATE A VAR THAT IS 1 IFF WE TRAVEL FROM LOC i to j
      add_variable(x[i, j], i = 1:n, j = 1:n, 
                   type = "integer", lb = 0, ub = 1) %>%
      
      # HELPER VAR FOR THE MTZ FORMULATION OF THE TSP
      add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
      
      # MINIMIZE THE TRAVEL DISTANCE WITH THE TIME AVAILABLE
      set_objective(sum_expr(dist_fun(i, j) * x[i, j], i = 1:n, j = 1:n)
                    , "min") %>%
      
      # YOU CAN ONLY VISIT A LOCATION ONE TIME, NO REPEATS
      set_bounds(x[i, i], ub = 0, i = 1:n) %>%
      
      # MUST LEAVE EACH LOCATION
      add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
      
      # VISIT EACH LOCATION
      add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
      
      # ENSURE THAT NO SUB-BRANCHES ARE USED
      add_constraint(u[i] >= 2, i = 2:n) %>% 
      add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
    
    result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
    
    solution <- get_solution(result, x[i, j]) %>% 
      filter(value > 0) 
    
    # BUILD PATHS FOR EACH VISIT TO EACH LOCATION
    paths <- select(solution, i, j) %>% 
      rename(from = i, to = j) %>% 
      mutate(trip_id = row_number()) %>% 
      tidyr::gather(property, idx_val, from:to) %>% 
      mutate(idx_val = as.integer(idx_val)) %>% 
      inner_join(locations, by = c("idx_val" = "id"))
    
    # CREATE PLOT OF PATH TO TAKE TO VISIT ALL LOCATIONS ONLY ONCE AT LOWEST COST
    output$optimal_path <- renderPlot({
      ggplot(locations, aes(x, y)) + 
        geom_point(size=5)  + 
        geom_point(data = locations %>% filter(loc_name == "Benson Hotel"), 
                   color = "red", size=5) +
        geom_text(data = locations, aes(label = loc_name), hjust = 0.75,  
                  vjust = -1) +
        geom_line(data = paths, aes(group = trip_id)) + 
        ggtitle("Optimal Travel Route by Distance from the Benson Hotel for Filtered Locations")
      
      #          ggtitle(paste0("Optimal route with cost: ", 
      #                 round(objective_value(result), 2)))
    })
    
    output$data <- renderDataTable({
      refined_locations
    })
    
  })
  
  # ASSOCIATED WITH THE BUDGET SLIDER VALUE IN THE USER INTERFACE
  observeEvent(input$budget, {
    
    refined_locations <<- tourist_locations
    refined_locations <<- subset(refined_locations, subset=(Cost<=input$budget))
    
    #refined_locations <- filter(refined_locations, input$start_time >= OpenTime, input$end_time <= CloseTime)
    
    # ENSURE THAT THE STARTING LOCATION IS ALWAYS IN THE LIST
    colnames(start_location) <- colnames(tourist_locations)
    refined_locations <<- rbind(refined_locations, start_location)
    
    #observe(print(input$start_time))
    #observe(print(input$end_time))
    #observe(print(refined_locations))
    
    # OUTPUT CONTENTS OF LEAFLET MAP TO THE "mymap" AREA OF THE USER INTERFACE
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(-122.6792634, 45.51867737, zoom = 14) %>%
        addCircleMarkers(lng = start_location[,7],
                         lat = start_location[,6],
                         label = as.character(start_location[,1]),
                         popup = as.character(start_location[,3]),
                         color = "red") %>%
        setView(-122.6792634, 45.51867737, zoom = 14) %>%
        addMarkers(lng = refined_locations[,7], 
                   lat = refined_locations[,6], 
                   label = as.character(refined_locations[,1]), 
                   popup = as.character(refined_locations[,3]))
    })
    
    output$data <- renderDataTable({
      refined_locations
    })
    
    total_budget <<- input$budget
  })
  
  # ASSOCIATED WITH THE INTEREST SELECTION VALUES IN THE USER INTERFACE
  observeEvent(input$interests, {
    
    #refined_locations <- tourist_locations
    
    #output$text <- renderText({ 
    #  input$interests 
    #})
    
    # OUTPUT CONTENTS OF LEAFLET MAP TO THE "mymap" AREA OF THE USER INTERFACE
    #    output$mymap <- renderLeaflet({
    #      leaflet() %>%
    #        addTiles() %>%
    #        setView(-122.6792634, 45.51867737, zoom = 14) %>%
    #        addCircleMarkers(lng = start_location[,7],
    #                         lat = start_location[,6],
    #                         label = as.character(start_location[,1]),
    #                         popup = as.character(start_location[,3]),
    #                         color = "red") %>%
    #        addMarkers(lng = refined_locations[,7], 
    #                   lat = refined_locations[,6], 
    #                   label = as.character(refined_locations[,1]), 
    #                   popup = as.character(refined_locations[,3]))
    #    })
    #    
    #    output$data <- renderDataTable({
    #      refined_locations
    #    })
      
  })
  
  # ASSOCIATED WITH THE INTEREST SELECTION VALUES IN THE USER INTERFACE
  observeEvent(input$locations, {
    
    #refined_locations <- tourist_locations
    
    #output$text <- renderText({ 
    #  input$locations 
    #})
    
    # OUTPUT CONTENTS OF LEAFLET MAP TO THE "mymap" AREA OF THE USER INTERFACE
    #    output$mymap <- renderLeaflet({
    #      leaflet() %>%
    #        addTiles() %>%
    #        setView(-122.6792634, 45.51867737, zoom = 14) %>%
    #        addCircleMarkers(lng = start_location[,7],
    #                         lat = start_location[,6],
    #                         label = as.character(start_location[,1]),
    #                         popup = as.character(start_location[,3]),
    #                         color = "red") %>%
    #        addMarkers(lng = refined_locations[,7], 
    #                   lat = refined_locations[,6], 
    #                   label = as.character(refined_locations[,1]), 
    #                   popup = as.character(refined_locations[,3]))
    #    })
    #    
    #    output$data <- renderDataTable({
    #      refined_locations
    #    })
    
  })
  
  # ASSOCIATED WITH THE START TIME VALUE IN THE USER INTERFACE
  observeEvent(input$start_time, {

    time_avail <<- input$end_time - input$start_time
    if (time_avail < 0) {time_avail = 0}
    #observe(print(time_avail))
    
  })
  
  # ASSOCIATED WITH THE END TIME VALUE IN THE USER INTERFACE
  observeEvent(input$end_time, {
    
    time_avail <<- input$end_time - input$start_time
    if (time_avail < 0) {time_avail = 0}
    #observe(print(time_avail))
    
  })
  
  # RESET ALL FORM INPUT AND OUTPUT ELEMENTS TO DEFAULTS
  observeEvent(input$reset_input, {
    
    # RESET THE FILTERED MATRIX TO THE FULL DATA SET MATRIX
    refined_locations <<- tourist_locations
    
    updateSliderInput(session, inputId = "budget" ,
                label="What is your budget?:",
                value = 50, min=0, max=50)
    
    updateSelectInput(session, "interests", "Choose the areas that you are 
                      interested in:",
                choices=list(`Interests` = list(
                  "Art",
                  "History",
                  "Adventure",
                  "Scenic",
                  "Sports",
                  "Foodie",
                  "Music",
                  "Shopping",
                  "Festivals",
                  "Eco Tourism",
                  "Architecture",
                  "Theatre",
                  "Landmark",
                  "Recreational")), selected = NULL)
    
    updateSelectInput(session, "locations", "Choose Specific Locations:", 
                choices=refined_locations[,1], selected = NULL)
    
    updateNumericInput(session, "start_time", label="Start Time (24 Hour)", 
                       value=1)
    
    updateNumericInput(session, "end_time", label="End Time (24 Hour)", 
                       value=24)
    
    # RESET THE LEAFLET MAP
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(-122.6792634, 45.51867737, zoom = 14) %>%
        addCircleMarkers(lng = start_location[,7],
                         lat = start_location[,6],
                         label = as.character(start_location[,1]),
                         popup = as.character(start_location[,3]),
                         color = "red") %>%
        addMarkers(lng = refined_locations[,7], 
                   lat = refined_locations[,6], 
                   label = as.character(refined_locations[,1]), 
                   popup = as.character(refined_locations[,3]))
    })
    
    # RESET THE DATA TABLE
    output$data <- renderDataTable({
      refined_locations
    })
    
    output$optimal_path <- renderPlot({
      # CLEAR THE GGPLOT OUTPUT AREA
    })
    
  })
  
  # OUTPUT CONTENTS OF LEAFLET MAP TO THE "mymap" AREA OF THE USER INTERFACE
#  output$mymap <- renderLeaflet({
#    leaflet() %>%
#    addTiles() %>%
#    setView(-122.6792634, 45.51867737, zoom = 14) %>%
#      addCircleMarkers(lng = start_location[,7],
#                       lat = start_location[,6],
#                       label = as.character(start_location[,1]),
#                       popup = as.character(start_location[,3]),
#                       color = "red") %>%
#      addMarkers(lng = refined_locations[,7], 
#                 lat = refined_locations[,6], 
#                 label = as.character(refined_locations[,1]), 
#                 popup = as.character(refined_locations[,3]))
#  })

  # OUTPUT THE CONTENTS OF THE refined_locations MATRIX IN A TABLE
  # DEFINED AS "data" IN THE USER INTERFACE
#  output$data <- renderDataTable({
#    refined_locations
#  })
  
} # END SERVER


# INIT THE SHINY APPLICATION
shinyApp(ui, server)
