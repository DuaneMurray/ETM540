#
####################################################################
# ETM-540 TEAM 1 FINAL PROJECT - SHINY APPLICATION
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
#suppressPackageStartupMessages(library(pander))
#library(DT)

setwd("G:/My Drive/FALL-2021/ETM640/Project/Code/") # SET WORKING DIR

tourist_locations <- read.csv("TEST_portland_location_data.csv") # LOAD DATA FROM FILE

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
  titlePanel("Attractions in Portland Oregon"),
  
  # CREATE A SIDEBAR SECTION
  sidebarLayout(  
    
    # CREATE PANEL - CAN BE SEVERAL DIFFERENT TYPES
    sidebarPanel(
      
      # USER BUDGET
      sliderInput(inputId = "budget" ,
                  label="What is your budget?:",
                  value = 40, min=0, max=200),
    
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
      
      # USER START TIME AVAILABLE - 24 HOUR CLOCK
      numericInput("start_time", "Start Time (24 Hour)", 1200),
      
      # USER END TIME AVAILABLE - 24 HOUR CLOCK
      numericInput("end_time", "End Time (24 Hour)", 2100),
      
      # RESET ALL INPUT VALUES TO DEFAULT
      actionButton("reset_input", "Reset"),
      
      # RUN THE DATA THROUGH THE OPTIMIZATION TSP MODEL
      actionButton("run_model", "RUN MODEL"),
      
      # BLANK ROW SPACE
      p(),
      
      # PLACE THE PLOT FROM THE OPTIMIZATION MODEL ONTO THE SHINY SCREEN
      plotOutput("optimal_path") # TEMP AREA FOR OPTIMIZATION OUTPUT
      
    ),
    
    # CREATE MAIN DATA OUTPUT AREA
    # NEED FUNCTION TO UPDATE DATA ON SELECTIONS IN SIDEBARPANEL()
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
    
    n <- nrow(refined_locations) # NUMBER OF TOTAL LOCATIONS TO VISIT

    # LONGITUDE = x, LATUTUDE = y
    locations <- data.frame(id = 1:n, x = refined_locations[,9], y = refined_locations[,8])
    
    distance <- as.matrix(stats::dist(select(locations, x, y), diag = TRUE, upper = TRUE))
    dist_fun <- function(i, j) {
      vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
    }
    
    model <- MIPModel() %>%
     
      # WE CREATE A VAR THAT IS 1 IFF WE TRAVEL FROM LOC i to j
      add_variable(x[i, j], i = 1:n, j = 1:n, 
                   type = "integer", lb = 0, ub = 1) %>%
      
      # HELPER VAR FOR THE MTZ FORMULATION OF THE TSP
      add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
      
      # MINIMIZE THE TRAVEL DISTANCE
      set_objective(sum_expr(dist_fun(i, j) * x[i, j], i = 1:n, j = 1:n), "min") %>%
      
      # YOU CAN ONLY VISIT A LOCATION ONE TIME, NO REPEATS
      set_bounds(x[i, i], ub = 0, i = 1:n) %>%
      
      # MUST LEAVE EACH LOCATION
      add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
      
      # VISIT EACH LOCATION
      add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
      
      # ENSURE THAT NO SUB-BRANHES ARE USED
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
        geom_point() + 
        geom_line(data = paths, aes(group = trip_id)) + 
        ggtitle(paste0("Optimal route with cost: ", round(objective_value(result), 2)))
    })
    
  })
  
  # ASSOCIATED WITH THE BUDGET SLIDER VALUE IN THE USER INTERFACE
  observeEvent(input$budget, {
    #
  })
  
  # ASSOCIATED WITH THE INTEREST SELECTION VALUES IN THE USER INTERFACE
  observeEvent(input$interests, {
    #
  })
  
  # ASSOCIATED WITH THE INTEREST SELECTION VALUES IN THE USER INTERFACE
  observeEvent(input$locations, {
    #
  })
  
  # ASSOCIATED WITH THE START TIME VALUE IN THE USER INTERFACE
  observeEvent(input$start_time, {
    #
  })
  
  # ASSOCIATED WITH THE END TIME VALUE IN THE USER INTERFACE
  observeEvent(input$end_time, {
    #
  })
  
  # RUN THE MODEL - UPDATE THE MAP AND LIST TO SHOW RESULTS
  observeEvent(input$run_model, {
    #
  })
  
  # RESET ALL FORM INPUT AND OUTPUT ELEMENTS TO DEFAULTS
  observeEvent(input$reset_input, {
    updateSliderInput(session, inputId = "budget" ,
                label="What is your budget?:",
                value = 40, min=0, max=200)
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
                       value=1200)
    updateNumericInput(session, "end_time", label="End Time (24 Hour)", 
                       value=2100)
    
    # RESET THE LEAFLET MAP
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(-122.6792634, 45.51867737, zoom = 14) %>%
        addMarkers(lng = refined_locations[,9], 
                   lat = refined_locations[,8], 
                   label = as.character(refined_locations[,1]), 
                   popup = as.character(refined_locations[,4]))
    })
    
    # RESET THE DATA TABLE
    output$data <- renderDataTable({
      refined_locations
    })
    
  })
  
  # OUTPUT CONTENTS OF LEAFLET MAP TO THE "mymap" AREA OF THE USER INTERFACE
  output$mymap <- renderLeaflet({
    leaflet() %>%
    addTiles() %>%
    setView(-122.6792634, 45.51867737, zoom = 14) %>%
    addMarkers(lng = refined_locations[,9], 
               lat = refined_locations[,8], 
               label = as.character(refined_locations[,1]), 
               popup = as.character(refined_locations[,4]))
  })

  # OUTPUT THE CONTENTS OF THE refined_locations MATRIX IN A TABLE
  # DEFINED AS "data" IN THE USER INTERFACE
  output$data <- renderDataTable({
    refined_locations
  })

  
  # A PLOT OF FIXED SIZE - TEMP AREA - INTO "optimal_path" AREA IN THE UI
  output$optimal_path <- renderImage({
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    png(outfile, width=400, height=400)
    
    # TEMP - PRINT SIMPLE HISTOGRAM
    hist(rnorm(input$n))
    dev.off()
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
} # END SERVER


# INIT THE SHINY APPLICATION
shinyApp(ui, server)

