library(shiny)
library(data.table)
library(shinyWidgets)

ui <- fluidPage(
  mainPanel(
    dataTableOutput("data")
  ),
  titlePanel("PDX Tourisim - we will plan for you!"),
  sliderInput(inputId = "num" ,
              label="How much you want to spend in $:",
              value = 40, min=0, max=200),
 
  selectInput("Category", "Choose a Category you are interested in:",
              list(`Historical / Landmark sites` = list("Columbia River Gorge National Scenic Area",
                                                        "Pittock Mansion ", "Japanese American Historical Plaza",
                                                        "Portlandia","Mt. Tabor Stairs","Mt. Tabor Park",
"St. Johns Bridge",
"Joan of Arc",
"Tilikum Crossing: Bridge of the People",
"Paul Bunyan Statue",
"International Rose Garden", 
"Japanese Garden",
"Oregon Zoo",
"Pittock Mansion ",
"Powell's City of Books",
"Oregon Musume of Science and Industry ",
"Lan Su Chinese Garden",
"Forest Park",
"International Rose Garden ",
"Japanese Garden",
"Oregon Zoo",
"Pittock Mansion ",
"Powell's City of Books",
"Oregon Musume of Science and Industry ",
"Lan Su Chinese Garden",
"Forest Park",
"International Rose Garden ",
"Japanese Garden",
"Oregon Zoo",
"Pittock Mansion",
"Powell's City of Books",
"Oregon Musume of Science and Industry ",
"Lan Su Chinese Garden",
"Forest Park",
"Portland Oregon White Stag Sign",
"The Lorax Houses",
"Japanese American Historical Plaza",
"Lovejoy Columns",
"Centenial Mills",
"Animals in Pools",
"The Quest",
"The Buddha Building",
"China Gates",
"Oregon Holocaust Memorial",
"Waterfall Fountain",
"Beverly Cleary Sculpture Garden at Grant Park",
"Share-It Square",
"Multnomah Falls",
"Fort Clatsop",
"Portland Spirit River Cruises",
"World Forestry Center Discovery Museum",
"The Grotto",
"Shanghai Tunnels",
"International Rose Test Garden",
"Hoyt Arboretum",
"Tom McCall Waterfront Park",
"Portland Art Museum",
"Pioneer Courthouse Square",
"Oregon Historical Society Museum",
"Mill Ends Park",
"Portland Saturday Market",
"Downtown Park Blocks",
"National Hat Museum",
"Providence Park",
"Portland Aerial Tram",
"Wishing Tree",
"Starkâ€™s Vacuum Museum",
"Lincoln Street Kayak & Canoe Museum",
"Laurelhurst Park",
"Kelley Point Park",
"Collin's Beach (Clothing Optional)",
"Haystock Rock",
"Columbia River Maritime Museum",
"Lewis & Clark National Historical Park",
"Fort Stevens State Park",
"Oregon Coast Aquarium",
"Powell Butte Nature Park",
"Sellwood Riverfront Park",
"Twin Lakes Trail (Upper Twin Lake & Lower Twin Lake via PCT)", 
"The Witch's Castle",
"International Rose Garden", 
"Japanese Garden",
"Oregon Zoo",
"Pittock Mansion", 
"Powell's City of Books",
"Oregon Musume of Science and Industry", 
"Lan Su Chinese Garden",
"Forest Park"),
                   
                   
                   `Recreational` = list("1/2 Hour Quick and Scenic / splash speed boat",
                                         "Florence sand dunes (ATV rentals)","Ziplines"),
                   
                   `Falls and hikes` = list("Will Add", "Multinomah", "Mount Hood", "Mount St. Helens"))
  )
)

server <- function(input, output, session) { 
  
  # READ THE CSV DATA FILE
  fileData <- reactiveFileReader(1000, session, 'location_data.csv', read.csv)
  
  output$data <- renderDataTable({
    fileData()
  })
## Subset data
  #fct <- reactive({
   #LP mathematical model: 
  
  # Maximize Locations
  #Objective: user input ($) + distance multiplier($) = max amount of places 
  
  #Constraints 
  # categories max ($) + corresponding multiplier distance ($) <= user input ($)
  # categories max ($) + multiplier distance ($) >= 10 places - Time Constraint
  # categories max ($) + multiplier distance ($) <= 200 miles? 
}
shinyApp(ui, server)
