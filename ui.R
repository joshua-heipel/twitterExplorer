# load packages

library(shiny)
library(shinyFiles)

# main
ui <- shinyUI(
  fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),

  titlePanel(
    h1("Shiny Twitter Explorer 0.1"),
    windowTitle = "Twitter Explorer 0.1"
  ),

  sidebarLayout(
    position = "left",
    
    sidebarPanel(
      textInput(inputId = "inputQuery", label = NULL, placeholder = "<Search query>", width="75%"),
      actionButton(inputId = "searchButton", label = "Search"),
      p(sliderInput(inputId = "number", label = "Number of Tweets", min = 1, max = 1000, value = 100)),
      actionButton(inputId = "chooseDirectoryButton", label = "Choose directory"),
      actionButton(inputId = "downloadTweetsButton", label = "Download Tweets"),
      actionButton(inputId = "downloadImagesButton", label = "Download Images"),
      width=8
    ),
    
    mainPanel(
      p(selectizeInput(inputId = "selectedLabel", label = NULL,
                       choices = list(biology = c("animals", "plants"), 
                                      climate = c("air_pressure", "clouds", "humidity", "ice", "precipitation", "severe_weather", "specific_climate_values", "temperature", "visibility_-_aerosols", "wind"), 
                                      economy = c("kind_of_goods", "market"), 
                                      environment = c("astronomy", "avalanche", "celestial_phenomena", "geophysics", "landslide", "wildfire"),
                                      hydrology = c("discharge", "erosion", "flash_flood", "flow_velocity", "irrigation_canals", "sediment_deposition", "storm_surge", "water_discoloration", "water_level", "water_temperature"), 
                                      phenology = c("animal_phenology", "number_of_animals", "plant_phenology", "seasons"), 
                                      society = c("affected_people", "damages", "human_health", "mitigation", "pollution", "reasoning", "religious_rituals", "social_problems", "traffic", "warnings")),
                       multiple = TRUE, size = 50,
                       options = list(
                         placeholder = "<Select label>",
                         create = TRUE,
                         onInitialize = I('function() { this.setValue(""); }')
                       )
      )),
      p(actionButton(inputId = "labelButton", label = "Label Tweets")),
      # h2("Results"),
      DT::dataTableOutput(outputId = "data", width="100%", height="auto"),
      width = 12
    )
  )
))

# downloadButton(outputId = "downloadData", label = "Download selected Tweets"),
# shinyFilesButton(id = "fileButton", label = "Directory", title="Something", multiple=FALSE),