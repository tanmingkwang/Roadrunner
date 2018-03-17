library(shiny)
library(shinythemes)
library(leaflet)
library(readr)
library(sf)
library(spatstat)
library(maptools)
library(sqldf)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("Singapore Traffic Accidents Analysis"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    
                    fileInput(inputId="file",
                              label = "Upload File"),
                    
                    hr(),
                    selectizeInput(inputId = "analysis", 
                                   label = "Choose Analysis:", 
                                   choices = c("Kernel Density Estimation (KDE)", "K-Function", "Multitype K-Function", "Nearest Neighbour Distance"),
                                   options = list(onInitialize = I('function() { this.setValue(""); }'))),
                    
                    selectizeInput(inputId = "expressWay", 
                                   label = "Choose Expressway:", 
                                   choices = c("AYE", "BKE", "CTE", "ECP", "KJE", "KPE", "MCE", "PIE", "SLE", "TPE"),
                                   options = list(onInitialize = I('function() { this.setValue(""); }'))),
                    
                    conditionalPanel(condition = "input.analysis == 'Kernel Density Estimation (KDE)'",
                                     selectizeInput(inputId = "kdeType",
                                                    label = "Choose Variable:",
                                                    choices = c("Accidents", "Heavy Traffic"),
                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                    
                    conditionalPanel(condition = "input.analysis == 'K-Function'",
                                     selectizeInput(inputId = "kfType",
                                                    label = "Choose Variable:",
                                                    choices = c("Accidents", "Heavy Traffic"),
                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                    
                    conditionalPanel(condition = "input.analysis == 'Multitype K-Function'",
                                     selectizeInput(inputId = "mkfType",
                                                    label = "Choose Variable:",
                                                    choices = c("Accidents", "Heavy Traffic"),
                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                    
                    conditionalPanel(condition = "input.analysis == 'Multitype K-Function' && input.mkfType == 'Accidents'",
                                     selectizeInput(inputId = "accidentsMKFunction",
                                                    label = "Choose 2nd Variable:",
                                                    choices = c("Traffic Camera"),
                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                    
                    conditionalPanel(condition = "input.analysis == 'Multitype K-Function' && input.mkfType == 'Heavy Traffic'",
                                     selectizeInput(inputId = "trafficMKFunction",
                                                    label = "Choose 2nd Variable:",
                                                    choices = c("Traffic Camera"),
                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                    
                    actionButton(inputId="compute",
                                 label = "Enter"),
                    
                    
                    
                    width = 3
                    
                    
                    
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    tabsetPanel(
                      tabPanel("With Road Network Constraint",
                               plotOutput("svy21"),
                               tableOutput("contents"),
                               
                               leafletOutput("map", height = "580px", width= "1050px")
                               ), 
                      tabPanel("Without Road Network Constraint")
                    )
                  )
                )
)

server <- function(input, output) {
  
  output$contents <- renderTable({
    inFile <-input$file

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath)
  })
  

  
  output$map <- renderLeaflet({
    #inFile <-input$file
    leaflet() %>%
      setView(103.8198, 1.3521,zoom = 11) %>% 
      addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
      addTiles(group = "OSM") %>% 
      addLayersControl(
        baseGroups = c( "CartoDB (default)","OSM"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  output$svy21 <- renderPlot({
    inFile <-input$file
    
    if (is.null(inFile))
      return(NULL)
    
    trafficReport <- read.csv(inFile$datapath)
    trafficReport_shp <- st_as_sf(trafficReport, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")
    traffic_Report_svy21 <- st_transform(trafficReport_shp, crs = 3414)
    st_write(traffic_Report_svy21, "temp.csv", layer_options = "GEOMETRY=AS_XY")
    plot(traffic_Report_svy21)
    
    temp <- read.csv("temp.csv")
    accidents <- temp %>% filter(Type == "Accident")
    roadworks <- temp %>% filter(Type == "Roadwork")
    heavytraffic <- temp %>% filter(Type == "Heavy Traffic")
    accidents_ppp <- ppp(accidents$X, accidents$Y, c(min(accidents$X), max(accidents$X)), c(min(accidents$Y), max(accidents$Y)))
    roadworks_ppp <- ppp(roadworks$X, roadworks$Y, c(min(roadworks$X), max(roadworks$X)), c(min(roadworks$Y), max(roadworks$Y)))
    heavytraffic_ppp <- ppp(heavytraffic$X, heavytraffic$Y, c(min(heavytraffic$X), max(heavytraffic$X)), c(min(heavytraffic$Y), max(heavytraffic$Y)))
    plot(accidents_ppp)
    plot(roadworks_ppp)
    plot(heavytraffic_ppp)
    roadNetwork <- readShapeSpatial("roads_expressway")
    roadNetwork_psp <- as.psp(roadNetwork, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
    roadNetwork_linnet <- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
    plot(roadNetwork_linnet)
  })
  
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

