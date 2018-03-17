library(shiny)
library(shinythemes)
library(leaflet)
library(readr)
library(sf)
library(spatstat)
library(maptools)
library(sqldf)
library(tidyverse)
library(polyCub)
library(tmap)
library(tmaptools)


costaloutline <- readOGR("CostalOutline.shp")
costaloutline_sp <- as(costaloutline, "SpatialPolygons")
costaloutline_owin <- as.owin.SpatialPolygons(costaloutline_sp)
costaloutline_mask <- as.mask(costaloutline_owin)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("Singapore Traffic Accidents Analysis"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    
                    fileInput(inputId="file",
                              label = "Upload Accidents Data .csv File"),
                    
                    fileInput(inputId="shpFile",
                              label = "Upload Road Network .shp File", accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), multiple=TRUE),
                    
                    
                    hr(),
                    selectizeInput(inputId = "analysis", 
                                   label = "Choose Analysis:", 
                                   choices = c("Kernel Density Estimation (KDE)", "K-Function", "Multitype K-Function", "Nearest Neighbour Distance"),
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
                    
                    actionButton(inputId="enter",
                                 label = "Enter"),
                    
                    
                    
                    width = 3
                    
                    
                    
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    tabsetPanel(
                      tabPanel("With Road Network Constraint",
                               plotOutput("plot"),
                               leafletOutput("map", height = "580px", width= "1050px")
                               ), 
                      tabPanel("Without Road Network Constraint")
                    )
                  )
                )
)

server <- function(input, output) {
  
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
  
  
  
  observeEvent(input$enter,
    {    
      inFile <-input$file
    if (is.null(inFile))
      return(NULL)
    
    inSHPFile <-input$shpFile
    if (is.null(inSHPFile))
      return(NULL)
    
    dir<-dirname(inSHPFile[1,4])
    for ( i in 1:nrow(inSHPFile)) {
      file.rename(inSHPFile[i,4], paste0(dir,"/",inSHPFile[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    trafficReport <- read.csv(inFile$datapath)
    trafficReport_shp <- st_as_sf(trafficReport, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")
    traffic_Report_svy21 <- st_transform(trafficReport_shp, crs = 3414)
    #st_write(traffic_Report_svy21, "temp.csv", layer_options = "GEOMETRY=AS_XY")
    temp <- read.csv("temp.csv")
    
    roadNetwork <- readShapeSpatial(getshp, CRS("+init=epsg:3414"))
    roadNetwork_psp <- as.psp(roadNetwork, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
    roadNetwork_linnet <- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
    
     if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Accidents'){
      output$plot <- renderPlot({
    accidents <- temp %>% filter(Type == "Accident")
    accidents_ppp <- ppp(accidents$X, accidents$Y, costaloutline_owin)
    accidents_lpp <- lpp(accidents_ppp, roadNetwork_linnet)
    accidentskde <- density.lpp(accidents_lpp, sigma=1)
    accidentkde_sgdf <- as.SpatialGridDataFrame.im(accidentskde)
    spplot(accidentkde_sgdf)
    # proj4string(accidentkde_sgdf) = CRS("+init=epsg:3414")
    # 
    # leaflet() %>%
    #   setView(103.8198, 1.3521,zoom = 11) %>% 
    #   addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
    #   addTiles(group = "OSM") %>% 
    #   addLayersControl(
    #     baseGroups = c( "CartoDB (default)","OSM"),
    #     options = layersControlOptions(collapsed = TRUE)
    #   ) %>%
    #   addPolylines(data=accidentkde_sgdf)
  })} else
    if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Heavy Traffic'){
      output$plot <- renderPlot({
      heavytraffic <- temp %>% filter(Type == "Heavy Traffic")
      heavytraffic_ppp <- ppp(heavytraffic$X, heavytraffic$Y, costaloutline_owin)
      heavytraffic_lpp <- lpp(heavytraffic_ppp, roadNetwork_linnet)
      heavytraffickde_sgdf <- as.SpatialGridDataFrame.im(heavytraffickde)
      spplot(heavytraffickde_sgdf)
      })
    }
  
  } 
  )
  
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

