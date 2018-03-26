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
library(rgdal)
library(RColorBrewer)
library(classInt)


costaloutline <- readOGR("CostalOutline.shp")
costaloutline_sp <- as(costaloutline, "SpatialPolygons")
costaloutline_owin <- as.owin.SpatialPolygons(costaloutline_sp)
costaloutline_mask <- as.mask(costaloutline_owin)

inFile <-NULL
inSHPFile <- NULL



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("Singapore Traffic Accidents Analysis"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    
                    conditionalPanel(condition = "input.analysis == 'Kernel Density Estimation (KDE)'",
                                     fileInput(inputId="file",
                                               label = "Upload Accidents Data .csv File"),
                                     
                                     fileInput(inputId="shpFile",
                                               label = "Upload Road Network .shp File", accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), multiple=TRUE),
                                     hr()
                                    
                                     ),
                    
                    conditionalPanel(condition = "input.analysis == 'K-Function'",
                                     fileInput(inputId="kfile",
                                               label = "Upload Accidents Data File", multiple=TRUE),
                                     
                                     fileInput(inputId="kshpFile",
                                               label = "Upload Road Network .shp File", accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), multiple=TRUE),
                                     hr()
                                     
                    ),
                    
                    
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
                    
                    conditionalPanel(condition = "input.analysis == 'K-Function'",
                                     selectizeInput(inputId = "expressway",
                                                    label = "Choose Expressway:",
                                                    choices = c("AYE", "BKE", "CTE", "ECP", "KJE", "KPE", "MCE", "PIE", "SLE","TPE"),
                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                    
                    conditionalPanel(condition = "input.analysis == 'K-Function'",
                                     sliderInput(inputId = "noOfSimulation",
                                                    label = "No. Of Simulation to Perform",
                                                    min = 3, max = 99, value = 3
                                                    )),
                    
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
                               
                               conditionalPanel(condition = "input.analysis = 'Kernel Density Estimation (KDE)'",
                                                leafletOutput("plotLeaf", height = "580px", width= "1050px")
                                                ),
                               conditionalPanel(condition = "input.analysis = 'K-Function'",
                                                plotOutput("plot")
                               )
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
  
  #KDE - Accidents file
  observeEvent(input$file, {
    inFile <-input$file
    
    trafficReport <- read.csv(inFile$datapath)
    trafficReport_shp <- st_as_sf(trafficReport, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")
    traffic_Report_svy21 <- st_transform(trafficReport_shp, crs = 3414)
    st_write(traffic_Report_svy21, "temp.csv", layer_options = "GEOMETRY=AS_XY")
    temp <<- read.csv("temp.csv")
    
    })
  
  #KDE - Road Network
  observeEvent(input$shpFile, {
    inSHPFile <-input$shpFile
    
    dir<-dirname(inSHPFile[1,4])
    for ( i in 1:nrow(inSHPFile)) {
      file.rename(inSHPFile[i,4], paste0(dir,"/",inSHPFile[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)

    roadNetwork <- readShapeSpatial(getshp, CRS("+init=epsg:3414"))
    roadNetwork_psp <- as.psp(roadNetwork, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
    roadNetwork_linnet <<- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
    })
  
  #KFunction - Accidents file
  observeEvent(input$kfile, {
    inFile <-input$kfile
    
    dir<-dirname(inFile[1,4])
    for ( i in 1:nrow(inFile)) {
      file.rename(inFile[i,4], paste0(dir,"/",inFile[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    accidents <<- readOGR(getshp)
  })
  
  #KFunction - Expressway file
  observeEvent(input$kshpFile, {
    inkSHPFile <-input$kshpFile
    
    dir<-dirname(inkSHPFile[1,4])
    for ( i in 1:nrow(inkSHPFile)) {
      file.rename(inkSHPFile[i,4], paste0(dir,"/",inkSHPFile[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    expressway <<- readShapeSpatial(getshp, CRS("+init=epsg:3414")) 
  })
  
  
  
  observeEvent(input$enter,
    { 
    #KDE Accidents
     if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Accidents'){
      output$plotLeaf <- renderLeaflet({
        
    accidents <- temp %>% filter(Type == "Accident")
    accidents_ppp <- ppp(accidents$X, accidents$Y, costaloutline_owin)
    accidents_lpp <- lpp(accidents_ppp, roadNetwork_linnet)
    accidentskde <- density.lpp(accidents_lpp, sigma=3000)
    accidentkde_sgdf <- as.SpatialGridDataFrame.im(accidentskde)
    proj4string(accidentkde_sgdf) = CRS("+init=epsg:3414")
    working_map <- tm_shape(accidentkde_sgdf) + tm_raster()
    tmap_leaflet(working_map) %>% 
      addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
      addTiles(group = "OSM") %>% 
      addLayersControl(
        baseGroups = c( "CartoDB (default)","OSM"),
        options = layersControlOptions(collapsed = TRUE)
      )
    
    #KDE Heavy Traffic
  })} else
    if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Heavy Traffic'){
      output$plotLeaf <- renderLeaflet({
        
      heavytraffic <- temp %>% filter(Type == "Heavy Traffic")
      heavytraffic_ppp <- ppp(heavytraffic$X, heavytraffic$Y, costaloutline_owin)
      heavytraffic_lpp <- lpp(heavytraffic_ppp, roadNetwork_linnet)
      heavytraffickde <- density.lpp(heavytraffic_lpp, sigma=3000)
      heavytraffickde_sgdf <- as.SpatialGridDataFrame.im(heavytraffickde)
      proj4string(heavytraffickde_sgdf) = CRS("+init=epsg:3414")
      working_map <- tm_shape(heavytraffickde_sgdf) + tm_raster()
      tmap_leaflet(working_map) %>% 
        addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
        addTiles(group = "OSM") %>% 
        addLayersControl(
          baseGroups = c( "CartoDB (default)","OSM"),
          options = layersControlOptions(collapsed = TRUE)
        )
      })
    } else
      if (input$analysis == 'K-Function' && input$kfType == 'Accidents'){
        output$plot <- renderPlot({
          
          analysis_area <- input$expressway
          simulation <- input$noOfSimulation
          
          accidents_subset <- accidents[accidents@data$Accident_L == analysis_area,]
          accidents_sp <- as(accidents_subset, 'SpatialPoints')
          accidents_ppp <- as.ppp(accidents_sp)
          
          expressway_subset <- expressway[expressway@data$ref == analysis_area,]
          expressway_subset_psp <- as.psp(expressway_subset)
          expressway_subset_linnet <- as.linnet.psp(expressway_subset_psp)
          
          accidents_lpp <- lpp(accidents_ppp, expressway_subset_linnet)

          plot(envelope.lpp(accidents_lpp,linearK, nsim = simulation, r = seq(0,10000)))
        })
      }
  
  } 
  )
  
  
  
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

