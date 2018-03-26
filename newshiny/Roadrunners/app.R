#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
library(polyCub)
library(raster)


costaloutline <- readOGR("CostalOutline/CostalOutline.shp")
costaloutline_sp <- as(costaloutline, "SpatialPolygons")
costaloutline_owin <<- as.owin.SpatialPolygons(costaloutline_sp)
costaloutline_mask <- as.mask(costaloutline_owin)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(
    # Include our custom CSS
      includeCSS("styles.css")
    )
  ),
  
  #Navbar

  navbarPage(strong("Singapore Traffic Accidents Analysis"),
             
             tabPanel("Map",
                      div(class="outer",
                          
                      leafletOutput("map", width="100%", height="100%"),
                      
                      
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 180, left = "auto", right = 10, bottom = "auto",
                                    width = 330, height = "auto",
                                    
                                    h2("Choose Analysis"),
                                    
                                    conditionalPanel(condition = "input.accidentsFile == null",
                                                     selectizeInput(inputId="analysis",
                                                                    label="Choose Analysis:",
                                                                    choices=c("Kernel Density Estimation (KDE)", "K-Function", "Multitype K-Function"),
                                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                                    
                                    conditionalPanel(condition = "input.analysis == 'Kernel Density Estimation (KDE)'",
                                                     selectizeInput(inputId = "kdeType",
                                                                    label = "Choose Variable:",
                                                                    choices = c("Accidents", "Heavy Traffic"),
                                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                                    
                                    conditionalPanel(condition = "input.analysis == 'Kernel Density Estimation (KDE)'",
                                                     sliderInput(inputId = "sigma",
                                                                 label = "Choose Sigma:",
                                                                 min = 1000, max = 6000, value = 3000
                                                     )),
                                    
                                    conditionalPanel(condition = "input.analysis == 'K-Function'",
                                                     selectizeInput(inputId = "kfType",
                                                                    label = "Choose Variable:",
                                                                    choices = c("Accidents", "Heavy Traffic"),
                                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                                    
                                    # conditionalPanel(condition = "input.analysis == 'K-Function'",
                                    #                  selectizeInput(inputId = "expressway",
                                    #                                 label = "Choose Expressway:",
                                    #                                 choices = c("AYE", "BKE", "CTE", "ECP", "KJE", "KPE", "MCE", "PIE", "SLE","TPE"),
                                    #                                 options = list(onInitialize = I('function() { this.setValue(""); }')))),
                                    
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
                                    
                                    conditionalPanel(condition="input.analysis == 'Kernel Density Estimation (KDE)'",
                                                     actionButton(inputId="kdeConstraint",
                                                                  label = "With Network Constraint"),
                                                     actionButton(inputId="kdeNoConstraint",
                                                                  label = "Without Network Constraint")
                                                     )
                                    
                                    
                      )            
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                      ) #end of absolutePanel

                      
                      ), #end of map tabPanel
             tabPanel("Upload Data",
                      absolutePanel(id = "uploadControls", class = "panel panel-default", fixed = TRUE,
                                    top = 60, left = 20, right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    
                                    h2("Upload Data Files"),
                                    
                                
                               fileInput(inputId="accidentsFile",
                                         label = "Upload Accidents Data", 
                                         accept=c('.csv'), 
                                         buttonLabel=icon("upload")),

                               fileInput(inputId="networkFile",
                                         label = "Upload Road Network Data", 
                                         accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), 
                                         multiple=TRUE,

                                         buttonLabel=icon("upload"))

                        
                      )
                      ) #end of upload data tabPanel
             )
  
)


server <- function(input, output) {
  #at <- seq(0, 0.0030, 0.0005)
  #cb <<- colorBin(palette = "YlGnBu", bins = at, domain = at, na.color = "#00000000")
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(103.8198, 1.3521,zoom = 12) %>% 
      addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
      #addLegend(pal = cb, values = at, title = "Legend", position='bottomleft') %>%
      addTiles(group = "OSM") %>% 
      addLayersControl(
        baseGroups = c( "CartoDB (default)","OSM"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  

  #if accident file is uploaded, process accident file first
  observeEvent(input$accidentsFile, {
    inputAccidentsFile <-input$accidentsFile
    
    trafficReport <- read.csv(inputAccidentsFile$datapath)
    trafficReport_shp <- st_as_sf(trafficReport, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")
    traffic_Report_svy21 <- st_transform(trafficReport_shp, crs = 3414)

    accidents <- traffic_Report_svy21 %>% filter(Type == "Accident")
    accidents_sp <- as(accidents, "Spatial")
    
    heavytraffic <- traffic_Report_svy21 %>% filter(Type == "Heavy Traffic")
    heavytraffic_sp <- as(heavytraffic, "Spatial")
    
    accidents_ppp <<- ppp(coordinates(accidents_sp)[,1], coordinates(accidents_sp)[,2], costaloutline_owin)
    heavytraffic_ppp <<- ppp(coordinates(heavytraffic_sp)[,1], coordinates(heavytraffic_sp)[,2], costaloutline_owin)
    
  })
  
  
  #if road network file is uploaded, process road network file first
  observeEvent(input$networkFile, {
    inputNetworkFile <-input$networkFile
    
    dir<-dirname(inputNetworkFile[1,4])
    for ( i in 1:nrow(inputNetworkFile)) {
      file.rename(inputNetworkFile[i,4], paste0(dir,"/",inputNetworkFile[i,1]))}
    
    getnetworkshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    roadNetwork <- readShapeSpatial(getnetworkshp, CRS("+init=epsg:3414"))
    roadNetwork_psp <- as.psp(roadNetwork, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
    roadNetwork_linnet <<- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
    
  })
  
  observeEvent(input$kdeConstraint,
               { 
                 if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Accidents'){
                 
                   sigma <- input$sigma
                   at <- seq(0, 0.0020, 0.00025)
                   cb <- colorBin(palette = "OrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
                   

                   
                   #constraint accident
                   accidents_lpp <- lpp(accidents_ppp, roadNetwork_linnet)
                   accidentskde <- density.lpp(accidents_lpp, sigma)
                   accidentkde_sgdf <- as.SpatialGridDataFrame.im(accidentskde)
                   accidentkde_raster <- raster(accidentkde_sgdf)
                   accidentkde_raster_scaled <- disaggregate(accidentkde_raster, fact=4 ,fun=mean)
                   proj4string(accidentkde_sgdf) = CRS("+init=epsg:3414")
                   proj4string(accidentkde_raster_scaled) = CRS("+init=epsg:3414")
                
                   
                   leafletProxy("map") %>%
                     removeControl("leg") %>%
                     clearGroup("Traffic with Constraint") %>%
                     clearGroup("Accidents without Constraint") %>%
                     clearGroup("Traffic without Constraint") %>%
                     clearGroup("Accidents with Constraint") %>%
                     addRasterImage(accidentkde_raster_scaled, colors=cb, group='Accidents with Constraint', opacity=0.80) %>%
                     addLegend(pal = cb, values = at, title = "Legend", position='bottomleft', labFormat = labelFormat(digits=6),layerId="leg") %>%
                     addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
                     addTiles(group = "OSM") %>% 
                     addLayersControl(
                       baseGroups = c( "CartoDB (default)","OSM"),
                       overlayGroups = c('Accidents with Constraint'),
                       options = layersControlOptions(collapsed = TRUE)
                     )
                   
                 
                 } else
                   if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Heavy Traffic'){
                     
                       sigma <- input$sigma
                       at <- seq(0, 0.0040, 0.0005)
                       cb <- colorBin(palette = "OrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
                       
                       #constraint traffic
                       heavytraffic_lpp <- lpp(heavytraffic_ppp, roadNetwork_linnet)
                       heavytraffickde <- density.lpp(heavytraffic_lpp, sigma)
                       heavytraffickde_sgdf <- as.SpatialGridDataFrame.im(heavytraffickde)
                       heavytraffickde_raster <- raster(heavytraffickde_sgdf)
                       heavytraffickde_raster_scaled <- disaggregate(heavytraffickde_raster, fact=4 ,fun=mean)
                       proj4string(heavytraffickde_raster_scaled) = CRS("+init=epsg:3414")
                       
                       leafletProxy("map") %>%
                         removeControl("leg") %>%
                         clearGroup("Accidents without Constraint") %>%
                         clearGroup("Traffic without Constraint") %>%
                         clearGroup("Accidents with Constraint") %>%
                         clearGroup("Traffic with Constraint") %>%
                         addRasterImage(heavytraffickde_raster_scaled, colors=cb, group="Traffic with Constraint",  opacity=0.80) %>%
                         addLegend(pal = cb, values = at, title = "Legend", position='bottomleft', labFormat = labelFormat(digits=6),layerId="leg") %>%
                         addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
                         addTiles(group = "OSM") %>% 
                         addLayersControl(
                           baseGroups = c( "CartoDB (default)","OSM"),
                           overlayGroups = c("Traffic with Constraint"),
                           options = layersControlOptions(collapsed = TRUE)
                         )
              
                      
                   }
               
               })
  
  observeEvent(input$kdeNoConstraint,
               { 
                 if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Accidents'){
                   
                   sigma <- input$sigma
                   at <- seq(0, 0.0000030, 0.0000005)
                   cb <- colorBin(palette = "OrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
                   
                   #non-constraint accident
                   accidentskde_ppp <- density.ppp(accidents_ppp, sigma)
                   accidentkde_ppp_sgdf <- as.SpatialGridDataFrame.im(accidentskde_ppp)
                   accidentkde_ppp_raster <- raster(accidentkde_ppp_sgdf)
                   proj4string(accidentkde_ppp_raster) = CRS("+init=epsg:3414")
                   
                   leafletProxy("map") %>%
                     removeControl("leg") %>%
                     clearGroup("Accidents with Constraint") %>%
                     clearGroup("Traffic with Constraint") %>%
                     clearGroup("Traffic without Constraint") %>%
                     clearGroup("Accidents without Constraint") %>%
                     addRasterImage(accidentkde_ppp_raster, group="Accidents without Constraint", colors=cb, opacity=0.50) %>%
                     addLegend(pal = cb, values = at, title = "Legend", position='bottomleft', labFormat = labelFormat(digits=8),layerId="leg") %>%
                     
                     addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
                     addTiles(group = "OSM") %>% 
                     addLayersControl(
                       baseGroups = c( "CartoDB (default)","OSM"),
                       overlayGroups = c("Accidents without Constraint"),
                       options = layersControlOptions(collapsed = TRUE)
                     )
                   
                 } else
                   if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Heavy Traffic'){
                     
                     sigma <- input$sigma
                     at <- seq(0, 0.000030, 0.000005)
                     cb <- colorBin(palette = "OrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
                     
                     
                     #non-constraint traffic
                     heavytraffickde_ppp <- density.ppp(heavytraffic_ppp, sigma)
                     heavytraffickde_ppp_sgdf <- as.SpatialGridDataFrame.im(heavytraffickde_ppp)
                     heavytraffickde_ppp_raster <- raster(heavytraffickde_ppp_sgdf)
                     proj4string(heavytraffickde_ppp_raster) = CRS("+init=epsg:3414")
                     
                     leafletProxy("map") %>%
                       removeControl("leg") %>%
                       clearGroup("Accidents with Constraint") %>%
                       clearGroup("Traffic with Constraint") %>%
                       clearGroup("Accidents without Constraint") %>%
                       clearGroup("Traffic without Constraint") %>%
                       addRasterImage(heavytraffickde_ppp_raster, group="Traffic without Constraint",colors=cb, opacity=0.50) %>%
                       addLegend(pal = cb, values = at, title = "Legend", position='bottomleft', labFormat = labelFormat(digits=8),layerId="leg") %>%
                       addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
                       addTiles(group = "OSM") %>% 
                       addLayersControl(
                         baseGroups = c( "CartoDB (default)","OSM"),
                         overlayGroups = c("Traffic without Constraint"),
                         options = layersControlOptions(collapsed = TRUE)
                       )

                     
                   }
                 
               })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

