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
library(rgeos)

costaloutline <<- readOGR("CostalOutline/CostalOutline.shp")
costaloutline_sp <- as(costaloutline, "SpatialPolygons")
costaloutline_owin <<- as.owin.SpatialPolygons(costaloutline_sp)
costaloutline_mask <- as.mask(costaloutline_owin)
costaloutline_kf <<- spTransform(costaloutline, CRS('+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs'))

speedcameras <- readOGR("Camera/cameras_combined.shp")
speedcameras_wgs84 <- spTransform(speedcameras, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
speedCameraIcon <- makeIcon(
  iconUrl = "https://lh3.googleusercontent.com/sLX-R-LLdMwtsFJXlYgW9g8CiGv4ogDm7fjmv_aK4818Mc1vxJVOPDJbagWt1zBxNQ=w300",
  iconWidth = 38, iconHeight = 38
)

roadNetwork_ogr <- readOGR("Network/roads_expressway.shp")
roadNetwork_wgs84 <- spTransform(roadNetwork_ogr, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
linecolor <- colorFactor(rainbow(9), roadNetwork_wgs84@data$ref)

#preloading of accidents data
trafficReport <- read.csv("Main/LTATrafficDataClean2.csv")
patterns <- c('AYE',	'BKE',	'CTE'	,'ECP',	'KJE'	,'KPE',	'MCE'	,'PIE',	'SLE',	'TPE')
accidents_filter <- trafficReport %>% filter(grepl(paste(patterns, collapse="|"), Descriptions)) %>% filter(Type == 'Accident')
accidents_sf <- st_as_sf(accidents_filter, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
accidents <- st_transform(accidents_sf, crs = 3414)
accidents_sp <<- as(accidents, "Spatial")

heavytraffic_filter <- trafficReport %>% filter(grepl(paste(patterns, collapse="|"), Descriptions)) %>% filter(Type == 'Heavy Traffic')
heavytraffic_sf <- st_as_sf(heavytraffic_filter, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
heavytraffic <- st_transform(heavytraffic_sf, crs = 3414)
heavytraffic_sp <<- as(heavytraffic, "Spatial")

accidents_ppp <<- ppp(coordinates(accidents_sp)[,1], coordinates(accidents_sp)[,2], costaloutline_owin)
heavytraffic_ppp <<- ppp(coordinates(heavytraffic_sp)[,1], coordinates(heavytraffic_sp)[,2], costaloutline_owin)

speedcameras@data <- speedcameras@data[ , -(1:ncol(speedcameras@data))]
speedcameras@data[['Type']] <- 'camera'
heavytraffic_sp <- as(heavytraffic, 'Spatial')
heavytraffic_sp@data <- heavytraffic_sp@data[ , -(1:ncol(heavytraffic_sp@data))]
heavytraffic_sp@data[['Type']] <- 'heavytraffic'
heavytraffic_cameras_sp <<- spRbind(speedcameras, heavytraffic_sp)

#preloading of network data
roadNetwork <<- readShapeSpatial("Network/roads_expressway.shp", CRS("+init=epsg:3414"))
roadNetwork_psp <- as.psp(roadNetwork, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
roadNetwork_linnet <<- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
roadNetwork_sp <<- spTransform(roadNetwork, CRS('+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs'))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(
    # Include our custom CSS
      includeCSS("styles.css")
    )
  ),
  
  #Navbar

  navbarPage(strong("Singapore Traffic Analysis"),
             
             tabPanel("Map",
                      
                      fluidRow(    
                        column(3,
                               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                             draggable = FALSE, top = 50, left = 15, right = 10, bottom = "auto",
                                             width = 315, height = 70,
                                             checkboxInput(inputId="expresswayCheck", label="Expressway", value = FALSE, width = 2),
                                             checkboxInput(inputId="camerasCheck", label="Cameras", value = FALSE, width = 2)
                               ),
      
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = 150, left = 15, right = 10, bottom = "auto",
                                    width = 315, height = "auto",
                                  
                                    #uiOutput("bounds"), 
                                    br(), 
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
                                                                 label = "Kernel distance (m):",
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
                                                                    choices = c("Traffic Cameras"),
                                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                                    
                                    conditionalPanel(condition = "input.analysis == 'Multitype K-Function' && input.mkfType == 'Heavy Traffic'",
                                                     selectizeInput(inputId = "trafficMKFunction",
                                                                    label = "Choose 2nd Variable:",
                                                                    choices = c("Traffic Cameras"),
                                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                                    
                                    conditionalPanel(condition="input.analysis == 'Multitype K-Function'", align = "center", 
                                                     actionButton(inputId="multiKEnter",
                                                                  label = "Enter")),
                                                     
                                    conditionalPanel(condition="input.analysis == 'Kernel Density Estimation (KDE)'", align = "center",
                                                     div(class="center",
                                                      actionButton(inputId="kdeConstraint",
                                                                  label = "With Network Constraint")),
                                                     br(),
                                                     actionButton(inputId="kdeNoConstraint",
                                                                  label = "Without Network Constraint")
                                                     ),
                                    
                                    conditionalPanel(condition="input.analysis == 'K-Function'", align = "center", 
                                                     actionButton(inputId="kfunctionEnter",
                                                                  label = "Enter")
                                    ),
                                  
                                    
                                    plotOutput(outputId = "plot")
                                    
                      )

                      ),
                      column(9, div(class="outer",
                             leafletOutput("map", width="100%", height="100%")
                             ))          
                      ) #fluidRow
                      #div class 
                      ), #end of map tabPanel
             tabPanel("Upload Data",
                      fluidRow(    
                        column(3,
  
                               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                             draggable = FALSE, top = 50, left = 15, right = 10, bottom = "auto",
                                             width = 315, height = "auto",
                                    
                                    h2("Upload Data Files"),
                                    
                                
                               fileInput(inputId="accidentsFile",
                                         label = "Upload Traffic Data", 
                                         accept=c('.csv'), 
                                         buttonLabel=icon("upload")),

                               fileInput(inputId="networkFile",
                                         label = "Upload Road Network Data", 
                                         accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), 
                                         multiple=TRUE,

                                         buttonLabel=icon("upload"))

                        
                      )),
                      
                      column(9, div(class="outer",tableOutput("dataTable"))
                      )
                      ) #end of upload data tabPanel
             )
  
))


server <- function(input, output) {
  #at <- seq(0, 0.0030, 0.0005)
  #cb <<- colorBin(palette = "YlGnBu", bins = at, domain = at, na.color = "#00000000")
  
  observeEvent(input$camerasCheck,
               if (input$camerasCheck){
                 leafletProxy("map") %>%
                   addTiles() %>%
                   addMarkers(data = speedcameras_wgs84, lat = speedcameras_wgs84@coords[,2], lng = speedcameras_wgs84@coords[,1], label = speedcameras_wgs84$ROAD_NAME, popup = speedcameras_wgs84$CameraType, icon = speedCameraIcon, group = "camera")
               }else
               {
                 leafletProxy("map") %>%
                   clearGroup("camera")
               }
  )
  
  observeEvent(input$expresswayCheck,
               if (input$expresswayCheck){
                 leafletProxy("map") %>%
                   addTiles() %>%
                   addPolylines(data = roadNetwork_wgs84, color = linecolor(roadNetwork_wgs84@data$ref), popup = roadNetwork_wgs84@data$name, label = roadNetwork_wgs84@data$ref, opacity = 0.3, group = "network")
               }else
               {
                 leafletProxy("map") %>%
                   clearGroup("network")
               }
  )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(103.8198, 1.3521,zoom = 12) %>% 
      #addTiles() %>%
      #addMarkers(data = speedcameras_wgs84, lat = speedcameras_wgs84@coords[,2], lng = speedcameras_wgs84@coords[,1], label = speedcameras_wgs84$ROAD_NAME, popup = speedcameras_wgs84$CameraType, icon = speedCameraIcon) %>%
      #addTiles() %>%
      #addPolylines(data = roadNetwork_wgs84, color = linecolor(roadNetwork_wgs84@data$ref), popup = roadNetwork_wgs84@data$name, label = roadNetwork_wgs84@data$ref, opacity = 0.3) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
      addTiles(group = "OSM") %>% 
      addLayersControl(
        baseGroups = c( "CartoDB (default)","OSM"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  

  #if accident file is uploaded, process accident file first
  observeEvent(input$accidentsFile, {
    output$dataTable <- renderTable({
      
    req(input$accidentsFile)
      
    inputAccidentsFile <-input$accidentsFile
    trafficReport <- read.csv(inputAccidentsFile$datapath)
    
    trafficReport <- read.csv(inputAccidentsFile$datapath)
    patterns <- c('AYE',	'BKE',	'CTE'	,'ECP',	'KJE'	,'KPE',	'MCE'	,'PIE',	'SLE',	'TPE')
    accidents_filter <- trafficReport %>% filter(grepl(paste(patterns, collapse="|"), Descriptions)) %>% filter(Type == 'Accident')
    accidents_sf <- st_as_sf(accidents_filter, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    accidents <- st_transform(accidents_sf, crs = 3414)
    accidents_sp <<- as(accidents, "Spatial")
    
    #trafficReport_shp <- st_as_sf(trafficReport, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")
    #traffic_Report_svy21 <- st_transform(trafficReport_shp, crs = 3414)
    #accidents <- traffic_Report_svy21 %>% filter(Type == "Accident")
    
    #heavytraffic <- traffic_Report_svy21 %>% filter(Type == "Heavy Traffic")
    #heavytraffic_sp <- as(heavytraffic, "Spatial")
    
    heavytraffic_filter <- trafficReport %>% filter(grepl(paste(patterns, collapse="|"), Descriptions)) %>% filter(Type == 'Heavy Traffic')
    heavytraffic_sf <- st_as_sf(heavytraffic_filter, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    heavytraffic <- st_transform(heavytraffic_sf, crs = 3414)
    heavytraffic_sp <<- as(heavytraffic, "Spatial")
    
    accidents_ppp <<- ppp(coordinates(accidents_sp)[,1], coordinates(accidents_sp)[,2], costaloutline_owin)
    heavytraffic_ppp <<- ppp(coordinates(heavytraffic_sp)[,1], coordinates(heavytraffic_sp)[,2], costaloutline_owin)
    
    return (trafficReport)
    })
  })
  
  
  #if road network file is uploaded, process road network file first
  observeEvent(input$networkFile, {
    inputNetworkFile <-input$networkFile
    
    dir<-dirname(inputNetworkFile[1,4])
    for ( i in 1:nrow(inputNetworkFile)) {
      file.rename(inputNetworkFile[i,4], paste0(dir,"/",inputNetworkFile[i,1]))}
    
    getnetworkshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    roadNetwork <<- readShapeSpatial(getnetworkshp, CRS("+init=epsg:3414"))
    roadNetwork_psp <- as.psp(roadNetwork, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
    roadNetwork_linnet <<- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
    roadNetwork_sp <<- spTransform(roadNetwork, CRS('+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs'))
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
                     addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=6),layerId="leg") %>%
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
                         addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=6),layerId="leg") %>%
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
                     addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=8),layerId="leg") %>%
                     
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
                       addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=8),layerId="leg") %>%
                       addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
                       addTiles(group = "OSM") %>% 
                       addLayersControl(
                         baseGroups = c( "CartoDB (default)","OSM"),
                         overlayGroups = c("Traffic without Constraint"),
                         options = layersControlOptions(collapsed = TRUE)
                       )

                     
                   }
                 
               })
  
  observeEvent(input$kfunctionEnter, {
    north <- as.numeric(input$map_bounds["north"]) #long_max
    south <- as.numeric(input$map_bounds["south"]) #long_min 
    east <- as.numeric(input$map_bounds["east"]) #lat_max
    west <- as.numeric(input$map_bounds["west"]) #lat_min
    
    bbox <- rbind(c(east, north), c(west, north), c(west, south), c(east, south), c(east, north))	
    colnames(bbox) <- c('long','lat')
    bbox <- as.data.frame(bbox)
    bbox <- Polygon(bbox)
    bbox <- Polygons(list(bbox), 'bbox')
    bbox <- SpatialPolygons(list(bbox))
    proj4string(bbox) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
    bbox <- spTransform(bbox , CRS('+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs'))
    
    if (input$kfType == 'Accidents'){
      
      output$plot <- renderPlot({
      accidents_intersect <- gIntersection(accidents_sp, bbox)
      #finding intersection between road network and bbox
      roadNetwork_intersect <- gIntersection(roadNetwork_sp, bbox) 
      
      #finding intersection between coastal outline and bbox
      costaloutline_intersect <- gIntersection(costaloutline_kf, bbox) 
      costaloutline_sp_k <- as(costaloutline_intersect, "SpatialPolygons")
      costaloutline_owin_k <- as.owin.SpatialPolygons(costaloutline_sp_k)
      
      # create ppp
      accidents_ppp <- ppp(accidents_intersect@coords[,1], accidents_intersect@coords[,2], costaloutline_owin_k)
      
      # simplify road network
      roadNetwork_psp <- as.psp(roadNetwork_intersect, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
      roadNetwork_linnet <- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
      
      # Create lpp
      accidents_lpp <- lpp(accidents_ppp, roadNetwork_linnet)
      simulation <- isolate({input$noOfSimulation})
      plot(envelope.lpp(accidents_lpp,linearK, simulation, r=seq(0,10000)))
      
      })
      
      } else
        if (input$kfType == 'Heavy Traffic'){
          output$plot <- renderPlot({
            heavytraffic_intersect <- gIntersection(heavytraffic_sp, bbox)
            #finding intersection between road network and bbox
            roadNetwork_intersect <- gIntersection(roadNetwork_sp, bbox) 
            
            #finding intersection between coastal outline and bbox
            costaloutline_intersect <- gIntersection(costaloutline_kf, bbox) 
            costaloutline_sp_k <- as(costaloutline_intersect, "SpatialPolygons")
            costaloutline_owin_k <- as.owin.SpatialPolygons(costaloutline_sp_k)
            
            # create ppp
            heavytraffic_ppp <- ppp(heavytraffic_intersect@coords[,1], heavytraffic_intersect@coords[,2], costaloutline_owin_k)
            
            # simplify road network
            roadNetwork_psp <- as.psp(roadNetwork_intersect, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
            roadNetwork_linnet <- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
            
            # Create lpp
            heavytraffic_lpp <- lpp(heavytraffic_ppp, roadNetwork_linnet)
            simulation <- isolate({input$noOfSimulation})
            
            plot(envelope.lpp(heavytraffic_lpp,linearK, simulation, r=seq(0,10000)))
          })
          
        }
    
  })
  
  observeEvent(input$multiKEnter, {
    output$plot <- renderPlot({
      # Create heavytraffic_cameras ppp
      heavytraffic_cameras_ppp <- as.ppp(heavytraffic_cameras_sp)
      marks(heavytraffic_cameras_ppp) <- heavytraffic_cameras_sp@data['Type']
      
      # Extract heavytraffic_cameras in bbox
      bbox_owin <- as(bbox, 'owin')
      Window(heavytraffic_cameras_ppp) <- bbox_owin
      
      # Create heavytraffic_cameras_lpp
      heavytraffic_cameras_lpp <- lpp(heavytraffic_cameras_ppp, roadNetwork_linnet)
      
      #Multitype K Function
      plot(envelope.lpp(heavytraffic_cameras_lpp,linearKcross, nsim = 3, i = 'camera', j = 'heavytraffic')) 
    })
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

