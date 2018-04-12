#Roadrunners - Singapore Expressway Traffic Analysis

#install and load packages
packages = c('shiny', 'leaflet', 'readr', 'sf', 'spatstat', 'maptools', 'tidyverse', 'polyCub', 'rgdal', 'RColorBrewer', 'classInt', 'polyCub', 'raster', 'rgeos') 
for (p in packages){
  if(!require(p, character.only = T)){ install.packages(p)
  }
  library(p,character.only = T) }

#custom map style
dark <<- "https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoid2xnd2VlIiwiYSI6ImNqZm02dWh0bTAzbWMyd25xY3ZrdWNrb3oifQ.uLcmOKcCxHqpj6rTIbCPKw"
map_attr <<- "<a href='https://www.mapbox.com/map-feedback/'>Mapbox</a>"

costaloutline <<- readOGR("CostalOutline/CostalOutline.shp")
costaloutline_sp <- as(costaloutline, "SpatialPolygons")
costaloutline_owin <<- as.owin.SpatialPolygons(costaloutline_sp)
costaloutline_mask <- as.mask(costaloutline_owin)
costaloutline_kf <<- spTransform(costaloutline, CRS('+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs'))

speedcameras <- readOGR("Camera/cameras_combined.shp")
speedcameras_wgs84 <- spTransform(speedcameras, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
speedCameraIcon <- makeIcon(
  iconUrl = "http://flaticons.net/gd/makefg.php?i=icons/Science%20and%20Technology/CCTV-Camera.png&r=255&g=255&b=255",
  iconWidth = 25, iconHeight = 25
)

roadNetwork_ogr <- readOGR("Network/roads_expressway_no_motorwaylink.shp")
roadNetwork_wgs84 <- spTransform(roadNetwork_ogr, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
linecolor <- colorFactor(rainbow(9), roadNetwork_wgs84@data$ref)

#preloading of accidents data
trafficReport <- read.csv("Main/LTATrafficDataClean2.csv")
patterns <- c('on AYE',	'on BKE',	'on CTE'	,'on ECP',	'on KJE'	,'on KPE',	'on MCE'	,'on PIE',	'on SLE',	'on TPE')
accidents_filter <- trafficReport %>% filter(grepl(paste(patterns, collapse="|"), Descriptions)) %>% filter(Type == 'Accident')
accidents_sf <- st_as_sf(accidents_filter, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
accidents <- st_transform(accidents_sf, crs = 3414)
accidents_sp <<- as(accidents, "Spatial")
accidents_sp_wgs84 <- spTransform(accidents_sp, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
accidentsIcon <- makeIcon(
  iconUrl = "https://www.freeiconspng.com/uploads/car-icon-png-28.png",
  iconWidth = 20, iconHeight = 20
)

# preloading of heavytraffic data
heavytraffic_filter <- trafficReport %>% filter(grepl(paste(patterns, collapse="|"), Descriptions)) %>% filter(Type == 'Heavy Traffic')
heavytraffic_sf <- st_as_sf(heavytraffic_filter, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
heavytraffic <- st_transform(heavytraffic_sf, crs = 3414)
heavytraffic_sp <<- as(heavytraffic, "Spatial")
heavytraffic_sp_wgs84 <- spTransform(heavytraffic_sp, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
heavytrafficIcon <- makeIcon(
  iconUrl = "https://png.icons8.com/android/1600/traffic-jam.png",
  iconWidth = 20, iconHeight = 20
)

accidents_ppp <<- ppp(coordinates(accidents_sp)[,1], coordinates(accidents_sp)[,2], costaloutline_owin)
heavytraffic_ppp <<- ppp(coordinates(heavytraffic_sp)[,1], coordinates(heavytraffic_sp)[,2], costaloutline_owin)


speedcameras@data <- speedcameras@data[ , -(1:ncol(speedcameras@data))]
speedcameras@data[['Type']] <- 'camera'

# Multitype K Function - speed cameras vs heavy traffic
heavytraffic_sp@data <- heavytraffic_sp@data[ , -(1:ncol(heavytraffic_sp@data))]
heavytraffic_sp@data[['Type']] <- 'heavytraffic'
heavytraffic_cameras_sp <<- spRbind(speedcameras, heavytraffic_sp)


# Multitype K Function - accidents vs camera
accidents_sp@data <- accidents_sp@data[ , -(1:ncol(accidents_sp@data))]
accidents_sp@data[['Type']] <- 'accident'
accidents_cameras_sp <<- spRbind(speedcameras, accidents_sp)

#preloading of network data
roadNetwork <<- readShapeSpatial("Network/roads_expressway_no_motorwaylink.shp", CRS("+init=epsg:3414"))
roadNetwork_psp <- as.psp(roadNetwork, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
roadNetwork_linnet <<- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
roadNetwork_sp <<- spTransform(roadNetwork, CRS('+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs'))

library(leaflet)
library(readr)
library(sf)
library(spatstat)
library(maptools)
library(tidyverse)
library(polyCub)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(polyCub)
library(raster)
library(rgeos)
ui <- fluidPage(
  
  tags$head(
    tags$style(
    # Include our custom CSS
      includeCSS("styles.css")
    )
  ),
  
  navbarPage("Singapore Expressway Traffic Analysis",
             
             tabPanel("Map",
                      
                      fluidRow(    
                        column(3,
                               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                             draggable = FALSE, top = 50, left = 15, right = 10, bottom = "auto",
                                             width = 315, height = 140,
                                             checkboxInput(inputId="expresswayCheck", label="Expressway", value = FALSE, width = 2),
                                             checkboxInput(inputId="camerasCheck", label="Cameras", value = FALSE, width = 2),
                                             checkboxInput(inputId="accidentsCheck", label="Accidents", value = FALSE, width = 2),
                                             checkboxInput(inputId="heavytrafficCheck", label="Heavy Traffic", value = FALSE, width = "100%")
                               ),
      
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = 210, left = 15, right = 10, bottom = "auto",
                                    width = 315, height = "auto",
                                     
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
                                                                 min = 200, max = 6000, value = 3000
                                                     )),
                                    
                                    conditionalPanel(condition = "input.analysis == 'K-Function'",
                                                     selectizeInput(inputId = "kfType",
                                                                    label = "Choose Variable:",
                                                                    choices = c("Accidents", "Heavy Traffic"),
                                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                                    
                                    conditionalPanel(condition = "input.analysis == 'K-Function'",
                                                     sliderInput(inputId = "noOfSimulation",
                                                                 label = "No. Of Simulation to Perform",
                                                                 min = 3, max = 99, value = 3
                                                     )),
                                    
                                    conditionalPanel(condition = "input.analysis == 'Multitype K-Function'",
                                                     selectizeInput(inputId = "mkfType",
                                                                    label = "Choose Variable:",
                                                                    choices = c("Accidents - Traffic Camera", "Heavy Traffic - Traffic Camera"),
                                                                    options = list(onInitialize = I('function() { this.setValue(""); }')))),
                                    
                                    conditionalPanel(condition = "input.analysis == 'Multitype K-Function'",
                                                     sliderInput(inputId = "noOfSimulationMK",
                                                                 label = "No. Of Simulation to Perform",
                                                                 min = 3, max = 99, value = 3
                                                     )),
                                    
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
                                  
                                    br(),
                                    
                                    plotOutput(outputId = "plot")
                                    
                      )),
                      column(9, div(class="outer",
                             leafletOutput("map", width="100%", height="100%")
                             )))), 
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
                      )))))


server <- function(input, output) {
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
                   addPolylines(data = roadNetwork_wgs84, color = "Azure", popup = roadNetwork_wgs84@data$name, label = roadNetwork_wgs84@data$ref, opacity = 0.1, group = "network")
               }else
               {
                 leafletProxy("map") %>%
                   clearGroup("network")
               }
  )
  
  observeEvent(input$accidentsCheck,
               if (input$accidentsCheck){
                 leafletProxy("map") %>%
                   addTiles() %>%
                   addCircleMarkers(data = accidents_sp_wgs84, lat = accidents_sp_wgs84@coords[,2], lng = accidents_sp_wgs84@coords[,1], label = accidents_sp_wgs84$Type, popup = accidents_sp_wgs84$Descriptions, radius = 6, color = "springgreen", stroke = FALSE, fillOpacity = 0.7, group = "accidents")
               }else
               {
                 leafletProxy("map") %>%
                   clearGroup("accidents")
               }
  )
  
  observeEvent(input$heavytrafficCheck,
               if (input$heavytrafficCheck){
                 leafletProxy("map") %>%
                   addTiles() %>%
                   addCircleMarkers(data = heavytraffic_sp_wgs84, lat = heavytraffic_sp_wgs84@coords[,2], lng = heavytraffic_sp_wgs84@coords[,1], label = heavytraffic_sp_wgs84$Type, popup = heavytraffic_sp_wgs84$Descriptions, radius = 6, color = "turquoise", stroke = FALSE, fillOpacity = 0.7, group = "heavytraffic")
               }else
               {
                 leafletProxy("map") %>%
                   clearGroup("heavytraffic")
               }
  )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(103.8198, 1.3521,zoom = 12) %>%
      addTiles(urlTemplate = dark, group = "Dark", attribution = map_attr) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>% 
      addTiles(group = "OSM") %>% 
      addLayersControl(
        baseGroups = c("Dark","CartoDB", "OSM"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  

  #if accident file is uploaded, process accident file first
  observeEvent(input$accidentsFile, {
    output$dataTable <- renderTable({
      
    req(input$accidentsFile)
      
    inputAccidentsFile <-input$accidentsFile
    
    trafficReport <- read.csv(inputAccidentsFile$datapath)
    patterns <- c('on AYE',	'on BKE',	'on CTE'	,'on ECP',	'on KJE'	,'on KPE',	'on MCE'	,'on PIE',	'on SLE',	'on TPE')
    accidents_filter <- trafficReport %>% filter(grepl(paste(patterns, collapse="|"), Descriptions)) %>% filter(Type == 'Accident')
    accidents_sf <- st_as_sf(accidents_filter, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    accidents <- st_transform(accidents_sf, crs = 3414)
    accidents_sp <<- as(accidents, "Spatial")
    accidents_sp_wgs84 <<- spTransform(accidents_sp, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    accidentsIcon <<- makeIcon(
      iconUrl = "https://www.freeiconspng.com/uploads/car-icon-png-28.png",
      iconWidth = 20, iconHeight = 20)
      
    heavytraffic_filter <- trafficReport %>% filter(grepl(paste(patterns, collapse="|"), Descriptions)) %>% filter(Type == 'Heavy Traffic')
    heavytraffic_sf <- st_as_sf(heavytraffic_filter, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    heavytraffic <- st_transform(heavytraffic_sf, crs = 3414)
    heavytraffic_sp <<- as(heavytraffic, "Spatial")
    heavytraffic_sp_wgs84 <<- spTransform(heavytraffic_sp, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    heavytrafficIcon <<- makeIcon(
      iconUrl = "https://png.icons8.com/android/1600/traffic-jam.png",
      iconWidth = 20, iconHeight = 20
    )
    
    accidents_ppp <<- ppp(coordinates(accidents_sp)[,1], coordinates(accidents_sp)[,2], costaloutline_owin)
    heavytraffic_ppp <<- ppp(coordinates(heavytraffic_sp)[,1], coordinates(heavytraffic_sp)[,2], costaloutline_owin)
    
    # Multitype K Function - speed cameras vs heavy traffic
    heavytraffic_sp@data <- heavytraffic_sp@data[ , -(1:ncol(heavytraffic_sp@data))]
    heavytraffic_sp@data[['Type']] <- 'heavytraffic'
    heavytraffic_cameras_sp <<- spRbind(speedcameras, heavytraffic_sp)
    
    
    # Multitype K Function - accidents vs camera
    accidents_sp@data <- accidents_sp@data[ , -(1:ncol(accidents_sp@data))]
    accidents_sp@data[['Type']] <- 'accident'
    accidents_cameras_sp <<- spRbind(speedcameras, accidents_sp)
    
    
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

                   #constraint accident
                   accidents_lpp <- lpp(accidents_ppp, roadNetwork_linnet)
                   accidentskde <- density.lpp(accidents_lpp, sigma)
                   accidentkde_sgdf <- as.SpatialGridDataFrame.im(accidentskde)
                   accidentkde_raster <- raster(accidentkde_sgdf)
                   accidentkde_raster_scaled <- disaggregate(accidentkde_raster, fact=4 ,fun=mean)
                   proj4string(accidentkde_sgdf) = CRS("+init=epsg:3414")
                   proj4string(accidentkde_raster_scaled) = CRS("+init=epsg:3414")
                   accidentkde_raster_scaled_adjusted <- setValues(accidentkde_raster_scaled, getValues(accidentkde_raster_scaled)*1000000)
                  
                   summarykde.values <- quantile(na.omit(getValues(accidentkde_raster_scaled_adjusted)), seq(0,1,0.2))
                   at <- c(summarykde.values[1], summarykde.values[2], summarykde.values[3], summarykde.values[4], summarykde.values[5],summarykde.values[6] )
                   cb <- colorBin(palette = "YlOrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
                   
                   leafletProxy("map") %>%
                     removeControl("leg") %>%
                     clearGroup("Traffic with Constraint") %>%
                     clearGroup("Accidents without Constraint") %>%
                     clearGroup("Traffic without Constraint") %>%
                     clearGroup("Accidents with Constraint") %>%
                     addRasterImage(accidentkde_raster_scaled_adjusted, colors=cb, group='Accidents with Constraint', opacity=1.0) %>%
                     addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=6),layerId="leg") %>%
                     addLayersControl(
                       baseGroups = c("Dark","CartoDB", "OSM"),
                       overlayGroups = c('Accidents with Constraint'),
                       options = layersControlOptions(collapsed = TRUE)
                     )
                   
                 
                 } else
                   if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Heavy Traffic'){
                     
                       sigma <- input$sigma
                      
                       #constraint traffic
                       heavytraffic_lpp <- lpp(heavytraffic_ppp, roadNetwork_linnet)
                       heavytraffickde <- density.lpp(heavytraffic_lpp, sigma)
                       heavytraffickde_sgdf <- as.SpatialGridDataFrame.im(heavytraffickde)
                       heavytraffickde_raster <- raster(heavytraffickde_sgdf)
                       heavytraffickde_raster_scaled <- disaggregate(heavytraffickde_raster, fact=4 ,fun=mean)
                       proj4string(heavytraffickde_raster_scaled) = CRS("+init=epsg:3414")
                       heavytraffickde_raster_scaled_adjusted <- setValues(heavytraffickde_raster_scaled, getValues(heavytraffickde_raster_scaled)*1000000)
                       
                       summarykdeht.values <- quantile(na.omit(getValues(heavytraffickde_raster_scaled_adjusted)), seq(0,1,0.2))
                       at <- c(summarykdeht.values[1], summarykdeht.values[2], summarykdeht.values[3], summarykdeht.values[4], summarykdeht.values[5], summarykdeht.values[6] )
                       cb <- colorBin(palette = "YlOrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
                       
                       leafletProxy("map") %>%
                         removeControl("leg") %>%
                         clearGroup("Accidents without Constraint") %>%
                         clearGroup("Traffic without Constraint") %>%
                         clearGroup("Accidents with Constraint") %>%
                         clearGroup("Traffic with Constraint") %>%
                         addRasterImage(heavytraffickde_raster_scaled_adjusted, colors=cb, group="Traffic with Constraint",  opacity=1.0) %>%
                         addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=6),layerId="leg") %>%
                         addLayersControl(
                           baseGroups = c("Dark","CartoDB", "OSM"),
                           overlayGroups = c("Traffic with Constraint"),
                           options = layersControlOptions(collapsed = TRUE)
                         )
              
                      
                   }
               
               })
  
  observeEvent(input$kdeNoConstraint,
               { 
                 if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Accidents'){
                   
                   sigma <- input$sigma
                   
                   #non-constraint accident
                   accidentskde_ppp <- density.ppp(accidents_ppp, sigma)
                   accidentkde_ppp_sgdf <- as.SpatialGridDataFrame.im(accidentskde_ppp)
                   accidentkde_ppp_raster <- raster(accidentkde_ppp_sgdf)
                   proj4string(accidentkde_ppp_raster) = CRS("+init=epsg:3414")
                   accidentkde_ppp_sgdf_adjusted <- setValues(accidentkde_ppp_raster, getValues(accidentkde_ppp_raster)*1000000)
                   
                   summarykdeppp.values <- quantile(na.omit(getValues(accidentkde_ppp_sgdf_adjusted)), seq(0,1,0.2))
                   at <- c(summarykdeppp.values[1], summarykdeppp.values[2], summarykdeppp.values[3], summarykdeppp.values[4], summarykdeppp.values[5], summarykdeppp.values[6])
                   cb <- colorBin(palette = "YlOrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
                   
                   leafletProxy("map") %>%
                     removeControl("leg") %>%
                     clearGroup("Accidents with Constraint") %>%
                     clearGroup("Traffic with Constraint") %>%
                     clearGroup("Traffic without Constraint") %>%
                     clearGroup("Accidents without Constraint") %>%
                     addRasterImage(accidentkde_ppp_sgdf_adjusted, group="Accidents without Constraint", colors=cb, opacity=1.0) %>%
                     addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=8),layerId="leg") %>%
                     addLayersControl(
                       baseGroups = c("Dark","CartoDB", "OSM"),
                       overlayGroups = c("Accidents without Constraint"),
                       options = layersControlOptions(collapsed = TRUE)
                     )
                   
                 } else
                   if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Heavy Traffic'){
                     
                     sigma <- input$sigma
                     
                     #non-constraint traffic
                     heavytraffickde_ppp <- density.ppp(heavytraffic_ppp, sigma)
                     heavytraffickde_ppp_sgdf <- as.SpatialGridDataFrame.im(heavytraffickde_ppp)
                     heavytraffickde_ppp_raster <- raster(heavytraffickde_ppp_sgdf)
                     proj4string(heavytraffickde_ppp_raster) = CRS("+init=epsg:3414")
                     heavytraffickde_ppp_raster_adjusted <- setValues(heavytraffickde_ppp_raster, getValues(heavytraffickde_ppp_raster)*1000000)
                     
                     summarykdepppht.values <- quantile(na.omit(getValues(heavytraffickde_ppp_raster_adjusted)), seq(0,1,0.2))
                     at <- c(summarykdepppht.values[1], summarykdepppht.values[2], summarykdepppht.values[3], summarykdepppht.values[4], summarykdepppht.values[5], summarykdepppht.values[6])
                     cb <- colorBin(palette = "YlOrRd", bins = at, domain = at, na.color = "#00000000", reverse=FALSE)
                     
                     leafletProxy("map") %>%
                       removeControl("leg") %>%
                       clearGroup("Accidents with Constraint") %>%
                       clearGroup("Traffic with Constraint") %>%
                       clearGroup("Accidents without Constraint") %>%
                       clearGroup("Traffic without Constraint") %>%
                       addRasterImage(heavytraffickde_ppp_raster_adjusted, group="Traffic without Constraint",colors=cb, opacity=1.0) %>%
                       addLegend(pal = cb, values = at, title = "Density Function", position='bottomleft', labFormat = labelFormat(digits=8),layerId="leg") %>%
                       addLayersControl(
                         baseGroups = c("Dark","CartoDB", "OSM"),
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
      
      # Run K function
      simulation <- isolate({input$noOfSimulation})
      lnrk <- envelope.lpp(accidents_lpp,linearK, simulation)
      plot(lnrk, . - r ~ r, xlab="d", ylab="K(d)-r",main = 'K Function with Linear Constraints')
      
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
            
            # Run Linear K
            simulation <- isolate({input$noOfSimulation})
            lnrk <- envelope.lpp(heavytraffic_lpp,linearK, simulation)
            plot(lnrk, . - r ~ r, xlab="d", ylab="K(d)-r",main = 'K Function with Linear Constraints')
          })
          
        }
    
  })
  
  observeEvent(input$multiKEnter, {

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
      
      
      #finding intersection between road network and bbox
      roadNetwork_intersect <- gIntersection(roadNetwork_sp, bbox) 
      
      
      # simplify road network
      roadNetwork_psp <- as.psp(roadNetwork_intersect, window=NULL, marks=NULL, check=spatstat.options("checksegments"), fatal=TRUE)
      roadNetwork_linnet <- as.linnet.psp(roadNetwork_psp, sparse=TRUE)
      
      
      if (input$mkfType == 'Heavy Traffic - Traffic Camera'){
      output$plot <-renderPlot({
      # Create heavytraffic_cameras ppp
      heavytraffic_cameras_ppp <- as.ppp(heavytraffic_cameras_sp)
      marks(heavytraffic_cameras_ppp) <- heavytraffic_cameras_sp@data['Type']
      
      # Extract heavytraffic_cameras in bbox
      bbox_owin <- as(bbox, 'owin')
      Window(heavytraffic_cameras_ppp) <- bbox_owin
      
      # Create heavytraffic_cameras_lpp
      heavytraffic_cameras_lpp <- lpp(heavytraffic_cameras_ppp, roadNetwork_linnet)
      
      #Multitype K Function
      simulation <- isolate({input$noOfSimulationMK})
      lnrkcross <- envelope.lpp(heavytraffic_cameras_lpp,linearKcross, simulation, i = 'camera', j = 'heavytraffic')
      plot(lnrkcross, . - r ~ r, xlab="d", ylab="K(d)-r",main = 'Multitype K Function with Linear Constraints')
      })
      } else 
        if (input$mkfType == 'Accidents - Traffic Camera'){
          output$plot <-renderPlot({  
          # Create heavytraffic_cameras ppp
          accidents_cameras_ppp <- as.ppp(accidents_cameras_sp)
          marks(accidents_cameras_ppp) <- accidents_cameras_sp@data['Type']
          
          # Extract heavytraffic_cameras in bbox
          bbox_owin <- as(bbox, 'owin')
          Window(accidents_cameras_ppp) <- bbox_owin
          
          # Create heavytraffic_cameras_lpp
          accidents_cameras_lpp <- lpp(accidents_cameras_ppp, roadNetwork_linnet)
        
          #Multitype K Function
          simulation <- isolate({input$noOfSimulationMK})
          lnrkcross <- envelope.lpp(accidents_cameras_lpp,linearKcross, simulation, i = 'camera', j = 'accident')
          plot(lnrkcross, . - r ~ r, xlab="d", ylab="K(d)-r",main = 'Multitype K Function with Linear Constraints')
          })
        }
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

