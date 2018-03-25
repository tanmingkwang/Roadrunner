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
    # Include our custom CSS
    includeCSS("styles.css")
  ),
  
  #Navbar
  navbarPage("Singapore Traffic Accidents Analysis",
             
             tabPanel("Map",
                      
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
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
                                    
                                    actionButton(inputId="enter",
                                                 label = "Enter")
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                      ), #end of absolutePanel
                      
                      fluidRow(
                        column(width=6, leafletOutput("plotLeaflet", height = "580px", width= "700px")),
                        column(width=6, leafletOutput("plotLeafletH", height = "580px", width= "700px"))
                         )
                      
                      ), #end of map tabPanel
             tabPanel("Upload Data",
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = 20, right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    
                                    h2("Upload Data Files"),
                                    
                                
                               fileInput(inputId="accidentsFile",
                                         label = "Upload Accidents Data", 
                                         accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), 
                                         multiple=TRUE, 
                                         buttonLabel=icon("upload")),
                               fileInput(inputId="trafficFile",
                                         label = "Upload Traffic Data", 
                                         accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), 
                                         multiple=TRUE,

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
  
  #if accident file is uploaded, process accident file first
  observeEvent(input$accidentsFile, {
    inputAccidentsFile <-input$accidentsFile
    
    dir<-dirname(inputAccidentsFile[1,4])
    for ( i in 1:nrow(inputAccidentsFile)) {
      file.rename(inputAccidentsFile[i,4], paste0(dir,"/",inputAccidentsFile[i,1]))}
    
    getaccidentsshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    accidents <- readOGR(getaccidentsshp)
    accidents_sp <- as(accidents, 'SpatialPoints')
    accidents_ppp <<- ppp(coordinates(accidents_sp)[,1], coordinates(accidents_sp)[,2], costaloutline_owin)
    
  })
  
  #if traffic file is uploaded, process traffic file first
  observeEvent(input$trafficFile, {
    inputTrafficFile <-input$trafficFile
    
    dir<-dirname(inputTrafficFile[1,4])
    for ( i in 1:nrow(inputTrafficFile)) {
      file.rename(inputTrafficFile[i,4], paste0(dir,"/",inputTrafficFile[i,1]))}
    
    gettrafficshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    
    heavytraffic <- readOGR(gettrafficshp)
    heavytraffic_sp <- as(heavytraffic, 'SpatialPoints')
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
  
  observeEvent(input$enter,
               { 
                 if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Accidents'){
                 output$plotLeaflet <- renderLeaflet({
                   sigma <- input$sigma
                   
                   #constraint
                   accidents_lpp <- lpp(accidents_ppp, roadNetwork_linnet)
                   accidentskde <- density.lpp(accidents_lpp, sigma)
                   accidentkde_sgdf <- as.SpatialGridDataFrame.im(accidentskde)
                   accidentkde_raster <- raster(accidentkde_sgdf)
                   accidentkde_raster_scaled <- disaggregate(accidentkde_raster, fact=4 ,fun=mean)
                   proj4string(accidentkde_sgdf) = CRS("+init=epsg:3414")
                   proj4string(accidentkde_raster_scaled) = CRS("+init=epsg:3414")
                   
                   #non-constraint
                   accidentskde_ppp <- density.ppp(accidents_ppp, sigma)
                   accidentkde_ppp_sgdf <- as.SpatialGridDataFrame.im(accidentskde_ppp)
                   proj4string(accidentkde_ppp_sgdf) = CRS("+init=epsg:3414")
                   
                   working_map <- tm_shape(accidentkde_raster_scaled, name = "With Constraint", group="With Constraint") + tm_raster() + tm_shape(accidentkde_ppp_sgdf, name = "Without Constraint", group="Without Constraint") + tm_raster()
                   tmap_leaflet(working_map) %>% 
                     addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
                     addTiles(group = "OSM") %>% 
                     addLayersControl(
                       baseGroups = c( "CartoDB (default)","OSM"),
                      overlayGroups = c("With Constraint", "Without Constraint"),
                       options = layersControlOptions(collapsed = TRUE)
                     ) 
                 })
                 } else
                   if (input$analysis == 'Kernel Density Estimation (KDE)' && input$kdeType == 'Heavy Traffic'){
                     output$plotLeafletH <- renderLeaflet({
                       sigma <- input$sigma
                       heavytraffic_lpp <- lpp(heavytraffic_ppp, roadNetwork_linnet)
                       heavytraffickde <- density.lpp(heavytraffic_lpp, sigma=3000)
                       heavytraffickde_ppp <- density.ppp(heavytraffic_ppp, sigma=500)
                       heavytraffickde_sgdf <- as.SpatialGridDataFrame.im(heavytraffickde)
                       heavytraffickde_ppp_sgdf <- as.SpatialGridDataFrame.im(heavytraffickde_ppp)
                       proj4string(heavytraffickde_ppp_sgdf) = CRS("+init=epsg:3414")

                       
                       working_map <- tm_shape(heavytraffickde_ppp_sgdf) + tm_raster()
                       tmap_leaflet(working_map) %>% 
                         addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
                         addTiles(group = "OSM") %>% 
                         addLayersControl(
                           baseGroups = c( "CartoDB (default)","OSM"),
                           options = layersControlOptions(collapsed = TRUE)
                         )
                     }) 
                   }
               
               })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

