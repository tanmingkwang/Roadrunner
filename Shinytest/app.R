library(shiny)
library(shinythemes)
library(leaflet)
library(readr)
library(sf)


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
                               tableOutput("contents"),
                               leafletOutput("shp", height = "580px", width= "1050px")
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
  
  map = leaflet() %>% 
    addProviderTiles("CartoDB.Positron", group = "CartoDB (default)") %>% 
    addTiles(group = "OSM") %>% 
    setView(103.8198, 1.3521,zoom = 11) %>% 
    addLayersControl(
      baseGroups = c( "CartoDB (default)","OSM"),
      options = layersControlOptions(collapsed = TRUE)
    ) 
  
  output$shp <- renderLeaflet({
    map
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

