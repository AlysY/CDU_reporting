#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(ggplot2)         # to make plots
library(dplyr)           # to clean up code and allow piping %>%
library(leaflet)         # for interactive maps
library(shinythemes)     # for the theme


#' Set the initial value of a shiny widget to be blank with a placeholder message
#'
#' @param variable A character string for the variable being Options in the widget. This will appear in the placeholder as "please select variable"
#'
#' @return list which is used in the options argument of a shiny widget
#'
#' @examples
#' start_empty("a region")
#' selectInput(inputId = "region",
#' label = "Select a region:",
#' choices = c("Melbourne", "Geelong", "Sydney"),
#' options = start_empty("antibiotic") )
start_empty <- function(variable){
  list(
    placeholder = paste('Please select', variable),
    onInitialize = I('function() { this.setValue(""); }')
  )
}


# input <- list()
# output <- list()
# input$researchersName <- "Sam Banks"

# library(rsconnect)
# deployApp()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage(title = "RIEL projects", # title for the title bar, could add the logo or symbols
             id = "nav",
             Options = NULL,
             collapsible = TRUE, 
             theme = shinytheme("paper"),
             #tags$style(type='text/css', '.navbar {font-size: 13px;}'),
             
             tabPanel("Upload a project", icon = icon("file-arrow-up"), # Name on the navigation bar
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          h3('Find your project'),
                          
                          selectizeInput("researchersName",
                                         "Select a researcher:",
                                         choices = c("A", "B"),
                                         options = start_empty("researcher's name")
                          ),
                          textOutput("text_peoplesNames"),
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.researchersName != '' ", 
                            selectizeInput("claimProject",
                                           "Select a project:",
                                           choices = c("A", "B"),
                                           options = start_empty("researcher's name"))
                            
                          )
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          h4(uiOutput("name_title")),
                          # textOutput("name_title"),
                          DT::dataTableOutput("table_projectsOptions"),
                          
                          br(),
                          h4(uiOutput("Options_title")),
                          uiOutput("Options_lead"),
                          uiOutput("Options_researchers"),
                          uiOutput("Options_location"),
                          br(),
                          h4("Place a point for the location of the project:"),
                          p("Zoom in to your project location and double click to add a point."),
                          leafletOutput("map_selectLocation", height = 300),
                          actionButton("addLocation", "Confirm project location"),
                          br(),
                          br()
                        )
                      )
             ),
             tabPanel("View project",
                      icon = icon("map"), 
                      h4("A map to show the projects"),
                      br(),
                      leafletOutput("map_projects", height = 600)
             )
                      
             
  )
)

# Define server logic required to draw a histogram
server <- function(input,output, session){
  
  data_projectList <- reactive({
    # data_projectList <- read.csv("Data/ProjectList.csv")
    
    # The data 
    data <- read.csv("Data/ProjectList.csv")
    
  })
  
  list_leadNames <- reactive({
    
    # list_leadNames <- stringr::str_split(data_projectList$leadInvestigator, pattern = ", ")
    
    data <- data_projectList()
    lead_ls <- stringr::str_split(data$leadInvestigator,
                                  pattern = ", ")
    return(lead_ls)
    

  })
  
  list_researcherNames <- reactive({

    # list_researcherNames <- stringr::str_split(data_projectList$researchers, pattern = ", ")
    
    data <- data_projectList()
    researchers_ls <- stringr::str_split(data$researchers,
                                         pattern = ", ")
    
    return(researchers_ls)

  })
  
  
  data_peoplesNames <- reactive({
    
    # leadInvestigator <- list_leadNames %>% unlist %>% unique
    # researchers <- list_researcherNames %>% unlist %>% unique
    # people <- c(leadInvestigator, researchers) %>% unique %>% sort
    # data_peoplesNames <- people[people != ""]
    
    # The data 
    lead_ls <- list_leadNames()
    researchers_ls <- list_researcherNames()
    
    leadInvestigator <- lead_ls %>% unlist %>% unique
    researchers <- researchers_ls %>% unlist %>% unique
    
    people <- c(leadInvestigator, researchers) %>% unique %>% sort
    people <- people[people != ""]
    return(people)
    
  })
  
  
  text_peoplesNames <- renderText({
    
    # leadInvestigator <- list_leadNames %>% unlist %>% unique
    # researchers <- list_researcherNames %>% unlist %>% unique
    # people <- c(leadInvestigator, researchers) %>% unique %>% sort
    # data_peoplesNames <- people[people != ""]
    
    # The data 
    lead_ls <- list_leadNames()
    researchers_ls <- list_researcherNames()
    
    leadInvestigator <- lead_ls %>% unlist %>% unique
    researchers <- researchers_ls %>% unlist %>% unique
    
    people <- c(leadInvestigator, researchers) %>% unique %>% sort
    people <- people[people != ""]
    return(people)
    
  })
  
  
  
  
  data_projectsOptions <- reactive({
    
    # name_lead <- sapply(list_leadNames, function(e) is.element(input$researchersName, e))
    # name_researcher <- sapply(list_researcherNames, function(e) is.element(input$researchersName, e))
    # projects_logical <- ifelse(name_lead == TRUE | name_researcher == TRUE, TRUE, FALSE)
    # data_projectsOptions <- data_projectList[projects_logical,]
    
    
    # The data 
    lead_ls <- list_leadNames()
    researchers_ls <- list_researcherNames()
    data_projectList <- data_projectList()
    
    name_lead <- sapply(lead_ls, function(e) is.element(input$researchersName, e))
    name_researcher <- sapply(researchers_ls, function(e) is.element(input$researchersName, e))
    projects_logical <- ifelse(name_lead == TRUE | name_researcher == TRUE, TRUE, FALSE)
    
    projects_Options <- data_projectList[projects_logical,]
  })
  
  
  
  
  
  
  
  
  
  
  observe({
    
    # Data
    data_peoplesNames <- data_peoplesNames() # filtered by onset, sample_type, organism
    
    # Update inputs
    updateSelectizeInput(session,
                         inputId = "researchersName",
                         label = "Select a researcher:",
                         choices = data_peoplesNames,
                         options = start_empty("researcher's name")
    ) 
    
  })
  
  
  
  
  output$name_title <-  renderUI({
    
    input$researchersName
    
    isolate({
      # Required inputs
      req( input$researchersName)
      text <- paste("Projects involving:", input$researchersName)
    })
    
    return(text)
  })
  
  
  
  
  observe({
    
    # Required inputs
    req( input$researchersName)
    
    # Data
    data_projectsOptions <- data_projectsOptions()
    
    # Update inputs
    updateSelectizeInput(session,
                         inputId = "claimProject",
                         label = "Select a project:",
                         choices = data_projectsOptions$Title,
                         options = start_empty("researcher's name")
    ) 
    
  })
  
  
  
  
  
  
  
  output$table_projectsOptions <- DT::renderDataTable({

    req( input$researchersName)
    input$researchersName

    data_projectsOptions <- data_projectsOptions()

    isolate({
      if(nrow(data_projectsOptions) == 0){ # if the data is empty

        # data_null is an empty dataframe
        table_projectsOptions <-  DT::datatable(data = data.frame(message = "No data Options"))

      } else if(nrow(data_projectsOptions) > 0 ){ # if the data is not empty

        table_projectsOptions <-  DT::datatable(data = data_projectsOptions)
      }
    })

    return(table_projectsOptions)
  })

  
  
  

  data_projectSelected <- reactive({
    data_projectsSelected <- data_projectsOptions() %>% filter(Title == input$claimProject)
  })
  
  
  
  output$Options_title <-  renderUI({
    
    input$claimProject
    
    isolate({
      
      # Required inputs
      req(input$claimProject)
      
      # data
      data_projectSelected <- data_projectSelected()
      
      # output
      text <- paste("Project title:", data_projectSelected$Title)
    })
    
    return(text)
  })
  
  
  output$Options_lead <-  renderUI({
    
    input$claimProject
    
    isolate({
      
      # Required inputs
      req(input$claimProject)
      
      # data
      data_projectSelected <- data_projectSelected()
      
      # output
      text <- paste("Project lead:", data_projectSelected$leadInvestigator)
    })
    
    return(text)
  })
  
  
  output$Options_researchers <-  renderUI({
    
    input$claimProject
    
    isolate({
      
      # Required inputs
      req(input$claimProject)
      
      # data
      data_projectSelected <- data_projectSelected()
      
      # output
      text <- paste("Project researchers:", data_projectSelected$researchers)
    })
    
    return(text)
  })
  
  
  output$Options_location <-  renderUI({
    
    input$claimProject
    
    isolate({
      
      # Required inputs
      req(input$claimProject)
      
      # data
      data_projectSelected <- data_projectSelected()
      
      # output
      text <- paste("Project location:", data_projectSelected$locationDescription)
    })
    
    return(text)
  })
  
  
  
  
  
  output$map_selectLocation <- renderLeaflet({

    leaflet() %>% # create a leaflet map
      fitBounds(lng1 = 114, lat1 = -40, lng2 = 150, lat2 =  -12) %>% # the starting position of the map
      addTiles() # The background map

  })
  
  output$map_projects <- renderLeaflet({
    
    leaflet() %>% # create a leaflet map
      fitBounds(lng1 = 114, lat1 = -40, lng2 = 150, lat2 =  -12) %>% # the starting position of the map
      addTiles() # The background map
    
  })
  
  observeEvent(input$map_selectLocation_click, {
    
    click <- input$map_selectLocation_click
    leafletProxy("map_selectLocation") %>% 
      clearGroup("new_point") %>%
      addMarkers(click$lng, click$lat, group = "new_point")
      #addCircles(click$lng, click$lat, radius = 10, color = "red", group = "new_point")
    
})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
