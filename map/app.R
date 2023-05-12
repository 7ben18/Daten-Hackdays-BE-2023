library(shinydashboard)
library(dplyr)
library(leaflet)
library(ggplot2)
library(arrow) 
# Get the current directory path
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the current directory
setwd(current_dir)

# Load the data from the relative path
data <- read_parquet("../Daten/Top50Words_onehotencoded_DataFrame_with_coordinates.parquet")


#----- Generate rda
# library(readxl)
# data <- read_excel("../Daten/Test.xlsx")
# #
# # # save data as an R data file (rda)
# save(data, file = "C:/Users/Student/Hackathon/Daten-Hackdays-BE-2023/map/data.rda")


#-------



sum_applikation <- sum(data$applikation)
sum_account <- sum(data$account)
sum_entsperrung <- sum(data$entsperrung)

n_ch <- nrow(subset(data, lat<47.81 & lat>45.77 & lon<10.49 & lon>5.97))

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "Map",
            fluidRow(
              valueBox(sum_applikation, "Anzahl Wörter Passwort", icon = icon(name = "fire")),
              valueBox(sum_account, "Anzahl Wörter Installation", icon = icon(name = "star")),
              valueBox(sum_entsperrung, "Anzahl Wörter Telefon", icon = icon(name = "lightbulb-o"))
            ),
            fluidRow(
              leafletOutput("plot")
            )
    ),
    
    # Second tab content
    tabItem(tabName = "Tables",
            fluidRow(
              DT::dataTableOutput("table")
            )
    ),
    # Third tab content
    tabItem(tabName = "Charts",
            fluidRow(
              plotOutput("barplot")
            )
    )
  )
)


sidebar <-dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "Map"),
    menuItem("Tables", tabName= "Tables"),
    menuItem("Charts", tabName= "Charts")
  )
)

ui <- dashboardPage(
  dashboardHeader(),
  sidebar,
  body
)

server <- function(input, output) {
  # output$us_box <- renderValueBox({
  #   valueBox(value = n_us, 
  #            subtitle = "Number of Fireball in the Us",
  #            icon = icon("globe"),
  #            color = color <- if (n_us < input$threshold){
  #              "blue"
  #            } else {
  #              "fuchsia"
  #            })
  # })
  
  output$plot <- renderLeaflet({
    
    # places <- data %>%
    #   group_by(lat) %>%
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng = data$lon,
        lat = data$lat,
        label = data$Nummer,
        radius = log(sum_applikation),
        weight = 2,
        clusterOptions = markerClusterOptions()
      ) %>%
      setView(lng = 8.2275, lat = 46.8182, zoom = 8)
      
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data[c(1,2,5,7,9,16,19,20,21,23,24,25,26)], options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$barplot <- renderPlot({
    # create a data frame with the counts of each application
    app_counts <- data %>%
      group_by(application) %>%
      summarize(count = n()) %>%
      arrange(desc(count))

    # create the barplot using ggplot2
    ggplot(data = app_counts, aes(x = application, y = count)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate x-axis labels if needed
  })
  
  
}

shinyApp(ui, server)