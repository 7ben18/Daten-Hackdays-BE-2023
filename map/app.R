library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
setwd("C:/Users/Student/OneDrive - Berner Fachhochschule/Desktop/Hackathon/map")
data <- read_excel("Test.xlsx")

sum_passwords <- sum(data$is_password)
sum_installation <- sum(data$is_installation)
sum_telefon <- sum(data$is_telefon)

# n_us <- nrow(subset(nasa_fireball, lat<64.9 & lat>19.5, lon<-68 & lon>-161.8))

body <- dashboardBody(
  fluidRow(
    valueBox(sum_passwords, "Anzahl Wörter Passwort", icon = icon(name = "fire")),
    valueBox(sum_installation, "Anzahl Wörter Installation", icon = icon(name = "star")),
    valueBox(sum_telefon, "Anzahl Wörter Telefon", icon = icon(name = "lightbulb-o"))
  ),
  fluidRow(
    leafletOutput("plot")
  )
)

sidebar <-dashboardSidebar(
  sliderInput("threshold", "Color Threshold", min = 0, max = 120, value = 24)
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
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng = as.numeric(data$lng),
        lat = as.numeric(data$lat),
        # label = nasa_fireball$date,
        radius = log(sum_passwords),
        weight = 2
      )
  })
  
}

shinyApp(ui, server)