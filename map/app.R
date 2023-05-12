
# Get the current directory path
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the current directory
setwd(current_dir)

# Load the data from the relative path
load("data.rda")




sum_passwords <- sum(data$is_password)
sum_installation <- sum(data$is_installation)
sum_telefon <- sum(data$is_telefon)

n_ch <- nrow(subset(data, lat<47.81 & lat>45.77 & lon<10.49 & lon>5.97))

body <- dashboardBody(
  fluidRow(
    valueBox(sum_passwords, "Anzahl Wörter Passwort", icon = icon(name = "fire")),
    valueBox(sum_installation, "Anzahl Wörter Installation", icon = icon(name = "star")),
    valueBox(sum_telefon, "Anzahl Wörter Telefon", icon = icon(name = "star"))
  ),
  fluidRow(
    leafletOutput("plot")
  )
)


sidebar <-dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "Map"),
    menuItem("Tables", tabName= "Tables")
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
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng = data$lon,
        lat = data$lat,
        label = data$Id,
        radius = log(sum_passwords),
        weight = 2,
        clusterOptions = markerClusterOptions()
      ) %>%
      setView(lng = 8.2275, lat = 46.8182, zoom = 8)
      
  })
  
}

shinyApp(ui, server)