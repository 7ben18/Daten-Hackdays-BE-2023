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
    data <- data %>%
      mutate(lat_rounded = round(lat, 2),
             lon_rounded = round(lon, 2))
    # group by lon and summarize by taking the first lat_rounded and count values
    places <- data %>%
      group_by(lon_rounded) %>%
      summarise(lat_rounded = first(lat_rounded),
                count = n(),
                across(35:84, sum)) %>%
       ungroup()
      # slice_max(order_by = count, n = 5)
    
    places <- places %>%
      mutate(rownum = row_number())
    
    top_cols <- places %>% slice_max(order_by = count, n = 5)
    
    top_5_names <- apply(places[4:53], 1, function(x) names(x)[order(-x)][1:5])
    top_5_values <- apply(places[4:53], 1, function(x) tail(sort(x), 5))
    
    # create leaflet map with markers for each unique lon value
    leaflet(places) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon_rounded,
        lat = ~lat_rounded,
        popup = ~paste0(
          "<table>",
          "<tr><td>Total:</td><td>", count, "</td></tr>",
          "<tr><td>1st:</td><td>", top_5_names[1, rownum], ":</td><td>", as.character(top_5_values[1, rownum]), "</td></tr>",
          "<tr><td>2nd:</td><td>", top_5_names[2, rownum], ":</td><td>", as.character(top_5_values[2, rownum]), "</td></tr>",
          "<tr><td>3rd:</td><td>", top_5_names[3, rownum], ":</td><td>", as.character(top_5_values[3, rownum]), "</td></tr>",
          "<tr><td>4th:</td><td>", top_5_names[4, rownum], ":</td><td>", as.character(top_5_values[4, rownum]), "</td></tr>",
          "<tr><td>5th:</td><td>", top_5_names[5, rownum], ":</td><td>", as.character(top_5_values[5, rownum]), "</td></tr>",
          "</table>"
        ),
        radius = ~log(count),
        weight = 2
      ) %>%
      setView(lng = 8.5, lat = 46.75, zoom = 8)
      
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