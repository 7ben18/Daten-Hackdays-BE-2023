library(shinydashboard)
library(dplyr)
library(leaflet)
library(ggplot2)
library(arrow) 
library(jsonlite)
library(DT)

# Get the current directory path
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the current directory
setwd(current_dir)

# Load the data from the relative path
# data <- read_parquet(file = "Daten-Hackdays-BE-2023/Daten/Top50Words_onehotencoded_DataFrame_with_coordinates.parquet")

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
              valueBox(sum_entsperrung, "Anzahl Wörter Telefon", icon = icon(name = "lightbulb"))
            ),
            fluidRow(
              leafletOutput("plot")
            )
    ),
    
    # Second tab content
    tabItem(tabName = "Tables",
            fluidRow(
              div(style = 'overflow-x: auto', DT::dataTableOutput("table"))
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
    menuItem("Charts", tabName= "Charts")),
      selectInput(
        inputId = "Serviceangebot",
        label = "Select Service",
        choices = unique(data$Serviceangebot),
        selected = "SOF: E-Mail"
      
  )
)

ui <- dashboardPage(
  dashboardHeader(),
  sidebar,
  body,
)

server <- function(input, output) {

  
  output$plot <- renderLeaflet({
    data <- data %>%
      mutate(lat_rounded = round(lat, 2),
             lon_rounded = round(lon, 2))
    # group by lon and summarize by taking the first lat_rounded and count values
    places <- data %>%
      group_by(lon_rounded) %>%
      summarise(lat_rounded = first(lat_rounded), Ort = first(Ort),
                count = n(),
                across(35:84, sum)) %>%
       ungroup()
      # slice_max(order_by = count, n = 5)
    
    places <- places %>%
      mutate(rownum = row_number())
    
    top_cols <- places %>% slice_max(order_by = count, n = 5)
    
    top_5_names <- apply(places[5:54], 1, function(x) names(x)[order(-x)][1:5])
    top_5_values <- apply(places[5:54], 1, function(x) tail(sort(x), 5))
    
    # create leaflet map with markers for each unique lon value
    leaflet(places) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon_rounded,
        lat = ~lat_rounded,
        popup = ~paste0(
          "<table>",
          "<tr><td style='border-bottom: 1px solid black;' colspan='2'><strong>Total:</strong> ", count, "</td></tr>",
          "<tr><td style='border-bottom: 1px solid black;' colspan='2'>", Ort, "</td></tr>",
          "<tr><td>", top_5_names[1, rownum], ":</td><td>", paste0(format(top_5_values[1, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "<tr><td>", top_5_names[2, rownum], ":</td><td>", paste0(format(top_5_values[2, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "<tr><td>", top_5_names[3, rownum], ":</td><td>", paste0(format(top_5_values[3, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "<tr><td>", top_5_names[4, rownum], ":</td><td>", paste0(format(top_5_values[4, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "<tr><td>", top_5_names[5, rownum], ":</td><td>", paste0(format(top_5_values[5, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "</table>"
        ),
        radius = ~log(count),
        weight = 2
      ) %>%
      setView(lng = 7.5, lat = 46.82, zoom = 8)
      
  })
  
  output$table <- DT::renderDT({
    data[1:34] %>%
      filter(Serviceangebot == input$Serviceangebot)
  })
  

  output$barplot <- renderPlot({
    # create a data frame with the counts of each application
    top_10 <- data %>%
      group_by(Serviceangebot) %>%
      summarize(Anzahl_tickets = n()) %>%
      arrange(desc(Anzahl_tickets)) %>% 
      slice(1:10) %>%
      mutate(Serviceangebot = factor(if_else(row_number() == 10, "Other", as.character(Serviceangebot)))) %>%
      group_by(Serviceangebot) %>%
      summarize(Anzahl_tickets = sum(Anzahl_tickets)) %>% 
      arrange(desc(Anzahl_tickets))  

    # create the barplot using ggplot2
    print(ggplot(data = top_10, aes(x = Serviceangebot, y = Anzahl_tickets, fill = Serviceangebot)) +
            geom_bar(stat = "identity") +
            scale_fill_brewer(palette = "Set1") +
            xlab("Serviceangebot") + 
            ylab("Number of Tickets") +
            ggtitle("Top 10 Serviceangebot with Most Tickets") +
            scale_x_discrete(labels = NULL) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))) + # rotate x-axis labels if needed
            theme_light()
  })
  
  
}

shinyApp(ui, server)
