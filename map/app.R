library(shinydashboard)
library(dplyr)
library(leaflet)
library(ggplot2)
library(arrow) 
library(jsonlite)
library(DT)
library(shinyWidgets)
# Get the current directory path
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the current directory
setwd(current_dir)

# Load the data from the relative path
data <- read_parquet("../Daten/tickets_export_topn_words_onehotencoded_with_coordinates.parquet")

sum_applikation <- sum(data$applikation)
sum_account <- sum(data$account)
sum_entsperrung <- sum(data$entsperrung)

n_ch <- nrow(subset(data, lat<47.81 & lat>45.77 & lon<10.49 & lon>5.97))

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "Map",
            tags$style(type = "text/css", "#plot {height: calc(100vh - 80px) !important;}"),
            fluidRow(
              valueBoxOutput("box_1"),
              valueBoxOutput("box_2"),
              valueBoxOutput("box_3")
            ),
            fluidRow(
              leafletOutput("plot")
            )
    ),
    
    # Second tab content
    tabItem(tabName = "Tables",
            fluidRow(
              div(style = 'overflow-x: auto', DT::DTOutput("table"))
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
  pickerInput("serviceInput","Serviceangebot", choices=unique(data$Serviceangebot), options = list(`actions-box` = TRUE),multiple = T, selected = unique(data$Serviceangebot)),
  pickerInput("ortInput","Ort", choices=unique(data$Ort), options = list(`actions-box` = TRUE),multiple = T, selected = unique(data$Ort)),
  pickerInput("kategorieInput","Kategorie", choices=unique(data$Kategorie), options = list(`actions-box` = TRUE), multiple = T, selected = unique(data$Kategorie))
)


js_code <- '
function copyToClipboard(text) {
    var textarea = document.createElement("textarea");
    textarea.value = text;
    document.body.appendChild(textarea);
    textarea.select();
    document.execCommand("copy");
    document.body.removeChild(textarea);
    alert("Text copied to clipboard: " + text);
}'

css <- "
custom-cursor:hover {
    cursor: pointer;
}
"


ui <- dashboardPage(
  dashboardHeader(),
  sidebar,
  body,
  tags$head(
    tags$script(HTML(js_code)),
    tags$style(HTML(css))
  )
)

server <- function(input, output, session) {
  
  # BOX 1:
  
  # Define reactive inputs
  filtered_data <- reactive({
    data %>% filter(Serviceangebot %in% input$serviceInput)
  })
  
  sum_rows <- reactive({
    nrow(filtered_data())
  })
  
  # Define output
  output$box_1 <- renderValueBox({
    valueBox(sum_rows(), subtitle = "Anzahl Tickets:", icon = icon(name = "fire"), color = "blue")
  })
  
  # Box 2:
  filtered_data_2 <- reactive({
    data %>% filter(Kategorie == "Fehler", Serviceangebot %in% input$serviceInput)
  })
  
  sum_applikation_2 <- reactive({
    sum(filtered_data_2()$applikation)
  })
  
  # Define output
  output$box_2 <- renderValueBox({
    valueBox(sum_applikation_2(), subtitle = "Anzahl Fehler", icon = icon(name = "fire"), color = "blue")
  })
  
  # Box 3:
  filtered_data_3 <- reactive({
    data %>% filter(Kategorie == "Anfrage", Serviceangebot %in% input$serviceInput)
  })
  
  sum_applikation_3 <- reactive({
    sum(filtered_data_3()$applikation)
  })
  
  # Define output
  output$box_3 <- renderValueBox({
    valueBox(sum_applikation_3(), subtitle = "Anzahl Anfragen", icon = icon(name = "fire"), color = "blue")
  })
  
  # Map:
  
  output$plot <- renderLeaflet({
    data <- data %>%
      filter(Serviceangebot %in% input$serviceInput) %>%
      mutate(lat_rounded = round(lat, 2),
             lon_rounded = round(lon, 2))
    # group by lon and summarize by taking the first lat_rounded and count values
    places <- data %>%
      group_by(lon_rounded) %>%
      summarise(lat_rounded = first(lat_rounded), Ort = first(Ort),
                count = n(),
                across(33:82, sum)) %>%
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
          "<tr><td onclick='copyToClipboard(\"", Ort, "\")',id = 'custom-cursor'' style='border-bottom: 1px solid black;' colspan='2'>", Ort, "</td></tr>",
          "<tr><td>", top_5_names[5, rownum], ":</td><td>", paste0(format(top_5_values[5, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "<tr><td>", top_5_names[4, rownum], ":</td><td>", paste0(format(top_5_values[4, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "<tr><td>", top_5_names[3, rownum], ":</td><td>", paste0(format(top_5_values[3, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "<tr><td>", top_5_names[2, rownum], ":</td><td>", paste0(format(top_5_values[2, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "<tr><td>", top_5_names[1, rownum], ":</td><td>", paste0(format(top_5_values[1, rownum] / count * 100, digits = 2), "%"), "</td></tr>",
          "</table>"
        ),
        radius = ~log(count^3),
        weight = 2
      ) %>%
      setView(lng = 7.5, lat = 46.82, zoom = 8.5)
      
  })
  
  # Table:
  
  output$table <- DT::renderDT({
    data[c(1,2,5,7,9,16,19,20,21,23,24,25,26)] %>%
      filter(Serviceangebot %in% input$serviceInput, Ort %in% input$ortInput, Kategorie %in% input$kategorieInput)
  })
  
  # Chart:

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
