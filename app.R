# Aaron Fresco
# 12-4-23
# Average Temperature per
# country over time

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)


# Main Ui Page
ui <- fluidPage(
  titlePanel("Average Temperature Over Years with Map"),
  
  mainPanel(
    leafletOutput("country", width = '700px',)
  ),
  
  sidebarLayout(
    selectInput("country", "Select a Country:",
                choices = unique(lat_lon$Area),
                selected = unique(lat_lon$Area)[1]),
    plotOutput("temperaturePlot"),
  ),
)

server <- function(input, output) {
  
  
  # Filter data only needed for temperature plot
  filtered_data <- reactive({
    filter(lat_lon, Area == input$country)
  })
  
  output$country <- renderLeaflet({
    
    map <- filtered_data()
    
    country <- leaflet()
    country <- addTiles(country)
    country <- setView(country, 
                       lng = map$Lon[1], 
                       lat = map$Lat[1], 
                       zoom = 5)
    
  })

  
  # Plots average temperate of countries
  output$temperaturePlot <- renderPlot({
    plot_data <- filtered_data()
    ggplot(plot_data, aes(x = Year, y = Average.Temperature..C)) +
      geom_line() +
      labs(title = paste("Average Temperature Over Years -", input$country),
           x = "Year", y = "Temperature (°C)")
  })
  
}

shinyApp(ui = ui, server = server)


