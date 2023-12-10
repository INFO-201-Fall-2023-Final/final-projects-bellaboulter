# Aaron Fresco
# 12-4-23
# Average Temperature per
# country over time

library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)

about_view <- fluidPage(
  h1("Climate Change Analysis"),
  h2("(Emissions, Temperatures, and Food Production/Consumption)"),
  p("
We have chosen to analyze data about climate change around the world. There are many different factors when it comes to climate change, but we have decided to mostly focus on countries’ emissions and rising temperatures. We are also curious how climate change relates to food production, so we have also analyzed data about several countries’ food production and food consumption.
"),
  p("We want our project to be up to interpretation, and we want to avoid making assumptions or drawing absolute conclusions, as we are fully aware that we have not examined every factor that contributes to the complicated topic of climate change. "),
  p("However, our project has revealed interesting observations. 
Our overall goal is to spark contemplation and conversation. 
So as you explore our data, we ask:
What patterns do you observe? What countries seem to be major outliers? How have emissions and temperatures changed in the last 30 years? How do food production and food consumption relate to each other and to climate change?
"),
  p("We hope this global climate change analysis was thought provoking and interesting! 
")
)

dina_page <- fluidPage(
  titlePanel("Food Production vs Consumption"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Country", "Select Country:",
                  choices = unique(CombinedDataFrame$Area)),
      hr(),
      helpText("Select a country to contrast food production and consumption.")
    ),
    mainPanel(
      plotOutput("production_vs_consumption"),
      tableOutput("summary_table")
    )
  )
)


bella_analysis_view <- fluidPage(
  titlePanel("Exploring Global Emissions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(average_emmisions$Area)),
      p("What countries seem to be striving for change?"),
      p("What could be the cause of dramatic drops or rises in emissions?")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("All Countries (1990-2020)", plotOutput("all_countries"), 
                 h3("Do you see the sudden drop in Brazil’s emissions?"),
                 p("“The changes in the Brazilian Amazon in the past decade, and the contribution that they have made to slow global warming, are unprecedented” (Boucher, 2014)."),
                 p("“Essentially all of the reduction in land use change emis- sions came from the decrease in deforestation in the Amazon, the world’s largest tropical forest” (Boucher, 2014)."),
                 p("Read more about this below. It’s crazy how important forests are in battling climate change!"),
                 tags$a(href = "https://www.jstor.org/stable/pdf/resrep00076.7.pdf", "Click here to read more!"),
                 p(" "),
                 h3("Why are China’s emissions so high?"),
                 p("“Between 1980 and 2017, China’s economy grew at breakneck speed, lifting millions out of poverty. Fossil fuels fueled much of China’s economic growth. As a result, China is now one of the world’s largest economies and the world’s largest emitter of carbon dioxide” (Cropper, 2020)."),
                 p("“China is responsible for 28% of the world’s annual carbon emissions” (Cropper, 2020)."),
                 tags$a(href = "https://www.jstor.org/stable/resrep25621#:~:text=To%20meet%20that%20demand%2C%20China,the%20world%27s%20largest%20carbon%20emitter.", "Click here to read more!")
        ),
        tabPanel("Selected Country (1990-2020)", plotOutput("plot1990_2020_line"), plotOutput("plot1990_2020_box")),
        tabPanel("Selected Country (1990-2000)", plotOutput("plot1990_2000_line"), plotOutput("plot1990_2000_box")),
        tabPanel("Selected Country (2000-2010)", plotOutput("plot2000_2010_line"), plotOutput("plot2000_2010_box")),
        tabPanel("Selected Country (2010-2020)", plotOutput("plot2010_2020_line"), plotOutput("plot2010_2020_box"))
      )
    )
  )
)

# Main Ui Page
aaron_page <- fluidPage(
  titlePanel("Average Temperature Over Years with Map"),
  
  mainPanel(
    leafletOutput("country", width = '700px',)
  ),
  
  sidebarLayout(
    selectInput("country", "Select a Country:",
                choices = unique(lat_lon$Area),
                selected = unique(lat_lon$Area)[1]),
    plotOutput("temperaturePlot"),
  )
)

# Navbar of shiny app
ui <- navbarPage(
  "Climate Change Analysis (Emissions, Temperatures, and Food Production/Consumption)",
  tabPanel("About Our Project", about_view),
  tabPanel("Global Emissions", bella_analysis_view),
  tabPanel("Analysis View", aaron_page),
  tabPanel("Food Production / Food Consumption", dina_page)
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    filter(average_emmisions, Area %in% input$country)
  })
  
  output$all_countries <- renderPlot({
    plot <- ggplot(average_emmisions, aes(x = Year, y = Avg_Emissions, color = Area, label = Area)) +
      geom_point(aes(color = Area)) +
      geom_line() +
      labs(title = "Average Emissions for All Countries 1990-2020",
           x = "Decade",
           y = "Average Emissions")
    return(plot)
  })
  
  output$plot1990_2020_line <- renderPlot({
    plot_decade_line("1990-2020")
  })
  
  output$plot1990_2000_line <- renderPlot({
    plot_decade_line("1990-2000")
  })
  
  output$plot2000_2010_line <- renderPlot({
    plot_decade_line("2000-2010")
  })
  
  output$plot2010_2020_line <- renderPlot({
    plot_decade_line("2010-2020")
  })
  
  output$plot1990_2020_box <- renderPlot({
    plot_decade_box("1990-2020")
  })
  
  output$plot1990_2000_box <- renderPlot({
    plot_decade_box("1990-2000")
  })
  
  output$plot2000_2010_box <- renderPlot({
    plot_decade_box("2000-2010")
  })
  
  output$plot2010_2020_box <- renderPlot({
    plot_decade_box("2010-2020")
  })
  
  plot_decade_line <- function(decade) {
    data <- filtered_data()
    data <- filter(data, Year >= as.numeric(substr(decade, 1, 4)) & Year <= as.numeric(substr(decade, 6, 9)))
    data <- group_by(data, Area, Year) 
    data <- summarise(data, Avg_Emissions = mean(Avg_Emissions))
    
    ggplot(data = data, aes(x = Year, y = Avg_Emissions, color = Area, label = Area)) +
      geom_point() +
      geom_line() +
      labs(title = paste("Selected Country's Average Emissions from", decade),
           x = "Year",
           y = "Total Emissions")
  }
  
  plot_decade_box <- function(decade) {
    data <- filtered_data()
    data <- filter(data, Year >= as.numeric(substr(decade, 1, 4)) & Year <= as.numeric(substr(decade, 6, 9)))
    
    ggplot(data = data, aes(x = Area, y = Avg_Emissions, fill = Area)) +
      geom_boxplot() +
      labs(title = paste("Box Plot of Selected Country's Emissions from", decade),
           x = "Country",
           y = "Average Emissions")
  }
  
  
  # Filter data only needed for temperature plot
  filtered_aaron_data <- reactive({
    filter(lat_lon, Area == input$country)
  })
  
  output$country <- renderLeaflet({
    
    map <- filtered_aaron_data()
    
    country <- leaflet()
    country <- addTiles(country)
    country <- setView(country, 
                       lng = map$Lon[1], 
                       lat = map$Lat[1], 
                       zoom = 5)
    
  })

  
  # Plots average temperate of countries
  output$temperaturePlot <- renderPlot({
    plot_data <- filtered_aaron_data()
    ggplot(plot_data, aes(x = Year, y = Average.Temperature..C)) +
      geom_line() +
      labs(title = paste("Average Temperature Over Years -", input$country),
           x = "Year", y = "Temperature (°C)")
  })
  
  selected_country_data <- reactive({
    filter(CombinedDataFrame, Area == input$country)
  })
  
  output$summary_table <- renderTable({
    summarise(selected_country_data(),
              Total_Production = sum(WH_2000 + RI_2000 + MZ_2000),
              Total_Consumption = sum(Food.Household.Consumption))
  })
  
  output$production_vs_consumption <- renderPlot({
    dina_data <- selected_country_data()
    total_prod <- dina_data$WH_2000[1] + dina_data$RI_2000[1] + dina_data$MZ_2000[1]
    total_cons <- dina_data$Food.Household.Consumption[1]
    
    ggplot(dina_data, aes(x = Year)) +
      geom_line(aes(y = total_prod, color = "blue", linetype = "solid")) +
      geom_line(aes(y = total_cons, color = "red", linetype = "dashed")) + 
      labs(title = paste("Food Production vs Consumption in", input$country),
           x = "Year",
           y = "Amount")
  })
  
}

shinyApp(ui = ui, server = server)


