library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(leaflet)

bella_df <- read.csv("average_emmisions.csv")
aaron_df <- read.csv("lat_lon.csv")
dina_df <-read.csv("CombinedDataFrame.csv")

about_view <- fluidPage(
  h1("Climate Change Analysis"),
  h2("(Emissions, Temperatures, and Food Consumption)"),
  p("We have chosen to analyze data about climate change around the world. There are many different factors when it comes to climate change, but we have decided to mostly focus on global emissions, rising temperatures, and food consumption."),
  p("We want to avoid making assumptions or drawing absolute conclusions, as we are fully aware that we have not examined every factor that contributes to the complicated topic of climate change. "),
  p("However, our project has revealed interesting observations that we have included on each page."),   
  p("Our overall goal is to spark contemplation and conversation."),
  p("So as you explore our data, we ask:"),
  p("What patterns do you observe? What countries seem to be major outliers? How have emissions and temperatures changed in the last 30 years? How do these subjects relate to each other and climate change as a whole?"),
  p("We hope this global climate change analysis was thought provoking and interesting!")
)

# Bella's UI
bella_page <- fluidPage(
  titlePanel("Exploring Global Emissions"),
  fluidRow(
    column(12,
           wellPanel(
             p("These graphs show the total greenhouse gas emissions from various sources."),
             p("What countries seem to be striving for change?"),
             p("What could be the cause of dramatic drops or rises in emissions?")
           )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("BellaCountry", "Select Country:", choices = unique(bella_df$Area)),
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

# Aaron's UI
aaron_page <- fluidPage(
  titlePanel("Temperatures Over 30 Years with Interactive Map"),
  fluidRow(
    column(12,
           wellPanel(
             p("These graphs show the rising temperatures of each country?"),
             p("What areas on the map seem to experience rising temperatures the most?"),
             p("We noticed that South America has experienced a significant rise in temperatures."),
             p("Might this be related to the deforestation of the Amazon that we explored on the previous page?"),
             p("Clearly trees have a big impact on both carbon emissions and temperatures!"),
             tags$a(href = "https://time.com/6213444/how-do-trees-affect-climate-change/", "Click here to read more about trees!")
           )
    )
  ),
  
  mainPanel(
    leafletOutput("AaronCountry", width = '700px')
  ),
  
  sidebarLayout(
      sidebarPanel(
        selectInput("AaronCountry", "Select a Country:",
                  choices = unique(aaron_df$Area),
                  selected = unique(aaron_df$Area)[1])
      ),
    plotOutput("temperaturePlot"),
  )
)

# Dina's UI
dina_page <- fluidPage(
  titlePanel("Exploring Food Consumption"),
  fluidRow(
    column(12,
           wellPanel(
             p("These graphs show the emissions from food consumption at the household level.")
           )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("DinaCountry", "Select Country:",
                  choices = unique(dina_df$Area))
    ),
    mainPanel(
      plotOutput("all_countries_food_consumption"),
      p("Recall the graph from the the Global Emissions of these same countries."),
      p("Notice how China is a major outlier again, but other outliers such as Indonesia and Brazil are not present on this graph."),
      p("This means that food consumption is a huge factor in China's total emissions, whereas Indonesia and Brazil might have other major causes."),
      tags$a(href = "https://www.jstor.org/stable/resrep25621#:~:text=To%20meet%20that%20demand%2C%20China,the%20world%27s%20largest%20carbon%20emitter.", "Click here to read about why China's food consumption emissions are so high."),
      p(" "),
      plotOutput("food_consumption_plot")
    )
  )
)

# Main UI
ui <- navbarPage(
  "Climate Change Analysis (Emissions, Temperatures, and Food Consumption)",
  tabPanel("About Our Project", about_view),
  tabPanel("Global Emissions", bella_page),
  tabPanel("Global Temperatures", aaron_page),
  tabPanel("Global Food Consumption", dina_page)
)

server <- function(input, output) {
  
  # Bella's server 
  filtered_data <- reactive({
    filter(bella_df, Area %in% input$BellaCountry)
  })
  
  output$all_countries <- renderPlot({
    plot <- ggplot(bella_df, aes(x = Year, y = Avg_Emissions, color = Area, label = Area)) +
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
      labs(title = paste(input$BellaCountry, "'s Average Emissions from", decade),
           x = "Year",
           y = "Total Emissions")
  }
  
  plot_decade_box <- function(decade) {
    data <- filtered_data()
    data <- filter(data, Year >= as.numeric(substr(decade, 1, 4)) & Year <= as.numeric(substr(decade, 6, 9)))
    
    ggplot(data = data, aes(x = Area, y = Avg_Emissions, fill = Area)) +
      geom_boxplot() +
      labs(title = paste("Box Plot of", input$BellaCountry, "'s Emissions from", decade),
           x = "Country",
           y = "Average Emissions")
  }
  
  # Aaron's server 
  filtered_aaron_data <- reactive({
    filter(aaron_df, Area == input$AaronCountry)
  })
  
  output$AaronCountry <- renderLeaflet({
    
    map <- filtered_aaron_data()
    
    AaronCountry <- leaflet()
    AaronCountry <- addTiles(AaronCountry)
    AaronCountry <- setView(AaronCountry, 
                       lng = map$Lon[1], 
                       lat = map$Lat[1], 
                       zoom = 5)
    
  })
  
    output$temperaturePlot <- renderPlot({
    plot_data <- filtered_aaron_data()
    ggplot(plot_data, aes(x = Year, y = Average.Temperature..C)) +
      geom_line() +
      labs(title = paste("Temperatures Over 30 Years in", input$AaronCountry),
           x = "Year", y = "Temperature (°C)")
  })
  
  # Dina's server 
  filtered_data_dina <- reactive({
      filter(dina_df, Area == input$DinaCountry)
  })
  
  output$food_consumption_plot <- renderPlot({
    ggplot(filtered_data_dina(), aes(x = Year)) +
      geom_line(aes(y = Food.Household.Consumption, color = "Food Consumption")) +
      labs(title = "Food Consumption Over Time",
           y = "Values",
           x = "Year",
           color = "Legend") 
  })
  
  countries_to_plot <- c("Afghanistan", "Brazil", "China", "France", "Germany", "India", "Japan", "Mexico", "Russia", "Malaysia",
                         "Chile", "Canada", "Australia", "Argentina", "Indonesia", "Spain", "Saudi Arabia", "Turkey")
  
  countries_to_plot_data <- filter(dina_df, `Area` %in% countries_to_plot)
  
  output$all_countries_food_consumption <- renderPlot({
    ggplot(countries_to_plot_data, aes(x = Year, y = Food.Household.Consumption, color = Area, label = Area)) +
      geom_point(aes(color = Area)) +
      geom_line() +
      labs(title = "Food Consumption for All Countries 1990-2020",
           x = "Decade",
           y = "Household Food Conspumtion")
  })
}

shinyApp(ui = ui, server = server)