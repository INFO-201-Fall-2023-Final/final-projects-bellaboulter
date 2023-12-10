library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

bella_df <- read.csv("average_emmisions.csv")

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

bella_analysis_view <- fluidPage(
  titlePanel("Exploring Global Emissions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(bella_df$Area)),
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

# UI
ui <- navbarPage(
  "Climate Change Analysis (Emissions, Temperatures, and Food Production/Consumption)",
  tabPanel("About Our Project", about_view),
  tabPanel("Global Emissions", bella_analysis_view)
)

# SERVER
server <- function(input, output) {
  
  filtered_data <- reactive({
    filter(bella_df, Area %in% input$country)
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
}

# Run the Shiny app
shinyApp(ui, server)