#Dagmawit Belete
#A country's food production VS consumption 

if (!require("shiny")) install.packages("shiny") 
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2") 

library(shiny)
library(dplyr)
library(ggplot2) 

crops_data <- read.csv("crops-yield-changes-CSV.csv")
co2_emission_data <- read.csv("Agrofood_co2_emission.csv") 

ui <- fluidPage(
  titlePanel("Food Production vs Consumption"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:",
                  choices = unique(crops_data$COUNTRY)),
      hr(),
      helpText("Select a country to contrast food production and consumption.")
    ),
    mainPanel(
      plotOutput("production_vs_consumption"),
      tableOutput("summary_table")
    )
  )
)


server <- function(input, output) {
  selected_country_data <- reactive({
    filter(crops_data, Country == input$country)
  })
  
  output$production_vs_consumption <- renderPlot({
    ggplot(selected_country_data(), aes(x = Year)) +
    geom_line(aes(y = production, color = crop), linetype = "solid") +
    geom_line(aes(y = consumption, color = crop), linetype = "dashed") + 
    labs(title = paste("Food Production vs Consumption in", input$country),
        x = "Year",
        y = "Amount") + 
      scale_color_manual(values = c("maize" = "blue", "wheat" = "red", "rice" =
              "green"))
  })
  
  output$summary_table <- renderTable({
    summarise(selected_country_data(),
  Total_Production = sum(Production),
  Total_Consumption = sum(Consumption))
  })
}

shinyApp(ui, server) 

