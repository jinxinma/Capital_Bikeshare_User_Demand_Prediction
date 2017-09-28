if (!require(shiny)) install.packages("shiny")
library(shiny)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(tidyr)) install.packages("tidyr")
library(tidyr)

if (!require(plotly)) install.packages("plotly")
library(plotly)

# Preprocess data
bikeshare <- read.csv("hourly_users_data.csv")
bikeshare <- bikeshare[c("yr", "mnth", "weekday", "hr", "cnt", "casual", "registered")]

summary_df <- bikeshare %>% group_by(yr, mnth, weekday, hr) %>% summarise(cnt = mean(cnt))
summary_df$yr[summary_df$yr == 0] <- 2011
summary_df$yr[summary_df$yr == 1] <- 2012

# Define UI for application that draws a heatmap
ui <- fluidPage(
  
  # Application title
  titlePanel("Average Bike Demand By Weekday and Hour"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("yr",
                  "Year:",
                  choices = c(2011, 2012),
                  selected = 2011),
      
      sliderInput("mnth",
                  "Month:",
                  min = 1,
                  max = 12,
                  value = 1,
                  animate=animationOptions(interval=1000))),
    
    mainPanel(
      plotlyOutput('heatmapPlot'))
    )
)

server <- function(input, output) {
  sliderYear <- reactive({input$yr})
  sliderMonth <- reactive({input$mnth})

  output$heatmapPlot <- renderPlotly({
    summary_subset <- subset(summary_df, yr == sliderYear() & mnth == sliderMonth())
    matrix_df <- spread(summary_subset, hr, cnt)
    
    matrix_df[is.na(matrix_df)] <- 0
    m <- data.matrix(matrix_df[, 4:length(matrix_df)])
    plot_ly(
      x = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM",
            "8 AM", "9 AM", "10 AM", "11 AM", "12 PM", "1 PM", "2 PM", "3 PM", 
            "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM"),
      y = c("Monday", "Tuesday", "Wednesday", "Thursday", 
            "Friday", "Saturday", "Sunday"), 
      z = m,
      type = "heatmap"
    ) %>% layout(xaxis = list(categoryorder = "trace"),
                 yaxis = list(categoryorder = "trace"))
  })
}

shinyApp(ui, server)