
library(shiny)
library(plotly)
library(readxl)
library(dplyr)

file_path <- "B:/DEV mini project/Factory_Performance_Data.xlsx"
data <- read_excel(file_path)

data$Timestamp <- as.POSIXct(data$Timestamp, format = "%m/%d/%y %H:%M")

ui <- fluidPage(
  titlePanel("Visualizing Real-Time IoT Data: Insights at a Glance"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Select a Metric to Visualize:", 
                  choices = c("Temperature (°C)", "Output Count", "Downtime (hrs)", 
                              "Energy Consumption (kW)", "Defect Rate (%)", 
                              "Idle Time (hrs)", "OEE (%)", "Worker Productivity (units/hr)", 
                              "Wastage (kg)", "Carbon Emissions (kg CO2)")),
      selectInput("machine", "Select a Machine:", 
                  choices = unique(data$Machine_ID), 
                  selected = unique(data$Machine_ID)[1]),
      selectInput("shift", "Select a Shift:", 
                  choices = c("Morning", "Afternoon", "Night"), 
                  selected = "Morning"),
      sliderInput("time_range", "Select Time Range:", 
                  min = min(data$Timestamp), 
                  max = max(data$Timestamp), 
                  value = c(min(data$Timestamp), max(data$Timestamp)), 
                  timeFormat = "%H:%M"),
      actionButton("update", "Update")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series Plot", plotlyOutput("time_series_plot")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary_stats"))
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$update)
    data %>% 
      filter(Machine_ID == input$machine, 
             Shift == input$shift, 
             Timestamp >= input$time_range[1], 
             Timestamp <= input$time_range[2])
  })

  output$time_series_plot <- renderPlotly({
    req(filtered_data())
    metric <- input$metric
    plot_data <- filtered_data()
    
    fig <- plot_ly(plot_data, 
                   x = ~Timestamp, 
                   y = as.formula(paste0("~`", metric, "`")), 
                   type = 'scatter', 
                   mode = 'lines+markers', 
                   line = list(color = 'blue'))
    fig <- fig %>% layout(
      title = paste("Time Series Plot of", metric),
      xaxis = list(title = "Timestamp"),
      yaxis = list(title = metric)
    )
    fig
  })
 
  output$summary_stats <- renderPrint({
    req(filtered_data())
    metric <- input$metric
    summary_stats <- filtered_data() %>% 
      summarise(
        Mean = mean(get(metric), na.rm = TRUE),
        Median = median(get(metric), na.rm = TRUE),
        SD = sd(get(metric), na.rm = TRUE),
        Min = min(get(metric), na.rm = TRUE),
        Max = max(get(metric), na.rm = TRUE)
      )
    summary_stats
  })
}

shinyApp(ui = ui, server = server)
