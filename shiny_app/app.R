library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Madison Weather from 2019 to 2024"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year: ", choices = c("2019", "2020", "2021", "2022", "2023", "2024"), selected = "2023"),
      selectInput("y_axis", "Select which variable to display on Y-axis:", choices = c("WDSP" = "WDSP", "VISIB" = "VISIB", "TEMP" = "TEMP", "DEWP" = "DEWP", "SLP" = "SLP"), selected = "WDSP")),
    mainPanel(
      plotOutput("weather_plot")
    )
  )  
)



server <- function(input, output, session) {
  data <- reactive({
    file_name <- paste0("corrected", input$year, ".csv")
    #print(file_name)  # Print file name for debugging
    data <- read.csv(file_name)
    
    data$DATE <- as.Date(as.character(data$DATE), format = "%Y%m%d")
    data
  })
  
  output$weather_plot <- renderPlot({
    y_label <- switch(input$y_axis, WDSP = "Wind Speed in Meters/Second", 
                      VISIB = "Visibility in Meters", TEMP = "Temperature in Celsius", 
                      DEWP = "Dewpoint Temperature in Celsius", SLP = "Atmospheric Pressure at Sea Level in Hectopascals")
    
    ggplot(data(), aes(x = DATE, y = .data[[input$y_axis]])) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Date", y = y_label) +
      ggtitle(paste("Weather Data for ", input$year)) +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_x_date(date_labels = "%Y%m%d", date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
}

shinyApp(ui, server)
