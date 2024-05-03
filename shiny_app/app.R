library(shiny)
library(ggplot2)


ui <- fluidPage(
    titlePanel("Madison Weather from 2019 to 2024"),
    sidebarLayout(
        sidebarPanel(
            selectInput("year", "Select Year: ", choices = c("2019", "2020", "2021", "2022", "2023", "2024"), selected = "2023"),
            selectInput("y_axis", "Select which variable to display on Y-axis:", choices = c("WDSP" = "wind_speed", "VISIB" = "visibility", "TEMP" = "temperature", "DEWP" = "dewpoint", "SLP" = "sea_level_pressure"), selected = "WDSP")),
        mainPanel(
          plotOutput("weather_plot")
        )
    )  
)



server <- function(input, output, session) {
  data <- reactive({
    file_name <- paste0("clean", input$year, ".csv")
    print(file_name)  # Print file name for debugging
    data <- read.csv(file_name)
  })

    output$weather_plot <- renderPlot({
        ggplot(data(), aes_string(x = "DATE", y = input$y_axis)) +
          geom_bar(fill = "skyblue") +
          labs(x = "Date", y = input$y_axis) +
          ggtitle(paste("Weather Data for ", input$year)) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })

}

shinyApp(ui, server)
  
