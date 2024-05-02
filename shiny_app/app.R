library(shiny)
library(ggplot2)
# library(plotly)
# library(gridlayout)
# library(bslib)
# library(DT)


ui <- fluidPage(
    # fileInput("data2019", "clean2019.csv", accept = c("text/csv", "text/comma-separated-values, .csv")),
    # fileInput("data2020", "clean2020.csv", accept = c("text/csv", "text/comma-separated-values, .csv")),
    # fileInput("data2021", "clean2021.csv", accept = c("text/csv", "text/comma-separated-values, .csv")),
    # fileInput("data2022", "clean2022.csv", accept = c("text/csv", "text/comma-separated-values, .csv")),
    # fileInput("data2023", "clean2023.csv", accept = c("text/csv", "text/comma-separated-values, .csv")),
    # fileInput("data2024", "clean2024.csv", accept = c("text/csv", "text/comma-separated-values, .csv"))
    titlePanel("Madison Weather from 2019 to 2024"),
    sidebarLayout(
        sidebarPanel(
            selectInput("year", "Select Year: ", choices = c("2019", "2020", "2021", "2022", "2023", "2024"), selected = "2023"),
            selectInput("y_axis", "Select which variable to display on Y-axis:", choices = c("WDSP" = "wind_speed", "VISIB" = "visibility", "TEMP" = "temperature", "DEWP" = "dewpoint", "SLP" = "sea_level_pressure"), selected = "WDSP")
        )
    ),
    mainPanel(
        plotOutput("weather_plot")
    )
)


server <- function(input, output, session) {
    data <- reactive({
        file_name <- paste0("clean", input$year, ".csv")
        read.csv(file_name)
    })

    output$weather_plot <- renderPlot({
        ggplot(data(), aes_string(x = "date", y = input$y_axis)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(x = "Date", y = input$y_axis) +
        ggtitle(paste("Weather Data for ", input$year)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })

}

shinyApp(ui, server)
  
