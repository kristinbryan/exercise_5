
library(shiny)
library(tidyverse)
library(rsconnect)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ui <- fluidPage(
    h1("COVID-19 Cases Over Time"),
    selectInput("states", 
                "Select the states you would like to compare:", 
                multiple = TRUE,
                choices = covid19$state),
    submitButton(text = "Compare cases!"),
    plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
    output$timeplot <- renderPlot({
        covid19 %>% 
            filter(state %in% input$states, 
                   cases >= 20) %>% 
            mutate(days_since_20 = date - min(date)) %>% 
            ggplot() +
            geom_line(aes(x = days_since_20, y = cases, color = state)) + 
            labs(x = "Days Since 20+ Cases", 
                 y = "Cases (on the logarithmic scale)", 
                 color = "State") + 
            scale_y_log10(label = scales::label_comma()) +
            theme_minimal()
    })
}

shinyApp(ui = ui, server = server)