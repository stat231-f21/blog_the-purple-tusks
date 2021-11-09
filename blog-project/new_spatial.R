#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(dplyr)
library(tidyr)
library(usmap)

us_sentiments <- read_csv2("data/textual/twitter_sentiments.csv")

sentiment_choices <- unique(us_sentiments$sentiments)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId ="weekslider",
                        label = "Choose a week timeframe",
                        min = 1,
                        max = 39,
                        step = 1,
                        animate = TRUE,
                        value = c(1,39)
            ),
            radioButtons(inputId = "sentimentvar",
                         label = "Choose a Sentiment:",
                         choices = sentiment_choices,
                         selected = "anger")
            )
        ,

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("map_plot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    data_sent <- reactive({
        data <- us_sentiments %>%
            filter(sentiments == input$sentimentvar,
                   week %in% input$weekslider[1]:input$weekslider[2]) %>%
            select(c("state", "percentage")) %>%
            group_by(state) %>%
            summarize(prc = mean(percentage))
                
    })

    output$map_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        plot_usmap(data = data_sent(), values = "prc") +
            scale_fill_continuous(low = "white", high = "red", name = "Sentiment", label = scales::comma)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
