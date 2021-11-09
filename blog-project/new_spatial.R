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

### Options ###
covid_19_spatial <- read_csv("data/covid/covid_19_spatial.csv")
covid_choice_values <- c("percentage_cases_spatial", "percentage_deaths_spatial")
covid_choice_names <- c("Cases Percentage", "Death Percentage")
names(covid_choice_values) <- covid_choice_names

mental_health_spatial <- read_csv("data/mental-health/mental_health_spatial.csv")
mental_choice_values <- c("anxiety_percentage", "depression_percentage")
mental_choice_names <- c("Anxiety Percentage", "Depression Percentage")
names(mental_choice_values) <- mental_choice_names

us_sentiments <- read_csv2("data/textual/twitter_sentiments.csv")
sentiment_choices <- unique(us_sentiments$sentiments)

### UI ###
ui <- fluidPage(

    # Application title
    titlePanel("Spatial Part"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId ="weekslider",
                        label = "Choose a week timeframe",
                        min = 1,
                        max = 39,
                        step = 1,
                        animate = TRUE,
                        value = c(1,39)),
            
            radioButtons(inputId = "covidvar",
                         label = "Choose Cases/Deaths:",
                         choices = covid_choice_values,
                         selected = "percentage_cases_spatial"),

            radioButtons(inputId = "mentalvar",
                         label = "Choose Anxiety/Depression:",
                         choices = mental_choice_values,
                         selected = "anxiety_percentage"),
            
            radioButtons(inputId = "sentimentvar",
                         label = "Choose a Sentiment:",
                         choices = sentiment_choices,
                         selected = "anger")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "covidmap"),
            plotOutput(outputId = "mentalmap"),
            plotOutput(outputId = "sentimentmap")
        )
    
    )
)

### SERVER ###
server <- function(input, output) {
    data_covid <- reactive({
        data <- covid_19_spatial %>%
            filter(week %in% input$weekslider[1]:input$weekslider[2])
    })
    
    data_mental <- reactive({
        data <- mental_health_spatial %>%
            filter(week %in% input$weekslider[1]:input$weekslider[2])
    })
    
    data_sent <- reactive({
        data <- us_sentiments %>%
            filter(sentiments == input$sentimentvar,
                   week %in% input$weekslider[1]:input$weekslider[2]) %>%
            select(c("state", "percentage")) %>%
            group_by(state) %>%
            summarize(sentprc = mean(percentage))
        
    })

    output$covidmap <- renderPlot({
        plot_usmap(data = data_covid(), values = input$covidvar) +
            viridis::scale_fill_viridis(name = "Percentages of Cases/Deaths", option = "magma", direction = -1) +
            theme_void() +
            theme(plot.title = element_text(size=23), 
                  plot.subtitle = element_text(size=20),
                  legend.text = element_text(size=17),
                  legend.title = element_text(size=21),
                  legend.key.size = unit(1, 'cm')) +
            labs(title = "Distribution of the Percentages of Deaths and Cases of COVID-19",
                 subtitle = "Statewise distribution over US")
    })
    
    output$mentalmap <- renderPlot({
        plot_usmap(data = data_mental(), values = input$mentalvar) +
            viridis::scale_fill_viridis(name = "Percentages of Anxiety/Depression", option = "magma", direction = -1) +
            theme_void() +
            theme(plot.title = element_text(size=23), 
                  plot.subtitle = element_text(size=20),
                  legend.text = element_text(size=17),
                  legend.title = element_text(size=21),
                  legend.key.size = unit(1, 'cm')) +
            labs(title = "Distribution of the Percentages of Anxiety/Depression during COVID-19",
                 subtitle = "Statewise over the US")
    })
    
    output$sentimentmap <- renderPlot({
        plot_usmap(data = data_sent(), values = "sentprc") +
            viridis::scale_fill_viridis(name = "Percentages of Selected Sentiment", option = "magma", direction = -1) +
            theme_void() +
            theme(plot.title = element_text(size=23), 
                  plot.subtitle = element_text(size=20),
                  legend.text = element_text(size=17),
                  legend.title = element_text(size=21),
                  legend.key.size = unit(1, 'cm')) +
            labs(title = "Distribution of the Percentages of Sentiments regarding COVID-19 protocols",
                 subtitle = "Statewise over the US")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
