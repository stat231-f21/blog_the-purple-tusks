#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# importing libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(usmap)

# importing data
covid_19_spatial <- read_csv("../data/covid/covid_19_spatial.csv")
mental_health_spatial <- read_csv("../data/mental-health/mental_health_spatial.csv")
us_sentiments <- read_csv2("../data/textual/twitter_sentiments.csv")
### Widgets ###

# radiobuttons
covid_choice_values <- c("percentage_cases_bar", "percentage_deaths_bar")
covid_choice_names <- c("Cases Percentage", "Death Percentage")
names(covid_choice_values) <- covid_choice_names

mental_choice_values <- c("anxiety_percentage", "depression_percentage")
mental_choice_names <- c("Anxiety Percentage", "Depression Percentage")
names(mental_choice_values) <- mental_choice_names

sentiment_choices <- unique(us_sentiments$sentiments)


### UI ###
ui <- fluidPage(
    
    # Application title
    titlePanel("Spatial Data Comparing COVID-19 (Cases/Deaths), Mental Health (Anxiety/Depression) and Sentiments by Senators"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId ="weekslider",
                        label = "Choose a week timeframe",
                        min = 1,
                        max = 39,
                        step = 1,
                        animate = TRUE,
                        value = c(1,18)),
            
            radioButtons(inputId = "covidvar",
                         label = "Choose Cases/Deaths:",
                         choices = covid_choice_values,
                         selected = "percentage_cases_bar"),
            
            radioButtons(inputId = "mentalvar",
                         label = "Choose Anxiety/Depression:",
                         choices = mental_choice_values,
                         selected = "anxiety_percentage"),
            
            radioButtons(inputId = "sentimentvar",
                         label = "Choose a Sentiment:",
                         choices = sentiment_choices,
                         selected = "anger"),
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
            filter(week %in% input$weekslider[1]:input$weekslider[2])%>%
            select(c("state", "percentage_cases_bar", "percentage_deaths_bar")) %>%
            group_by(state) %>%
            summarize(percentage_cases_bar = mean(percentage_cases_bar),
                      percentage_deaths_bar = mean(percentage_deaths_bar))
    })
    
    data_mental <- reactive({
        data <- mental_health_spatial %>%
            filter(week %in% input$weekslider[1]:input$weekslider[2])%>%
            select(c("state", "anxiety_percentage", "depression_percentage")) %>%
            group_by(state) %>%
            summarize(anxiety_percentage = mean(anxiety_percentage),
                      depression_percentage = mean(depression_percentage))
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
            theme(plot.title = element_text(size=15), 
                  plot.subtitle = element_text(size=11),
                  legend.text = element_text(size=10),
                  legend.title = element_text(size=11),
                  legend.key.size = unit(1, 'cm'),
                  plot.caption.position = "plot",
                  plot.caption = element_text(size = 8.5, hjust = 0)) +
            labs(title = "The Distribution of Percentages of New COVID-19 Cases/Deaths",
                 subtitle = "During Selected COVID-19 weeks time-frame",
                 caption = paste("Percentage = average of number of new cases for the state during this time period over number of new cases during weeks 1-39", " ", sep="\n"))
    })
    
    output$mentalmap <- renderPlot({
        plot_usmap(data = data_mental(), values = input$mentalvar) +
            viridis::scale_fill_viridis(name = "Percentages of Anxiety/Depression", option = "magma", direction = -1) +
            theme_void() +
            theme(plot.title = element_text(size=12), 
                  plot.subtitle = element_text(size=9),
                  legend.text = element_text(size=10),
                  legend.title = element_text(size=10),
                  legend.key.size = unit(1, 'cm'),
                  plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 0)) +
            labs(title = "The Distribution of Percentages of People Associated with Anxiety/Depression",
                 subtitle = "During Selected COVID-19 weeks time-frame", 
                 caption = paste("Percentage = average of number of people with anxiety/depression over number of people who took the survey during this time period", " ", sep="\n"))
    })
    
    output$sentimentmap <- renderPlot({
        plot_usmap(data = data_sent(), values = "sentprc") +
            viridis::scale_fill_viridis(name = "Percentages of Selected Sentiment", option = "magma", direction = -1) +
            theme_void() +
            theme(plot.title = element_text(size=12), 
                  plot.subtitle = element_text(size=9),
                  legend.text = element_text(size=10),
                  legend.title = element_text(size=10),
                  legend.key.size = unit(1, 'cm'),
                  plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 0)) +
            labs(title = "The Distribution of the Percentages of Sentiments regarding COVID-19 protocols among Senators",
                 subtitle = "During Selected COVID-19 weeks time-frame",
                 caption = paste("Percentage = average number of words bearing the given sentiment over the total number of words by the senators during this time period", " ", sep="\n"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
