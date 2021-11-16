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
covid_choice_names <- c("Cases Percentage S", "Death Percentage S")
names(covid_choice_values) <- covid_choice_names

covid_19_spatial <- read_csv("data/covid/covid_19_spatial.csv")
covid_choice_values2 <- c("percentage_cases_bar", "percentage_deaths_bar")
covid_choice_names2 <- c("Cases Percentage B", "Death Percentage B")
names(covid_choice_values2) <- covid_choice_names2

mental_health_spatial <- read_csv("data/mental-health/mental_health_spatial.csv")
mental_choice_values <- c("anxiety_percentage", "depression_percentage")
mental_choice_names <- c("Anxiety Percentage", "Depression Percentage")
names(mental_choice_values) <- mental_choice_names

table_for_mental2 <- read_csv("data/timeline_table2.csv")
mental_choice_values2 <- c("Anxiety", "Depression")
mental_choice_names2 <- c("Anxiety Percentage B", "Depression Percentage B")
names(mental_choice_values2) <- mental_choice_names2

us_sentiments <- read_csv2("data/textual/twitter_sentiments.csv")
sentiment_choices <- unique(us_sentiments$sentiments)

table_for_sent2 <- read_csv("data/timeline_table2.csv")
sent_choice_values2 <- c("anger", "anticipation", "disgust", "fear", "joy",
                         "negative", "positive", "sadness", "surprise", "trust")
sent_choice_names2 <- c("Anger B", "Anticipation B", "Disgust B", "Fear B",
                        "Joy B", "Negative B", "Positive B", "Sadness B",
                        "Surprise B", "Trust B")
names(sent_choice_values2) <- sent_choice_names2


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
                         label = "Choose Cases/Deaths S:",
                         choices = covid_choice_values,
                         selected = "percentage_cases_spatial"),
            
            radioButtons(inputId = "covidvar2",
                         label = "Choose Cases/Deaths B:",
                         choices = covid_choice_values2,
                         selected = "percentage_cases_bar"),

            radioButtons(inputId = "mentalvar",
                         label = "Choose Anxiety/Depression:",
                         choices = mental_choice_values,
                         selected = "anxiety_percentage"),
            
            radioButtons(inputId = "mentalvar2",
                         label = "Choose Anxiety/Depression B:",
                         choices = mental_choice_values2,
                         selected = "Anxiety"),
            
            radioButtons(inputId = "sentimentvar",
                         label = "Choose a Sentiment:",
                         choices = sentiment_choices,
                         selected = "anger"),
            
            radioButtons(inputId = "sentimentvar2",
                         label = "Choose a Sentiment:",
                         choices = sent_choice_values2,
                         selected = "anger")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "covidmap"),
            plotOutput(outputId = "covidmap2"),
            plotOutput(outputId = "mentalmap"),
            plotOutput(outputId = "mentalmap2"),
            plotOutput(outputId = "sentimentmap"),
            plotOutput(outputId = "sentimentmap2")
        )
    
    )
)

### SERVER ###
server <- function(input, output) {
    data_covid <- reactive({
        data <- covid_19_spatial %>%
            filter(week %in% input$weekslider[1]:input$weekslider[2])%>%
            select(c("state", "percentage_cases_spatial", "percentage_deaths_spatial")) %>%
            group_by(state) %>%
            summarize(percentage_cases_spatial = mean(percentage_cases_spatial),
                      percentage_deaths_spatial = mean(percentage_deaths_spatial))
    })
    
    data_covid_2 <- reactive({
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
    
    data_mental_2 <- reactive({
        data <- table_for_mental2 %>%
            filter(week %in% input$weekslider[1]:input$weekslider[2])%>%
            select(c("state", "Anxiety", "Depression")) %>%
            group_by(state) %>%
            summarize(Anxiety = mean(Anxiety),
                      Depression = mean(Depression))
    })
    
    data_sent <- reactive({
        data <- us_sentiments %>%
            filter(sentiments == input$sentimentvar,
                   week %in% input$weekslider[1]:input$weekslider[2]) %>%
            select(c("state", "percentage")) %>%
            group_by(state) %>%
            summarize(sentprc = mean(percentage))
        
    })
    
    data_sent_2 <- reactive({
        data <- table_for_sent2 %>%
            filter(week %in% input$weekslider[1]:input$weekslider[2]) %>%
            select(c("state", "anger", "anticipation", "disgust", "fear", "joy",
                     "negative", "positive", "sadness", "surprise", "trust")) %>%
            group_by(state) %>%
            summarize(anger = mean(anger),
                      anticipation = mean(anticipation),
                      disgust = mean(disgust),
                      fear = mean(fear),
                      joy = mean(joy),
                      negative = mean(negative),
                      positive = mean(positive),
                      sadness = mean(sadness),
                      surprise = mean(surprise),
                      trust = mean(trust))
        
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
    
    output$covidmap2 <- renderPlot({
        plot_usmap(data = data_covid_2(), values = input$covidvar2) +
            viridis::scale_fill_viridis(name = "Percentages of Cases/Deaths bar", option = "magma", direction = -1) +
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
    
    output$mentalmap2 <- renderPlot({
        plot_usmap(data = data_mental_2(), values = input$mentalvar2) +
            viridis::scale_fill_viridis(name = "Percentages of Anxiety/Depression", option = "magma", direction = -1) +
            theme_void() +
            theme(plot.title = element_text(size=23), 
                  plot.subtitle = element_text(size=20),
                  legend.text = element_text(size=17),
                  legend.title = element_text(size=21),
                  legend.key.size = unit(1, 'cm')) +
            labs(title = "Distribution of the Percentages of Anxiety/Depression during COVID-19 B",
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
    
    output$sentimentmap2 <- renderPlot({
        plot_usmap(data = data_sent_2(), values = input$sentimentvar2) +
            viridis::scale_fill_viridis(name = "Percentages of Selected Sentiment", option = "magma", direction = -1) +
            theme_void() +
            theme(plot.title = element_text(size=23), 
                  plot.subtitle = element_text(size=20),
                  legend.text = element_text(size=17),
                  legend.title = element_text(size=21),
                  legend.key.size = unit(1, 'cm')) +
            labs(title = "Distribution of the Percentages of Sentiments regarding COVID-19 protocols B",
                 subtitle = "Statewise over the US")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
