# Load necessary packages
library(shiny)
library(tidyverse)
library(formatR)
library(csv)
library(RColorBrewer)

# import data
timeline_graph2 <- read_csv("../data/mental-health/timeline_graph2.csv")
timeline_graph1 <- read_csv("../data/mental-health/timeline_graph.csv")
timeline_table2 <- read_csv("../data/mental-health/timeline_table2.csv")
sentiments <- read_csv("../data/textual/twitter_sentiments_total.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For Timeline widgets:

## For selectInput choices
state_choices <- unique(timeline_graph2$state)

## For checkboxGroupInputchoices
mentalhealth_choices <- c("Anxiety", "Depression")

covid_choices <- c("Covid Cases", "Covid Deaths")

sentiment_choices <- c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust")

# For Table widgets:

## For selectInput choices
region_choices <- unique(timeline_table2$state)

############
#    ui    #
############
ui <- navbarPage(
  
  title = "Mental Health, Covid-19, and Sentiments",
  
  # Timeline
  tabPanel(
    title = "Timeline Series",
    
    sidebarLayout(
      sidebarPanel(
        
        selectInput(inputId = "region",
                    label = "Choose a region:",
                    choices = state_choices,
                    selected = 1),
        checkboxGroupInput(inputId = "mentalhealth",
                           label = "Choose a symptom:",
                           choices = mentalhealth_choices,
                           selected = c("Anxiety", "Depression"),
                           inline = TRUE),
        checkboxGroupInput(inputId = "covid",
                           label = "Choose a covid response:",
                           choices = covid_choices,
                           selected = c("Covid Cases", "Covid Deaths"),
                           inline = TRUE),
        checkboxGroupInput(inputId = "sentimentvar",
                           label = "Choose a sentiment:",
                           choices = sentiment_choices,
                           selected = c("negative", "positive"),
                           inline = TRUE),
        sliderInput(inputId ="weekslider",
                    label = "Choose a week timeframe",
                    min = 1,
                    max = 39,
                    step = 1,
                    animate = TRUE,
                    value = c(1,39),
                    dragRange = TRUE),
        selectInput(inputId = "area",
                    label = "Choose a region:",
                    choices = region_choices,
                    selected = 1)
        
      ),
      
      mainPanel(plotOutput(outputId = "timeline_mentalhealth"),
                plotOutput(outputId = "timeline_covid"),
                plotOutput(outputId = "timeline_sentiment"),
                "Percentages of Indicators of Covid-19",
                DT::dataTableOutput(outputId = "table"))
    )
  )
  
  
)

############
# server   #
############
server <- function(input, output){
  
  # Timeline
  data_for_timeline <- reactive ({
    data <- filter(timeline_graph2, 
                   state %in% input$region, 
                   response %in% input$covid,
                   week %in% input$weekslider[1]:input$weekslider[2])
  })
  data_for_timeline2 <- reactive ({
    data <- filter(timeline_graph1, 
                   state %in% input$region, 
                   response %in% input$mentalhealth,
                   week %in% input$weekslider[1]:input$weekslider[2])
  })
  data_for_sentiment <- reactive ({
    data <- filter(sentiments, 
                   state %in% input$region,
                   week %in% input$weekslider[1]:input$weekslider[2],
                   response %in% input$sentimentvar)
  })
  
  
  output$timeline_covid <- renderPlot({
    ggplot(data = data_for_timeline(),
           mapping = aes(x = week,
                         y = percentage,
                         colour = response)) +
      geom_line(aes(x = week,
                    y = percentage,
                    colour = response)) +
      geom_point(aes(x = week,
                     y = percentage,
                     colour = response)) + 
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            axis.text = element_text(size=12),
            axis.title = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
      scale_color_brewer(palette = "Set3") +
      labs(title = "The Change in Covid-19",
           subtitle = "Week 1 - Week 39",
           y = "Percentage",
           x = "Week") +
      scale_x_continuous(n.breaks = 13, limits = c(1,39))
  })
  
  output$timeline_mentalhealth <- renderPlot({
    ggplot(data = data_for_timeline2(),
           mapping = aes(x = week,
                         y = percentage,
                         colour = response)) +
      geom_line(aes(x = week,
                    y = percentage,
                    colour = response)) +
      geom_point(aes(x = week,
                     y = percentage,
                     colour = response)) + 
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            axis.text = element_text(size=12),
            axis.title = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
      scale_color_brewer(palette = "Paired") +
      labs(title = "The Distribution of Mental Health in the U.S.",
           subtitle = "During the Covid-19 Period",
           y = "Percentage",
           x = "Week") +
      scale_x_continuous(n.breaks = 13, limits = c(1,39))
  })  
  
  output$timeline_sentiment <- renderPlot({
    ggplot(data = data_for_sentiment(),
           mapping = aes(x = week,
                         y = percentage,
                         colour = response)) +
      geom_line(aes(x = week,
                    y = percentage,
                    colour = response)) +
      geom_point(aes(x = week,
                     y = percentage,
                     colour = response)) + 
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            axis.text = element_text(size=12),
            axis.title = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
      scale_color_brewer(palette = "Accent") +
      labs(title = "The Distribution of Sentiments from Senators",
           subtitle = "During the Covid-19 Period",
           y = "Percentage",
           x = "Week") +
      scale_x_continuous(n.breaks = 13, limits = c(1,39))
  })
  
  # Table
  data_for_table <- reactive({
    data <- filter(timeline_table2, state %in% input$area) %>%
      select(-state)
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)