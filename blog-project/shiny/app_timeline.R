# Load necessary packages
library(shiny)
library(tidyverse)
library(formatR)
library(csv)
library(RColorBrewer)

# import data
timeline_graph2 <- read_csv("timeline_graph2.csv")
timeline_table2 <- read_csv("timeline_table2.csv")
sentiments <- read_csv("twitter_sentiments_total.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For Timeline widgets:

## For selectInput choices
state_choices <- unique(timeline_graph2$state)

## For checkboxGroupInputchoices
response_choices <- c("Anxiety", "Depression", "Covid Cases", "Covid Deaths")

## For checkboxGroupInputchoices
sentiment_choices <- c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust")

# For Table widgets:

## For selectInput choices
region_choices <- unique(timeline_table2$state)

############
#    ui    #
############
ui <- navbarPage(
  
  title = "Timeline Series",
  
  # Timeline
  tabPanel(
    title = "Timeline Series",
    
    sidebarLayout(
      sidebarPanel(
        
        selectInput(inputId = "region",
                     label = "Choose a region:",
                     choices = state_choices,
                     selected = 1),
        checkboxGroupInput(inputId = "responsevar",
                    label = "Choose a response variable:",
                    choices = response_choices,
                    selected = c("Anxiety", "Depression", "Covid Cases", "Covid Deaths"),
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
      
      mainPanel(plotOutput(outputId = "timeline"),
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
                   response %in% input$responsevar,
                   week %in% input$weekslider[1]:input$weekslider[2])
  })
  sentiment_data <- reactive ({
    data <- filter(sentiments, 
                   state %in% input$region,
                   week %in% input$weekslider[1]:input$weekslider[2],
                   response %in% input$sentimentvar)
  })
  
  
  output$timeline <- renderPlot({
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
      geom_line(data = sentiment_data(),
                aes(x = week,
                    y = percentage,
                    colour = response)) +
      geom_point(data = sentiment_data(),
                 aes(x = week,
                     y = percentage,
                     colour = response)) +
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            axis.text = element_text(size=12),
            axis.title = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
            scale_y_continuous(trans='log10') +

      scale_color_brewer(palette = "Set3") +
      labs(title = "The Effects of COVID-19 on Mental Health, Physical Health, and Sentiment",
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