# Load necessary packages
library(shiny)
library(tidyverse)
library(formatR)
library(csv)
library(utils)
library(wordcloud)
library(ggwordcloud)

# import data
timeline_graph <- read_csv("timeline_graph.csv")
word_cloud <- read_csv2("senator_wordcloud.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For Timeline widgets:

## For selectInput choices
state_choices <- unique(timeline_graph$state)

## For checkboxGroupInputchoices
response_choices <- unique(timeline_graph$response)

############
#    ui    #
############
ui <- navbarPage(
  
  title = "Timeline Series",
  
  # Tab 1: Histogram
  tabPanel(
    title = "Timeline Series",
    
    sidebarLayout(
      sidebarPanel(
        
        selectInput(inputId = "region",
                    label = "Choose a region:",
                    choices = state_choices,
                    selected = 1),
        checkboxGroupInput(inputId = "responsevar",
                           label = "Choose a response variable you want to plot:",
                           choices = response_choices,
                           selected = c("Anxiety", "Depression", "Covid Cases", "Covid Deaths", "Negative", "Positive"),
                           inline = TRUE),
        sliderInput(inputId ="weekslider",
                    label = "Choose a week timeframe",
                    min = 1,
                    max = 39,
                    step = 1,
                    animate = TRUE,
                    value = c(1,39),
                    dragRange = TRUE
        )
        
      ),
      
      mainPanel(plotOutput(outputId = "timeline"),
                "Percentages of Indicators of Covid-19",
                plotOutput(outputId = "wordcloud"),
                "Wordcloud of Senator Tweets")
    )
  )
  
  
)

############
# server   #
############
server <- function(input, output){
  
  # Timeline
  data_for_timeline <- reactive ({
    data <- filter(timeline_graph, 
                   state %in% input$region, 
                   response %in% input$responsevar,
                   week %in% input$weekslider[1]:input$weekslider[2])
  })
  
  wc_data <- reactive({
    data <- word_cloud %>%
      filter(week %in% input$weekslider[1]:input$weekslider[2], 
             state == input$region)
  })
  
  output$timeline <- renderPlot({
    ggplot(data = data_for_timeline(),
           mapping = aes(x = week,
                         y = percentage, 
                         color = response)) +
      geom_line(aes(x = week,
                    y = percentage,
                    color = response)) +
      geom_point(aes(x = week,
                     y = percentage,
                     color = response)) + 
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            axis.text = element_text(size=17),
            axis.title = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
            scale_y_continuous(trans='log10') +

      scale_colour_manual(values = c("Anxiety" = "palevioletred1", "Depression" = "yellow1", "Covid Cases" = "violet", "Covid Deaths" = "seagreen2", "Negative" = "slateblue1", "Positive" = "turquoise2")) +
      labs(title = "The Effects of COVID-19 on Mental Health, Physical Health, and Sentiment",
           subtitle = "During the Covid-19 Period",
           y = "Percentage",
           x = "Week") +
      scale_x_continuous(n.breaks = 39, breaks = c(1:39), limits = c(1,39))
  })
  
  output$wordcloud <- renderPlot({
    wordcloud(words = wc_data()$tokens, freq = wc_data()$n, max.words = 30, scale=c(3.5,0.25))
  })
  
  # output$wordcloud <- renderPlot({
  #   ggplot(wc_data(), aes(label = tokens)) +
  #     geom_text_wordcloud(aes(size = n, color = value)) +
  #     scale_color_gradient(low = "red", high = "blue") +
  #     theme_minimal() +
  #     scale_size_area(max_size = 15) 
  # })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)