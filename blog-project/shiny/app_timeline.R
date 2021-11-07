# Load necessary packages
library(shiny)
library(tidyverse)
library(formatR)
library(csv)
library(kableExtra)

# import data
timeline_graph <- read_csv("timeline_graph.csv")
timeline_table <- read_csv("timeline_table.csv")

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

# For Table widgets:

## For selectInput choices
region_choices <- unique(timeline_table$state)

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
    data <- filter(timeline_graph, state %in% input$region, response %in% input$responsevar)
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
           x = "Week")
  })
  
  # Table
  data_for_table <- reactive({
    data <- filter(timeline_table, state %in% input$area) %>%
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