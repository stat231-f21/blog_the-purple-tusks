# Load necessary packages
library(shiny)
library(tidyverse)
library(formatR)
library(csv)

# import data
covid_mental_health_longer <- read_csv("covid_mental_health_longer.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For Timeline widgets:

## For selectInput choices
state_choices <- unique(covid_mental_health_longer$state)

## For checkboxGroupInputchoices
response_choices <- unique(covid_mental_health_longer$Response)

## For sliderInput choices


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
                     label = "Choose a region you want to plot:",
                     choices = state_choices,
                     selected = 1),
        checkboxGroupInput(inputId = "responsevar",
                    label = "Choose a response variable you want to plot:",
                    choices = response_choices,
                    selected = c("anxiety_percentage", "depression_percentage", "percentage_cases_state", "percentage_deaths_state"),
                    inline = TRUE),
        
      ),
      
      mainPanel(plotOutput(outputId = "timeline"))
    )
  )
  
  
)

############
# server   #
############
server <- function(input, output){
  
  data_for_timeline <- reactive ({
    data <- filter(covid_mental_health_longer, state %in% input$region, Response %in% input$responsevar)
  })
  
  output$timeline <- renderPlot({
    ggplot(data = data_for_timeline(),
           mapping = aes(x = week,
                         y = Percentage, 
                         color = Response)) +
      geom_line(aes(x = week,
                    y = Percentage,
                    color = Response)) +
      geom_point(aes(x = week,
                     y = Percentage,
                     color = Response)) + 
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            axis.text = element_text(size=17),
            axis.title = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
      
      #scale_fill_manual("Response", 
                        #values = c("anxiety_percentage" = "plum", 
                                   #"Depression Score" = "paleturquoise1")) +
      
      labs(title = "The Effects of COVID-19 on Mental Health, Physical Health, and Sentiment",
           subtitle = "During the Covid-19 Period",
           y = "Date",
           x = "Percentage")
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)