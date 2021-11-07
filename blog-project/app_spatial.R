library(tidyverse)
library(dplyr)
library(tidyr)

# import data
MergedStatescovid <- read_csv("MergedStatescovid.csv")
MergedStatesmental <- read_csv("MergedStatesMental.csv")
twitter_sentiments <- read_csv("twitter_sentiments.csv")

# For TAB 2 Map widgets:
## for select input choices
type_choice_values <- c("percentage_cases_spatial", "percentage_deaths_spatial")
type_choice_names <- c("Cases Percentage", "Death Percentage")
names(type_choice_values) <- type_choice_names

type2_choice_values <- c("anxiety_percentage", "depression_percentage")
type2_choice_names <- c("Anxiety Percentage", "Depression Percentage")
names(type2_choice_values) <- type2_choice_names

type3_choice_values <- c("anger","anticipation","disgust","fear","joy",
                         "negative","positive","sadness","surprise","trust")
type3_choice_names <- c("Anger", "Anticipation","Disgust","Fear","Joy",
                        "Negative","Positive","Sadness","Surprise","Trust")
names(type3_choice_values) <- type3_choice_names

########
#  ui  #
########

ui <- navbarPage(
  title = "Percentage cases and deaths during COVID-19",
  
  # Tab 2: Map
  tabPanel(
    title = "U.S. Map",
    
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(inputId = "covidmapgraph",
                     label = "Choose a variable of interest to plot:",
                     choices = type_choice_values,
                     selected = "percentage_cases_spatial"),
        
        sliderInput(inputId ="weekslider",
                    label = "Choose a week timeframe",
                    min = 1,
                    max = 39,
                    step = 1,
                    animate = TRUE,
                    value = 1
                    ),
        
        radioButtons(inputId = "mentalmapgraph",
                     label = "Choose a variable of interest to plot:",
                     choices = type2_choice_values,
                     selected = "anxiety_percentage"),
        
        radioButtons(inputId = "sentimentmapgraph",
                     label = "Choose a variable of interest to plot:",
                     choices = type3_choice_values,
                     selected = "anger")
      ),
      mainPanel(plotOutput(outputId = "covidmap"),
                plotOutput(outputId = "mentalmap"),
                plotOutput(outputId = "sentimentmap"))
    )
  )  
)  



############
# server   #
############
server <- function(input, output){
  
  # TAB 2: Map
  data_for_week_covid <- reactive({
     data <- filter(MergedStatescovid, week %in% input$weekslider)
  })
  
  data_for_week_mental <- reactive({
    data <- filter(MergedStatesmental, week %in% input$weekslider)
  })
  
  data_for_week_sentiment <- reactive({
    data <- filter(twitter_sentiments, week %in% input$weekslider,
                   sentiments %in% input$sentimentmapgraph)
  })
  
  output$covidmap <- renderPlot({ 
    ggplot() +
      geom_polygon(data = data_for_week_covid(),
                   aes_string(x = "long", y = "lat", group = "group", fill = input$covidmapgraph),
                   color = "black", size = 0.2) +
      viridis::scale_fill_viridis(name = "Percentages of Cases/Deaths", option = "magma", direction = -1) +
      theme_void() +
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
      labs(title = "Distribution of the Percentages of Deaths and Cases of COVID-19",
           subtitle = "Statewise")
  })
  
  
  output$mentalmap <- renderPlot({ 
    ggplot() +
      geom_polygon(data = data_for_week_mental(),
                   aes_string(x = "long", y = "lat", group = "group", fill = input$mentalmapgraph),
                   color = "black", size = 0.2) +
      viridis::scale_fill_viridis(name = "Percentages of Anxiety/Depression", option = "magma", direction = -1) +
      theme_void() +
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
      labs(title = "Distribution of the Percentages of Anxiety and Depression during COVID-19",
           subtitle = "Statewise")
  })
  
  output$sentimentmap <- renderPlot({ 
    ggplot() +
      geom_polygon(data = data_for_week_sentiment(),
                   aes_string(x = "long", y = "lat", group = "group", fill = data$percentage_of_national_sentiment),
                   color = "black", size = 0.2) +
      viridis::scale_fill_viridis(name = "Percentages of Sentiments", option = "magma", direction = -1) +
      theme_void() +
      theme(plot.title = element_text(size=23), 
            plot.subtitle = element_text(size=20),
            legend.text = element_text(size=17),
            legend.title = element_text(size=21),
            legend.key.size = unit(1, 'cm')) +
      labs(title = "Distribution of the Percentages of Different Sentiments during COVID-19",
           subtitle = "Statewise")
  })
}


####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
