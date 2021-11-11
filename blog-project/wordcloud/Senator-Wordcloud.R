# Load necessary packages
library(shiny)
library(tidyverse)
library(formatR)
library(csv)
library(utils)
library(wordcloud)
library(ggwordcloud)
library(lubridate)
library(tidytext)
setwd("~/Documents/Amherst/STAT 231 DATA SCIENCE/blog_the-purple-tusks/blog-project")

# import data
sentiments <- read_csv2("data/textual/twitter_sentiments.csv")
senator_tweets <- read.csv2("data/textual/senator_tweets.csv")


#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For Timeline widgets:

## For selectInput choices
state_choices <- state.name

sentiments_choices <- unique(sentiments$sentiments)

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
        
        selectizeInput(inputId = "region",
                    label = "Choose a region:",
                    choices = state_choices,
                    selected = "California",
                    multiple = TRUE),
        textInput(inputId = "text", label = h3("Keyword"), value = "US"),
        sliderInput(inputId ="date",
                    label = "Choose a week timeframe",
                    min = ymd(20090304),
                    max = ymd(20211104),
                    animate = TRUE,
                    value = c(ymd(20200304),ymd(20210305)),
                    dragRange = TRUE
        )
        
      ),
      
      mainPanel("Wordcloud of Senator Tweets",
                plotOutput(outputId = "wordcloud")
                )
    )
  )
  
  
)

############
# server   #
############
server <- function(input, output){
  
  # Timeline
  wc_data <- reactive({
    data <- senator_tweets %>% 
      filter(grepl(input$text,text,ignore.case = TRUE),
             created_at >= as.Date(input$date[1]) & created_at <= as.Date(input$date[2])) %>%
      unnest_tokens(input = text, output = tokens) %>% 
      #get rid of stop words
      anti_join(stop_words, by = c("tokens" = "word")) %>%
      count(tokens) %>%
      filter(!grepl(paste("https|t.co|amp|rt|",input$text,sep = ""), tokens)) %>%
      #join with sentiments
      #inner_join(get_sentiments(lexicon = "afinn"), by = c("tokens" = "word")) %>%
      arrange(desc(n))
  })

  output$wordcloud <- renderPlot({
    wordcloud(words = wc_data()$tokens, 
              freq = wc_data()$n, 
              max.words = 80, 
              ordered.colors = TRUE
              )
  }, height = 800, width = 600)
  
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