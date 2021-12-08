
# load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(formatR)
library(DT)
library(ggrepel)
library(csv)
library(ggplot2)


## mental health


## combine-data

covid_updated <- read_csv("../data/covid/covid_updated.csv")
mental_health <- read_csv("../data/mental-health/mental_health.csv")

covid_updated <- covid_updated %>%
  select(-"...1") %>%
  rename(percentage_cases_state = percentage_cases, percentage_deaths_state = percentage_deaths)

mental_health <- mental_health %>%
  rename(state = State, week = WEEK)

covid_mental_health <- inner_join(mental_health, covid_updated, by = c("state", "week"))

covid_mental_health_longer <- covid_mental_health %>%
  pivot_longer(cols = c("anxiety_percentage", "depression_percentage", "percentage_cases_state", "percentage_deaths_state"),
               names_to = "Response",
               values_to = "Percentage")



## 

twitter_sentiments <- read_csv2("../data/textual/twitter_sentiments.csv")
twitter_sentiments_national <- read_csv2("../data/textual/ntl_twitter_sentiments.csv")

covid_mental_health_longer_new <- covid_mental_health_longer %>%
  select(state, week, Response, Percentage) %>%
  rename(response = Response, percentage = Percentage)

# out of all the sentiments for the 29 weeks for that state, what percent is that sentiment for that week
twitter_sentiments <- twitter_sentiments %>%
  select(state, week, sentiments, percentage_of_total_sentiment) %>%
  rename(response = sentiments, percentage = percentage_of_total_sentiment) %>%
  filter(week != 0)

twitter_sentiments_national <- twitter_sentiments_national %>%
  mutate(percentage = (count/sum)*100) %>%
  select(week, sentiments, percentage) %>%
  add_column(state = "National") %>%
  filter(week != 0) %>%
  rename(response = sentiments)

twitter_sentiments_total <- rbind(twitter_sentiments, twitter_sentiments_national)

#write_csv(twitter_sentiments_total, "twitter_sentiments_total.csv", append = FALSE)

timeline_data <- rbind(twitter_sentiments_total, covid_mental_health_longer_new)




## 

timeline_table <- timeline_data %>%
  pivot_wider(
    names_from = response,
    values_from = percentage
  )

timeline_table <- timeline_table %>%
  rename(Anxiety = anxiety_percentage, Depression = depression_percentage, Negative = negative, Positive = positive, "Covid Cases" = percentage_cases_state, "Covid Deaths" = percentage_deaths_state) %>%
  select(-anger, -anticipation, -disgust, -fear, -joy, -sadness, -surprise, -trust)

timeline_graph <-timeline_table %>%
  pivot_longer(
    cols = c("Anxiety", "Depression", "Covid Cases", "Covid Deaths", "Negative", "Positive"),
    names_to = "response",
    values_to = "percentage"
  )

#write_csv(timeline_graph, "timeline_graph.csv", append = FALSE)




## new-percentages
twitter_sentiments <- read_csv2("../data/textual/twitter_sentiments.csv")
twitter_sentiments_national <- read_csv2("../data/textual/ntl_twitter_sentiments.csv")

covid_mental_health_longer_new <- covid_mental_health_longer %>%
  select(state, week, Response, Percentage) %>%
  rename(response = Response, percentage = Percentage)

twitter_sentiments2 <- twitter_sentiments %>%
  select(state, week, sentiments, percentage) %>%
  rename(response = sentiments) %>%
  filter(week != 0)

twitter_sentiments_national <- twitter_sentiments_national %>%
  mutate(percentage = (count/sum)*100) %>%
  select(week, sentiments, percentage) %>%
  add_column(state = "National") %>%
  filter(week != 0) %>%
  rename(response = sentiments)

twitter_sentiments_total2 <- rbind(twitter_sentiments2, twitter_sentiments_national)

timeline_data2 <- rbind(twitter_sentiments_total2, covid_mental_health_longer_new)



## 
timeline_table2 <- timeline_data2 %>%
  pivot_wider(
    names_from = response,
    values_from = percentage
  ) %>%
  rename(Anxiety = anxiety_percentage, Depression = depression_percentage, "Covid Cases" = percentage_cases_state, "Covid Deaths" = percentage_deaths_state) %>%
  mutate_if(is.numeric, round, digits = 2)

timeline_graph2 <-timeline_table2 %>%
  pivot_longer(
    cols = c("anger":"Covid Deaths"),
    names_to = "response",
    values_to = "percentage"
  )

#write_csv(timeline_graph2, "timeline_graph2.csv", append = FALSE)

#write_csv(timeline_table2, "timeline_table2.csv", append = FALSE)




