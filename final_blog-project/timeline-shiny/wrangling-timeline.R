
# load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(formatR)
library(DT)
library(ggrepel)
library(csv)
library(ggplot2)


  ## create mental health data

# import weeks 1-39 csv files
week_1 <- read_csv("../data/mental-health/pulse2020_puf_01.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST)) %>%
  mutate(EST_ST = as.character(EST_ST))
week_2 <- read_csv("../data/mental-health/pulse2020_puf_02.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_3 <- read_csv("../data/mental-health/pulse2020_puf_03.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_4 <- read_csv("../data/mental-health/pulse2020_puf_04.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_5 <- read_csv("../data/mental-health/pulse2020_puf_05.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_6 <- read_csv("../data/mental-health/pulse2020_puf_06.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_7 <- read_csv("../data/mental-health/pulse2020_puf_07.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_8 <- read_csv("../data/mental-health/pulse2020_puf_08.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_9 <- read_csv("../data/mental-health/pulse2020_puf_09.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_10 <- read_csv("../data/mental-health/pulse2020_puf_10.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_11 <- read_csv("../data/mental-health/pulse2020_puf_11.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_12 <- read_csv("../data/mental-health/pulse2020_puf_12.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_13 <- read_csv("../data/mental-health/pulse2020_puf_13.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_14 <- read_csv("../data/mental-health/pulse2020_puf_14.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_15 <- read_csv("../data/mental-health/pulse2020_puf_15.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_16 <- read_csv("../data/mental-health/pulse2020_puf_16.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_17 <- read_csv("../data/mental-health/pulse2020_puf_17.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_18 <- read_csv("../data/mental-health/pulse2020_puf_18.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_19 <- read_csv("../data/mental-health/pulse2020_puf_19.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_20 <- read_csv("../data/mental-health/pulse2020_puf_20.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_21 <- read_csv("../data/mental-health/pulse2020_puf_21.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_22 <- read_csv("../data/mental-health/pulse2021_puf_22.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_23 <- read_csv("../data/mental-health/pulse2021_puf_23.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_24 <- read_csv("../data/mental-health/pulse2021_puf_24.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_25 <- read_csv("../data/mental-health/pulse2021_puf_25.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_26 <- read_csv("../data/mental-health/pulse2021_puf_26.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_27 <- read_csv("../data/mental-health/pulse2021_puf_27.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_28 <- read_csv("../data/mental-health/pulse2021_puf_28.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_29 <- read_csv("../data/mental-health/pulse2021_puf_29.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_30 <- haven::read_sas("../data/mental-health/pulse2021_puf_30.zip") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_31 <- read_csv("../data/mental-health/pulse2021_puf_31.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_32 <- read_csv("../data/mental-health/pulse2021_puf_32.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_33 <- read_csv("../data/mental-health/pulse2021_puf_33.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_34 <- read_csv("../data/mental-health/pulse2021_puf_34.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_35 <- read_csv("../data/mental-health/pulse2021_puf_35.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_36 <- read_csv("../data/mental-health/pulse2021_puf_36.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_37 <- read_csv("../data/mental-health/pulse2021_puf_37.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_38 <- read_csv("../data/mental-health/pulse2021_puf_38.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))
week_39 <- read_csv("../data/mental-health/pulse2021_puf_39.csv") %>%
  subset(select = c(WEEK, ANXIOUS, WORRY, INTEREST, DOWN, EST_ST))

# combine the weeks into the appropriate phases
phase1_mental_health <- bind_rows(week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8, week_9, week_10, week_11, week_12)

phase2_mental_health <- bind_rows(week_13, week_14, week_15, week_16, week_17)

phase3_mental_health <- bind_rows(week_18, week_19, week_20, week_21, week_22, week_23, week_24, week_25, week_26, week_27)

phase4_mental_health <- bind_rows(week_28, week_29, week_30, week_31, week_32, week_33, week_34, week_35, week_36, week_37, week_38, week_39)

# assign negative numbers as 0 because they are null
phase1_mental_health[phase1_mental_health == -99] <- 0
phase1_mental_health[phase1_mental_health == -88] <- 0
phase2_mental_health[phase2_mental_health == -99] <- 0
phase2_mental_health[phase2_mental_health == -88] <- 0
phase3_mental_health[phase3_mental_health == -99] <- 0
phase3_mental_health[phase3_mental_health == -88] <- 0
phase4_mental_health[phase4_mental_health == -99] <- 0
phase4_mental_health[phase4_mental_health == -88] <- 0

# combine all phases into a total phases data
total_phases <- bind_rows(phase1_mental_health, phase2_mental_health, phase3_mental_health, phase4_mental_health)

# Context:
# For each scale, the answers are assigned a numerical value: not at all = 1, several days = 2, more than half the days = 3, and nearly every day = 4. The two responses for each scale are added together. 
# A sum equal to four or greater has been shown to be associated with diagnoses of generalized anxiety/major depressive disorder.

# 2 is the lowest possible score
# 8 is highest possible score
# 0 means the data is not reported/missing

# create `ANXIETY_SCORE` and `DEPRESSION_SCORE` by adding `ANXIOUS` and `WORRY` and by adding `INTERESET` and `DOWN`, respectively
# create a new column called ANXIETY_ASSOCIATED according to the score
# a value of 1 indicates `ANXIETY_SCORE` of four or greater 
# a value of 0 indicates `ANXIETY_SCORE` of less than four
# create a new column called DEPRESSION_ASSOCIATED according to the score
# a value of 1 indicates `DEPRESSION_SCORE` of four or greater 
# a value of 0 indicates `DEPRESSION_SCORE` of less than four

# calculate anxiety and depression scores
total_mental_health <- total_phases %>%
  mutate(ANXIETY_SCORE = ANXIOUS + WORRY, 
         DEPRESSION_SCORE = INTEREST + DOWN,
         ANXIETY_ASSOCIATED = ifelse(ANXIETY_SCORE > 3, 1, 0),
         DEPRESSION_ASSOCIATED = ifelse(DEPRESSION_SCORE > 3, 1, 0))

# add column with `EST_ST`
states <- averages %>%
  rename(Anxiety_score = "Anxiety Score", Depression_score = "Depression Score") %>%
  subset.data.frame(select = -c(Income, Anxious, Worry, Interest, Down, Anxiety_score, Depression_score)) %>%
  add_column(EST_ST = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56))

# calculate count by states
mental_health_new <- total_mental_health %>%
  mutate(EST_ST = as.numeric(EST_ST)) %>%
  group_by(EST_ST, WEEK) %>%
  summarize(count = n())

mental_health_new2 <- total_mental_health %>%
  mutate(EST_ST = as.numeric(EST_ST)) %>%
  group_by(EST_ST, WEEK) %>%
  arrange(EST_ST)

# total phase data with `EST_ST` and scores
final_mental_health <- inner_join(mental_health_new, mental_health_new2, by = c("EST_ST", "WEEK"))

# calculate percentages by states
new_mental_health <- final_mental_health %>%
  mutate(anxiety_percentage = sum(ANXIETY_ASSOCIATED)/count, depression_percentage = sum(DEPRESSION_ASSOCIATED)/count) %>%
  subset(select = c(EST_ST, WEEK, count, anxiety_percentage, depression_percentage))

new_mental_health <- unique(new_mental_health)

new_mental_health <- inner_join(states, new_mental_health, by = "EST_ST")

# calculate count nationally
mental_health_national <- total_mental_health %>%
  mutate(EST_ST = as.numeric(EST_ST)) %>%
  group_by(WEEK) %>%
  summarize(count = n()) %>%
  mutate(count = as.numeric(count))

mental_health_national2 <- total_mental_health %>%
  mutate(EST_ST = as.numeric(EST_ST)) %>%
  group_by(WEEK) %>%
  arrange(WEEK)

final_mental_health_national <- inner_join(mental_health_national, mental_health_national2, by = c("WEEK")) 

final_mental_health_national <- final_mental_health_national %>%
  subset(select = -c(EST_ST)) %>%
  add_column(EST_ST = 0)

# calculate national percentages
new_mental_health_national <- final_mental_health_national %>%
  mutate(anxiety_percentage = sum(ANXIETY_ASSOCIATED)/count, depression_percentage = sum(DEPRESSION_ASSOCIATED)/count) %>%
  subset(select = c(EST_ST, WEEK, count, anxiety_percentage, depression_percentage))

new_mental_health_national <- unique(new_mental_health_national)

new_mental_health_national <- new_mental_health_national %>%
  add_column(State = "National")

# combine state and national percentages
mental_health <- rbind(new_mental_health, new_mental_health_national)

#write_csv(mental_health, "data/mental-health/mental_health.csv", append = FALSE)


  ## create covid and mental combined data

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

  ## create twitter sentiment data

twitter_sentiments <- read_csv2("../data/textual/twitter_sentiments.csv")
twitter_sentiments_national <- read_csv2("../data/textual/ntl_twitter_sentiments.csv")

covid_mental_health_longer_new <- covid_mental_health_longer %>%
  select(state, week, Response, Percentage) %>%
  rename(response = Response, percentage = Percentage)

# data for states
twitter_sentiments <- twitter_sentiments %>%
  select(state, week, sentiments, percentage_of_total_sentiment) %>%
  rename(response = sentiments, percentage = percentage_of_total_sentiment) %>%
  filter(week != 0)

# data for nation
twitter_sentiments_national <- twitter_sentiments_national %>%
  mutate(percentage = (count/sum)*100) %>%
  select(week, sentiments, percentage) %>%
  add_column(state = "National") %>%
  filter(week != 0) %>%
  rename(response = sentiments)

# combine state and nation data
twitter_sentiments_total <- rbind(twitter_sentiments, twitter_sentiments_national)

#write_csv(twitter_sentiments_total, "twitter_sentiments_total.csv", append = FALSE)

timeline_data <- rbind(twitter_sentiments_total, covid_mental_health_longer_new)




  ## create timeline graph data

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



  ## create timeline graph2 and table2 data

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




