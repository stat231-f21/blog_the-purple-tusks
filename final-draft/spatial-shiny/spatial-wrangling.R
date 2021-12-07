
  

# COVID-19 data wrangling



library(dplyr)
library(readr)
library(grid)



# data from the NY Times
uscovid <- read_csv("../data/covid/uscovid.csv")

# filtering specific dates from my uscovid dataset in accordance with the mental health data which we already have splitted in terms of weeks
us_week1 <- filter(uscovid, date >= as.Date("2020-04-23"), date <= as.Date("2020-05-05")) %>%
  mutate(week = 1)
us_week2 <- filter(uscovid, date >= as.Date("2020-05-07"), date <= as.Date("2020-05-12"))%>%
  mutate(week = 2)
us_week3 <- filter(uscovid, date >= as.Date("2020-05-14"), date <= as.Date("2020-05-19"))%>%
  mutate(week = 3)
us_week4 <- filter(uscovid, date >= as.Date("2020-05-21"), date <= as.Date("2020-05-26"))%>%
  mutate(week = 4)
us_week5 <- filter(uscovid, date >= as.Date("2020-05-28"), date <= as.Date("2020-06-02"))%>%
  mutate(week = 5)
us_week6 <- filter(uscovid, date >= as.Date("2020-06-04"), date <= as.Date("2020-06-09"))%>%
  mutate(week = 6)
us_week7 <- filter(uscovid, date >= as.Date("2020-06-11"), date <= as.Date("2020-06-16"))%>%
  mutate(week = 7)
us_week8 <- filter(uscovid, date >= as.Date("2020-06-18"), date <= as.Date("2020-06-23"))%>%
  mutate(week = 8)
us_week9 <- filter(uscovid, date >= as.Date("2020-06-25"), date <= as.Date("2020-06-30")) %>%
  mutate(week = 9)
us_week10 <- filter(uscovid, date >= as.Date("2020-07-02"), date <= as.Date("2020-07-07"))%>%
  mutate(week = 10)
us_week11 <- filter(uscovid, date >= as.Date("2020-07-09"), date <= as.Date("2020-07-14"))%>%
  mutate(week = 11)
us_week12 <- filter(uscovid, date >= as.Date("2020-07-16"), date <= as.Date("2020-07-21"))%>%
  mutate(week = 12)
us_week13 <- filter(uscovid, date >= as.Date("2020-08-19"), date <= as.Date("2020-08-31"))%>%
  mutate(week = 13)
us_week14 <- filter(uscovid, date >= as.Date("2020-09-02"), date <= as.Date("2020-09-14"))%>%
  mutate(week = 14)
us_week15 <- filter(uscovid, date >= as.Date("2020-09-16"), date <= as.Date("2020-09-28")) %>%
  mutate(week = 15)
us_week16 <- filter(uscovid, date >= as.Date("2020-09-30"), date <= as.Date("2020-10-12")) %>%
  mutate(week = 16)
us_week17 <- filter(uscovid, date >= as.Date("2020-10-14"), date <= as.Date("2020-10-26"))%>%
  mutate(week = 17)
us_week18 <- filter(uscovid, date >= as.Date("2020-10-28"), date <= as.Date("2020-11-09"))%>%
  mutate(week = 18)
us_week19 <- filter(uscovid, date >= as.Date("2020-11-11"), date <= as.Date("2020-11-23"))%>%
  mutate(week = 19)
us_week20 <- filter(uscovid, date >= as.Date("2020-11-25"), date <= as.Date("2020-12-07"))%>%
  mutate(week = 20)
us_week21 <- filter(uscovid, date >= as.Date("2020-12-09"), date <= as.Date("2020-12-21"))%>%
  mutate(week = 21)
us_week22 <- filter(uscovid, date >= as.Date("2021-01-06"), date <= as.Date("2021-01-18"))%>%
  mutate(week = 22)
us_week23 <- filter(uscovid, date >= as.Date("2021-01-20"), date <= as.Date("2021-02-01"))%>%
  mutate(week = 23)
us_week24 <- filter(uscovid, date >= as.Date("2021-02-03"), date <= as.Date("2021-02-15"))%>%
  mutate(week = 24)
us_week25 <- filter(uscovid, date >= as.Date("2021-02-17"), date <= as.Date("2021-03-01"))%>%
  mutate(week = 25)
us_week26 <- filter(uscovid, date >= as.Date("2021-03-03"), date <= as.Date("2021-03-15"))%>%
  mutate(week = 26)
us_week27 <- filter(uscovid, date >= as.Date("2021-03-17"), date <= as.Date("2021-03-29")) %>%
  mutate(week = 27) 
us_week28 <- filter(uscovid, date >= as.Date("2021-04-14"), date <= as.Date("2021-04-26")) %>%
  mutate(week = 28)
us_week29 <- filter(uscovid, date >= as.Date("2021-04-28"), date <= as.Date("2021-05-10")) %>%
  mutate(week = 29) 
us_week30 <- filter(uscovid, date >= as.Date("2021-05-12"), date <= as.Date("2021-05-24")) %>%
  mutate(week = 30)
us_week31 <- filter(uscovid, date >= as.Date("2021-05-26"), date <= as.Date("2021-06-07")) %>%
  mutate(week = 31)
us_week32 <- filter(uscovid, date >= as.Date("2021-06-09"), date <= as.Date("2021-06-21")) %>%
  mutate(week = 32)
us_week33 <- filter(uscovid, date >= as.Date("2021-06-23"), date <= as.Date("2021-07-05")) %>%
  mutate(week = 33) 
us_week34 <- filter(uscovid, date >= as.Date("2021-07-21"), date <= as.Date("2021-08-02")) %>%
  mutate(week = 34)
us_week35 <- filter(uscovid, date >= as.Date("2021-08-04"), date <= as.Date("2021-08-16")) %>%
  mutate(week = 35)
us_week36 <- filter(uscovid, date >= as.Date("2021-08-18"), date <= as.Date("2021-08-30")) %>%
  mutate(week = 36) 
us_week37 <- filter(uscovid, date >= as.Date("2021-09-01"), date <= as.Date("2021-09-13")) %>%
  mutate(week = 37)
us_week38 <- filter(uscovid, date >= as.Date("2021-09-15"), date <= as.Date("2021-09-27")) %>%
  mutate(week = 38)
us_week39 <- filter(uscovid, date >= as.Date("2021-09-29"), date <= as.Date("2021-10-11")) %>%
  mutate(week = 39)

# combining all the data needed with specific notion of weeks
us <- rbind(us_week1,us_week2,us_week3,us_week4,us_week5,us_week6,us_week7,us_week8,us_week9,us_week10,us_week11,us_week12,us_week13,us_week14,us_week15,us_week16,us_week17,us_week18,us_week19,us_week20,us_week21,us_week22,us_week23,us_week24,us_week25,us_week26,us_week27,us_week28,us_week29,us_week30,us_week31,us_week32,us_week33,us_week34,us_week35,us_week36,us_week37,us_week38,us_week39)

# creating a new dataset which has total cases/deaths of each state by weeks
state_total_by_week <- us %>%
  group_by(state, week) %>%
  summarize(total_cases = sum(cases), total_deaths = sum(deaths))

# creating a new dataset which has total cases/deaths of the entire nation by weeks
national_total_by_week <- us %>%
  group_by(week) %>%
  summarize(total_cases_us = sum(cases), total_deaths_us = sum(deaths)) %>%
  mutate(state = "National") %>%
  rename("total_cases" = total_cases_us,"total_deaths" = total_deaths_us)

# binding to have a notion of national
final_state_total_by_week <- rbind(state_total_by_week,national_total_by_week)

# totaling us by state
us_total_by_state <- us %>%
  group_by(state) %>%
  summarize(total_cases_state_all_weeks = sum(cases), total_deaths_state_all_weeks = sum(deaths)) 

# adding a sense of national
us_total_by_state <- us_total_by_state %>%
  add_row(state="National",
          total_cases_state_all_weeks=sum(us_total_by_state$total_cases_state_all_weeks),
          total_deaths_state_all_weeks=sum(us_total_by_state$total_deaths_state_all_weeks))

# joined data for spatial
us_joined_spatial <- inner_join(final_state_total_by_week,us_total_by_week,by = "week")

# joined data for bar
us_joined_bar <- inner_join(us_joined_spatial,us_total_by_state,by = "state")

# adding the respective percentages
covid_us_final <- us_joined_bar %>% 
  mutate(percentage_cases_spatial = (total_cases/total_cases_us) *100,
         percentage_deaths_spatial = (total_deaths/total_deaths_us) *100,
         percentage_cases_bar = (total_cases/total_cases_state_all_weeks) *100,
         percentage_deaths_bar = (total_deaths/total_deaths_state_all_weeks) *100
  )

#write.csv(covid_us_final, "covid_updated.csv")
mental_health <- read_csv("../data/mental-health/mental_health.csv")
mental_health_spatial <- mental_health %>%
  rename(week = WEEK, state = State)

mental_health_spatial$state <- tolower(mental_health_spatial$state)
