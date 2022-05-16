# Author : Alex Bass
# Date : 10 October 2021
# Description : Generates figures and sends an email daily

#loading packages
library(tidyverse)
library(googlesheets4)
library(gargle)
library(lubridate)
library(zoo)
library(patchwork)
library(scales)
library(remotes)
library(emayili)
library(htmltools)
library(glue)

#fonts
library(showtext)
font_add_google("Cairo", family = "Cairo")
showtext_auto()

#getting encrypting functions
source("encrypt_functions.R")

#theme addon here
additional_theme <- 
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = element_text(family = "Cairo", size = 28),
    axis.ticks = element_blank(),
    plot.caption = element_text(size = 22, face = "italic", colour = "grey67"),#6
    plot.title = element_text(hjust = .5, size = 40, face = "bold"), #14
    plot.subtitle = element_text(hjust = .5, size = 30),#10
    axis.title.x = element_blank()
  )

#do auto authorization
#googlesheets4::gs4_deauth() Could use this function if sheet was public

json <- secret_read("encrypted_file.json")

googlesheets4::gs4_auth(path = rawToChar(json))

survey <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Ud9zguI-R_ipnoZTAJTfUEq2tT-uU-ugJAttpMxgqaU/edit#gid=1289407448")

survey <- survey[complete.cases(survey), ]
names(survey) <- c("time", "email", "state")

survey <- survey[!duplicated(survey[,c("email", "state")]),]

# Returns string without leading white space
trim.leading <- function (x)  sub("^\\s+", "", x)

# Returns string without trailing white space
trim.trailing <- function (x) sub("\\s+$", "", x)

survey$email <- trim.trailing(trim.leading(survey$email))


#data from usafacts.com
cases <- read.csv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv")
deaths <- read.csv("https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv")
vaccination <- read.csv("https://data.cdc.gov/api/views/rh2h-3yt2/rows.csv?accessType=DOWNLOAD")

#data wrangling
case_total <- cases %>% 
  pivot_longer(starts_with("X"), names_to = "date") %>% 
  select(State, date, value) %>% 
  mutate(date = ymd(gsub("^X", "", date))) %>% 
  group_by(date) %>% 
  summarise(value = sum(value)) %>% 
  mutate(
    value = value - dplyr::lag(value),
    rolling_average = zoo::rollmean(value, 7, fill = NA))

deaths_total <- deaths %>% 
  pivot_longer(starts_with("X"), names_to = "date") %>% 
  select(State, date, value) %>% 
  mutate(date = ymd(gsub("^X", "", date))) %>% 
  group_by(date) %>% 
  summarise(value = sum(value)) %>% 
  mutate(
    value = value - dplyr::lag(value),
    rolling_average = zoo::rollmean(value, 7, fill = NA))

states <- readxl::read_xlsx("states.xlsx")$`USPS Abbreviation`

vax_total <- vaccination %>% 
  filter(date_type == "Report",
         Location %in% states) %>% 
  mutate(Date = mdy(Date)) %>% 
  group_by(Date) %>% 
  summarise(vax = sum(Series_Complete_Day_Rolling_Average),
            one_dose = sum(Admin_Dose_1_Cumulative),
            vax_daily = sum(Series_Complete_Cumulative)) %>% 
  mutate(vax_prop = vax_daily/332859432,
         one_dose_prop = one_dose/332859432)

case_by_state <- cases %>% 
  pivot_longer(starts_with("X"), names_to = "date") %>% 
  select(State, date, value) %>% 
  mutate(date = ymd(gsub("^X", "", date))) %>% 
  group_by(State, date) %>% 
  summarise(value = sum(value)) %>% 
  mutate(
    value = value - dplyr::lag(value),
    rolling_average = zoo::rollmean(value, 7, fill = NA))

state_data <- readxl::read_xlsx("states.xlsx")[,c(2,4)]
names(state_data) <- c("State", "Population")

#state
vax_state <- vaccination %>% 
  filter(date_type == "Report",
         Location %in% states) %>% 
  rename(State = Location) %>% 
  left_join(state_data, by = "State") %>% 
  mutate(Date = mdy(Date)) %>% 
  group_by(State, Date) %>% 
  summarise(vax = sum(Series_Complete_Day_Rolling_Average),
            one_dose = sum(Admin_Dose_1_Cumulative),
            vax_daily = sum(Series_Complete_Cumulative),
            Population = Population) %>% 
  mutate(vax_prop = vax_daily/Population,
         one_dose_prop = one_dose/Population)

death_by_state <- deaths %>% 
  pivot_longer(starts_with("X"), names_to = "date") %>% 
  select(State, date, value) %>% 
  mutate(date = ymd(gsub("^X", "", date))) %>% 
  group_by(State, date) %>% 
  summarise(value = sum(value)) %>% 
  mutate(
    value = value - dplyr::lag(value),
    rolling_average = zoo::rollmean(value, 7, fill = NA))

starting <- ymd(Sys.Date())-7
ending <- ymd(Sys.Date())

date_interval <- starting %--% ending

deaths_today <- deaths_total %>% 
  filter(date %within% date_interval) %>% 
  summarise(total = sum(value))

cases_today <- case_total %>% 
  filter(date %within% date_interval) %>% 
  summarise(total = sum(value))

vax_today <- vax_total %>% 
  filter(Date %within% date_interval) %>% 
  top_n(vax_prop,n=1) %>% 
  select(vax_prop)

one_dose_today <- vax_total %>% 
  filter(Date %within% date_interval) %>% 
  top_n(one_dose_prop,n=1) %>% 
  select(one_dose_prop)
  
deaths_total

#generate figures for states
p1 <- deaths_total %>%
  ggplot(aes(date, rolling_average)) +
  geom_area() +
  labs(title = "National: Deaths Over Time", y = "7-Day Average",
       caption = "Source: USAFacts", 
       subtitle = paste("total recorded deaths in last 7 days:", 
                        deaths_today$total))+
  scale_x_date(date_labels = "%b %d") +
  additional_theme

p2 <- case_total %>%
  ggplot(aes(date, rolling_average)) +
  geom_area(fill = "DodgerBlue4") +
  labs(title = "National: Cases Over Time", y = "7-Day Average",
       caption = "Source: USAFacts", 
       subtitle = paste("total recorded cases in last 7 days:", 
                        cases_today$total))+
  scale_x_date(date_labels = "%b %d") +
  scale_y_continuous(
    labels = scales::number_format(
      accuracy = 1
    )
  ) +
  additional_theme

p3 <- vax_total %>% 
  ggplot(aes(x = Date)) +
  geom_area(aes(y = one_dose_prop), fill = "darkorchid4", alpha = 0.7) +
  geom_area(aes(y = vax_prop), fill = "deeppink3", alpha = 0.7) +
  labs(title = "National: Vaccinations Over Time", y = "Cumulative Percent",
       caption = "Source: CDC", 
       subtitle = "Compared to entire US population of 332,859,432 people") +
  annotate(geom = "curve", xend = ymd(Sys.Date())-1, yend = one_dose_today$one_dose_prop,
           curvature = -0.3, arrow = arrow(length = unit(2, "mm")),
           x = ymd(Sys.Date())-30, y = one_dose_today$one_dose_prop +.3) +
  annotate(geom = "text", x = ymd(Sys.Date())-33, 
           y = one_dose_today$one_dose_prop +.3, 
           label = paste0(100*round(one_dose_today$one_dose_prop, 3), "% recieved just one dose"),
           hjust = "right", family = "Cairo", color = "purple", size = 12) +
  annotate(geom = "curve", xend = ymd(Sys.Date())-1, yend = vax_today$vax_prop,
           curvature = -0.3, arrow = arrow(length = unit(2, "mm")),
           x = ymd(Sys.Date())-60, y = vax_today$vax_prop +.2) +
  annotate(geom = "text", x = ymd(Sys.Date())-63, 
           y = vax_today$vax_prop +.2, 
           label = paste0(100*round(vax_today$vax_prop, 3), "% fully vaccinated"),
           hjust = "right", family = "Cairo", color = "deeppink3", size = 12) +
  scale_x_date(date_labels = "%b %d") +
  additional_theme +
  scale_y_continuous(limits = c(0,1), labels = scales::percent)

national_vax <- 100*round(vax_today$vax_prop, 3)

patchwork <- p1 / p2 / p3
patchwork + plot_annotation(
  title = paste0('NATIONAL COVID update for ', format(Sys.Date(), "%b %d, %Y") )) & 
  theme(plot.title = element_text(family = "Cairo", face = "bold", size = 42))

ggsave("national.png", units = "in", width = 6.92, height = 6.85)

starting <- ymd(Sys.Date())-12
ending <- ymd(Sys.Date())-5

date_interval <- starting %--% ending

insert_in_email <- as.character(case_total %>% 
                                  filter(date %within% date_interval) %>% 
                                  mutate(change = rolling_average - dplyr::lag(rolling_average)) %>% 
                                  filter(!is.na(change)) %>% 
                                  mutate(change2 = ifelse(change>200, "increasing", 
                                                          ifelse(change<-200, "decreasing", "staying about the same"))) %>% 
                                  count(change2) %>% arrange(-n) %>% top_n(n = 1) %>% select(change2))

if (length(insert_in_email)>1){
  insert_in_email <- "staying about the same"
}

starting <- ymd(Sys.Date())-7
ending <- ymd(Sys.Date())

date_interval <- starting %--% ending

for (email_state in unique(survey$state)) {
  deaths_today <- death_by_state %>%
  filter(date %within% date_interval, State == email_state) %>%
  summarise(total = sum(value))

  cases_today <- case_by_state %>%
    filter(date %within% date_interval, State == email_state) %>%
    summarise(total = sum(value))

  vax_today <- vax_state %>%
    filter(Date %within% date_interval, State == email_state) %>%
    top_n(vax_prop,n=1) %>%
    select(vax_prop)

  one_dose_today <- vax_state %>%
    filter(Date %within% date_interval, State == email_state) %>%
    top_n(one_dose_prop,n=1) %>%
    select(one_dose_prop)


  #generate figures for states
  p1 <- death_by_state %>%
    filter(State == email_state) %>%
    ggplot(aes(date, rolling_average)) +
    geom_area() +
    labs(title = paste0(email_state,": Deaths Over Time"), y = "7-Day Average",
         caption = "Source: USAFacts",
         subtitle = paste("total recorded deaths in last 7 days:",
                          deaths_today$total))+
    scale_x_date(date_labels = "%b %d") +
    additional_theme

  p2 <- case_by_state %>%
    filter(State == email_state) %>%
    ggplot(aes(date, rolling_average)) +
    geom_area(fill = "DodgerBlue4") +
    labs(title = paste0(email_state,": Cases Over Time"), y = "7-Day Average",
         caption = "Source: USAFacts",
         subtitle = paste("total recorded cases in last 7 days:",
                          cases_today$total))+
    scale_x_date(date_labels = "%b %d") +
    scale_y_continuous(
      labels = scales::number_format(
        accuracy = 1
      )
    ) +
    additional_theme

  p3 <- vax_state %>%
    filter(State == email_state) %>%
    ggplot(aes(x = Date)) +
    geom_area(aes(y = one_dose_prop), fill = "darkorchid4", alpha = 0.7) +
    geom_area(aes(y = vax_prop), fill = "deeppink3", alpha = 0.7) +
    labs(title = paste0(email_state,": Vaccinations Over Time"), y = "Cumulative Percent",
         caption = "Source: CDC",
         subtitle = paste0("Compared to entire ", email_state, " population of ", state_data[state_data$State == email_state,]$Population, " people")) +
    annotate(geom = "curve", xend = ymd(Sys.Date())-1, yend = one_dose_today$one_dose_prop,
             curvature = -0.3, arrow = arrow(length = unit(2, "mm")),
             x = ymd(Sys.Date())-30, y = one_dose_today$one_dose_prop +.3) +
    annotate(geom = "text", x = ymd(Sys.Date())-33,
             y = one_dose_today$one_dose_prop +.3,
             label = paste0(100*round(one_dose_today$one_dose_prop, 3), "% recieved just one dose"),
             hjust = "right", family = "Cairo", color = "purple", size = 12) +
    annotate(geom = "curve", xend = ymd(Sys.Date())-1, yend = vax_today$vax_prop,
             curvature = -0.3, arrow = arrow(length = unit(2, "mm")),
             x = ymd(Sys.Date())-60, y = vax_today$vax_prop +.2) +
    annotate(geom = "text", x = ymd(Sys.Date())-63,
             y = vax_today$vax_prop +.2,
             label = paste0(100*round(vax_today$vax_prop, 3), "% fully vaccinated"),
             hjust = "right", family = "Cairo", color = "deeppink3", size = 12) +
      scale_x_date(date_labels = "%b %d") +
    additional_theme +
    scale_y_continuous(limits = c(0,1), labels = scales::percent)


  patchwork <- p1 / p2 / p3
  patchwork + plot_annotation(
    title = paste0(email_state,' COVID update for ', format(Sys.Date(), "%b %d, %Y") )) &
    theme(plot.title = element_text(family = "Cairo", face = "bold", size = 42))

  ggsave("state.png", units = "in", width = 6.92, height = 6.85)

  state_text <- paste0("\n")

  for(emails in survey[survey$state==email_state,]$email){
    person_name <- gsub("@.+", "", emails)

    email <- emayili::envelope(
      from = "covidemailupdate@gmail.com",
      subject = "Weekly COVID Update:)",
    ) %>%
      emayili::to(emails) %>%
      emayili::text(paste0("Hi ",person_name,"!\n\nHere is your weekly check in:\n\n","   -", "This week, cases are ", toupper(insert_in_email), " nationally.\n","   -The national full vaccination rate is at ",national_vax,"%.\n","   -The full vaccination rate in ", email_state, " is ", paste0(100*round(vax_today$vax_prop, 3), "%.\n\n"))) %>%
      emayili::text("\n\nCheck out the 2 images below for more COVID info. If you are still left wanting, USAfacts.com might help. Unsubscribe by replying. \n\nHope you have a great Saturday:)\n\nAlex") %>%
      emayili::attachment(path = "national.png") %>%
      emayili::attachment(path = "state.png")

    smtp <- emayili::gmail(
      username = "covidemailupdate@gmail.com",
      password = Sys.getenv('SECRETS')
    )

    smtp(email, verbose = FALSE)
  }
}
