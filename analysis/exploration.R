################################################################################
#            Influence of School Closure on COVID-19 Contaminations 
#                     and Repercussions across Age Groups  
################################################################################

# references -------------------------------------------------------------------
## ts causality
"https://towardsdatascience.com/inferring-causality-in-time-series-data-b8b75fe52c46"
"https://jofam.github.io/covidBE_analysis/Overdispersion.html"
## school closure
"https://www.nature.com/articles/s41591-021-01571-8"
"https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8165592/pdf/rstb.2020.0277.pdf"
"https://www.ijidonline.com/article/S1201-9712(20)30598-1/fulltext"
"https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0239455"

# libraries --------------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(zoo)
library(mgcv)
library(gratia)
# variables --------------------------------------------------------------------
age_order <- c("aged1to4", "aged5to14", "aged15to24", "aged25to34", "aged35to44", "aged45to54", "aged55to64", "aged65to74", "aged75to84", "aged85up")
age_name  <- c("1 to 4",   "5 to 14",   "15 to 24",   "25 to 34",   "35 to 44",   "45 to 54",   "55 to 64",   "65 to 74",   "75 to 84",   "85 and up")
age_color <- c("#999999",  "#E69F00",   "#56B4E9",    "#009E73",    "#F0E442",    "#0072B2",    "#D55E00",    "#CC79A7",    "#000000",    "#CAB2D6")

# data -------------------------------------------------------------------------
df <- 
  read_csv(
    here("data/age_groups.csv"), 
    col_types = list(
      .default = col_double(), 
      Date = col_date(format = "%d/%m/%Y")
    )
  ) |> 
  clean_names() |> 
  rename(total_cases = total) |> 
  arrange(date) |> 
  mutate(timeline = date - lag(date)) # check if gap between dates

df_school <- df |> 
  select(date, schools_closed) |> 
  mutate(schools_closure_day = schools_closed - lag(schools_closed)) |> 
  filter(schools_closure_day == 1) |> 
  rowid_to_column("schools_closure_wave") |> 
  full_join(df, by = c("date", "schools_closed")) |> 
  arrange(date) |> 
  fill(schools_closure_wave, .direction = "down") |> 
  mutate(schools_closure_wave = replace_na(schools_closure_wave, 0)) |> 
  group_by(schools_closure_wave) |> 
  mutate(schools_time_since_closure = row_number()) |> 
  ungroup()

df_long <- df |> 
  select(c(date, starts_with("aged"))) |> 
  pivot_longer(-date, names_to = "age_group", values_to = "covid_cases") |> 
  mutate(age_group = factor(age_group, levels = age_order)) |> 
  group_by(age_group) |> 
  mutate(covid_7davg = rollmean(covid_cases, k = 7, fill = NA)) |> 
  ungroup()

df_school |> 
  filter(schools_closed == 1) |> 
  count(schools_closure_wave) |> 
  rename(schools_closure_wave_n = n)

df_school_long <- df_school |> 
  filter(schools_closed == 1) |> 
  count(schools_closure_wave) |> 
  rename(schools_closure_wave_n = n) |> 
  right_join(df_school, by = "schools_closure_wave") |> 
  filter(schools_closure_wave_n > 20 & schools_time_since_closure < 40) |> # keep only closure waves longer than 14 days but display only the first 20 days
  pivot_longer(c("aged1to4", "aged5to14", "aged15to24"), names_to = "age_group", values_to = "covid_cases") |> 
  mutate(
    schools_closure_wave = factor(schools_closure_wave),
    age_group = factor(age_group, levels = c("aged1to4", "aged5to14", "aged15to24"))
  ) |> 
  group_by(age_group) |> 
  mutate(covid_changes = covid_cases - lag(covid_cases)) |> 
  ungroup()

df_school_long |> 
  filter(schools_closure_day == 1 & age_group == "aged1to4") |> 
  select(date, schools_closure_wave, schools_closure_wave_n)

# visual exploration -----------------------------------------------------------
acf(df$aged1to4, na.action = na.pass)
acf(df$aged5to14, na.action = na.pass)
acf(df$aged15to24, na.action = na.pass)

df |> 
  ggplot(aes(total_cases)) +
  geom_density(na.rm = TRUE) +
  labs(
    title = "Distribution of COVID-19 Daily Cases in Ireland",
    subtitle = "",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Covid Cases", 
    y = "Density"
  ) +
  theme_bw()

df |> 
  ggplot(aes(date, total_cases)) +
  geom_tile(aes(fill = factor(schools_closed), height = Inf), na.rm = TRUE, alpha = .4) +
  geom_line(na.rm = TRUE) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  scale_fill_manual(labels = c("Open", "Closed", "Na"), values = c("light blue", "red", "orange")) +
  labs(
    title = "Evolution of COVID-19 Cases in Ireland and Impact of School Closures",
    subtitle = "Raw data",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Date", 
    y = "Covid Cases",
    fill = "Schools"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

df_long |> 
  ggplot(aes(date, covid_cases, color = age_group)) +
  geom_line(na.rm = TRUE) +
  scale_x_date("Date") +
  scale_y_continuous("Covid Cases") +
  scale_color_manual(labels = age_name, values = age_color) +
  labs(
    title = "Evolution of COVID-19 Cases by Age Groups in Ireland",
    subtitle = "Raw data",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Date", 
    y = "Covid Cases",
    color = "Age Groups"
  ) +
  theme_bw()

df_long |> 
  ggplot(aes(date, covid_7davg, color = age_group)) +
  geom_line(na.rm = TRUE) +
  scale_color_manual(labels = age_name, values = age_color) +
  labs(
    title = "Evolution of COVID-19 Cases by Age Groups in Ireland",
    subtitle = "7-day rolling average",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Date", 
    y = "Covid Cases (7-day rolling average)",
    color = "Age Groups"
  ) +
  theme_bw()
  
df_school_long |> 
  ggplot(aes(schools_time_since_closure, covid_cases, color = age_group)) +
  geom_line() +
  scale_color_manual(labels = c("aged1to4" = "1 to 4", "aged5to14" = "5 to 14", "aged15to24" = "15 to 24"), values = c("#999999",  "#E69F00",   "#56B4E9")) +
  facet_wrap(
    ~ schools_closure_wave, 
    ncol = 1, 
    scales = "free", 
    labeller = labeller(schools_closure_wave = c(
      `1` = "First School Closure", 
      `3` = "Third School Closure", 
      `4` = "Fourth School Closure", 
      `5` = "Fifth School Closure"))
  ) +
  labs(
    title = "Evolution of COVID-19 Cases in Age Groups up to 24, the First 20 Days since School Closures in Ireland",
    subtitle = "Raw data - Closure longer than 14 days only",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Days Since Closure", 
    y = "Covid Cases",
    color = "Age Groups"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

df_school_long |> 
  ggplot(aes(schools_time_since_closure, covid_changes, color = age_group)) +
  geom_line() +
  scale_color_manual(labels = c("aged1to4" = "1 to 4", "aged5to14" = "5 to 14", "aged15to24" = "15 to 24"), values = c("#999999",  "#E69F00",   "#56B4E9")) +
  facet_wrap(
    ~ schools_closure_wave, 
    ncol = 1, 
    scales = "free", 
    labeller = labeller(schools_closure_wave = c(
      `1` = "First School Closure", 
      `3` = "Third School Closure", 
      `4` = "Fourth School Closure", 
      `5` = "Fifth School Closure"))
  ) +
  labs(
    title = "Changes in day-by-day COVID-19 Cases in Age Groups up to 24, the First 20 Days since School Closures in Ireland",
    subtitle = "Raw data - Closure longer than 14 days only",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Days Since Closure", 
    y = "Day-by-day Changes in Covid Cases",
    color = "Age Groups"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# modelling --------------------------------------------------------------------
## gam #########################################################################ß
df_model <- 
  gam(
    covid_cases ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_school_long,
    family = nb(), 
    method = "REML"
  )
summary(df_model)
draw(df_model)
appraise(df_model)
k.check(df_model)

df_model <- 
  gam(
    covid_changes ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_school_long,
    method = "REML"
  )
summary(df_model)
draw(df_model)
appraise(df_model)
k.check(df_model)

# df_model <- 
#   gamm(
#     covid_cases ~ s(schools_time_since_closure, by = age_group) + age_group,
#     data = df_school_long,
#     random = list(schools_closure_wave =~ 1),
#     family = poisson, 
#     method = "REML"
#   )
# 
# summary(df_model$gam)



