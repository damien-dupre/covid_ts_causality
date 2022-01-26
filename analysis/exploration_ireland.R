
# libraries --------------------------------------------------------------------
library(here)
library(httr2)
library(gratia)
library(janitor)
library(magrittr)
library(mgcv)
library(tidyverse)
library(zoo)

# api access -------------------------------------------------------------------
req <- request("https://services1.arcgis.com/eNO7HHeQ3rUcBllm/arcgis/rest/services/CovidStatisticsProfileHPSCIrelandOpenData/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json") |> 
  req_headers("Accept" = "application/json")
resp <- req_perform(req) |> 
  resp_body_json() |> 
  use_series(features) |> 
  flatten_dfr() |> 
  clean_names() |> 
  mutate(date = as.Date(as.POSIXct(date/1000, origin = "1970-01-01"))) |> 
  select(c(date, starts_with("aged")))

# variables --------------------------------------------------------------------
age_order <- c("aged1to4", "aged5to14", "aged15to24", "aged25to34", "aged35to44", "aged45to54", "aged55to64", "aged65to74", "aged75to84", "aged85up")
age_name  <- c("1 to 4",   "5 to 14",   "15 to 24",   "25 to 34",   "35 to 44",   "45 to 54",   "55 to 64",   "65 to 74",   "75 to 84",   "85 and up")
age_color <- c("#999999",  "#E69F00",   "#56B4E9",    "#009E73",    "#F0E442",    "#0072B2",    "#D55E00",    "#CC79A7",    "#000000",    "#CAB2D6")

# data -------------------------------------------------------------------------
df_school_closure <- read_csv(
  "https://en.unesco.org/sites/default/files/covid_impact_education.csv", 
  col_types = list(.default = col_character(), Date = col_date(format = "%d/%m/%Y"))) |> 
  clean_names() |>
  select(date, country, status) |> 
  filter(country == "Ireland")

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
  mutate(
    timeline = date - lag(date),  # check if gap between dates
    total_changes = total_cases - lag(total_cases)
  ) |> 
  left_join(df_school_closure, by = "date") |> 
  fill(status) |> 
  mutate(schools_closed = case_when(
    status == "Fully open" ~ 0,
    status != "Fully open" ~ 1,
    TRUE ~ NA_real_
  ))

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
  mutate(
    covid_cases_7davg = rollmean(covid_cases, k = 7, fill = NA),
    covid_changes = covid_cases - lag(covid_cases),
    covid_changes_7davg = rollmean(covid_changes, k = 7, fill = NA)
  ) |> 
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
  filter(schools_closure_wave_n > 20 & schools_time_since_closure < 40) |> # keep only closure waves longer than 20 days but display only the first 39 days
  pivot_longer(c("aged1to4", "aged5to14", "aged15to24"), names_to = "age_group", values_to = "covid_cases") |> 
  mutate(
    schools_closure_wave = factor(schools_closure_wave),
    age_group = factor(age_group, levels = c("aged1to4", "aged5to14", "aged15to24"))
  ) |> 
  group_by(age_group) |> 
  arrange(date) |> 
  mutate(
    covid_changes = covid_cases - lag(covid_cases),
    covid_cases_7davg = rollmean(covid_cases, k = 7, fill = NA),
    covid_changes_7davg = rollmean(covid_changes, k = 7, fill = NA)
  ) |> 
  ungroup()

df_school_long |> 
  filter(schools_closure_day == 1 & age_group == "aged1to4") |> 
  select(date, schools_closure_wave, schools_closure_wave_n)

df_wide_changes <- df_long |> 
  select(date, age_group, covid_changes) |> 
  pivot_wider(names_from = age_group, values_from = covid_changes) |> 
  drop_na()

df_long_changes <- df_wide_changes |> 
  pivot_longer(c("aged1to4", "aged5to14", "aged15to24"), names_to = "age_group", values_to = "covid_changes_cases") |> 
  left_join(df_school_long |> select(date, schools_time_since_closure, schools_closure_wave, schools_closure_wave_n), by = "date") |> 
  filter(schools_closure_wave_n > 20 & schools_time_since_closure < 40) |> 
  mutate(
    schools_closure_wave = factor(schools_closure_wave),
    age_group = factor(age_group, levels = c("aged1to4", "aged5to14", "aged15to24"))
  )

df_wide_7davg <- df_long |> 
  select(date, age_group, covid_changes_7davg) |> 
  pivot_wider(names_from = age_group, values_from = covid_changes_7davg) |> 
  drop_na() |> 
  left_join(df_school |> select(!starts_with("aged")), by = "date")

df_long_7davg <- df_wide_7davg |> 
  left_join(df_school_long |> select(date, schools_closure_wave_n), by = "date") |> 
  filter(schools_closure_wave_n > 20 & schools_time_since_closure < 40) |> # keep only closure waves longer than 20 days but display only the first 39 days
  pivot_longer(c("aged1to4", "aged5to14", "aged15to24"), names_to = "age_group", values_to = "covid_changes_7davg") |> 
  mutate(
    schools_closure_wave = factor(schools_closure_wave),
    age_group = factor(age_group, levels = c("aged1to4", "aged5to14", "aged15to24"))
  )
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

df |> 
  ggplot(aes(date, total_changes)) +
  geom_tile(aes(fill = factor(schools_closed), height = Inf), na.rm = TRUE, alpha = .4) +
  geom_line(na.rm = TRUE) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  scale_fill_manual(labels = c("Open", "Closed", "Na"), values = c("light blue", "red", "orange")) +
  labs(
    title = "Day-by-day Changes in COVID-19 Cases in Ireland and Impact of School Closures",
    subtitle = "Data Lag 1",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Date", 
    y = "Day-by-day Changes in Covid Cases",
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
  ggplot(aes(date, covid_changes, color = age_group)) +
  geom_line(na.rm = TRUE) +
  scale_x_date("Date") +
  scale_y_continuous("Day-by-day Changes in Covid Cases") +
  scale_color_manual(labels = age_name, values = age_color) +
  facet_grid(age_group ~ ., scales = "free") +
  labs(
    title = "Day-by-day Changes in COVID-19 Cases by Age Groups in Ireland",
    subtitle = "Data Lag 1",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Date", 
    y = "Covid Cases",
    color = "Age Groups"
  ) +
  theme_bw() +
  theme(legend.position = "none")

df_long |> 
  ggplot() +
  geom_col(aes(date, covid_changes), na.rm = TRUE) +
  geom_line(aes(date, covid_changes_7davg, color = age_group), na.rm = TRUE) +
  scale_x_date("Date") +
  scale_y_continuous("Day-by-day Changes in Covid Cases") +
  scale_color_manual(labels = age_name, values = age_color) +
  facet_grid(age_group ~ ., scales = "free") +
  labs(
    title = "Day-by-day Changes in COVID-19 Cases by Age Groups in Ireland",
    subtitle = "Raw Data (Bars) and 7 Day Rolling Average of Data Lag 1 (Lines)",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Date", 
    y = "Covid Cases",
    color = "Age Groups"
  ) +
  theme_bw() +
  theme(legend.position = "none")

df_long |> 
  ggplot(aes(date, covid_cases_7davg, color = age_group)) +
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
    title = "Evolution of COVID-19 Cases in Age Groups up to 24, the First 39 Days since School Closures in Ireland",
    subtitle = "Raw data - Closure longer than 20 days only",
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
    title = "Changes in day-by-day COVID-19 Cases in Age Groups up to 24, the First 39 Days since School Closures in Ireland",
    subtitle = "Data Lag 1 - Closure longer than 20 days only",
    caption = "Source: Irish Department of Health (last update 2022-01-01)",
    x = "Days Since Closure", 
    y = "Day-by-day Changes in Covid Cases",
    color = "Age Groups"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

df_wide_7davg <- df_long |> 
  select(date, age_group, covid_cases_7davg) |> 
  pivot_wider(names_from = age_group, values_from = covid_cases_7davg) |> 
  drop_na() |> 
  left_join(df_school |> select(!starts_with("aged")), by = "date")


# modelling --------------------------------------------------------------------
## Generalised Additive Model ##################################################
#99%
gam_model <- 
  gam(
    covid_cases_7davg ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_school_long,
    method = "REML",
    family = nb()
  ); summary(gam_model)

gam_results <- broom::tidy(gam_model) |> 
  mutate(
    term = c("s(date):1to4", "s(date):5to14", "s(date):15to24", "s(date*wave"),
    p.value = scales::pvalue(p.value, accuracy = 0.001, add_p = FALSE, prefix = c("< ", "= ", "> ")),
    across(where(is.numeric), round, 2),
    print = glue::glue("$\chi^2({edf}) = {statistic}$, $p {p.value}$")
  ) |> 
  select(term, print) |> 
  deframe()

gam_summary <- summary(gam_model)
scales::percent(gam_summary$dev.expl, accuracy = 0.1)
# 96%
df_model <- 
  gam(
    covid_cases ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_school_long,
    family = nb(), 
    method = "REML"
  ); summary(df_model)

# 65%
df_model <- 
  gam(
    covid_changes_7davg ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_long_7davg,
    method = "REML"
  ); summary(df_model)

# 7%
df_model <- 
  gam(
    covid_changes_cases ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_long_changes,
    method = "REML"
  ); summary(df_model)

# 3%
df_model <- 
  gam(
    covid_changes ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_school_long,
    method = "REML"
  ); summary(df_model)

equatiomatic::extract_eq(df_model)
draw(df_model)
appraise(df_model)
k.check(df_model)


