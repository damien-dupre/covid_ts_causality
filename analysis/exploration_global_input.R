
# options ----------------------------------------------------------------------
options(scipen = 999)

# libraries --------------------------------------------------------------------
library(janitor)
library(mgcv)
library(osfr)
library(tidyverse)

# data -------------------------------------------------------------------------
df_school_closure <- 
  read_csv(
    "https://en.unesco.org/sites/default/files/covid_impact_education.csv",
    col_types = list(.default = col_character(), Date = col_date(format = "%d/%m/%Y"))) |> 
  clean_names() |>
  select(date, country, status)

  osf_retrieve_file("9dsfk") |> 
  osf_download(conflicts = "overwrite", progress = TRUE, verbose = TRUE)

df <- 
  read_csv(
    "inputDB.zip", 
    skip = 1, col_types = "cccccciccdc"
  ) |> 
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) |> 
  clean_names()

file.remove("inputDB.zip")

df_check <- 
  read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") |> 
  clean_names() |> 
  rename(date = date_reported)

# test -------------------------------------------------------------------------
## integrity data Ireland: non exploitable
df_ireland <- df |> 
  filter(country == "Ireland")
## full length data: 27 countries
df |> 
  group_by(country) |> 
  summarise(
    range = max(date) - min(date),
    full_duration = if_else(
      min(date) < as.Date("2020-04-01") & max(date) > as.Date("2021-12-01"), 
      "OK", 
      "Not OK"
    )
  ) |> 
  count(full_duration)
# cases similarity between files: example France
df_france <- df |> 
  filter(country == "France" & region == "All" & sex == "b" & age == "TOT" & measure == "Cases")

df_france_check <- df_check |> 
  filter(country == "France") |> 
  select(date, cumulative_cases)

df_comparison <- 
  full_join(df_france, df_france_check, by = "date")

ggplot(df_comparison) +
  geom_line(aes(date, value), color = "blue") +
  geom_line(aes(date, cumulative_cases), color = "red")

# list countries with age groups by 5
list_countries_with_age_by_5 <- df |> 
  distinct(country, age) |> 
  filter(str_detect(age, "^[0-9]*$")) |> 
  mutate(age  = parse_number(age), availability = 1) |> 
  filter(between(age, 0, 105), age %% 1 == 0) |> 
  pivot_wider(names_from = country, values_from = availability) |> 
  filter(age %in% seq(10, 70, by = 5)) %>% 
  select(where(~!any(is.na(.)))) |> 
  pivot_longer(-age, names_to = "country", values_to = "availability") |> 
  distinct(country) |> 
  pull(country)
################################################################################
df_global <- df |> 
  filter(
    date > as.Date("2020-03-01"),
    country %in% list_countries_with_age_by_5,
    str_detect(age, "^[0-9]*$"),
    region == "All",
    measure == "Cases",
    metric == "Count",
    sex != "b" # countries only with non specified gender are excluded
  ) |> 
  mutate(
    age = parse_number(age),
    age_grp = case_when(
      between(age, 0, 4) ~ "aged0to4",
      between(age, 5, 9) ~ "aged5to9",
      between(age, 10, 14) ~ "aged10to14",
      between(age, 15, 19) ~ "aged15to19",
      between(age, 20, 24) ~ "aged20to24",
      between(age, 25, 34) ~ "aged25to34",
      between(age, 35, 44) ~ "aged35to44",
      between(age, 45, 54) ~ "aged45to54",
      between(age, 55, 64) ~ "aged55to64",
      age > 64 ~ "aged64+"
    )
  ) |> 
  group_by(country, date) |> 
  summarise(cases = sum(value)) |> 
  group_by(country) |> 
  complete(date = full_seq(date, 1)) |> 
  mutate(
    timeline = date - lag(date),  # check if gap between dates
    cases_changes = cases - lag(cases)
  ) |> 
  ungroup() |> 
  left_join(df_school_closure, by = c("date", "country")) |> 
  group_by(country) |> 
  fill(status) |> 
  ungroup() |> 
  mutate(
    schools_closed = case_when(
      status != "Fully open" ~ 1,
      TRUE ~ 0
    )) |> 
  left_join(df_check, by = c("date", "country"))

df_global_age <- df |> 
  filter(
    date > as.Date("2020-03-01"),
    country %in% list_countries_with_age_by_5,
    str_detect(age, "^[0-9]*$"),
    region == "All",
    measure == "Cases",
    metric == "Count",
    sex != "b" # countries only with non specified gender are excluded
  ) |> 
  mutate(
    age = parse_number(age),
    age_grp = case_when(
      between(age, 0, 4) ~ "aged0to4",
      between(age, 5, 9) ~ "aged5to9",
      between(age, 10, 14) ~ "aged10to14",
      between(age, 15, 19) ~ "aged15to19",
      between(age, 20, 24) ~ "aged20to24",
      between(age, 25, 34) ~ "aged25to34",
      between(age, 35, 44) ~ "aged35to44",
      between(age, 45, 54) ~ "aged45to54",
      between(age, 55, 64) ~ "aged55to64",
      age > 64 ~ "aged64+"
    )
  ) |> 
  group_by(country, date, age_grp) |> 
  summarise(cases = sum(value)) |> 
  group_by(country, age_grp) |> 
  complete(date = full_seq(date, 1)) |> 
  mutate(
    timeline = date - lag(date),  # check if gap between dates
    cases_changes = cases - lag(cases)
  ) |> 
  ungroup()

df_global_age_wide <- df_global_age |> 
  group_by(country, date) |> 
  summarise(cases = sum(cases)) |>
  complete(date = full_seq(date, 1)) |> 
  mutate(
    timeline = date - lag(date),  # check if gap between dates
    cases_changes = cases - lag(cases)
  ) |> 
  ungroup() |> 
  select(date, country, cases_changes) |> 
  pivot_wider(names_from = country, values_from = cases_changes)

df_school_closure_wave <- df_global |> 
  select(date, country, schools_closed) |> 
  group_by(country) |> 
  mutate(schools_closure_day = schools_closed - lag(schools_closed)) |> 
  filter(schools_closure_day == 1) |> 
  mutate(schools_closure_wave = row_number())

df_global_school <- df_global |> 
  select(date, country, schools_closed) |> 
  group_by(country) |> 
  mutate(schools_closure_day = schools_closed - lag(schools_closed)) |> 
  filter(schools_closure_day == 1) |> 
  mutate(schools_closure_wave = row_number()) |> 
  full_join(df_global, by = c("date", "country", "schools_closed")) |> 
  group_by(country) |> 
  arrange(date) |> 
  fill(schools_closure_wave, .direction = "down") |> 
  mutate(schools_closure_wave = replace_na(schools_closure_wave, 0)) |> 
  group_by(country, schools_closure_wave) |> 
  mutate(schools_time_since_closure = row_number()) |> 
  ungroup()

df_global_school_age <- df_global_school |> 
  select(date, country, schools_closed, schools_closure_day, schools_closure_wave, schools_time_since_closure, who_region) |> 
  full_join(df_global_age, by = c("date", "country"))

df_global_school_age_long <- df_global_school |> 
  filter(schools_closed == 1) |> 
  count(country, schools_closure_wave) |> 
  rename(schools_closure_wave_n = n) |> 
  right_join(df_global_school_age, by = c("country", "schools_closure_wave")) |> 
  filter(schools_closure_wave_n > 20 & schools_time_since_closure < 40) |> # keep only closure waves longer than 20 days but display only the first 39 days
  filter(age_grp %in% c("aged0to4", "aged5to9", "aged10to14", "aged15to19", "aged20to24")) |> 
  mutate(
    age_grp = factor(age_grp),
    country = factor(country)
  ) |> 
  filter(cases_changes >= 0)
# visual exploration -----------------------------------------------------------
df_global |> 
  ggplot() +
  geom_line(aes(date, cases), na.rm = TRUE, color = "red") +  
  geom_line(aes(date, cumulative_cases), na.rm = TRUE, color = "blue") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  facet_grid(country ~ ., scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")  

df_global_age |> 
  group_by(country, date) |> 
  summarise(cases = sum(cases)) |> 
  ungroup() |> 
  ggplot(aes(date, cases, color = country)) +
  geom_line(na.rm = TRUE) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_bw() +
  theme(legend.position = "bottom")

df_global_age |> 
  ggplot(aes(date, cases_changes, color = country)) +
  geom_line(na.rm = TRUE) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  facet_grid(age_grp ~ who_region) +
  theme_bw() +
  theme(legend.position = "bottom")

library(future)
plan(multisession)

gam_model <- 
  bam(
    cases ~ age_grp + 
      s(schools_time_since_closure, by = age_grp) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_global_school_age_long,
    family = nb(), 
    method = "REML",
    control = list(nthreads = 4)
  ); summary(gam_model)

gam_model <- 
  bam(
    cases_changes ~ age_grp + 
      s(schools_time_since_closure, by = age_grp) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_global_school_age_long,
    family = nb(), 
    method = "REML",
    control = list(nthreads = 4)
  ); summary(gam_model)

gam_model <- 
  bam(
    cases ~ age_grp + country +
      s(schools_time_since_closure, by = age_grp) +
      s(schools_time_since_closure, by = country) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_global_school_age_long,
    family = nb(), 
    method = "REML",
    control = list(nthreads = 4)
  ); summary(gam_model)

gam_model <- 
  bam(
    cases_changes ~ age_grp + country +
      s(schools_time_since_closure, by = age_grp) +
      s(schools_time_since_closure, by = country) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_global_school_age_long,
    family = nb(), 
    method = "REML",
    control = list(nthreads = 4)
  ); summary(gam_model)

gam_model <- 
  bam(
    cases ~ age_grp * country +
      s(schools_time_since_closure, by = interaction(age_grp, country)) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_global_school_age_long,
    family = nb(), 
    method = "REML",
    control = list(nthreads = 4)
  ); summary(gam_model)

gam_model <- 
  bam(
    cases_changes ~ age_grp * country +
      s(schools_time_since_closure, by = interaction(age_grp, country)) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_global_school_age_long,
    family = nb(), 
    method = "fREML",
    discrete = TRUE, nthreads = 4
  ); summary(gam_model)

plot(modelbased::estimate_relation(gam_model, length = 100, preserve_range = FALSE))
