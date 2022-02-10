
# options ----------------------------------------------------------------------
options(scipen = 999)
future::plan("future::multisession")

# libraries --------------------------------------------------------------------
library(janitor)
library(mgcv)
library(osfr)
library(tidyverse)

# raw data ---------------------------------------------------------------------
osf_retrieve_file("7tnfh") |>
  osf_download(conflicts = "overwrite", progress = TRUE, verbose = TRUE)

df <- "Output_5.zip" |>
  read_csv(skip = 3, col_types = "ccccciiddd") |>
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) |>
  clean_names()

file.remove("Output_5.zip")

df_school_closure <- 
  read_csv(
    "https://en.unesco.org/sites/default/files/covid_impact_education_full.csv",
    col_types = list(.default = col_character(), Date = col_date(format = "%d/%m/%Y"))) |> 
  clean_names() |>
  select(date, country, status) |> 
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",  
    country == "Central African republic" ~ "Central African Republic",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Moldova" ~ "Moldova",
    country == "Republic of Korea" ~ "South Korea",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    country == "United States of America" ~ "USA",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country
    )
  )

df_check <- 
  read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") |> 
  clean_names() |> 
  rename(date = date_reported) |> 
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",  
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "occupied Palestinian territory, including east Jerusalem" ~ "Palestine",
    country == "Republic of Moldova" ~ "Moldova",
    country == "Republic of Korea" ~ "South Korea",
    country == "United States of America" ~ "USA",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ country
    )
  )

# variables --------------------------------------------------------------------
## keep only closure waves longer than `days_min_closure` but display only the first `days_after_closure`
days_min_closure <- 21
days_after_closure <- 28
## type of school closure to investigate
period_closure_to_keep <- c(
  #"Partially open",
  "Academic break",
  "Closed due to COVID-19"
  )

## countries of interest
countries_of_interest <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", "Estonia", "France", 
  "Germany", "Greece", "Netherlands", "Portugal", "Slovakia", "Spain"
  )
# transformed data -------------------------------------------------------------
df_global <- df |> 
  filter(
    country %in% countries_of_interest,
    date > as.Date("2020-01-01"),
    region == "All",
    sex == "b"
  ) |> 
  group_by(country, date) |> 
  summarise(cumulative_cases_cal = sum(cases)) |> 
  group_by(country) |> 
  complete(date = full_seq(date, 1)) |> 
  mutate(
    timeline = date - lag(date),  # check if gap between dates
    daily_cases = round(cumulative_cases_cal - lag(cumulative_cases_cal), 0)
  ) |> 
  ungroup() |> 
  left_join(df_school_closure, by = c("date", "country")) |> 
  group_by(country) |> 
  fill(status) |> 
  ungroup() |> 
  mutate(schools_closed = if_else(status %in% period_closure_to_keep, 1, 0)) |> 
  drop_na(status) |> 
  left_join(df_check, by = c("date", "country"))

df_global_age <- df |> 
  filter(
    country %in% countries_of_interest,
    date > as.Date("2020-01-01"),
    region == "All",
    sex == "b"
  ) |> 
  mutate(
    age_grp = case_when(
      age == 0  ~ "aged0to4",
      age == 5  ~ "aged5to9",
      age == 10 ~ "aged10to14",
      age == 15 ~ "aged15to19",
      age == 20 ~ "aged20to24",
      between(age, 25, 34) ~ "aged25to34",
      between(age, 35, 44) ~ "aged35to44",
      between(age, 45, 54) ~ "aged45to54",
      between(age, 55, 64) ~ "aged55to64",
      age > 64 ~ "aged64+"
    )
  ) |> 
  group_by(country, date, age_grp) |> 
  summarise(cumulative_cases_cal = sum(cases)) |> 
  group_by(country, age_grp) |> 
  complete(date = full_seq(date, 1)) |> 
  mutate(
    timeline = date - lag(date),  # check if gap between dates
    daily_cases = round(cumulative_cases_cal - lag(cumulative_cases_cal), 0)
  ) |> 
  ungroup()

df_school_closure_wave <- df_global |> 
  select(date, country, schools_closed) |> 
  group_by(country) |> 
  mutate(schools_closure_day = schools_closed - lag(schools_closed)) |> 
  filter(schools_closure_day == 1) |> 
  mutate(schools_closure_wave = row_number())

df_global_school <- df_school_closure_wave |> 
  full_join(df_global, by = c("date", "country", "schools_closed")) |> 
  group_by(country) |> 
  arrange(date) |> 
  fill(schools_closure_wave, .direction = "down") |> 
  drop_na(schools_closure_wave) |> 
  group_by(country, schools_closure_wave) |> 
  mutate(schools_time_since_closure = row_number()) |> 
  ungroup()

df_global_school_age <- df_global_school |> 
  select(date, country, schools_closed, schools_closure_day, schools_closure_wave, schools_time_since_closure, who_region) |> 
  full_join(df_global_age, by = c("date", "country"))

df_global_school_length <- df_global_school |> 
  filter(schools_closed == 1) |> 
  count(country, schools_closure_wave) |> 
  rename(schools_closure_wave_n = n)

df_global_school_age_long <- df_global_school_length |> 
  right_join(df_global_school_age, by = c("country", "schools_closure_wave")) |> 
  filter(
    schools_closure_wave_n >= days_min_closure,
    schools_time_since_closure <= days_after_closure,
    age_grp %in% c("aged0to4", "aged5to9", "aged10to14", "aged15to19", "aged20to24")
  )

negative_daily_case_wave <- df_global_school_age |> 
  filter(daily_cases < 0) |> 
  distinct(country, schools_closure_wave)

missing_daily_case_wave <- df_global_school_age_long |> 
  group_by(country, age_grp, schools_closure_wave) |> 
  summarise(n_missing = sum(is.na(daily_cases))) |> 
  filter(n_missing != 0) |> 
  distinct(country, schools_closure_wave)

df_global_school_age_long_clean <- df_global_school_age_long |> 
  anti_join(negative_daily_case_wave, by = c("country", "schools_closure_wave")) |> 
  anti_join(missing_daily_case_wave, by = c("country", "schools_closure_wave")) 

df_global_school_age_long_euro <- df_global_school_age_long_clean |> 
  filter(who_region == "EURO")

df_gam <- df_global_school_age_long_euro |>  
  mutate(
    age_grp = factor(age_grp),
    country = factor(country)
  )

# analysis choosen -------------------------------------------------------------
system.time(
  gam_model <- 
    bam(
      daily_cases ~ age_grp * country +
        s(schools_time_since_closure, by = age_grp) +
        s(schools_time_since_closure, by = country) +
        s(schools_time_since_closure, schools_closure_wave, country, bs = "fs"),
      data = df_gam,
      family = nb(), 
      method = "fREML",
      control = list(nthreads = parallel::detectCores())
    )
); summary(gam_model)

saveRDS(gam_model, "gam_model.rds")

gam_model <- readRDS("gam_model.rds")
# analyses ---------------------------------------------------------------------
sink("results/exploration_global_output_5_breaks+covid.txt", append = FALSE, split = TRUE)

system.time(
  gam_model <- 
    bam(
      daily_cases ~ age_grp * country +
        s(schools_time_since_closure, by = age_grp) +
        s(schools_time_since_closure, schools_closure_wave, country, bs = "fs"),
      data = df_gam,
      family = nb(), 
      method = "fREML",
      control = list(nthreads = parallel::detectCores())
    )
); summary(gam_model)

system.time(
  gam_model <- 
    bam(
      daily_cases ~ age_grp * country +
        s(schools_time_since_closure, by = age_grp) +
        s(schools_time_since_closure, by = country) +
        s(schools_time_since_closure, schools_closure_wave, country, bs = "fs"),
      data = df_gam,
      family = nb(), 
      method = "fREML",
      control = list(nthreads = parallel::detectCores())
    )
  ); summary(gam_model)

system.time(
  gam_model <- 
    bam(
      daily_cases ~ age_grp * country +
        s(schools_time_since_closure, by = interaction(age_grp, country)) +
        s(schools_time_since_closure, schools_closure_wave, country, bs = "fs"),
      data = df_gam,
      family = nb(), 
      method = "fREML",
      control = list(nthreads = parallel::detectCores())
    )
  ); summary(gam_model)
sink()
