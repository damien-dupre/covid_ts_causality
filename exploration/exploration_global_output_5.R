
# options ----------------------------------------------------------------------
options(scipen = 999)
future::plan(multisession)

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
  "Partially open",
  "Academic break",
  "Closed due to COVID-19"
  )
## remove all records from countries 
countries_to_exclude <- c(
  "Moldova" # daily_cases artefact
)

# transformed data -------------------------------------------------------------
df_global <- df |> 
  filter(
    !country %in% countries_to_exclude,
    date > as.Date("2020-03-01"),
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
    date > as.Date("2020-03-01"),
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

negative_daily_case_wave <- df_global_school_age |> 
  filter(daily_cases < 0) |> 
  distinct(country, schools_closure_wave)

df_global_school_age_long <- df_global_school_length |> 
  anti_join(negative_daily_case_wave, by = c("country", "schools_closure_wave")) |> 
  right_join(df_global_school_age, by = c("country", "schools_closure_wave")) |> 
  filter(
    schools_closure_wave_n >= days_min_closure,
    schools_time_since_closure <= days_after_closure,
    age_grp %in% c("aged0to4", "aged5to9", "aged10to14", "aged15to19", "aged20to24")
  )

missing_daily_case_wave <- df_global_school_age_long |> 
  group_by(country, age_grp, schools_closure_wave) |> 
  summarise(n_missing = sum(is.na(daily_cases))) |> 
  filter(n_missing != 0) |> 
  distinct(country, schools_closure_wave)

df_global_school_age_long_clean <- df_global_school_age_long |> 
  anti_join(missing_daily_case_wave, by = c("country", "schools_closure_wave")) |>  
  mutate(
    age_grp = factor(age_grp),
    country = factor(country)
  )

df_global_school_age_long_euro <- df_global_school_age_long_clean |> 
  filter(
    who_region == "EURO"
  )

# test -------------------------------------------------------------------------
# match countries
country_school_closure <- df_school_closure |> 
  distinct(country) |> 
  mutate(df_school_closure_col = row_number())

country_df <- df |> 
  distinct(country) |> 
  mutate(df_col = row_number())

country_check <- df_check |> 
  distinct(country) |> 
  mutate(df_check_col = row_number())

match_countries <- 
  full_join(country_df, country_school_closure, by = "country") |> 
  filter(is.na(df_school_closure_col) | is.na(df_col))

match_countries <- 
  full_join(country_df, country_check, by = "country") |> 
  filter(is.na(df_col) | is.na(df_check_col))

# n countries
df |> distinct(country) |> nrow()
## integrity data Ireland: exploitable
df_ireland <- df |> 
  filter(country == "Ireland")
## full length data: 27 countries
df |> 
  group_by(country) |> 
  summarise(
    full_duration = if_else(
      min(date) < as.Date("2020-04-01") & max(date) > as.Date("2021-12-01"), 
      "OK", 
      "Not OK"
    )
  ) |> 
  count(full_duration)
# cases similarity between files: example France
df_france <- df |> 
  filter(country == "France" & region == "All" & sex == "b") |> 
  group_by(date) |> 
  summarise(cumulative_cases_cal = sum(cases))

df_country_check <- df |> 
  filter(country == "Finland")

# df <- df |> 
#   filter(!country %in% countries_to_filter)
check_length <- df_global_school_age_long_euro |> 
  count(country, schools_closure_wave, age_grp)

test <- df_global_school_age_long_euro |> 
  filter(country == "Greece")

# visual exploration -----------------------------------------------------------
df_global |> 
  ggplot() +
  geom_line(aes(date, cumulative_cases_cal), na.rm = TRUE, color = "red") +  
  geom_line(aes(date, cumulative_cases), na.rm = TRUE, color = "blue") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  facet_grid(who_region ~ ., scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")

df_global |> 
  ggplot(aes(date, cumulative_cases_cal, group = country)) +
  geom_line(na.rm = TRUE) +  
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  facet_grid(who_region ~ ., scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

df_global |> 
  filter(who_region == "EURO") |> 
  ggplot(aes(date, cumulative_cases_cal)) +
  geom_line(na.rm = TRUE) +  
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  facet_grid(country ~ ., scales = "free") +
  theme_bw() +
  theme(
    strip.text.y.right = element_text(angle = 0),
    legend.position = "none"
    )

df_global_age |> 
  ggplot(aes(date, daily_cases, color = country)) +
  geom_line(na.rm = TRUE) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_bw() +
  theme(legend.position = "none")

df_global_age |> 
  ggplot(aes(date, daily_cases, color = country)) +
  geom_line(na.rm = TRUE) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  facet_grid(age_grp ~ .) +
  theme_bw() +
  theme(legend.position = "none")

df_global_school_age_long_clean |> 
  ggplot(aes(schools_time_since_closure, daily_cases, color = age_grp, shape = factor(schools_closure_wave))) +
  facet_wrap( ~ country) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom")

df_global_school_age_long_euro |> 
  ggplot(aes(schools_time_since_closure, daily_cases, color = age_grp, shape = factor(schools_closure_wave))) +
  facet_wrap( ~ country) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom")

library(geofacet)
df_global_school_age_long_euro |> 
  ggplot(aes(schools_time_since_closure, daily_cases, color = age_grp, shape = factor(schools_closure_wave))) +
  facet_geo(~ country, grid = "eu_grid1", scales = "free_y") +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom")
