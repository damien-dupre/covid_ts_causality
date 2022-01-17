################################################################################
#            Influence of School Closure on COVID-19 Contaminations 
#                     and Repercussions across Age Groups  
################################################################################

# references -------------------------------------------------------------------
## ts causality
"https://towardsdatascience.com/inferring-causality-in-time-series-data-b8b75fe52c46"
"https://jofam.github.io/covidBE_analysis/Overdispersion.html"
"https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7558856/"
"https://github.com/rollerb/cods"
"https://www.sciencedirect.com/science/article/pii/S105752192030140X?casa_token=Rm23h3yv9y4AAAAA:-spSI7y0xg4yH_-JLt_KsAzwIX1WNvM8-2QE6nup83Dpx1LiJI0hTTFOCRVhL2GnWSigRcHGMA
https://www.tandfonline.com/doi/full/10.1080/02692171.2020.1864301?casa_token=dhB_NdvreHgAAAAA%3AZRV6FGrikueeIQCBlx_ahepZ4J1oH84r0FOdBBQCDMnQOb7d5bzTdNph3U7ZTeSfxY8p-g5n8m_fQZDOMg
https://www.sciencedirect.com/science/article/pii/S1544612320308497?casa_token=UQ0NiuBKKfUAAAAA:FBGglVAA5LQZlalUaDLqXicL0AjDBMjMd5WiuFFJdS9zwDNQk5iMANBa8H28GjbOZ-F0_aSirA
https://link.springer.com/article/10.1007/s11356-021-12938-2
https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0244920
https://www.emerald.com/insight/content/doi/10.1108/SEF-09-2020-0385/full/html?casa_token=v7AqtRlwgDAAAAAA:bA-cYq18HmVxWAOW3bKUKfSx_FJ-dr5WrQ1WYdgsqbDJHJJ1uZN1h2GxOVWdnM_HT9Kj9ZCz6kW1tDphWm6YGs3vc4x9dkEPP3Dd8gp5BE-t-RHwtemv
https://www.sciencedirect.com/science/article/pii/S0301420720309296?casa_token=Gf4xUdEoi_YAAAAA:PapQN5Tvb94DVnUFUAOEyDdOnQAN8buDMUm8cgO0fRtwdIbJ7ud78x2Ah5c5CLpcY2c52WrYsA
https://www.researchgate.net/profile/Martin-Enilov/publication/341062334_The_Global_Impact_of_COVID-19_on_Financial_Markets/links/5fa5df54a6fdcc06241cbf69/The-Global-Impact-of-COVID-19-on-Financial-Markets.pdf
file:///Users/damiendcu/Downloads/21380-sectoral-nonlinear-causality-between-stock-market-volatility-and-the-covid-19-pandemic-evidence-from-india.pdf
https://dergipark.org.tr/en/download/article-file/1344725
file:///Users/damiendcu/Downloads/ijerph-17-06729.pdf
https://www.frontiersin.org/articles/10.3389/fpubh.2020.626055/full#h6
https://rskuzma.github.io/files/influencing_the_influencers.pdf
https://www.frontiersin.org/articles/10.3389/fpubh.2020.626055/full#h6
file:///Users/damiendcu/Downloads/engproc-05-00035-v2.pdf
https://www.sciencedirect.com/science/article/pii/S0960077920303921?casa_token=m1mU0YbMBdQAAAAA:j-r1O_L68bD2zC4PssVFLA7b5o0VyHDaltCv-__QUEA_e3Q0scWO2AxrseJFwFckLCH9Rapd3Q
https://www.scirp.org/journal/paperinformation.aspx?paperid=113444
https://dl.acm.org/doi/pdf/10.1145/3441452
https://www.nature.com/articles/s41598-019-49278-8
"

# libraries --------------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(zoo)
library(mgcv)
#  library(gratia)
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
  mutate(
    timeline = date - lag(date),  # check if gap between dates
    total_changes = total_cases - lag(total_cases)
  )

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

## Augmented Dickey-Fuller Test ################################################
### significant p-value indicates the TS is stationary
aTSA::adf.test(df_long$covid_changes)
tseries::adf.test(na.omit(df_long$covid_changes), k = 6)
aTSA::adf.test(df_long$covid_changes)
tseries::adf.test(na.omit(df_long$covid_changes), k = 6)

adf_res <- df_long |> 
  group_by(age_group) |> 
  summarise(
    aTSA::adf.test(covid_changes, output = FALSE) |> 
      map_df(as_tibble)
  )

## Granger Causality ###########################################################
### for stationary time series only
lmtest::grangertest(aged1to4 ~ aged15to24, order = 3, data = df_wide_changes)
NlinTS::causality.test(df_wide_changes$aged1to4, df_wide_changes$aged15to24, lag = 3)$summary()

## Transfer Entropy ############################################################
### see package vignette https://cran.r-project.org/web/packages/RTransferEntropy/vignettes/transfer-entropy.html
### see corresponding paper https://www.sciencedirect.com/science/article/pii/S2352711019300779
library(RTransferEntropy)

fruits <- tibble(
  type   = c("apple", "orange", "apple", "orange", "orange", "orange"),
  year   = c(2010, 2010, 2012, 2010, 2010, 2012),
  size  =  factor(
    c("XS", "S",  "M", "S", "S", "M"),
    levels = c("XS", "S", "M", "L")
  ),
  weights = rnorm(6, as.numeric(size) + 2)
)


test <- fruits %>% expand(type, year)

df_model <- transfer_entropy(df_wide_changes$aged1to4, df_wide_changes$aged25to34);df_model
df_model <- transfer_entropy(df_wide_changes$aged1to4, df_wide_changes$aged35to44);df_model
df_model <- transfer_entropy(df_wide_changes$aged1to4, df_wide_changes$aged45to54);df_model
df_model <- transfer_entropy(df_wide_changes$aged5to14, df_wide_changes$aged25to34);df_model
df_model <- transfer_entropy(df_wide_changes$aged5to14, df_wide_changes$aged35to44);df_model
df_model <- transfer_entropy(df_wide_changes$aged5to14, df_wide_changes$aged45to54);df_model
df_model <- transfer_entropy(df_wide_changes$aged15to24, df_wide_changes$aged35to44);df_model
df_model <- transfer_entropy(df_wide_changes$aged15to24, df_wide_changes$aged45to54);df_model

test <- transfer_entropy(df_wide_changes$aged15to24, df_wide_changes$aged45to54)[["coef"]] |> 
  as_tibble() |> 
  mutate(direction = c("X->Y", "Y->X"))

test <- expand_grid(
  X = c("aged1to4", "aged5to14", "aged15to24", "aged25to34", "aged35to44", "aged45to54", "aged55to64", "aged65to74", "aged75to84", "aged85up"),
  Y = c("aged1to4", "aged5to14", "aged15to24", "aged25to34", "aged35to44", "aged45to54", "aged55to64", "aged65to74", "aged75to84", "aged85up")
)

age_order <- c("aged1to4", "aged5to14", "aged15to24")

te_agegroup <- function(var_x, var_y){
  
  var_x <- select(df_wide_changes, all_of(var_x))
  var_y <- select(df_wide_changes, all_of(var_y))
  
  transfer_entropy(var_x, var_y)[["coef"]] |> 
    as_tibble() |> 
    mutate(direction = c("X->Y", "Y->X"))
}

test <- gtools::combinations(n = length(age_order), r = 2, v = age_order, repeats.allowed = FALSE) |> 
  as_tibble() |> 
  rename(
    X = V1,
    Y = V2
  ) |> 
  group_by(X, Y) |> 
  summarise(te_agegroup(X, Y))

