# libraries --------------------------------------------------------------------
library(broom)
library(future)
library(glue)
library(gratia)
library(here)
library(mgcv)
library(parallel)
library(scales)
library(tidyverse)

# options ----------------------------------------------------------------------
options(scipen = 999)
plan("future::multisession")

# data -------------------------------------------------------------------------
df_global <- read_csv(here("data/df_global.csv"))
df_global_age <- read_csv(here("data/df_global_age.csv"))

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

days_min_closure <- 21
days_after_closure <- 28

negative_daily_case_wave <- df_global_school_age |>
  filter(daily_cases < 0) |>
  distinct(country, schools_closure_wave)

missing_daily_case_wave <- df_global_school |>
  filter(schools_closed == 1) |>
  count(country, schools_closure_wave) |>
  rename(schools_closure_wave_n = n) |>
  right_join(df_global_school_age, by = c("country", "schools_closure_wave")) |>
  filter(
    schools_closure_wave_n >= days_min_closure,
    schools_time_since_closure <= days_after_closure,
    age_grp %in% c("aged0to4", "aged5to9", "aged10to14", "aged15to19", "aged20to24")
  ) |>
  group_by(country, age_grp, schools_closure_wave) |>
  summarise(n_missing = sum(is.na(daily_cases))) |>
  filter(n_missing != 0) |>
  distinct(country, schools_closure_wave)

df_gam <- df_global_school |>
  filter(schools_closed == 1) |>
  count(country, schools_closure_wave) |>
  rename(schools_closure_wave_n = n) |>
  right_join(df_global_school_age, by = c("country", "schools_closure_wave")) |>
  filter(
    schools_closure_wave_n >= days_min_closure,
    schools_time_since_closure <= days_after_closure,
    age_grp %in% c("aged0to4", "aged5to9", "aged10to14", "aged15to19", "aged20to24")
  ) |>
  anti_join(negative_daily_case_wave, by = c("country", "schools_closure_wave")) |>
  anti_join(missing_daily_case_wave, by = c("country", "schools_closure_wave")) |>
  filter(who_region == "EURO") |>
  mutate(
    age_grp = factor(age_grp),
    country = factor(country)
  )

system.time(
  gam_model <-
    bam(
      daily_cases ~ 
        s(schools_time_since_closure, bs="tp", k=7) +
        s(age_grp, bs="re") +
        s(country, bs="re") +
        s(schools_time_since_closure, by = age_grp, bs="tp", k=7) +
        s(schools_time_since_closure, by = country, bs="tp", k=7) +
        s(schools_time_since_closure, schools_closure_wave, bs="re"),
      data = df_gam,
      family = poisson,
      method = "fREML",
      knots = list(schools_time_since_closure = c(0, 28)),
      control = list(nthreads = detectCores()),
      drop.unused.levels = FALSE,
      discrete = TRUE,
      rho = 0.88 # acf(residuals(gam_model))$acf[2]
    )
)#  summary(gam_model); draw(gam_model); gam_model$aic

# summary(aov(gam_model))

gam_df <- gam_model |> 
  smooth_estimates() |> 
  add_confint()
# statistics -------------------------------------------------------------------
gam_summary <- summary(gam_model)

gam_results <- 
  tidy(gam_model) |> 
  mutate(
    term = term |> 
      str_remove("s\\(schools_time_since_closure\\):age_grpaged") |> 
      str_remove("s\\(schools_time_since_closure\\):country"),
    p.value = pvalue(
      p.value, 
      accuracy = 0.001, 
      add_p = FALSE, 
      prefix = c("< ", "= ", "> ")
    ),
    across(where(is.numeric), round, 2),
    print = glue("$\\chi^2({edf}) = {statistic}$, $p {p.value}$")
  ) |> 
  select(term, print) |> 
  deframe()

# visualisation ----------------------------------------------------------------
gam_plot_age <- gam_df |>
  filter(by == "age_grp") |>
  mutate(
    smooth = smooth |>
      str_remove("s\\(schools_time_since_closure\\):age_grpaged") |>
      factor(levels = c("0to4", "5to9", "10to14", "15to19", "20to24"))
  ) |>
  ggplot(aes(x = schools_time_since_closure, color = smooth)) +
  geom_line(aes(y = est)) +
  scale_x_continuous(
    "Day Since Closure",
    breaks = seq(0, 28, by = 7)
  ) +
  scale_y_continuous(
    "Standardized Effect"
  ) +
  scale_color_manual(
    "Age Group",
    values = c("#a6611a", "#dfc27d", "#bababa", "#80cdc1", "#018571"),
    labels = c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24")
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10, family = "serif")
  )

gam_plot_country <- gam_df |>
  filter(by == "country") |>
  mutate(
    smooth = smooth |>
      str_remove("s\\(schools_time_since_closure\\):country")
  ) |>
  ggplot(aes(x = schools_time_since_closure, y = est, group = smooth)) +
  geom_line(color = "gray40", alpha = 0.4) +
  geom_text_repel(
    data = gam_df |>
      group_by(country) |>
      slice_max(schools_time_since_closure, with_ties = FALSE),
    aes(label = country),
    hjust = 0,
    direction = "y",
    segment.color = NA,
    seed = 123,
    size = 4,
    force = 0.5,
    family = "serif"
  ) +
  scale_x_continuous(
    "Day Since Closure",
    breaks = seq(0, 28, by = 7),
    limits = c(0, 31)
  ) +
  scale_y_continuous(
    "Standardized Effect"
  ) +
  scale_color_discrete(
    "Country"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 10, family = "serif")
  )

