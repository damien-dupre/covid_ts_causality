# model ------------------------------------------------------------------------
gam_model <- 
  gam(
    covid_cases ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_school_long,
    family = nb(), 
    method = "REML"
  ); summary(gam_model)

# statistics -------------------------------------------------------------------
gam_summary <- summary(gam_model)

gam_results <- 
  broom::tidy(gam_model) |> 
  mutate(
    term = c("s(date):1to4", "s(date):5to14", "s(date):15to24", "s(date*wave"),
    p.value = scales::pvalue(p.value, accuracy = 0.001, add_p = FALSE, prefix = c("< ", "= ", "> ")),
    across(where(is.numeric), round, 2),
    print = glue::glue("$\\chi^2({edf}) = {statistic}$, $p {p.value}$")
  ) |> 
  select(term, print) |> 
  deframe()

# visualisation ----------------------------------------------------------------
gam_plot <- 
  confint(gam_model, "s(schools_time_since_closure):age_groupaged15to24") |> 
  mutate(age_group = factor(age_group, levels = c("aged1to4", "aged5to14", "aged15to24"))) |> 
  ggplot(aes(x = schools_time_since_closure)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) + 
  geom_line(aes(y = est)) +
  facet_wrap(
    ~ age_group,
    labeller = as_labeller(
      c(
        aged1to4 = "1 to 4",
        aged5to14 = "5 to 14",
        aged15to24 = "15 to 24"
      )
    )) +
  labs(
    x = "Days since School Closure", 
    y = "Standardized Effect"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 10, family = "serif"),
    axis.text = element_text(size = 10),
    strip.background = element_blank(),
    panel.background = element_rect(colour = "black", size = 0.1)
  )
