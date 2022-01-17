# model ------------------------------------------------------------------------
gam_summary <- 
  gam(
    covid_cases ~ age_group +
      s(schools_time_since_closure, by = age_group) +
      s(schools_time_since_closure, schools_closure_wave, bs = "fs"),
    data = df_school_long,
    family = nb(), 
    method = "REML"
  ); summary(gam_summary)

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
gam_plot <- plot(gam_model, residuals = TRUE)

gam_plot_indiv <- gam_plot[[1]]
gam_plot_indiv_df <- as.data.frame(gam_plot_indiv[c("x", "se", "fit")])
#gam_plot_indiv_point <- as.data.frame(gam_plot_indiv[c("raw", "p.resid")])
gam_plot_1 <- ggplot(gam_plot_indiv_df, aes(x = x, y = fit)) +
  # geom_rug(data = gam_plot_indiv_point, mapping = aes(x = raw, y = NULL), sides = "b") +
  # geom_point(data = gam_plot_indiv_point, mapping = aes(x = raw, y = p.resid)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(
    x = "Days since School Closure", 
    y = "Standardized Effect in Age 1 to 4"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 10, family = "serif"),
    axis.text = element_text(size = 10),
    panel.background = element_rect(colour = "black", size = 0.1)
  )

gam_plot_indiv <- gam_plot[[2]]
gam_plot_indiv_df <- as.data.frame(gam_plot_indiv[c("x", "se", "fit")])
#gam_plot_indiv_point <- as.data.frame(gam_plot_indiv[c("raw", "p.resid")])
gam_plot_2 <- ggplot(gam_plot_indiv_df, aes(x = x, y = fit)) +
  # geom_rug(data = gam_plot_indiv_point, mapping = aes(x = raw, y = NULL), sides = "b") +
  # geom_point(data = gam_plot_indiv_point, mapping = aes(x = raw, y = p.resid)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(
    x = "Days since School Closure", 
    y = "Standardized Effect in Age 4 to 14"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 10, family = "serif"),
    axis.text = element_text(size = 10),
    panel.background = element_rect(colour = "black", size = 0.1)
  )

gam_plot_indiv <- gam_plot[[3]]
gam_plot_indiv_df <- as.data.frame(gam_plot_indiv[c("x", "se", "fit")])
#gam_plot_indiv_point <- as.data.frame(gam_plot_indiv[c("raw", "p.resid")])
gam_plot_3 <- ggplot(gam_plot_indiv_df, aes(x = x, y = fit)) +
  # geom_rug(data = gam_plot_indiv_point, mapping = aes(x = raw, y = NULL), sides = "b") +
  # geom_point(data = gam_plot_indiv_point, mapping = aes(x = raw, y = p.resid)) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, y = NULL), alpha = 0.3) +
  geom_line() +
  labs(
    x = "Days since School Closure", 
    y = "Standardized Effect in Age 15 to 24"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 10, family = "serif"),
    axis.text = element_text(size = 10),
    panel.background = element_rect(colour = "black", size = 0.1)
  )
