test <- df |> 
  select(date, schools_closed) |> 
  mutate(schools_closure_day = schools_closed - lag(schools_closed)) 

df_school_long |>
  filter(schools_closure_day %in% c(1,-1) & age_group == "aged1to4") |>
  select(date, schools_closure_wave, schools_closure_wave_n)

overall_plot <- df |> 
  ggplot(aes(date, total_cases)) +
  geom_co(aes(fill = factor(schools_closed), height = Inf), na.rm = TRUE, alpha = .4) +
  geom_line(na.rm = TRUE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_fill_manual(labels = c("Open", "Closed", "Na"), values = c("light blue", "red", "orange")) +
  labs(
    x = "Date", 
    y = "Covid-19 Cases in Ireland",
    fill = "Schools"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10, family = "serif"),
    axis.text = element_text(size = 10),
    panel.background = element_rect(colour = "black", size = 0.1)
  ); overall_plot
