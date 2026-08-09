# coppock_2022/maintained/figure_5.15_table_a19_trump_white.R
# Output: output/figure_5.15_trump_white.pdf, output/table_a19_trump_white.csv
# Depends on: helpers.R, data/trump_white_clean.rds
# Description: Trump & White income inequality experiment by demographic subgroup.
# Note: This corresponds to figure_5.15 in the archive (file was named figure_5.15.R).

source(here::here("maintained", "helpers.R"))

trump_white <- read_rds(path_original("data", "trump_white_clean.rds"))

gg_df <-
  trump_white |>
  filter(!is.na(Z)) |>
  pivot_longer(cols = c(pid_3, female_2, educ_3, race_4),
               names_to = "key", values_to = "value") |>
  filter(value != "Independent") |>
  mutate(
    Z = factor(Z),
    key = factor(key,
                 levels = c("pid_3", "female_2", "educ_3", "race_4"),
                 labels = c("Partisanship", "Gender", "Education", "Race")),
    value = factor(value,
                   levels = c("Democrat", "Republican", "Female", "Male",
                              "High School or Less", "Some College", "Bachelor's Degree or Higher",
                              "White", "Black", "Hispanic"),
                   labels = c("Democrat", "Republican", "Women", "Men",
                              "High School", "Some College", "College",
                              "White", "Black", "Hispanic"))
  ) |>
  filter(!is.na(value))

gg_df_plot <-
  gg_df |>
  group_by(key, value, Z, Y) |>
  mutate(
    y_s = sunflower_compat(y = Y, width = 0.08, height = 0.08),
    x_s = sunflower_compat(x = as.numeric(Z), width = 0.08, height = 0.08),
    x_s = case_when(
      value == "Republican" ~ x_s + (1 / 8),  value == "Democrat"     ~ x_s - (1 / 8),
      value == "Men" ~ x_s + (1 / 8),  value == "Women"        ~ x_s - (1 / 8),
      value == "High School" ~ x_s - (1 / 6),  value == "Some College" ~ x_s,
      value == "College" ~ x_s + (1 / 6),  value == "White"        ~ x_s - (1 / 6),
      value == "Black" ~ x_s,             value == "Hispanic"     ~ x_s + (1 / 6)
    ),
    plot_letter = str_sub(value, 1, 1)
  )

summary_df <-
  gg_df_plot |>
  group_by(key, value, Z) |>
  reframe(tidy(lm_robust(Y ~ 1, weight = weight, data = pick(everything())))) |>
  mutate(Y = estimate)

label_df <-
  summary_df |>
  filter(Z == "Treatment") |>
  ungroup() |>
  mutate(Y = c(0.85, 0.50, 0.50, 0.80, 0.45, 0.85, 0.55, 0.75, 0.40, 0.85))

figure_5.15 <-
  ggplot(summary_df, aes(Z, Y, group = value)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
  geom_text(data = gg_df_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.20, size = 1) +
  geom_text(data = label_df, aes(label = value),
            position = position_dodge(width = 0.5), size = 2) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 5),
        strip.background = element_blank()) +
  ylab("Please indicate if you believe the statement below is factually correct or incorrect:\nIncome inequality in the United States has increased dramatically over time.") +
  facet_wrap(~key, scales = "free")

ggsave(path_output("figure_5.15_trump_white.pdf"),
       plot = figure_5.15, width = 7, height = 7)
ggsave(path_output("figure_5.15_trump_white.png"),
       plot = figure_5.15, width = 7, height = 7, dpi = 300)

write_csv(summary_df, path_output("figure_5.15_trump_white.csv"))

# Appendix table ----

regressions_df <-
  gg_df_plot |>
  group_by(key, value) |>
  reframe(tidy(lm_robust(Y ~ Z, weight = weight, data = pick(everything())))) |>
  filter(term == "ZTreatment", !is.na(value)) |>
  ungroup() |>
  transmute(value,
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high))

overall <-
  trump_white |>
  reframe(tidy(lm_robust(Y ~ Z, weight = weight, data = pick(everything())))) |>
  filter(term == "ZTreatment") |>
  ungroup() |>
  transmute(value = "Overall",
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high))

table_entries <- bind_rows(overall, regressions_df)
write_csv(table_entries, path_output("table_a19_trump_white.csv"))
