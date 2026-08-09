# coppock_2022/maintained/figure_5.10_table_a12_gash_murakami.R
# Output: output/figure_5.10_gash_murakami.pdf, output/table_a12_gash_murakami.csv
# Depends on: helpers.R, data/gash_murakami_clean.rds
# Description: Gash & Murakami affirmative action experiment by demographic subgroup and institution.

source(here::here("maintained", "helpers.R"))

gash_murakami <- read_rds(path_original("data", "gash_murakami_clean.rds"))

long <-
  gash_murakami |>
  pivot_longer(cols = c(Ballot, Courts, Legislature),
               names_to = "experiment", values_to = "condition") |>
  filter(!is.na(condition)) |>
  pivot_longer(cols = c(pid_3, female_2, educ_3, race_4),
               names_to = "key", values_to = "value") |>
  filter(value != "Independent") |>
  mutate(
    condition = factor(condition),
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

long_plot <-
  long |>
  group_by(experiment, condition, key, value, Y) |>
  mutate(
    y_s = sunflower_compat(y = Y, width = 0.09, height = 0.1),
    x_s = sunflower_compat(x = as.numeric(condition), width = 0.09, height = 0.1),
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
  long_plot |>
  group_by(experiment, condition, key, value) |>
  reframe(tidy(lm_robust(Y ~ 1, weights = weight, data = pick(everything())))) |>
  mutate(Y = estimate)

label_df <-
  summary_df |>
  filter(condition == "Treatment", experiment == "Ballot") |>
  ungroup() |>
  mutate(Y = c(2.5, 3.7, 3.5, 2.7, 2.7, 3.8, 2.5, 3.8, 2.1, 3.6))

figure_5.10 <-
  ggplot(summary_df, aes(condition, Y, group = value, shape = value)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
  geom_text(data = long_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.25, size = 1) +
  geom_text(data = label_df, aes(label = value),
            position = position_dodge(width = 0.5), size = 2) +
  scale_shape_manual(values = c(19, 17, 19, 17, 19, 17, 15, 19, 17, 15)) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), strip.background = element_blank()) +
  ylab("Do you agree or disagree with the idea that these companies should not be able to give\nspecial consideration to women when making hiring decisions? [1: Strongly disagree, 4: Strongly agree]") +
  facet_grid(key ~ experiment, scales = "free")

ggsave(path_output("figure_5.10_gash_murakami.pdf"),
       plot = figure_5.10, width = 7, height = 9)
ggsave(path_output("figure_5.10_gash_murakami.png"),
       plot = figure_5.10, width = 7, height = 9, dpi = 300)

write_csv(summary_df, path_output("figure_5.10_gash_murakami.csv"))

# Appendix table ----

regressions_df <-
  long_plot |>
  group_by(experiment, key, value) |>
  reframe(tidy(lm_robust(Y ~ condition, weight = weight, data = pick(everything())))) |>
  filter(term != "(Intercept)", !is.na(value)) |>
  ungroup() |>
  transmute(experiment, value,
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high))

overall <-
  gash_murakami |>
  pivot_longer(cols = c(Ballot, Courts, Legislature), names_to = "experiment", values_to = "condition") |>
  group_by(experiment) |>
  reframe(tidy(lm_robust(Y ~ condition, weight = weight, data = pick(everything())))) |>
  filter(term != "(Intercept)") |>
  ungroup() |>
  transmute(experiment, value = "Overall",
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high))

table_entries <- bind_rows(overall, regressions_df) |> arrange(experiment, .locale = "en")
write_csv(table_entries, path_output("table_a12_gash_murakami.csv"))
