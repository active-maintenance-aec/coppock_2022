# coppock_2022/maintained/figure_5.12_table_a16_kreps_wallace.R
# Output: output/figure_5.12_kreps_wallace.pdf, output/table_a16_kreps_wallace.csv
# Depends on: helpers.R, data/kreps_wallace_clean.rds
# Description: Kreps & Wallace drone strikes experiment by demographic subgroup.

source(here::here("maintained", "helpers.R"))

kreps_wallace <- read_rds(path_original("data", "kreps_wallace_clean.rds"))

gg_df <-
  kreps_wallace |>
  filter(!is.na(Z)) |>
  pivot_longer(cols = c(pid_3, female_2, educ_3, race_4),
               names_to = "key", values_to = "value") |>
  filter(value != "Independent") |>
  mutate(
    Z_lab = factor(Z_lab),
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
    y_s = sunflower_compat(y = Y, width = 0.06, height = 0.22),
    x_s = sunflower_compat(x = as.numeric(Z_lab), width = 0.06, height = 0.22),
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
  group_by(key, value, Z_lab) |>
  reframe(tidy(lm_robust(Y ~ 1, weight = weight, data = pick(everything())))) |>
  mutate(Y = estimate)

label_df <-
  summary_df |>
  filter(Z_lab == "Strikes violate\ninternational law") |>
  ungroup() |>
  mutate(Y = c(2.8, 3.7, 2.7, 3.8, 2.6, 3.7, 3.0, 3.8, 3.6, 3.4))

figure_5.12 <-
  ggplot(summary_df, aes(Z_lab, Y, group = value, shape = value)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
  geom_text(data = gg_df_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.10, size = 1) +
  geom_text(data = label_df, aes(label = value),
            position = position_dodge(width = 0.5), size = 2) +
  scale_shape_manual(values = c(19, 17, 19, 17, 19, 17, 15, 19, 17, 15)) +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 6),
        strip.background = element_blank()) +
  ylab("Do you approve or disapprove of the use of drone strikes\nby the United States? [1: Disapprove strongly, 5: Approve strongly]") +
  facet_wrap(~key, scales = "free")

ggsave(path_output("figure_5.12_kreps_wallace.pdf"),
       plot = figure_5.12, width = 7, height = 7)
ggsave(path_output("figure_5.12_kreps_wallace.png"),
       plot = figure_5.12, width = 7, height = 7, dpi = 300)

write_csv(summary_df, path_output("figure_5.12_kreps_wallace.csv"))

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
  kreps_wallace |>
  reframe(tidy(lm_robust(Y ~ Z, weight = weight, data = pick(everything())))) |>
  filter(term == "ZTreatment") |>
  ungroup() |>
  transmute(value = "Overall",
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high))

table_entries <- bind_rows(overall, regressions_df)
write_csv(table_entries, path_output("table_a16_kreps_wallace.csv"))
