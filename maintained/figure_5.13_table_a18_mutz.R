# coppock_2022/maintained/figure_5.13_table_a18_mutz.R
# Output: output/figure_5.13_mutz.pdf, output/table_a18_mutz.csv
# Depends on: helpers.R, data/mutz_clean.rds
# Description: Mutz trade experiment by demographic subgroup.

source(here::here("maintained", "helpers.R"))

mutz <- read_rds(path_original("data", "mutz_clean.rds"))

gg_df <-
  mutz |>
  filter(!is.na(Z)) |>
  pivot_longer(cols = c(pid_3, female_2, educ_3),
               names_to = "key", values_to = "value") |>
  filter(value != "Independent") |>
  mutate(
    Z = factor(Z),
    key = factor(key,
                 levels = c("pid_3", "female_2", "educ_3"),
                 labels = c("Partisanship", "Gender", "Education")),
    value = factor(value,
                   levels = c("Democrat", "Republican", "Female", "Male",
                              "High School or Less", "Some College", "Bachelor's Degree or Higher"),
                   labels = c("Democrat", "Republican", "Women", "Men",
                              "High School", "Some College", "College"))
  ) |>
  filter(!is.na(value))

gg_df_plot <-
  gg_df |>
  group_by(key, value, Z, Y) |>
  mutate(
    y_s = sunflower_compat(y = Y, width = 0.08, height = 0.22),
    x_s = sunflower_compat(x = as.numeric(Z), width = 0.08, height = 0.22),
    x_s = case_when(
      value == "Republican" ~ x_s + (1 / 8),  value == "Democrat"     ~ x_s - (1 / 8),
      value == "Men" ~ x_s + (1 / 8),  value == "Women"        ~ x_s - (1 / 8),
      value == "High School" ~ x_s - (1 / 6),  value == "Some College" ~ x_s,
      value == "College" ~ x_s + (1 / 6)
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
  filter(Z == "Job lost to trade") |>
  ungroup() |>
  mutate(Y = c(2.9, 1.8, 2.9, 1.8, 3.0, 2.8, 2.6))

figure_5.13 <-
  ggplot(summary_df, aes(Z, Y, group = value, shape = value)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
  geom_text(data = gg_df_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.20, size = 1) +
  geom_text(data = label_df, aes(label = value),
            position = position_dodge(width = 0.5), size = 2) +
  scale_shape_manual(values = c(19, 17, 19, 17, 19, 17, 15)) +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 6),
        strip.background = element_blank()) +
  ylab("Do you favor or oppose the federal government in Washington negotiating\nmore free trade agreements? [1: Strongly favor, 4: Strongly oppose]") +
  facet_wrap(~key, scales = "free", ncol = 2)

ggsave(path_output("figure_5.13_mutz.pdf"),
       plot = figure_5.13, width = 7, height = 7)
ggsave(path_output("figure_5.13_mutz.png"),
       plot = figure_5.13, width = 7, height = 7, dpi = 300)

write_csv(summary_df, path_output("figure_5.13_mutz.csv"))

# Appendix table ----

regressions_df <-
  gg_df_plot |>
  group_by(key, value) |>
  reframe(tidy(lm_robust(Y ~ Z, weight = weight, data = pick(everything())))) |>
  filter(term == "ZJob lost to trade", !is.na(value)) |>
  ungroup() |>
  transmute(value,
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high))

overall <-
  mutz |>
  reframe(tidy(lm_robust(Y ~ Z, weight = weight, data = pick(everything())))) |>
  filter(term == "ZJob lost to trade") |>
  ungroup() |>
  transmute(value = "Overall",
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high))

table_a18_mutz <- bind_rows(overall, regressions_df)
write_csv(table_a18_mutz, path_output("table_a18_mutz.csv"))
