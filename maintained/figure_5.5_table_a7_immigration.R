# coppock_2022/maintained/figure_5.5_table_a7_immigration.R
# Output: output/figure_5.5_immigration.pdf, output/table_a7_immigration.csv
# Depends on: helpers.R, data/immigration_stacked.rds
# Description: Immigration experiment: support by party and sample (Brader replication).

source(here::here("maintained", "helpers.R"))

immigration_stacked <- read_rds(path_original("data", "immigration_stacked.rds"))

# Independents are dropped from the figure and from the printed rows of the appendix
# table, but not from the sample the pooled "Overall" regression is fitted on. That is
# the deposit's own sample definition and it is load-bearing: fitting the pooled model
# on Democrats and Republicans alone moves all four cells of each Overall row.
gg_df_plot <-
  immigration_stacked |>
  filter(pid_3 != "Independent") |>
  group_by(pid_3, Z_brader_pos_neg, sample_label, Y) |>
  mutate(
    y_s = sunflower_compat(y = Y, width = 0.1, height = 0.1),
    x_s = sunflower_compat(x = as.numeric(Z_brader_pos_neg), width = 0.1, height = 0.1),
    x_s = if_else(pid_3 == "Democrat", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(pid_3 == "Democrat" ~ "D", pid_3 == "Republican" ~ "R")
  )

summary_df <-
  gg_df_plot |>
  group_by(pid_3, Z_brader_pos_neg, sample_label) |>
  reframe(tidy(lm_robust(Y ~ 1, weights = weights, data = pick(everything())))) |>
  mutate(Y = estimate)

label_df <-
  summary_df |>
  filter(Z_brader_pos_neg == "control", sample_label == "Original Study") |>
  ungroup() |>
  mutate(Y = c(3.5, 1.5), label = c("Democrats", "Republicans"))

figure_5.5 <-
  ggplot(summary_df, aes(Z_brader_pos_neg, Y, group = pid_3, shape = pid_3)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.5)) +
  geom_text(data = gg_df_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.20, size = 1) +
  geom_text(data = label_df, aes(label = label),
            position = position_dodge(width = 0.5), size = 2) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_text(size = 6),
        panel.grid.minor = element_blank(), strip.background = element_blank()) +
  ylab("Do you think the number of immigrants from foreign countries\nwho are permitted to come to the United States to live should be...?\n[1: Decreased a lot, 5: Increased a lot]") +
  facet_wrap(~sample_label)

ggsave(path_output("figure_5.5_immigration.pdf"),
       plot = figure_5.5, width = 7, height = 5)
ggsave(path_output("figure_5.5_immigration.png"),
       plot = figure_5.5, width = 7, height = 5, dpi = 300)

write_csv(summary_df, path_output("figure_5.5_immigration.csv"))

# Appendix table ----

imm_relevel <-
  immigration_stacked |>
  mutate(Z_brader_pos_neg = relevel(Z_brader_pos_neg, ref = "control"))

regressions_df <-
  imm_relevel |>
  group_by(pid_3, sample_label) |>
  reframe(tidy(lm_robust(Y ~ Z_brader_pos_neg, weights = weights, data = pick(everything()))))

overall <-
  imm_relevel |>
  group_by(sample_label) |>
  reframe(tidy(lm_robust(Y ~ Z_brader_pos_neg, weights = weights, data = pick(everything())))) |>
  mutate(pid_3 = "Overall")

table_a7 <-
  bind_rows(overall, regressions_df) |>
  filter(term != "(Intercept)", pid_3 != "Independent") |>
  ungroup() |>
  transmute(
    sample_label,
    term = gsub("Z_brader_pos_neg", "", term),
    pid_3,
    se_entry = make_se_entry(estimate, std.error),
    ci_entry = make_interval_entry(conf.low, conf.high)
  ) |>
  arrange(sample_label, term, pid_3, .locale = "en")

write_csv(table_a7, path_output("table_a7_immigration.csv"))
