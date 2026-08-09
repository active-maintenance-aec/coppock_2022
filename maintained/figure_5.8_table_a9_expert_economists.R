# coppock_2022/maintained/figure_5.8_table_a9_expert_economists.R
# Output: output/figure_5.8_expert_economists.pdf, output/table_a9_expert_economists.csv
# Depends on: helpers.R, data/expert_economists_stacked_long.rds
# Description: Expert economist endorsement effects by party, topic, and sample.

source(here::here("maintained", "helpers.R"))

expert_economists_stacked_long <-
  read_rds(path_original("data", "expert_economists_stacked_long.rds")) |>
  filter(pid_3 != "Independent")

gg_df_plot <-
  expert_economists_stacked_long |>
  group_by(pid_3, Z_expert_all, topic, sample_label, Y_w1) |>
  mutate(
    y_s = sunflower_compat(y = Y_w1, width = 0.11, height = 0.085),
    x_s = sunflower_compat(x = as.numeric(Z_expert_all), width = 0.11, height = 0.085),
    x_s = if_else(pid_3 == "Democrat", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(pid_3 == "Democrat" ~ "D", pid_3 == "Republican" ~ "R")
  )

summary_df <-
  gg_df_plot |>
  group_by(pid_3, Z_expert_all, topic, sample_label) |>
  reframe(tidy(lm_robust(Y_w1 ~ 1, weight = weights, data = pick(everything())))) |>
  mutate(Y_w1 = estimate)

label_df <-
  summary_df |>
  filter(Z_expert_all == "Expert Treatment", sample_label == "Original Study", topic == "Immigration") |>
  ungroup() |>
  mutate(Y_w1 = c(0.5, 0.1), label = c("Democrats", "Republicans"))

figure_5.8 <-
  ggplot(summary_df, aes(Z_expert_all, Y_w1, group = pid_3, shape = pid_3)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.5)) +
  geom_text(data = gg_df_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.15, size = 1) +
  geom_text(data = label_df, aes(label = label),
            position = position_dodge(width = 0.5), size = 2) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), strip.background = element_blank()) +
  ylab("1: Agree with economists' position; 0 otherwise") +
  facet_grid(topic ~ sample_label)

ggsave(path_output("figure_5.8_expert_economists.pdf"),
       plot = figure_5.8, width = 7, height = 7)
ggsave(path_output("figure_5.8_expert_economists.png"),
       plot = figure_5.8, width = 7, height = 7, dpi = 300)

write_csv(summary_df, path_output("figure_5.8_expert_economists.csv"))

# Appendix table ----

expert_economists_all <-
  read_rds(path_original("data", "expert_economists_stacked_long.rds"))

estimates_pid_3 <-
  expert_economists_all |>
  group_by(pid_3, topic, sample_label) |>
  reframe(tidy(lm_robust(Y_w1 ~ Z_expert_all, weight = weights, data = pick(everything()))))

estimates_overall <-
  expert_economists_all |>
  group_by(topic, sample_label) |>
  reframe(tidy(lm_robust(Y_w1 ~ Z_expert_all, weight = weights, data = pick(everything())))) |>
  mutate(pid_3 = "Overall")

table_entries <-
  bind_rows(estimates_overall, estimates_pid_3) |>
  filter(term != "(Intercept)", pid_3 != "Independent") |>
  ungroup() |>
  transmute(sample_label, topic, pid_3,
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high)) |>
  arrange(sample_label, topic, pid_3, .locale = "en")

write_csv(table_entries, path_output("table_a9_expert_economists.csv"))
