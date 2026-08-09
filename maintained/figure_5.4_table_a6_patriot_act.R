# coppock_2022/maintained/figure_5.4_table_a6_patriot_act.R
# Output: output/figure_5.4_patriot_act.pdf, output/table_a6_patriot_act.csv
# Depends on: helpers.R, data/patriot_act_stacked.rds
# Description: Patriot Act framing experiment: support by party and sample.

source(here::here("maintained", "helpers.R"))

patriot_act_stacked <-
  read_rds(path_original("data", "patriot_act_stacked.rds")) |>
  filter(T1_content != "Both", pid_3 != "Independent") |>
  mutate(
    Y = PA_support,
    T1_content = fct_drop(T1_content),
    sample_label = factor(sample, levels = c("original", "mt"),
                          labels = c("Original Study", "Mechanical Turk Replication"))
  )

gg_df_plot <-
  patriot_act_stacked |>
  group_by(pid_3, T1_content, sample_label, Y) |>
  mutate(
    y_s = sunflower_compat(y = Y, width = 0.12, height = 0.18),
    x_s = sunflower_compat(x = as.numeric(T1_content), width = 0.12, height = 0.18),
    x_s = if_else(pid_3 == "Democrat", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(pid_3 == "Democrat" ~ "D", pid_3 == "Republican" ~ "R")
  )

summary_df <-
  gg_df_plot |>
  group_by(pid_3, T1_content, sample_label) |>
  reframe(tidy(lm_robust(Y ~ 1, weights = weights, data = pick(everything())))) |>
  mutate(Y = estimate)

label_df <-
  summary_df |>
  filter(T1_content == "Control", sample_label == "Original Study") |>
  ungroup() |>
  mutate(Y = c(3.25, 6), label = c("Democrats", "Republicans"))

figure_5.4 <-
  ggplot(summary_df, aes(T1_content, Y, group = pid_3, shape = pid_3)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.5)) +
  geom_text(data = gg_df_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.2, size = 1) +
  scale_y_continuous(breaks = 1:7) +
  geom_text(data = label_df, aes(label = label),
            position = position_dodge(width = 0.5), size = 2) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        panel.grid.minor = element_blank(), strip.background = element_blank()) +
  ylab("Do you oppose or support the Patriot Act?\n[1: Oppose very strongly to 7: Support very strongly]") +
  facet_wrap(~sample_label, scales = "free")

ggsave(path_output("figure_5.4_patriot_act.pdf"),
       plot = figure_5.4, width = 7, height = 5)
ggsave(path_output("figure_5.4_patriot_act.png"),
       plot = figure_5.4, width = 7, height = 5, dpi = 300)

write_csv(summary_df, path_output("figure_5.4_patriot_act.csv"))

# Appendix table ----

pa_stacked_relevel <-
  read_rds(path_original("data", "patriot_act_stacked.rds")) |>
  mutate(
    sample_label = factor(sample, levels = c("original", "mt"),
                          labels = c("Original Study", "Mechanical Turk Replication")),
    T1_content = relevel(T1_content, ref = "Control")
  )

regressions_df <-
  pa_stacked_relevel |>
  group_by(pid_3, sample_label) |>
  reframe(tidy(lm_robust(Y ~ T1_content, weights = weights, data = pick(everything()))))

overall <-
  pa_stacked_relevel |>
  group_by(sample_label) |>
  reframe(tidy(lm_robust(Y ~ T1_content, weights = weights, data = pick(everything())))) |>
  mutate(pid_3 = "Overall")

table_a6 <-
  bind_rows(overall, regressions_df) |>
  filter(term != "(Intercept)", pid_3 != "Independent") |>
  ungroup() |>
  transmute(
    sample_label,
    term = str_remove(term, "T1_content"),
    pid_3,
    se_entry = make_se_entry(estimate, std.error),
    ci_entry = make_interval_entry(conf.low, conf.high)
  ) |>
  arrange(sample_label, term, .locale = "en")

write_csv(table_a6, path_output("table_a6_patriot_act.csv"))
