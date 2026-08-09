# coppock_2022/maintained/figure_5.18_patriot_act_both.R
# Output: output/figure_5.18_patriot_act_both.pdf
# Depends on: helpers.R, data/patriot_act_stacked.rds
# Description: Two-sided framing: Patriot Act support under control vs. both-sides condition.

source(here::here("maintained", "helpers.R"))

patriot_act_stacked <-
  read_rds(path_original("data", "patriot_act_stacked.rds")) |>
  filter(T1_content %in% c("Control", "Both"),
         pid_3 != "Independent") |>
  mutate(
    Y = PA_support,
    T1_content = factor(T1_content, levels = c("Control", "Both")),
    sample_label = factor(sample, levels = c("original", "mt"),
                          labels = c("Original Study", "Mechanical Turk Replication"))
  )

gg_df_plot <-
  patriot_act_stacked |>
  group_by(pid_3, T1_content, sample_label, Y) |>
  mutate(
    y_s = sunflower_compat(y = Y, width = 0.09, height = 0.19),
    x_s = sunflower_compat(x = as.numeric(T1_content), width = 0.09, height = 0.19),
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
  filter(T1_content == "Both", sample_label == "Original Study") |>
  ungroup() |>
  mutate(Y = c(3.25, 6), label = c("Democrats", "Republicans"))

figure_5.18 <-
  ggplot(summary_df, aes(T1_content, Y, group = pid_3, shape = pid_3)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
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

ggsave(path_output("figure_5.18_patriot_act_both.pdf"),
       plot = figure_5.18, width = 7, height = 5)
ggsave(path_output("figure_5.18_patriot_act_both.png"),
       plot = figure_5.18, width = 7, height = 5, dpi = 300)

write_csv(summary_df, path_output("figure_5.18_patriot_act_both.csv"))
