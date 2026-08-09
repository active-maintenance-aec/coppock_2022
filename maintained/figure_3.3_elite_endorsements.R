# coppock_2022/maintained/figure_3.3_elite_endorsements.R
# Output: output/figure_3.3_elite_endorsements.pdf
# Depends on: helpers.R, data/elite_endorsements_stacked.rds
# Description: Elite endorsement effects on policy support by topic, sample, and partisanship.

source(here::here("maintained", "helpers.R"))

elite_endorsements_stacked <-
  read_rds(path_original("data", "elite_endorsements_stacked.rds")) |>
  filter(!is.na(Z_party), Z_party != "No cue", pid_3 != "Independent") |>
  mutate(Z_party = factor(Z_party),
         Y = as.numeric(haven::zap_labels(Y)))

elite_plot <-
  elite_endorsements_stacked |>
  group_by(pid_3, Z_party, sample_label, topic, Y) |>
  mutate(
    y_s = sunflower_compat(y = Y, width = 0.09, height = 0.13),
    x_s = sunflower_compat(x = as.numeric(Z_party), width = 0.09, height = 0.13),
    x_s = if_else(pid_3 == "Democrat", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(pid_3 == "Democrat" ~ "D", pid_3 == "Republican" ~ "R")
  )

summary_df <-
  elite_plot |>
  group_by(pid_3, Z_party, sample_label, topic) |>
  reframe(tidy(lm_robust(Y ~ 1, weights = weights, data = pick(everything())))) |>
  mutate(Y = estimate)

label_df <-
  summary_df |>
  filter(Z_party == "Republican party cue",
         sample_label == "Original Study",
         topic == "Foreclosure") |>
  ungroup() |>
  mutate(Y = c(0.5, -0.5), label = c("Democrats", "Republicans"))

figure_3.3 <-
  ggplot(summary_df, aes(factor(Z_party), Y, group = pid_3, shape = pid_3)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5)
  ) +
  geom_text(data = elite_plot, aes(x = x_s, y = y_s, label = plot_letter),
            position = position_dodge(width = 0.5), alpha = 0.35, size = 1) +
  scale_y_continuous(breaks = -1:1) +
  coord_cartesian(ylim = c(-1.1, 1.1)) +
  geom_text(data = label_df, aes(label = label), size = 2,
            position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  ) +
  facet_grid(topic ~ sample_label, scales = "free") +
  ylab("Policy support [-1 oppose, 0 Not sure, 1 support]")

ggsave(path_output("figure_3.3_elite_endorsements.pdf"),
       plot = figure_3.3, width = 7, height = 7)
ggsave(path_output("figure_3.3_elite_endorsements.png"),
       plot = figure_3.3, width = 7, height = 7, dpi = 300)

write_csv(summary_df, path_output("figure_3.3_elite_endorsements.csv"))
