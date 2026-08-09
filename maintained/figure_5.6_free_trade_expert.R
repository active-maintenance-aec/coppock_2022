# coppock_2022/maintained/figure_5.6_free_trade_expert.R
# Output: output/figure_5.6_free_trade_expert.pdf
# Depends on: helpers.R, data/free_trade_stacked.rds
# Description: Free trade experiment: expert cue effect by party and sample.

source(here::here("maintained", "helpers.R"))

free_trade <-
  read_rds(path_original("data", "free_trade_stacked.rds")) |>
  filter(pid_3 %in% c("Democrat", "Republican"),
         Z_Hiscox_valence != "Both",
         !is.na(Z_Hiscox_expert),
         !is.na(Y))

ft_plot <-
  free_trade |>
  group_by(sample_label, Z_Hiscox_expert, pid_3, Y) |>
  mutate(
    y_s = sunflower_compat(y = Y, width = 0.11, height = 0.10),
    x_s = sunflower_compat(x = as.numeric(Z_Hiscox_expert), width = 0.11, height = 0.10),
    x_s = if_else(pid_3 == "Democrat", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(pid_3 == "Democrat" ~ "D", pid_3 == "Republican" ~ "R")
  )

expert <-
  ft_plot |>
  group_by(sample_label, Z_Hiscox_expert, pid_3) |>
  reframe(tidy(lm_robust(Y ~ 1, data = pick(everything())))) |>
  mutate(Y = estimate)

label_df <-
  expert |>
  filter(Z_Hiscox_expert == "Expert treatment", sample_label == "Original Study") |>
  ungroup() |>
  mutate(Y = c(0.60, 0.90), label = c("Democrats", "Republicans"))

figure_5.6 <-
  ggplot(expert, aes(Z_Hiscox_expert, Y, group = pid_3, shape = pid_3)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_text(data = ft_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.2, size = 1) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  geom_text(data = label_df, aes(label = label),
            position = position_dodge(width = 0.5), size = 2) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), strip.background = element_blank()) +
  ylab("Do you favor or oppose increasing trade with other nations?\n[0: oppose, 1: favor]") +
  facet_wrap(~sample_label)

ggsave(path_output("figure_5.6_free_trade_expert.pdf"),
       plot = figure_5.6, width = 7, height = 5)
ggsave(path_output("figure_5.6_free_trade_expert.png"),
       plot = figure_5.6, width = 7, height = 5, dpi = 300)

write_csv(expert, path_output("figure_5.6_free_trade_expert.csv"))
