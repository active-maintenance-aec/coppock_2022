# coppock_2022/maintained/figure_5.2_table_a2_gun_control.R
# Output: output/figure_5.2_gun_control.pdf, output/table_a2_gun_control.csv,
#   output/table_a2_gun_control_unrounded.csv
# Depends on: helpers.R, data/GC_study_1_clean.rds
# Description: Gun control information experiment: post-treatment support by proponent status.

source(here::here("maintained", "helpers.R"))

gun_control <-
  read_rds(path_original("data", "GC_study_1_clean.rds")) |>
  filter(!is.na(Z_information)) |>
  mutate(
    Z_information_plot = factor(
      Z_information,
      levels = c("con_information", "control", "pro_information"),
      labels = c("Anti Gun Control Study", "Control", "Pro Gun Control Study")
    ),
    proponent = factor(
      W1_Q1,
      levels = 0:1,
      labels = c("Gun control opponents", "Gun control proponents")
    )
  )

gun_plot <-
  gun_control |>
  group_by(Z_information_plot, proponent, W2_Q1) |>
  mutate(
    y_s = sunflower_compat(y = W2_Q1, width = 0.115, height = 0.06),
    x_s = sunflower_compat(x = as.numeric(Z_information_plot), width = 0.115, height = 0.06),
    x_s = if_else(proponent == "Gun control opponents", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(
      proponent == "Gun control opponents" ~ "O",
      proponent == "Gun control proponents" ~ "P"
    )
  )

summary_df <-
  gun_plot |>
  group_by(Z_information_plot, proponent) |>
  reframe(tidy(lm_robust(W2_Q1 ~ 1, weights = weight, data = pick(everything())))) |>
  mutate(W2_Q1 = estimate)

label_df <-
  summary_df |>
  filter(Z_information_plot == "Control") |>
  ungroup() |>
  mutate(W2_Q1 = c(0.30, 0.82))

figure_5.2 <-
  ggplot(summary_df, aes(Z_information_plot, W2_Q1, group = proponent, shape = proponent)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_text(data = gun_plot, aes(x = x_s, y = y_s, label = plot_letter),
            alpha = 0.35, size = 1) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5)
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(-.1, 1.1)) +
  geom_text(data = label_df, aes(label = proponent),
            position = position_dodge(width = 0.5), size = 2) +
  ylab("Do you support or oppose stricter gun control laws\nin the United States? [0: oppose, 1: support]") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 7),
    axis.title.y = element_text(size = 7)
  )

ggsave(path_output("figure_5.2_gun_control.pdf"),
       plot = figure_5.2, width = 7, height = 5)
ggsave(path_output("figure_5.2_gun_control.png"),
       plot = figure_5.2, width = 7, height = 5, dpi = 300)

write_csv(summary_df, path_output("figure_5.2_gun_control.csv"))

# Appendix table ----

gun_relevel <-
  gun_control |>
  mutate(Z_information_plot = relevel(Z_information_plot, ref = "Control")) |>
  ungroup()

regressions_df <-
  gun_relevel |>
  group_by(proponent) |>
  reframe(tidy(lm_robust(W2_Q1 ~ Z_information_plot, weights = weight, data = pick(everything()))))

overall <-
  gun_relevel |>
  reframe(tidy(lm_robust(W2_Q1 ~ Z_information_plot, weights = weight, data = pick(everything())))) |>
  mutate(proponent = "Overall")

# The unrounded cells are written alongside the formatted table because chapter 5 states
# these effects on the percentage point scale: reading 7.4 back out of a cell already
# rounded to "0.07" gives 7.0.
table_a2_cells <-
  bind_rows(overall, regressions_df) |>
  filter(term != "(Intercept)") |>
  ungroup() |>
  transmute(
    term = gsub("Z_information_plot", "", term),
    proponent,
    estimate, std.error, conf.low, conf.high
  )

table_entries <-
  table_a2_cells |>
  transmute(
    term, proponent,
    se_entry = make_se_entry(estimate, std.error),
    ci_entry = make_interval_entry(conf.low, conf.high)
  )

write_csv(table_a2_cells, path_output("table_a2_gun_control_unrounded.csv"))
write_csv(table_entries, path_output("table_a2_gun_control.csv"))
