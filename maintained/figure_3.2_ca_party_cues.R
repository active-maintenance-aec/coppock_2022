# coppock_2022/maintained/figure_3.2_ca_party_cues.R
# Output: output/figure_3.2_ca_party_cues.pdf, output/figure_3.2_ca_party_cues.csv,
#   output/figure_3.2_ca_party_cues_n.csv
# Depends on: helpers.R, data/CA_clean.rds
# Description: California judicial retention votes by party cue and partisanship.

source(here::here("maintained", "helpers.R"))

CA <- read_rds(path_original("data", "CA_clean.rds"))

long_df <-
  CA |>
  pivot_longer(
    cols = c(reynoso_Y_3, kaus_Y_3, broussard_Y_3, richardson_Y_3, bird_Y_3),
    names_to = "key",
    values_to = "value"
  ) |>
  mutate(
    Z_coded = case_when(
      Z == "Party Cue" & key %in% c("reynoso_Y_3", "kaus_Y_3", "broussard_Y_3", "bird_Y_3") ~ "Democratic Cue",
      Z == "Party Cue" & key %in% c("richardson_Y_3") ~ "Republican Cue",
      Z == "No Party Cue" ~ "No Party Cue"
    ),
    Z_coded = relevel(factor(Z_coded), ref = "No Party Cue"),
    facet_label = recode(
      key,
      "reynoso_Y_3" = "Justice Reynoso (Brown Appointee)",
      "kaus_Y_3" = "Justice Kaus (Brown Appointee)",
      "broussard_Y_3" = "Justice Broussard (Brown Appointee)",
      "richardson_Y_3" = "Justice Richardson (Reagan Appointee)",
      "bird_Y_3" = "Justice Bird (Brown Appointee)"
    ),
    facet_label = factor(facet_label, levels = c(
      "Justice Reynoso (Brown Appointee)",
      "Justice Kaus (Brown Appointee)",
      "Justice Broussard (Brown Appointee)",
      "Justice Richardson (Reagan Appointee)",
      "Justice Bird (Brown Appointee)"
    ))
  ) |>
  filter(pid_7 != 4)

long_df_plot <-
  long_df |>
  group_by(facet_label, pid_3, Z_coded, value) |>
  mutate(
    y_s = sunflower_compat(y = value, width = 0.12, height = 0.14),
    x_s = sunflower_compat(x = as.numeric(Z_coded), width = 0.12, height = 0.14),
    x_s = if_else(pid_3 == "Democrats", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(pid_3 == "Democrats" ~ "D", pid_3 == "Republicans" ~ "R")
  ) |>
  ungroup() |>
  mutate(x_s = if_else(as.numeric(Z_coded) == 3, x_s - 1, x_s))

gg_df <-
  long_df_plot |>
  group_by(facet_label, pid_3, Z_coded) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything())))) |>
  mutate(value = estimate)

label_df <-
  gg_df |>
  filter(Z_coded != "No Party Cue", facet_label == "Justice Reynoso (Brown Appointee)") |>
  ungroup() |>
  mutate(value = c(0.55, -0.5))

figure_3.2 <-
  ggplot(gg_df, aes(Z_coded, value, group = pid_3, shape = pid_3)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5)
  ) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_text(data = long_df_plot, aes(x = x_s, y = y_s, label = plot_letter),
            position = position_dodge(width = 0.5), alpha = 0.25, size = 1) +
  geom_text(data = label_df, aes(label = pid_3), size = 2) +
  scale_y_continuous(breaks = -1:1, labels = c("Remove", "No opinion", "Keep")) +
  facet_wrap(~facet_label, scales = "free_x", ncol = 3) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 4),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title = element_blank()
  )

ggsave(path_output("figure_3.2_ca_party_cues.pdf"),
       plot = figure_3.2, width = 9, height = 5)
ggsave(path_output("figure_3.2_ca_party_cues.png"),
       plot = figure_3.2, width = 9, height = 5, dpi = 300)

write_csv(gg_df, path_output("figure_3.2_ca_party_cues.csv"))

# Sample size ----
# The note under figure 3.2 states how many subjects the 1982 California poll had.
ca_n <- tibble(n = nrow(CA))

write_csv(ca_n, path_output("figure_3.2_ca_party_cues_n.csv"))
