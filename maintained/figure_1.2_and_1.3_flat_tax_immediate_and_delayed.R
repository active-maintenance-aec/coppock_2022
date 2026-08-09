# coppock_2022/maintained/figure_1.2_and_1.3_flat_tax_immediate_and_delayed.R
# Output: output/figure_1.2_flat_tax_immediate.pdf, output/figure_1.3_flat_tax_10day.pdf,
#   output/figure_1.2_and_1.3_flat_tax_effects.csv,
#   output/figure_1.2_and_1.3_flat_tax_persistence.csv,
#   output/figure_1.2_and_1.3_flat_tax_n.csv
# Depends on: helpers.R, data/elite_opeds_cleaned.rds, data/mturk_opeds_cleaned.rds
# Description: Flat tax op-ed experiment: immediate (Fig 1.2) and 10-day (Fig 1.3) attitude means by party.

source(here::here("maintained", "helpers.R"))

elite_opeds <- read_rds(path_original("data", "elite_opeds_cleaned.rds"))
mturk_opeds <- read_rds(path_original("data", "mturk_opeds_cleaned.rds"))

opeds <-
  bind_rows(
    `Policy Professional Sample` = elite_opeds,
    `Mechanical Turk Sample` = mturk_opeds,
    .id = "sample"
  ) |>
  filter(!is.na(pid_3_cat),
         pid_3_cat %in% c("Democrat", "Republican"))

tax_df <-
  opeds |>
  filter(Z %in% c("control", "paul"),
         !is.na(dv_flat_1_w2)) |>
  mutate(Z_label = factor(
    Z,
    levels = c("control", "paul"),
    labels = c("No op-ed", "Pro-flat tax op-ed")
  ))

# Figure 1.2: immediate attitude (wave 1) ----

tax_df_w1 <-
  tax_df |>
  group_by(pid_3_cat, Z_label, sample, dv_flat_1_w1) |>
  mutate(
    y_s = sunflower_compat(y = dv_flat_1_w1, width = 0.09, height = 0.2),
    x_s = sunflower_compat(x = as.numeric(Z_label), width = 0.09, height = 0.2),
    x_s = if_else(pid_3_cat == "Democrat", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(
      pid_3_cat == "Democrat" ~ "D",
      pid_3_cat == "Republican" ~ "R"
    )
  )

summary_w1 <-
  tax_df_w1 |>
  group_by(pid_3_cat, Z_label, sample) |>
  reframe(tidy(lm_robust(dv_flat_1_w1 ~ 1, data = pick(everything())))) |>
  mutate(dv_flat_1_w1 = estimate)

label_w1 <-
  summary_w1 |>
  filter(Z_label == "Pro-flat tax op-ed",
         sample == "Mechanical Turk Sample") |>
  ungroup() |>
  mutate(dv_flat_1_w1 = c(3.5, 6.5))

figure_1.2 <-
  ggplot(summary_w1,
         aes(Z_label, dv_flat_1_w1,
             group = pid_3_cat, shape = pid_3_cat)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5)
  ) +
  geom_text(
    data = tax_df_w1,
    aes(x = x_s, y = y_s, label = plot_letter),
    alpha = 0.35, size = 1
  ) +
  geom_text(data = label_w1, aes(label = pid_3_cat),
            position = position_dodge(width = 0.5), size = 3) +
  scale_y_continuous(breaks = 1:7) +
  facet_grid(~sample) +
  ylab(
    "Would you favor or oppose changing the federal tax system to a flat tax,\nwhere everyone making more than $50,000 a year pays the same percentage\nof his or her income in taxes? [1: Strongly oppose to 7: Strongly favor]"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  )

ggsave(path_output("figure_1.2_flat_tax_immediate.pdf"),
       plot = figure_1.2, width = 7, height = 5)
ggsave(path_output("figure_1.2_flat_tax_immediate.png"),
       plot = figure_1.2, width = 7, height = 5, dpi = 300)

write_csv(summary_w1, path_output("figure_1.2_flat_tax_immediate.csv"))

# Figure 1.3: 10-day attitude (wave 2) ----

tax_df_w2 <-
  tax_df |>
  group_by(pid_3_cat, Z_label, sample, dv_flat_1_w2) |>
  mutate(
    y_s = sunflower_compat(y = dv_flat_1_w2, width = 0.09, height = 0.2),
    x_s = sunflower_compat(x = as.numeric(Z_label), width = 0.09, height = 0.2),
    x_s = if_else(pid_3_cat == "Democrat", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(
      pid_3_cat == "Democrat" ~ "D",
      pid_3_cat == "Republican" ~ "R"
    )
  )

summary_w2 <-
  tax_df_w2 |>
  group_by(pid_3_cat, Z_label, sample) |>
  reframe(tidy(lm_robust(dv_flat_1_w2 ~ 1, data = pick(everything())))) |>
  mutate(dv_flat_1_w2 = estimate)

label_w2 <-
  summary_w2 |>
  filter(Z_label == "Pro-flat tax op-ed",
         sample == "Mechanical Turk Sample") |>
  ungroup() |>
  mutate(dv_flat_1_w2 = c(3.0, 5.70))

figure_1.3 <-
  ggplot(summary_w2,
         aes(Z_label, dv_flat_1_w2,
             group = pid_3_cat, shape = pid_3_cat)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5)
  ) +
  geom_text(
    data = tax_df_w2,
    aes(x = x_s, y = y_s, label = plot_letter),
    alpha = 0.35, size = 1
  ) +
  geom_text(data = label_w2, aes(label = pid_3_cat),
            position = position_dodge(width = 0.5), size = 3) +
  scale_y_continuous(breaks = 1:7) +
  facet_grid(~sample) +
  ylab(
    "Would you favor or oppose changing the federal tax system to a flat tax...?\n[1: Strongly oppose to 7: Strongly favor]\nAsked 10 days after treatment"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  )

ggsave(path_output("figure_1.3_flat_tax_10day.pdf"),
       plot = figure_1.3, width = 7, height = 5)
ggsave(path_output("figure_1.3_flat_tax_10day.png"),
       plot = figure_1.3, width = 7, height = 5, dpi = 300)

write_csv(summary_w2, path_output("figure_1.3_flat_tax_10day.csv"))

# Treatment effects and persistence ----
# The figures plot group means; the book's text states the op-ed's average effect by
# party and sample at each wave, and the share of the immediate effect still present
# after ten days. Those are differences rather than means, so they need their own fit
# and their own file: computing them anywhere but an analysis script would leave the
# only numbers chapter 1 quotes outside the reproduction check.

flat_tax_effects <-
  bind_rows(
    `Immediate` = tax_df |> mutate(Y = dv_flat_1_w1),
    `10-day follow-up` = tax_df |> mutate(Y = dv_flat_1_w2),
    .id = "wave"
  ) |>
  group_by(wave, sample, pid_3_cat) |>
  reframe(tidy(lm_robust(Y ~ Z_label, data = pick(everything())))) |>
  filter(term != "(Intercept)") |>
  select(wave, sample, pid_3_cat, estimate, std.error, conf.low, conf.high)

flat_tax_persistence <-
  flat_tax_effects |>
  select(wave, sample, pid_3_cat, estimate) |>
  pivot_wider(names_from = wave, values_from = estimate) |>
  transmute(sample, pid_3_cat,
            persistence_ratio = `10-day follow-up` / Immediate)

write_csv(flat_tax_effects, path_output("figure_1.2_and_1.3_flat_tax_effects.csv"))
write_csv(flat_tax_persistence, path_output("figure_1.2_and_1.3_flat_tax_persistence.csv"))

# Panel sizes ----
# The notes under both figures state how many respondents provided an immediate and a
# ten-day response, so the count belongs in output/ like any other quoted number.
flat_tax_n <- tax_df |> summarize(n = n(), .by = sample)

write_csv(flat_tax_n, path_output("figure_1.2_and_1.3_flat_tax_n.csv"))
