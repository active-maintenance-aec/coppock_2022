# coppock_2022/maintained/figure_5.1_table_a4_minimum_wage.R
# Output: output/figure_5.1_minimum_wage.pdf, output/table_a4_minimum_wage.csv,
#   output/table_a4_minimum_wage_unrounded.csv,
#   output/figure_5.1_minimum_wage_pro_vs_anti.csv
# Depends on: helpers.R, data/GC_study_2_clean.rds
# Description: Minimum wage experiment: post-treatment means and ATEs by initial position.

source(here::here("maintained", "helpers.R"))

mw <- read_rds(path_original("data", "GC_study_2_clean.rds"))

gg_df <-
  mw |>
  filter(
    initial_position != "inconsistent_raise",
    condition_type %in% c("ConYoung_ConOld", "Placebo", "ProYoung_ProOld")
  ) |>
  select(initial_position, condition_type, amount_T2, favor_T2_recode) |>
  group_by(initial_position, condition_type, favor_T2_recode) |>
  mutate(
    condition_type = fct_drop(condition_type),
    y_s = sunflower_compat(y = favor_T2_recode, width = 0.09, height = 0.13),
    x_s = sunflower_compat(x = as.numeric(condition_type), width = 0.09, height = 0.15),
    x_s = if_else(initial_position == "pro_raise", x_s + (1 / 8), x_s - (1 / 8)),
    plot_letter = case_when(
      initial_position == "pro_raise" ~ "P",
      initial_position == "con_raise" ~ "O"
    )
  )

long_df <-
  gg_df |>
  pivot_longer(cols = c(amount_T2, favor_T2_recode), names_to = "name", values_to = "value") |>
  mutate(
    dv_label = factor(
      name,
      levels = c("amount_T2", "favor_T2_recode"),
      labels = c(
        "What do you think the federal minimum wage should be? [$0.00 - $25.00]",
        "Do you favor or oppose raising the federal minimum wage? [1: Very much opposed, 7: Very much in favor]"
      )
    ),
    y_s = if_else(name == "favor_T2_recode", y_s, value),
    condition_type = factor(
      condition_type,
      levels = c("ConYoung_ConOld", "Placebo", "ProYoung_ProOld"),
      labels = c("Two Anti Minimum Wage\nVideos", "Two Placebo\nVideos", "Two Pro Minimum Wage\nVideos")
    ),
    initial_position = factor(
      initial_position,
      levels = c("con_raise", "pro_raise"),
      labels = c("Opponents", "Proponents")
    )
  )

summary_df <-
  long_df |>
  group_by(initial_position, condition_type, dv_label) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything())))) |>
  rename(value = estimate)

label_df <-
  summary_df |>
  ungroup() |>
  filter(
    condition_type == "Two Placebo\nVideos",
    dv_label == "What do you think the federal minimum wage should be? [$0.00 - $25.00]"
  ) |>
  mutate(value = c(5, 13))

my_breaks <- function(x) {
  if (max(x) > 10) seq(0, 25, 5) else 1:7
}

figure_5.1 <-
  ggplot(summary_df, aes(condition_type, value, group = initial_position)) +
  geom_point(aes(shape = initial_position), size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5)
  ) +
  geom_text(data = long_df, aes(label = plot_letter, x = x_s, y = y_s),
            alpha = 0.3, size = 1) +
  geom_text(data = label_df, aes(label = initial_position),
            position = position_dodge(width = 0.5), size = 2) +
  scale_y_continuous(breaks = my_breaks) +
  facet_wrap(~dv_label, labeller = label_wrap_gen(width = 60), scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(path_output("figure_5.1_minimum_wage.pdf"),
       plot = figure_5.1, width = 9, height = 5)
ggsave(path_output("figure_5.1_minimum_wage.png"),
       plot = figure_5.1, width = 9, height = 5, dpi = 300)

write_csv(summary_df, path_output("figure_5.1_minimum_wage.csv"))

# Appendix table ----

long_df_relevel <-
  long_df |>
  mutate(condition_type = relevel(condition_type, ref = "Two Placebo\nVideos"))

regressions_df <-
  long_df_relevel |>
  group_by(initial_position, dv_label) |>
  reframe(tidy(lm_robust(value ~ condition_type, data = pick(everything())))) |>
  filter(term != "(Intercept)")

overall <-
  long_df_relevel |>
  group_by(dv_label) |>
  reframe(tidy(lm_robust(value ~ condition_type, data = pick(everything())))) |>
  filter(term != "(Intercept)") |>
  mutate(initial_position = "Overall")

# The unrounded cells are written alongside the formatted table. Reading a value back
# out of "0.28 (0.20)" is fine when the comparison is against a page printing two
# decimals, and wrong the moment anything derives a further quantity from it: the text's
# pro-versus-anti contrasts below are differences of these cells, and differencing two
# numbers that have already been rounded moves the answer.
table_a4_cells <-
  bind_rows(overall, regressions_df) |>
  ungroup() |>
  transmute(
    dv = if_else(str_detect(dv_label, "minimum wage should be"), "Amount", "Favor"),
    term = gsub("condition_type", "", term),
    initial_position,
    estimate, std.error, conf.low, conf.high
  ) |>
  arrange(dv, term, initial_position, .locale = "en")

table_a4 <-
  table_a4_cells |>
  transmute(
    dv, term, initial_position,
    se_entry = make_se_entry(estimate, std.error),
    ci_entry = make_interval_entry(conf.low, conf.high)
  )

write_csv(table_a4_cells, path_output("table_a4_minimum_wage_unrounded.csv"))
write_csv(table_a4, path_output("table_a4_minimum_wage.csv"))

# Pro versus anti contrast ----
# Chapter 5 quotes the effect of the two pro videos relative to the two anti videos,
# which is a different contrast from the against-placebo one the appendix table reports
# and needs its own fit for the standard error.
long_df_pro_vs_anti <-
  long_df |>
  filter(condition_type != "Two Placebo\nVideos") |>
  mutate(condition_type = fct_drop(relevel(condition_type, ref = "Two Anti Minimum Wage\nVideos")))

mw_pro_vs_anti <-
  long_df_pro_vs_anti |>
  group_by(initial_position, dv_label) |>
  reframe(tidy(lm_robust(value ~ condition_type, data = pick(everything())))) |>
  filter(term != "(Intercept)") |>
  transmute(
    dv = if_else(str_detect(dv_label, "minimum wage should be"), "Amount", "Favor"),
    initial_position,
    estimate, std.error, conf.low, conf.high
  ) |>
  arrange(dv, initial_position, .locale = "en")

write_csv(mw_pro_vs_anti, path_output("figure_5.1_minimum_wage_pro_vs_anti.csv"))
