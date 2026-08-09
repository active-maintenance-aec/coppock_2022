# coppock_2022/maintained/figure_2.2_table_2.2_lrl_biased_assimilation.R
# Output: output/figure_2.2_lrl_biased_assimilation.pdf, output/table_2.2_lrl_reanalysis.csv
# Depends on: helpers.R, data/GC_study_3_clean.rds
# Description: Lord, Ross, and Lepper (1979) reanalysis: attitude and belief change by predisposition and study content.

source(here::here("maintained", "helpers.R"))

lrl <- read_rds(path_original("data", "GC_study_3_clean.rds"))

gg_df <-
  lrl |>
  pivot_longer(
    cols = -c(anon_id, treatment, support_recode_T1, support_recode_T2, support_recode_T3,
              condition.factor, predisposition, deter_recode_change, support_recode_change),
    names_to = "key",
    values_to = "raw_value",
    values_transform = list(raw_value = as.character)
  ) |>
  separate(key, c("study_number", "var"), sep = "_", extra = "merge") |>
  pivot_wider(names_from = var, values_from = raw_value) |>
  pivot_longer(cols = c(attitude, belief), names_to = "outcome_variable", values_to = "value") |>
  mutate(
    value = as.numeric(value),
    content_factor = factor(
      content,
      levels = c("con", "null", "pro"),
      labels = c("Con Study", "Null Study", "Pro Study")
    ),
    study_number_factor = factor(
      study_number,
      levels = c("first", "second"),
      labels = c("First study seen", "Second study seen")
    ),
    outcome_text = factor(
      outcome_variable,
      levels = c("attitude", "belief"),
      labels = c(
        "How, if at all, has your attitude toward the death penalty changed based on the results and subsequent description and critiques of the study?",
        "How, if at all, has your belief changed about the efficacy of the death penalty in deterring crime?"
      )
    )
  )

# Table 2.2 ----
# Cell means by content (pro/con) × predisposition × outcome.
# "Combined" = sum of the pro-study and con-study cell means, matching the book's row
# structure. The labels here are the ones the data support: published table 2.2 prints the
# con-study means under "After pro-capital punishment study" and the pro-study means under
# "After anti-capital punishment study", which is the transposition errata.qmd corrects.
cell_means <-
  gg_df |>
  filter(content_factor != "Null Study") |>
  group_by(outcome_variable, content_factor, predisposition) |>
  summarize(Y = mean(value, na.rm = TRUE), .groups = "drop")

combined_rows <-
  cell_means |>
  group_by(outcome_variable, predisposition) |>
  summarize(Y = sum(Y), .groups = "drop") |>
  mutate(content_factor = factor("Combined",
                                 levels = c("Con Study", "Pro Study", "Combined")))

table_2.2 <-
  bind_rows(cell_means, combined_rows) |>
  mutate(
    content_factor = factor(
      content_factor,
      levels = c("Con Study", "Pro Study", "Combined"),
      labels = c("After anti capital punishment study",
                 "After pro capital punishment study",
                 "Combined")
    )
  ) |>
  pivot_wider(names_from = predisposition, values_from = Y) |>
  arrange(outcome_variable, content_factor, .locale = "en")

write_csv(table_2.2, path_output("table_2.2_lrl_reanalysis.csv"))
print(table_2.2 |> mutate(across(where(is.numeric), \(x) round(x, 2))))

# Figure 2.2 ----

gg_df_plot <-
  gg_df |>
  group_by(predisposition, content_factor, outcome_text, value) |>
  mutate(
    y_s = sunflower_compat(y = value, width = 0.06, height = 0.25),
    x_s = sunflower_compat(x = as.numeric(content_factor), width = 0.06, height = 0.25),
    x_s = if_else(predisposition == "Capital Punishment Opponents", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(
      predisposition == "Capital Punishment Opponents" ~ "O",
      predisposition == "Capital Punishment Proponents" ~ "P"
    )
  )

summary_df <-
  gg_df_plot |>
  group_by(content_factor, predisposition, outcome_text) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything())))) |>
  mutate(value = estimate)

pro_label_df <-
  summary_df |>
  filter(content_factor == "Pro Study",
         predisposition == "Capital Punishment Proponents")

opp_label_df <-
  summary_df |>
  filter(content_factor == "Con Study",
         predisposition == "Capital Punishment Opponents")

figure_2.2 <-
  ggplot(summary_df,
         aes(content_factor, value, group = predisposition, fill = predisposition)) +
  geom_point(aes(shape = predisposition), size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5)
  ) +
  geom_text(
    data = gg_df_plot,
    aes(x = x_s, y = y_s, label = plot_letter),
    position = position_dodge(width = 0.5),
    size = 1, alpha = 0.25
  ) +
  geom_text(data = pro_label_df, aes(label = predisposition),
            size = 2, hjust = 1, nudge_y = 1.5, nudge_x = 0.25) +
  geom_text(data = opp_label_df, aes(label = predisposition),
            size = 2, hjust = 0, nudge_y = -1.5, nudge_x = -0.25) +
  scale_y_continuous(breaks = seq(-8, 8, 2)) +
  coord_cartesian(ylim = c(-8, 8)) +
  facet_grid(~outcome_text, labeller = label_wrap_gen(55)) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 5)
  ) +
  ylab("Self-reported attitude change (-8 to 8)")

ggsave(path_output("figure_2.2_lrl_biased_assimilation.pdf"),
       plot = figure_2.2, width = 9, height = 5)
ggsave(path_output("figure_2.2_lrl_biased_assimilation.png"),
       plot = figure_2.2, width = 9, height = 5, dpi = 300)

write_csv(summary_df, path_output("figure_2.2_lrl_biased_assimilation.csv"))
