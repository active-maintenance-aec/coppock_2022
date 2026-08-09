# coppock_2022/maintained/figure_5.17_gc_pro_con.R
# Output: output/figure_5.17_gc_pro_con.pdf, output/figure_5.17_gc_pro_con.csv,
#   output/figure_5.17_gc_pro_con_effects.csv
# Depends on: helpers.R, data/GC_study_3_clean.rds
# Description: LRL/GC experiment: capital punishment attitude change under pro+con vs. null+null.

source(here::here("maintained", "helpers.R"))

lrl <- read_rds(path_original("data", "GC_study_3_clean.rds"))

long_df <-
  lrl |>
  select(predisposition, treatment, deter_recode_change, support_recode_change) |>
  pivot_longer(
    cols = c(deter_recode_change, support_recode_change),
    names_to = "dv",
    values_to = "value"
  ) |>
  mutate(dv_label = factor(
    dv,
    levels = c("support_recode_change", "deter_recode_change"),
    labels = c(
      "Which view of capital punishment best summarizes your own?\n[1: I am very much against capital punishment,\n7: I am very much in favor of capital punishment]",
      "Does capital punishment reduce crime?\n[1: I am very certain that capital punishment does not reduce crime,\n7: I am very certain that capital punishment reduces crime]"
    )
  ))

long_df_plot <-
  long_df |>
  group_by(predisposition, treatment, dv_label, value) |>
  mutate(
    y_s = sunflower_compat(y = value, width = 0.08, height = 0.24),
    x_s = sunflower_compat(x = as.numeric(treatment), width = 0.08, height = 0.24),
    x_s = if_else(predisposition == "Capital Punishment Opponents", x_s - (1 / 6), x_s + (1 / 5)),
    plot_letter = case_when(
      predisposition == "Capital Punishment Opponents" ~ "O",
      predisposition == "Capital Punishment Proponents" ~ "P"
    )
  )

summary_df <-
  long_df_plot |>
  group_by(treatment, predisposition, dv_label) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything())))) |>
  rename(value = estimate)

label_df <-
  summary_df |>
  filter(
    treatment == "Null Null",
    dv_label  == "Which view of capital punishment best summarizes your own?\n[1: I am very much against capital punishment,\n7: I am very much in favor of capital punishment]"
  ) |>
  ungroup() |>
  mutate(value = c(0.75, -1.5))

figure_5.17 <-
  ggplot(filter(summary_df, treatment %in% c("Null Null", "Pro Con")),
         aes(treatment, value, group = predisposition)) +
  geom_point(aes(shape = predisposition), size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
  geom_text(data = filter(long_df_plot, treatment %in% c("Null Null", "Pro Con")),
            aes(x = x_s - 2, y = y_s, label = plot_letter), size = 1, alpha = 0.25) +
  geom_text(data = filter(label_df, treatment %in% c("Null Null", "Pro Con")),
            size = 2, hjust = 0.2, aes(label = predisposition),
            position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = -4:4) +
  coord_cartesian(ylim = c(-4, 4)) +
  facet_wrap(~dv_label) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), strip.background = element_blank(),
        strip.text = element_text(size = 4)) +
  ylab("Post - pre difference in capital punishment views")

ggsave(path_output("figure_5.17_gc_pro_con.pdf"),
       plot = figure_5.17, width = 9, height = 5)
ggsave(path_output("figure_5.17_gc_pro_con.png"),
       plot = figure_5.17, width = 9, height = 5, dpi = 300)

write_csv(summary_df, path_output("figure_5.17_gc_pro_con.csv"))

# Treatment effects ----
# The figure plots condition means; the text of chapter 5 quotes the effect of the
# two-sided (Pro Con) condition against the pure control, by predisposition, for both
# outcomes. Those are contrasts rather than means, so without this write the only
# numbers the surrounding paragraph states would have no counterpart in output/.
gc_effects <-
  long_df |>
  mutate(treatment = relevel(factor(treatment), ref = "Null Null")) |>
  group_by(dv, predisposition) |>
  reframe(tidy(lm_robust(value ~ treatment, data = pick(everything())))) |>
  bind_rows(
    long_df |>
      mutate(treatment = relevel(factor(treatment), ref = "Null Null")) |>
      group_by(dv) |>
      reframe(tidy(lm_robust(value ~ treatment, data = pick(everything())))) |>
      mutate(predisposition = "Overall")
  ) |>
  filter(term != "(Intercept)") |>
  transmute(dv, predisposition, term = str_remove(term, "^treatment"),
            estimate, std.error, conf.low, conf.high) |>
  arrange(dv, term, predisposition, .locale = "en")

write_csv(gc_effects, path_output("figure_5.17_gc_pro_con_effects.csv"))
