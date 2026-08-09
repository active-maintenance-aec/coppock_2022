# coppock_2022/maintained/figure_2.3_table_2.3_gc_replication.R
# Output: output/figure_2.3_gc_replication.pdf, output/figure_2.3_gc_replication.csv,
#   output/table_2.3_gc_replication.csv, output/figure_2.3_gc_replication_n.csv
# Depends on: helpers.R, data/GC_study_3_clean.rds
# Description: Guess & Coppock biased-assimilation replication (direct attitude/belief changes by predisposition).

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
    y_s = sunflower_compat(y = value, width = 0.125, height = 0.125),
    x_s = sunflower_compat(x = as.numeric(treatment), width = 0.125, height = 0.125),
    x_s = if_else(
      predisposition == "Capital Punishment Opponents",
      x_s - (1 / 6), x_s + (1 / 5)
    ),
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

# Table 2.3, lower half ----
# Table 2.3 has two halves. The upper half reproduces Lord, Ross, and Lepper's own table 1
# and is transcribed from the 1979 article, so nothing here can produce it. The lower half
# is the Guess and Coppock replication's own biased-assimilation measures, and the
# deposited data carry them: first_quality/second_quality and
# first_convincing/second_convincing, rated study by study, alongside which study each
# subject saw first. The deposit ships no code for this table, but the quantity is a mean
# of a deposited column rather than a new analysis.
#
# Each subject rated two studies, so the ratings are stacked before averaging: a subject
# contributes one rating to the pro-capital-punishment column and one to the
# anti-capital-punishment column, and the null study is rated too.
ratings <-
  bind_rows(
    lrl |> transmute(predisposition, content = first_content,
                     quality = first_quality, convincing = first_convincing),
    lrl |> transmute(predisposition, content = second_content,
                     quality = second_quality, convincing = second_convincing)
  ) |>
  filter(!is.na(content)) |>
  pivot_longer(c(quality, convincing), names_to = "rating", values_to = "value") |>
  summarize(mean_rating = mean(value, na.rm = TRUE),
            .by = c(rating, content, predisposition))

table_2.3 <-
  ratings |>
  filter(content != "null") |>
  pivot_wider(names_from = content, values_from = mean_rating) |>
  transmute(
    rating,
    predisposition,
    pro_study = pro,
    anti_study = con,
    difference = pro - con
  ) |>
  arrange(rating, predisposition, .locale = "en")

write_csv(table_2.3, path_output("table_2.3_gc_replication.csv"))

# Figure 2.3 ----

figure_2.3 <-
  ggplot(summary_df, aes(treatment, value, group = predisposition)) +
  geom_point(aes(shape = predisposition), size = 2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5)
  ) +
  geom_text(
    data = long_df_plot,
    aes(x = x_s, y = y_s, label = plot_letter),
    size = 1, alpha = 0.25
  ) +
  geom_text(data = label_df, size = 2, aes(label = predisposition),
            position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = -4:4) +
  coord_cartesian(ylim = c(-4, 4)) +
  facet_wrap(~dv_label) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7),
    axis.text.x = element_text(size = 4),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 4)
  ) +
  ylab("Post - pre difference in capital punishment views")

ggsave(path_output("figure_2.3_gc_replication.pdf"),
       plot = figure_2.3, width = 9, height = 5)
ggsave(path_output("figure_2.3_gc_replication.png"),
       plot = figure_2.3, width = 9, height = 5, dpi = 300)

write_csv(summary_df, path_output("figure_2.3_gc_replication.csv"))

# Sample size ----
# Chapter 2 and the note under figure 2.3 both state the number of MTurk subjects.
lrl_n <- tibble(n = nrow(lrl))

write_csv(lrl_n, path_output("figure_2.3_gc_replication_n.csv"))
