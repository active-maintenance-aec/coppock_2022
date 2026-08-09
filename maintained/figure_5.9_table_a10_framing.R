# coppock_2022/maintained/figure_5.9_table_a10_framing.R
# Output: output/figure_5.9_framing.pdf, output/table_a10_framing.csv
# Depends on: helpers.R, data/frame_breath_topic.rds
# Description: Hopkins & Mummolo framing experiment: spending support by party, topic, and sample.

source(here::here("maintained", "helpers.R"))

frame_breath_topic_all <- read_rds(path_original("data", "frame_breath_topic.rds"))

frame_breath_topic <-
  frame_breath_topic_all |>
  filter(pid_3 != "Independent") |>
  mutate(Z = factor(Z, levels = 0:1, labels = c("Control", "Treatment Argument")))

gg_df_plot <-
  frame_breath_topic |>
  group_by(pid_3, Z, topic, sample_label, Y_w1) |>
  mutate(
    y_s = sunflower_compat(y = Y_w1, width = 0.09, height = 0.28),
    x_s = sunflower_compat(x = as.numeric(Z), width = 0.09, height = 0.28),
    x_s = if_else(pid_3 == "Democrat", x_s - (1 / 8), x_s + (1 / 8)),
    plot_letter = case_when(pid_3 == "Democrat" ~ "D", pid_3 == "Republican" ~ "R")
  )

summary_df <-
  gg_df_plot |>
  group_by(pid_3, Z, topic, sample_label) |>
  reframe(tidy(lm_robust(Y_w1 ~ 1, weights = weights, data = pick(everything())))) |>
  mutate(Y_w1 = estimate)

label_df <-
  summary_df |>
  filter(Z == "Treatment Argument", topic == "Crime", sample_label == "Original Study") |>
  ungroup() |>
  mutate(Y_w1 = c(5.5, 3.5), label = c("Democrats", "Republicans"))

topics <- unique(summary_df$topic)
wordings <- c(
  "Should federal spending on dealing with crime be \n increased, decreased, or kept the same? [1-7]",
  "Should federal spending on health care be \n increased, decreased, or kept the same? [1-7]",
  "Should federal spending to stimulate the economy \n be increased, decreased, or kept the same? [1-7]",
  "Should federal spending on the war on terrorism \n be increased, decreased, or kept the same? [1-7]"
)

panel_list <- vector("list", length(topics))
for (i in seq_along(topics)) {
  panel_list[[i]] <-
    ggplot(filter(summary_df, topic == topics[i]),
           aes(Z, Y_w1, group = pid_3, shape = pid_3)) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_line(position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5)) +
    geom_text(data = filter(gg_df_plot, topic == topics[i]),
              aes(x = x_s, y = y_s, label = plot_letter), alpha = 0.07, size = 1) +
    scale_y_continuous(breaks = 1:7) +
    geom_text(data = filter(label_df, topic == topics[i]),
              aes(label = label), position = position_dodge(width = 0.5), size = 2) +
    theme_bw() +
    theme(legend.position = "none", axis.title.x = element_blank(),
          axis.title.y = element_text(size = 4),
          panel.grid.minor = element_blank(), strip.background = element_blank()) +
    ylab(wordings[i]) +
    facet_grid(topic ~ sample_label)
}

figure_5.9 <- wrap_plots(A = panel_list[[1]], B = panel_list[[2]],
                          C = panel_list[[3]], D = panel_list[[4]], ncol = 1)

ggsave(path_output("figure_5.9_framing.pdf"),
       plot = figure_5.9, width = 7, height = 12)
ggsave(path_output("figure_5.9_framing.png"),
       plot = figure_5.9, width = 7, height = 12, dpi = 300)

write_csv(summary_df, path_output("figure_5.9_framing.csv"))

# Appendix table ----

fbt_all_z <-
  frame_breath_topic_all |>
  mutate(Z = factor(Z, levels = 0:1, labels = c("Control", "Treatment Argument")))

bypid <-
  fbt_all_z |>
  group_by(pid_3, topic, sample_label) |>
  reframe(tidy(lm_robust(Y_w1 ~ Z, weights = weights, data = pick(everything()))))

overall <-
  fbt_all_z |>
  group_by(topic, sample_label) |>
  reframe(tidy(lm_robust(Y_w1 ~ Z, weights = weights, data = pick(everything())))) |>
  mutate(pid_3 = "Overall")

table_entries <-
  bind_rows(overall, bypid) |>
  filter(term != "(Intercept)", pid_3 != "Independent") |>
  ungroup() |>
  transmute(sample_label, topic, pid_3,
            se_entry = make_se_entry(estimate, std.error),
            ci_entry = make_interval_entry(conf.low, conf.high)) |>
  arrange(sample_label, topic, pid_3, .locale = "en")

write_csv(table_entries, path_output("table_a10_framing.csv"))
