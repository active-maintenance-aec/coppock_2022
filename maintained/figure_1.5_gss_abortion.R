# coppock_2022/maintained/figure_1.5_gss_abortion.R
# Output: output/figure_1.5_gss_abortion.pdf
# Depends on: helpers.R, data/gss_abortion.rds
# Description: GSS abortion support over time by party, seven items plus index.

source(here::here("maintained", "helpers.R"))

gss_ab <- read_rds(path_original("data", "gss_abortion.rds")) |>
  mutate(
    pid_3 = case_when(
      partyid %in% c(0, 1, 2) ~ "Democrats",
      partyid %in% c(3) ~ "Independents",
      partyid %in% c(4, 5, 6) ~ "Republicans",
      TRUE ~ NA_character_
    ),
    abnomore_bin = as.numeric(abnomore == 1),
    abdefect_bin = as.numeric(abdefect == 1),
    abhlth_bin = as.numeric(abhlth == 1),
    abpoor_bin = as.numeric(abpoor == 1),
    abrape_bin = as.numeric(abrape == 1),
    absingle_bin = as.numeric(absingle == 1),
    abany_bin = as.numeric(abany == 1),
    ab_index = (abnomore_bin + abdefect_bin + abhlth_bin + abpoor_bin +
                       abrape_bin + absingle_bin + abany_bin) / 7,
    ab_index_2 = as.numeric(ab_index != 0)
  ) |>
  filter(!is.na(pid_3), year > 1976, year != 1986, pid_3 != "Independents")

long_df <-
  gss_ab |>
  pivot_longer(
    cols = c(abnomore_bin, abdefect_bin, abhlth_bin, abpoor_bin,
             abrape_bin, absingle_bin, abany_bin, ab_index_2),
    names_to = "key",
    values_to = "value"
  ) |>
  mutate(key = factor(
    key,
    levels = c("abhlth_bin", "abdefect_bin", "abrape_bin", "abpoor_bin",
               "absingle_bin", "abnomore_bin", "abany_bin", "ab_index_2"),
    labels = c(
      "...if the woman's own health is seriously endangered by the pregnancy?",
      "...if there is a strong chance of serious defect in the baby?",
      "...if she became pregnant as a result of rape?",
      "...if the family has a very low income and cannot afford any more children?",
      "...if she is not married and does not want to marry the man?",
      "...if she is married and does not want any more children?",
      "...if the woman wants it for any reason?",
      "Index: at least one 'Yes'"
    )
  ))

gg_df <-
  long_df |>
  group_by(pid_3, year, key) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything())))) |>
  mutate(
    estimate = estimate * 100,
    conf.low = conf.low * 100,
    conf.high = conf.high * 100
  )

label_df <-
  filter(
    gg_df,
    year == 2018,
    key == "...if the woman's own health is seriously endangered by the pregnancy?"
  )

figure_1.5 <-
  ggplot(gg_df, aes(x = year, y = estimate, shape = pid_3)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 2, alpha = 0.9) +
  geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.2) +
  geom_text(
    data = filter(label_df, pid_3 == "Democrats"),
    aes(label = pid_3), hjust = 1, nudge_y = 5, size = 3
  ) +
  geom_text(
    data = filter(label_df, pid_3 == "Republicans"),
    aes(label = pid_3), hjust = 1, nudge_y = -10, size = 3
  ) +
  theme_bw() +
  coord_cartesian(xlim = c(1975, 2020), ylim = c(0, 100)) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 6),
    axis.title.x = element_blank()
  ) +
  facet_wrap(~key, labeller = label_wrap_gen(width = 45), ncol = 2) +
  labs(y = "Percentage of each group supporting legal abortion...")

ggsave(path_output("figure_1.5_gss_abortion.pdf"),
       plot = figure_1.5, width = 7, height = 8)
ggsave(path_output("figure_1.5_gss_abortion.png"),
       plot = figure_1.5, width = 7, height = 8, dpi = 300)

write_csv(gg_df, path_output("figure_1.5_gss_abortion.csv"))
