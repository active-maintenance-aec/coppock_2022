# coppock_2022/maintained/figure_1.4_gay_marriage_trends.R
# Output: output/figure_1.4_gay_marriage_trends.{pdf,png,csv},
#         output/figure_1.4_gay_marriage_slopes.csv
# Depends on: helpers.R, data/pew_gay_marriage.rds
# Description: Gay marriage support over time by group; meta-analytic slope estimate.

source(here::here("maintained", "helpers.R"))

gg_df <- read_rds(path_original("data", "pew_gay_marriage.rds")) |>
  filter(demographic != "party2")

summary_df <-
  gg_df |>
  group_by(key) |>
  summarize(
    change_in_points = value[Year == 2019] - value[Year == 2001],
    percent_change = (value[Year == 2019] - value[Year == 2001]) / value[Year == 2001]
  )

write_csv(summary_df, path_output("figure_1.4_gay_marriage_trends.csv"))

figure_1.4 <-
  ggplot(gg_df, aes(Year, value, group = key)) +
  geom_line(alpha = 0.4) +
  geom_text(
    data = filter(gg_df, Year == 2019),
    aes(label = key),
    size = 2.5, hjust = 1, nudge_y = 4
  ) +
  facet_wrap(~demographic, scales = "free", ncol = 2) +
  ylim(0, 100) +
  scale_x_continuous(limits = c(2000, 2020),
                     breaks = c(2000, 2005, 2010, 2015, 2020)) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Percentage of each group favoring gay marriage")

ggsave(path_output("figure_1.4_gay_marriage_trends.pdf"),
       plot = figure_1.4, width = 7, height = 5)
ggsave(path_output("figure_1.4_gay_marriage_trends.png"),
       plot = figure_1.4, width = 7, height = 5, dpi = 300)

# Slope estimates ----
# The book gives one average slope, and the archive reaches it two ways: a random
# effects pooling of the group-specific slopes, and a single regression of support on
# year with group fixed effects. Both are kept here.
#
# The archive pools with rmeta::meta.summaries(method = "random"), which has not been
# updated since 2018. metafor::rma(method = "DL") is the same DerSimonian and Laird
# estimator and reproduces it; REML is reported alongside as an addition, not a
# substitute, because it is what a random effects pooling would use today.
slopes_df <-
  gg_df |>
  group_by(demographic, key) |>
  reframe(tidy(lm_robust(value ~ Year, data = pick(everything())))) |>
  filter(term == "Year")

pooled_dl <- rma(yi = slopes_df$estimate, sei = slopes_df$std.error, method = "DL")
pooled_reml <- rma(yi = slopes_df$estimate, sei = slopes_df$std.error, method = "REML")
fixed_effects_fit <- lm_robust(value ~ Year + key, data = gg_df)

average_slopes <-
  bind_rows(
    tidy(pooled_dl) |> mutate(method = "Random effects pooling (DerSimonian and Laird)"),
    tidy(pooled_reml) |> mutate(method = "Random effects pooling (REML)"),
    tidy(fixed_effects_fit) |>
      filter(term == "Year") |>
      select(estimate, std.error, conf.low, conf.high) |>
      mutate(method = "Regression on year with group fixed effects")
  ) |>
  select(method, estimate, std.error, conf.low, conf.high)

print(average_slopes)

write_csv(
  bind_rows(
    slopes_df |>
      transmute(method = paste0("Group slope: ", demographic, ", ", key),
                estimate, std.error, conf.low, conf.high),
    average_slopes
  ),
  path_output("figure_1.4_gay_marriage_slopes.csv")
)
