# coppock_2022/maintained/figure_5.16_cate_correlations.R
# Output: output/figure_5.16_cate_correlations.pdf, output/figure_5.16_cate_correlations.csv,
#   output/figure_5.16_cate_pairs.csv
# Depends on: helpers.R, multiple data files
# Description: Cross-study CATE correlations across demographic subgroups (partisanship, ideology, race, gender, age, education).
# Note: archive used geom_errorbarh() (deprecated in ggplot2 4.0); replaced with geom_linerange().
# Note: archive used melt()/dcast() from reshape2; replaced with pivot_longer()/pivot_wider().

source(here::here("maintained", "helpers.R"))

# Load data ----
patriot_act_stacked <- read_rds(path_original("data", "patriot_act_stacked.rds"))
immigration_stacked <- read_rds(path_original("data", "immigration_stacked.rds"))
free_trade_stacked <- read_rds(path_original("data", "free_trade_stacked.rds"))
expert_economists_stacked_long <- read_rds(path_original("data", "expert_economists_stacked_long.rds"))
opeds_long <- read_rds(path_original("data", "opeds_long.rds"))
frame_breath_topic <- read_rds(path_original("data", "frame_breath_topic.rds"))
death_penalty_stacked <- read_rds(path_original("data", "death_penalty_stacked.rds"))
kreps_wallace <- read_rds(path_original("data", "kreps_wallace_clean.rds"))
trump_white <- read_rds(path_original("data", "trump_white_clean.rds"))
flavin <- read_rds(path_original("data", "flavin_clean.rds"))
gash_murakami <- read_rds(path_original("data", "gash_murakami_clean.rds"))
mutz <- read_rds(path_original("data", "mutz_clean.rds"))

# Reshape each dataset to long attribute format ----
attrs <- c("pid_3", "female", "race_4", "ideo_3", "college", "age_3")

# values_transform is needed throughout because the attribute columns are mixed types
# (factor and double), which pivot_longer refuses to combine.

patriot_act <-
  patriot_act_stacked |>
  filter(T1_content != "Both", !is.na(Y_w1_s)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

immigration <-
  immigration_stacked |>
  filter(!is.na(Y_w1_s)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

free_trade <-
  free_trade_stacked |>
  filter(Z_Hiscox_valence != "both", !is.na(Y_w1_s)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

expert_economists <-
  expert_economists_stacked_long |>
  filter(!is.na(Y_w1_s)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

opeds <-
  opeds_long |>
  filter(!is.na(Y_w1_s)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

frame_breath <-
  frame_breath_topic |>
  filter(!is.na(Y_w1_s)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

death_penalty <-
  death_penalty_stacked |>
  filter(!is.na(Y_w1_s)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

flavin_l <-
  flavin |>
  filter(!is.na(Z)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

gash_murakami_l <-
  gash_murakami |>
  pivot_longer(cols = c(Ballot, Courts, Legislature), names_to = "topic", values_to = "condition") |>
  filter(!is.na(condition)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

mutz_l <-
  mutz |>
  filter(!is.na(Z)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

trump_white_l <-
  trump_white |>
  filter(!is.na(Z)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

kreps_wallace_l <-
  kreps_wallace |>
  filter(!is.na(Z)) |>
  pivot_longer(cols = all_of(attrs), names_to = "attribute", values_to = "level",
               values_transform = list(level = as.character)) |>
  filter(!is.na(level))

# Estimate CATEs ----
patriot_act_ests <- patriot_act     |> group_by(attribute, level, sample_label) |> reframe(tidy(lm_robust(Y_w1_s ~ T1_content, weights = weights, data = pick(everything()))))
immigration_ests <- immigration     |> group_by(attribute, level, sample_label) |> reframe(tidy(lm_robust(Y_w1_s ~ Z_brader_pos_neg, weights = weights, data = pick(everything()))))
free_trade_expert <- free_trade      |> group_by(attribute, level, sample_label) |> reframe(tidy(lm_robust(Y_w1_s ~ Z_Hiscox_expert, weights = weights, data = pick(everything()))))
free_trade_valence <- free_trade      |> group_by(attribute, level, sample_label) |> reframe(tidy(lm_robust(Y_w1_s ~ Z_Hiscox_valence, weights = weights, data = pick(everything()))))
expert_economists_ests <- expert_economists |> group_by(attribute, level, sample_label) |> reframe(tidy(lm_robust(Y_w1_s ~ Z_expert_all, weights = weights, data = pick(everything()))))
opeds_ests <- opeds           |> group_by(attribute, level, sample_label, topic) |> reframe(tidy(lm_robust(Y_w1_s ~ Z, weights = weights, data = pick(everything()))))
frame_breath_ests <- frame_breath    |> group_by(attribute, level, sample_label) |> reframe(tidy(lm_robust(Y_w1_s ~ Z, weights = weights, data = pick(everything()))))
death_penalty_ests <- death_penalty   |> group_by(attribute, level, sample_label) |> reframe(tidy(lm_robust(Y_w1_s ~ Z, weights = weights, data = pick(everything()))))
flavin_ests <- flavin_l        |> filter(!is.na(Z)) |> group_by(attribute, level) |> reframe(tidy(lm_robust(Y_s ~ Z, weights = weight, data = pick(everything()))))
gash_murakami_ests <- gash_murakami_l |> mutate(Z = condition) |> filter(!is.na(Z)) |> group_by(attribute, level, topic) |> reframe(tidy(lm_robust(Y_s ~ Z, weights = weight, data = pick(everything()))))
mutz_ests <- mutz_l          |> filter(!is.na(Z)) |> group_by(attribute, level) |> reframe(tidy(lm_robust(Y_s ~ Z, weights = weight, data = pick(everything()))))
trump_white_ests <- trump_white_l   |> filter(!is.na(Z)) |> group_by(attribute, level) |> reframe(tidy(lm_robust(Y_s ~ Z, weights = weight, data = pick(everything()))))
kreps_wallace_ests <- kreps_wallace_l |> filter(!is.na(Z)) |> group_by(attribute, level) |> reframe(tidy(lm_robust(Y_s ~ Z, weights = weight, data = pick(everything()))))

stacked_ests <-
  bind_rows(
    patriot_act = patriot_act_ests,
    immigration = immigration_ests,
    free_trade_expert = free_trade_expert,
    free_trade_valence = free_trade_valence,
    expert_economist = expert_economists_ests,
    opeds = opeds_ests,
    frame_breath = frame_breath_ests,
    death_penalty = death_penalty_ests,
    flavin = flavin_ests,
    gash_murakami = gash_murakami_ests,
    mutz = mutz_ests,
    trump_white = trump_white_ests,
    kreps_wallace = kreps_wallace_ests,
    .id = "study"
  )

# Pivot to wide by level ----
level_cast <- function(data, attr) {
  data |>
    filter(term != "(Intercept)", attribute == attr) |>
    select(study, attribute, level, sample_label, topic, term,
           estimate, std.error, conf.low, conf.high) |>
    pivot_wider(
      id_cols = c(study, sample_label, topic, term),
      names_from = level,
      values_from = c(estimate, std.error, conf.low, conf.high),
      names_sep = "_"
    )
}

pid_3_df <-
  level_cast(stacked_ests, "pid_3") |>
  mutate(
    difference = estimate_Democrat - estimate_Republican,
    se_difference = sqrt(std.error_Democrat^2 + std.error_Republican^2),
    t_difference = difference / se_difference,
    p_difference = pnorm(abs(t_difference), lower.tail = FALSE) * 2,
    sig_difference = p_difference <= 0.05
  )

female_df <-
  level_cast(stacked_ests, "female") |>
  rename_with(~str_replace(.x, "^(estimate|std\\.error|conf\\.low|conf\\.high)_1$", "\\1_female"),
              matches("_1$")) |>
  rename_with(~str_replace(.x, "^(estimate|std\\.error|conf\\.low|conf\\.high)_0$", "\\1_male"),
              matches("_0$")) |>
  mutate(
    difference = estimate_female - estimate_male,
    se_difference = sqrt(std.error_female^2 + std.error_male^2),
    t_difference = difference / se_difference,
    p_difference = pnorm(abs(t_difference), lower.tail = FALSE) * 2,
    sig_difference = p_difference <= 0.05
  )

race_4_df <-
  level_cast(stacked_ests, "race_4") |>
  mutate(
    difference = estimate_White - estimate_Black,
    se_difference = sqrt(std.error_White^2 + std.error_Black^2),
    t_difference = difference / se_difference,
    p_difference = pnorm(abs(t_difference), lower.tail = FALSE) * 2,
    sig_difference = p_difference <= 0.05
  )

ideo_3_df <-
  level_cast(stacked_ests, "ideo_3") |>
  mutate(
    difference = estimate_Liberal - estimate_Conservative,
    se_difference = sqrt(std.error_Liberal^2 + std.error_Conservative^2),
    t_difference = difference / se_difference,
    p_difference = pnorm(abs(t_difference), lower.tail = FALSE) * 2,
    sig_difference = p_difference <= 0.05
  )

college_df <-
  level_cast(stacked_ests, "college") |>
  rename_with(~str_replace(.x, "^(estimate|std\\.error|conf\\.low|conf\\.high)_1$", "\\1_college"),
              matches("_1$")) |>
  rename_with(~str_replace(.x, "^(estimate|std\\.error|conf\\.low|conf\\.high)_0$", "\\1_nocollege"),
              matches("_0$")) |>
  mutate(
    difference = estimate_college - estimate_nocollege,
    se_difference = sqrt(std.error_college^2 + std.error_nocollege^2),
    t_difference = difference / se_difference,
    p_difference = pnorm(abs(t_difference), lower.tail = FALSE) * 2,
    sig_difference = p_difference <= 0.05
  )

age_3_df <-
  level_cast(stacked_ests, "age_3") |>
  mutate(
    difference = `estimate_More than 60` - `estimate_18 - 39`,
    se_difference = sqrt(`std.error_More than 60`^2 + `std.error_18 - 39`^2),
    t_difference = difference / se_difference,
    p_difference = pnorm(abs(t_difference), lower.tail = FALSE) * 2,
    sig_difference = p_difference <= 0.05
  )

# Correlations ----
# These six numbers are quoted in the text of chapter 5 and are the point of the figure.
# They were previously computed by six bare `with(...)` calls at the top level, which
# print nothing when the script is sourced and wrote nothing, so the figure's headline
# quantities had no committed output and no reproduction check at all.
cate_correlations <-
  tribble(
    ~facet,          ~correlation,
    "Partisanship",  with(pid_3_df,   cor(estimate_Democrat, estimate_Republican)),
    "Ideology",      with(ideo_3_df,  cor(estimate_Liberal, estimate_Conservative)),
    "Race",          with(race_4_df,  cor(estimate_White, estimate_Black, use = "complete.obs")),
    "Gender",        with(female_df,  cor(estimate_female, estimate_male)),
    "Age",           with(age_3_df,   cor(`estimate_More than 60`, `estimate_18 - 39`, use = "complete.obs")),
    "Education",     with(college_df, cor(estimate_college, estimate_nocollege))
  )

write_csv(cate_correlations, path_output("figure_5.16_cate_correlations.csv"))

# The plotted contrasts themselves, so the figure's own estimates can be diffed rather
# than only the six correlations summarizing them.
cate_pairs <-
  bind_rows(
    `Partisanship` = pid_3_df |> transmute(estimate_group_1 = estimate_Democrat, estimate_group_2 = estimate_Republican),
    `Ideology` = ideo_3_df |> transmute(estimate_group_1 = estimate_Liberal, estimate_group_2 = estimate_Conservative),
    `Race` = race_4_df |> transmute(estimate_group_1 = estimate_White, estimate_group_2 = estimate_Black),
    `Gender` = female_df |> transmute(estimate_group_1 = estimate_female, estimate_group_2 = estimate_male),
    `Age` = age_3_df |> transmute(estimate_group_1 = `estimate_More than 60`, estimate_group_2 = `estimate_18 - 39`),
    `Education` = college_df |> transmute(estimate_group_1 = estimate_college, estimate_group_2 = estimate_nocollege),
    .id = "facet"
  )

write_csv(cate_pairs, path_output("figure_5.16_cate_pairs.csv"))

# Build plots ----

plot_spec <-
  list(
    geom_point(alpha = 0.7, stroke = 0),
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.1),
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.1),
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.1),
    theme_bw(),
    theme(legend.position = "none", panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 7)),
    coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))
  )

pid_3_g <-
  ggplot(pid_3_df, aes(estimate_Democrat, estimate_Republican)) +
  # geom_linerange replaces deprecated geom_errorbarh
  geom_linerange(aes(xmin = conf.low_Democrat, xmax = conf.high_Democrat), alpha = 0.25, linewidth = 0.2) +
  geom_linerange(aes(y = estimate_Republican, xmin = estimate_Republican, xmax = estimate_Republican,
                     ymin = conf.low_Republican, ymax = conf.high_Republican),
                 alpha = 0.25, linewidth = 0.2) +
  xlab("CATE estimate | Democrat") + ylab("CATE estimate | Republican") +
  ggtitle("Partisanship") +
  annotate("text", x = -0.7, y = 0.75, size = 2,
           label = paste0("Correlation:\n",
                          with(pid_3_df, format_num(cor(estimate_Democrat, estimate_Republican), 2)))) +
  plot_spec

female_g <-
  ggplot(female_df, aes(estimate_female, estimate_male)) +
  geom_linerange(aes(xmin = conf.low_female, xmax = conf.high_female), alpha = 0.25, linewidth = 0.2) +
  geom_linerange(aes(ymin = conf.low_male, ymax = conf.high_male), alpha = 0.25, linewidth = 0.2) +
  xlab("CATE estimate | Woman") + ylab("CATE estimate | Man") +
  ggtitle("Gender") +
  annotate("text", x = -0.75, y = 0.75, size = 2,
           label = with(female_df, format_num(cor(estimate_female, estimate_male), 2))) +
  plot_spec

white_g <-
  ggplot(race_4_df, aes(estimate_White, estimate_Black)) +
  geom_linerange(aes(xmin = conf.low_White, xmax = conf.high_White), alpha = 0.25, linewidth = 0.2) +
  geom_linerange(aes(ymin = conf.low_Black, ymax = conf.high_Black), alpha = 0.25, linewidth = 0.2) +
  xlab("CATE estimate | White") + ylab("CATE estimate | Black") +
  ggtitle("Race") +
  annotate("text", x = -0.75, y = 0.75, size = 2,
           label = with(race_4_df, format_num(cor(estimate_White, estimate_Black, use = "complete.obs"), 2))) +
  plot_spec

ideo_3_g <-
  ggplot(ideo_3_df, aes(estimate_Liberal, estimate_Conservative)) +
  geom_linerange(aes(xmin = conf.low_Liberal, xmax = conf.high_Liberal), alpha = 0.25, linewidth = 0.2) +
  geom_linerange(aes(ymin = conf.low_Conservative, ymax = conf.high_Conservative), alpha = 0.25, linewidth = 0.2) +
  xlab("CATE estimate | Liberal") + ylab("CATE estimate | Conservative") +
  ggtitle("Ideology") +
  annotate("text", x = -0.75, y = 0.75, size = 2,
           label = with(ideo_3_df, format_num(cor(estimate_Liberal, estimate_Conservative), 2))) +
  plot_spec

college_g <-
  ggplot(college_df, aes(estimate_college, estimate_nocollege)) +
  geom_linerange(aes(xmin = conf.low_college, xmax = conf.high_college), alpha = 0.25, linewidth = 0.2) +
  geom_linerange(aes(ymin = conf.low_nocollege, ymax = conf.high_nocollege), alpha = 0.25, linewidth = 0.2) +
  xlab("CATE estimate | College or more") + ylab("CATE estimate | Less than college") +
  ggtitle("Education") +
  annotate("text", x = -0.75, y = 0.75, size = 2,
           label = with(college_df, format_num(cor(estimate_college, estimate_nocollege), 2))) +
  plot_spec

age_3_g <-
  ggplot(age_3_df, aes(`estimate_More than 60`, `estimate_18 - 39`)) +
  geom_linerange(aes(xmin = `conf.low_More than 60`, xmax = `conf.high_More than 60`),
                 alpha = 0.25, linewidth = 0.2) +
  geom_linerange(aes(ymin = `conf.low_18 - 39`, ymax = `conf.high_18 - 39`),
                 alpha = 0.25, linewidth = 0.2) +
  xlab("CATE estimate | aged 18 - 39") + ylab("CATE estimate | older than 60") +
  ggtitle("Age") +
  annotate("text", x = -0.75, y = 0.75, size = 2,
           label = with(age_3_df, format_num(cor(`estimate_More than 60`, `estimate_18 - 39`, use = "complete.obs"), 2))) +
  plot_spec

figure_5.16 <- wrap_plots(pid_3_g, ideo_3_g, white_g, female_g, age_3_g, college_g, ncol = 2)

ggsave(path_output("figure_5.16_cate_correlations.pdf"),
       plot = figure_5.16, width = 7, height = 9)
ggsave(path_output("figure_5.16_cate_correlations.png"),
       plot = figure_5.16, width = 7, height = 9, dpi = 300)
