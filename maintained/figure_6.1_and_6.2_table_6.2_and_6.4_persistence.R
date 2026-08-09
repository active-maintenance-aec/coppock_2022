# coppock_2022/maintained/figure_6.1_and_6.2_table_6.2_and_6.4_persistence.R
# Output: output/figure_6.1_persistence_decay.pdf, output/figure_6.2_persistence_ratio.pdf,
#         output/table_6.2_persistence_ratio.csv, output/table_6.4_all_persistence.csv
# Depends on: helpers.R, multiple data files
# Description: Cross-study persistence ratios (time 2 / time 1 ATE).
#   Bootstrap SEs via rsample (2000 sims). Meta-analytic pooling via metafor::rma(),
#   matching the archive estimator by estimator: DerSimonian and Laird for table 6.2,
#   REML for figure 6.2. See the comments at each call.
# Note: archive used melt()/dcast() from reshape2; replaced with pivot_longer()/pivot_wider().
# Note: archive used do(tidy_estimator(., ...)); replaced with reframe().
# Note: the bootstrap dominates the runtime of this script. run_all.R prints a measured
#   duration per script rather than an estimate, because a guess in a comment is a claim
#   nothing tests.

source(here::here("maintained", "helpers.R"))

set.seed(script_seed(12345))

# Load data ----
patriot_act_stacked <- read_rds(path_original("data", "patriot_act_stacked.rds"))
immigration_stacked <- read_rds(path_original("data", "immigration_stacked.rds"))
free_trade_stacked <- read_rds(path_original("data", "free_trade_stacked.rds"))
expert_economists_stacked_long <- read_rds(path_original("data", "expert_economists_stacked_long.rds"))
opeds_long <- read_rds(path_original("data", "opeds_long.rds"))
frame_breath_topic <- read_rds(path_original("data", "frame_breath_topic.rds"))
lrl <- read_rds(path_original("data", "GC_study_3_clean.rds"))
mw <- read_rds(path_original("data", "GC_study_2_clean.rds"))

# Filter to relevant obs ----
patriot_act <-
  patriot_act_stacked |>
  filter(T1_content != "Both", pid_3 != "Independent", !is.na(Y_w1_s), !is.na(Y_w2_s)) |>
  mutate(T1_content = relevel(T1_content, ref = "Control"))

immigration <-
  immigration_stacked |>
  filter(pid_3 != "Independent", !is.na(Y_w1_s), !is.na(Y_w2_s)) |>
  mutate(Z_brader_pos_neg = relevel(Z_brader_pos_neg, ref = "control"))

free_trade <-
  free_trade_stacked |>
  filter(pid_3 != "Independent", Z_Hiscox_valence != "Both", !is.na(Y_w1_s), !is.na(Y_w2_s)) |>
  mutate(Z_Hiscox_valence = relevel(Z_Hiscox_valence, ref = "Control"))

expert_economists <- expert_economists_stacked_long |>
  filter(pid_3 != "Independent", !is.na(Y_w1_s), !is.na(Y_w2_s))

opeds <- opeds_long |> filter(pid_3 != "Independent", !is.na(Y_w1_s), !is.na(Y_w2_s))

frame_breath <- frame_breath_topic |> filter(pid_3 != "Independent", !is.na(Y_w1_s), !is.na(Y_w2_s))

mw <-
  mw |>
  mutate(
    Y_w1_s = amount_T2 / sd(amount_T1, na.rm = TRUE),
    Y_w2_s = amount_T3 / sd(amount_T1, na.rm = TRUE),
    weights = 1
  ) |>
  filter(
    initial_position != "inconsistent_raise",
    condition_type %in% c("ConYoung_ConOld", "Placebo", "ProYoung_ProOld"),
    !is.na(Y_w1_s), !is.na(Y_w2_s)
  )

lrl <-
  lrl |>
  mutate(
    Y_w1_s = support_recode_T2 / sd(support_recode_T1, na.rm = TRUE),
    Y_w2_s = support_recode_T3 / sd(support_recode_T1, na.rm = TRUE),
    weights = 1
  ) |>
  filter(condition.factor %in% c("CC", "NN", "PP"), !is.na(Y_w1_s), !is.na(Y_w2_s))

# Bootstrapping helpers ----
estimation_function <- function(data, rhs) {
  model_1 <- lm_robust(formula(paste0("Y_w1_s", rhs)), weights = weights, data = data)
  model_2 <- lm_robust(formula(paste0("Y_w2_s", rhs)), weights = weights, data = data)
  tibble(
    term = names(coef(model_2)),
    estimate_w1 = coef(model_1),
    estimate_w2 = coef(model_2),
    estimate_ratio = coef(model_2) / coef(model_1)
  ) |>
    pivot_longer(cols = -term, names_to = "model", values_to = "estimate")
}

tidy_estimator <- function(data, rhs, sims = 2000) {
  est <- estimation_function(data, rhs)
  boot_out <-
    bootstraps(data, sims)$splits |>
    map_df(~estimation_function(analysis(.), rhs))
  se <-
    boot_out |>
    group_by(term, model) |>
    summarise(
      std.error = sd(estimate),
      conf.low = quantile(estimate, probs = 0.025),
      conf.high = quantile(estimate, probs = 0.975),
      .groups = "drop"
    )
  left_join(est, se, by = c("term", "model"))
}

# Persistence estimates ----
message("Bootstrapping patriot_act...")
patriot_act_ests <- patriot_act |> group_by(sample_label) |>
  reframe(tidy_estimator(pick(everything()), "~ T1_content + pid_7"))

message("Bootstrapping free_trade_expert...")
free_trade_expert_ests <- free_trade |> group_by(sample_label) |>
  reframe(tidy_estimator(pick(everything()), "~ Z_Hiscox_expert + pid_7"))

message("Bootstrapping free_trade_valence...")
free_trade_valence_ests <- free_trade |> group_by(sample_label) |>
  reframe(tidy_estimator(pick(everything()), "~ Z_Hiscox_valence + pid_7"))

message("Bootstrapping immigration...")
immigration_ests <- immigration |> group_by(sample_label) |>
  reframe(tidy_estimator(pick(everything()), "~ Z_brader_pos_neg + pid_7"))

message("Bootstrapping expert_economists...")
expert_economists_ests <- expert_economists |> group_by(sample_label, topic) |>
  reframe(tidy_estimator(pick(everything()), "~ Z_expert_all + pid_7"))

message("Bootstrapping opeds...")
oped_ests <- opeds |> group_by(sample_label, topic) |>
  reframe(tidy_estimator(pick(everything()), "~ Z + pid_7"))

message("Bootstrapping frame_breath...")
frame_breath_ests <- frame_breath |> group_by(sample_label, topic) |>
  reframe(tidy_estimator(pick(everything()), "~ Z + pid_7"))

message("Bootstrapping mw...")
mw_ests <- mw |> reframe(tidy_estimator(pick(everything()), " ~ condition_type + amount_T1")) |>
  filter(term != "amount_T1") |>
  mutate(sample_label = "Mechanical Turk Sample")

message("Bootstrapping lrl...")
lrl_ests <- lrl |> reframe(tidy_estimator(pick(everything()), " ~ condition.factor + support_recode_T1")) |>
  filter(term != "support_recode_T1") |>
  mutate(sample_label = "Mechanical Turk Sample")

results_df <-
  bind_rows(
    patriot_act = patriot_act_ests,
    free_trade = free_trade_expert_ests,
    free_trade = free_trade_valence_ests,
    immigration = immigration_ests,
    expert_economists = expert_economists_ests,
    opeds = oped_ests,
    frame_breath = frame_breath_ests,
    minimum_wage = mw_ests,
    capital_punishment = lrl_ests,
    .id = "study"
  ) |>
  ungroup() |>
  mutate(
    prediction = case_when(
      study %in% c("capital_punishment", "minimum_wage", "patriot_act", "opeds") ~ "stronger persistence",
      study %in% c("free_trade", "frame_breath", "immigration", "expert_economists") ~ "weaker persistence"
    ),
    study_label = recode(study,
      "capital_punishment" = "Lord, Ross, and Lepper (1979)",
      "expert_economists" = "Johnston and Ballard (2016)",
      "frame_breath" = "Hopkins and Mummolo (2017)",
      "free_trade" = "Hiscox (2006)",
      "immigration" = "Brader, Valentino, and Suhay (2008)",
      "minimum_wage" = "Guess and Coppock (2018)",
      "opeds" = "Coppock, Ekins, and Kirby (2018)",
      "patriot_act" = "Chong and Druckman (2010)"
    ),
    sample_label2 = recode(sample_label,
      "Mechanical Turk Replication" = "MTurk",
      "GfK Replication" = "GfK",
      "Mechanical Turk Sample" = "MTurk",
      "Policy Professional Sample" = "Policy Professionals",
      "Original Study" = "Original Study"
    ),
    term_label = case_when(
      term == "condition.factorCC" ~ "Two con studies",
      term == "condition.factorPP" ~ "Two pro studies",
      topic == "Gold Standard" ~ "Gold Standard",
      topic == "Health Care" ~ "Health Care",
      topic == "Immigration" ~ "Immigration",
      topic == "Tax Cut" ~ "Tax Cut",
      topic == "Trade with China" ~ "Trade with China",
      topic == "Crime" ~ "Crime statement",
      topic == "Health" ~ "Health statement",
      topic == "Stimulus" ~ "Stimulus statement",
      topic == "Terror" ~ "Terror statement",
      term == "Z_Hiscox_expertExpert treatment" ~ "Expert cue",
      term == "Z_Hiscox_valenceNegative" ~ "Negative frame",
      term == "Z_Hiscox_valencePositive" ~ "Positive frame",
      term == "Z_brader_pos_negnegative" ~ "Negative article",
      term == "Z_brader_pos_negpositive" ~ "Positive article",
      term == "condition_typeConYoung_ConOld" ~ "Two anti-minimum wage videos",
      term == "condition_typeProYoung_ProOld" ~ "Two pro-minimum wage videos",
      term == "Zamtrak" ~ "Amtrak op-ed",
      term == "Zpaul" ~ "Flat tax op-ed",
      term == "Zveterans" ~ "Veterans op-ed",
      term == "Zwallstreet" ~ "Wallstreet op-ed",
      term == "T1_contentCon" ~ "Six anti-Patriot Act statements",
      term == "T1_contentPro" ~ "Six pro-Patriot Act statements"
    )
  )

# Wide format: one row per treatment arm ----
gg_df <-
  results_df |>
  filter(!term %in% c("(Intercept)", "pid_7")) |>
  pivot_wider(
    id_cols = c(study_label, sample_label2, term_label, topic, prediction),
    names_from = model,
    values_from = c(estimate, std.error, conf.low, conf.high),
    names_sep = "_"
  ) |>
  mutate(
    w1_sign = sign(estimate_estimate_w1),
    estimate_w1_estimate_abs = estimate_estimate_w1 * w1_sign,
    estimate_w1_conf.low_abs = conf.low_estimate_w1 * w1_sign,
    estimate_w1_conf.high_abs = conf.high_estimate_w1 * w1_sign,
    estimate_w2_estimate_abs = estimate_estimate_w2 * w1_sign,
    estimate_w2_conf.low_abs = conf.low_estimate_w2 * w1_sign,
    estimate_w2_conf.high_abs = conf.high_estimate_w2 * w1_sign,
    estimate_id = paste0(study_label, sample_label2, term_label, topic)
  )

# Table 6.2 ----
# The archive pools this table with rmeta::meta.summaries(method = "random"), which is
# DerSimonian and Laird, so metafor::rma(method = "DL") is the same estimator. The
# archive uses REML for the figure below and DL for this table, pooling the same
# quantity two ways; both are kept as the archive has them, because choosing one would
# be an analytical decision rather than a port.
re_fit_1 <-
  tidy(rma(yi = estimate_estimate_ratio, sei = std.error_estimate_ratio,
           data = gg_df, method = "DL"))

re_fit_2 <-
  gg_df |>
  split(~prediction) |>
  map_df(~tidy(rma(yi = .x$estimate_estimate_ratio, sei = .x$std.error_estimate_ratio,
                   method = "DL")),
         .id = "prediction")

table_6.2 <-
  bind_rows(re_fit_1, re_fit_2) |>
  transmute(
    group = recode(prediction,
      "stronger persistence" = "Prediction: stronger persistence",
      "weaker persistence" = "Prediction: weaker persistence",
      .missing = "Overall"
    ),
    est = make_se_entry(estimate, std.error),
    ci = make_interval_entry(conf.low, conf.high)
  )

write_csv(table_6.2, path_output("table_6.2_persistence_ratio.csv"))

# Table 6.4 ----
table_6.4 <-
  gg_df |>
  transmute(
    study_label = paste0(study_label, ". Prediction: ", prediction),
    Sample = sample_label2,
    Treatment = term_label,
    `ATE Time 1` = make_se_entry(estimate_estimate_w1, std.error_estimate_w1),
    `ATE Time 2` = make_se_entry(estimate_estimate_w2, std.error_estimate_w2),
    `Persistence Ratio` = make_se_entry(estimate_estimate_ratio, std.error_estimate_ratio)
  )

write_csv(table_6.4, path_output("table_6.4_all_persistence.csv"))

# Figure 6.2: persistence ratio vs. immediate effect ----
# REML here rather than DL above is the archive's own choice, not a porting decision:
# its figure block calls metafor::rma(method = "REML") directly while its table block
# calls rmeta::meta.summaries(method = "random"). The two pooled lines the figure draws
# therefore need not equal the Overall row of table 6.2.

re_fit <-
  gg_df |>
  group_by(prediction) |>
  reframe(tidy(rma(
    yi = estimate_estimate_ratio,
    sei = std.error_estimate_ratio,
    data = pick(everything()),
    method = "REML"
  )))

ribbon_df <- bind_rows(re_fit, re_fit) |> mutate(x = c(-0.5, -0.5, 1.5, 1.5))
label_df <- re_fit |> mutate(label = paste0("Prediction: ", prediction))

figure_6.2 <-
  ggplot(gg_df, aes(estimate_w1_estimate_abs, estimate_estimate_ratio, shape = prediction)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(data = re_fit, aes(yintercept = estimate), alpha = 0.7) +
  geom_ribbon(data = ribbon_df,
              aes(ymin = conf.low, ymax = conf.high, x = x, y = NULL, color = NULL),
              alpha = 0.2) +
  # geom_linerange replaces deprecated geom_errorbarh
  geom_linerange(aes(xmin = estimate_w1_conf.low_abs, xmax = estimate_w1_conf.high_abs),
                 linewidth = 0.3, alpha = 0.15) +
  geom_linerange(aes(ymin = conf.low_estimate_ratio, ymax = conf.high_estimate_ratio),
                 linewidth = 0.3, alpha = 0.15) +
  geom_point(stroke = 0, size = 2, alpha = 0.7) +
  geom_text(data = label_df, aes(x = 0.8, y = estimate, label = label), size = 2, nudge_y = -.03) +
  scale_color_manual(values = pro_con_colors) +
  scale_fill_manual(values = pro_con_colors) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.1, 1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Standardized effect size (immediately post-treatment)") +
  ylab("Persistence ratio\n(10 days post-treatment)/(immediately post-treatment)")

ggsave(path_output("figure_6.2_persistence_ratio.pdf"),
       plot = figure_6.2, width = 7, height = 5)
ggsave(path_output("figure_6.2_persistence_ratio.png"),
       plot = figure_6.2, width = 7, height = 5, dpi = 300)

write_csv(gg_df, path_output("figure_6.2_persistence_ratio.csv"))

# Figure 6.1: effect size over time per study ----

gg_df_2 <-
  results_df |>
  filter(!term %in% c("(Intercept)", "pid_7"), !model %in% c("estimate_ratio")) |>
  mutate(estimate_id = paste0(study_label, sample_label2, term_label, topic)) |>
  left_join(select(gg_df, estimate_id, w1_sign, estimate_w1_estimate_abs), by = "estimate_id") |>
  mutate(
    estimate_abs = estimate * w1_sign,
    estimate_conf.low = conf.low * w1_sign,
    estimate_conf.high = conf.high * w1_sign,
    estimate_id = fct_reorder(factor(estimate_id), estimate_w1_estimate_abs),
    time = recode(model,
      "estimate_w1" = "Immediately\npost-treatment",
      "estimate_w2" = "10 days\npost-treatment"
    ),
    time = factor(time, levels = c("Immediately\npost-treatment", "10 days\npost-treatment")),
    facet_label = paste0(study_label, "\n", sample_label2)
  )

figure_6.1 <-
  ggplot(gg_df_2, aes(time, estimate_abs, group = estimate_id)) +
  geom_line(position = position_dodge(width = 0.2), alpha = 0.5) +
  geom_linerange(aes(ymin = estimate_conf.low, ymax = estimate_conf.high),
                 alpha = 0.5, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2), stroke = 0, size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~facet_label, ncol = 3) +
  theme_bw() +
  theme(strip.background = element_blank(), axis.title.x = element_blank()) +
  ylab("Standardized effect size")

ggsave(path_output("figure_6.1_persistence_decay.pdf"),
       plot = figure_6.1, width = 9, height = 9)
ggsave(path_output("figure_6.1_persistence_decay.png"),
       plot = figure_6.1, width = 9, height = 9, dpi = 300)

write_csv(gg_df_2, path_output("figure_6.1_persistence_decay.csv"))
