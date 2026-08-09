# coppock_2022/maintained/figure_6.3_table_6.3_oped_persistence.R
# Output: output/figure_6.3_oped_persistence.pdf, output/table_6.3_oped_persistence.csv
# Depends on: helpers.R, data/opeds_long.rds
# Description: Op-ed persistence ratios at 10 and 30 days by party and overall.
#   Bootstrap SEs via rsample (500 sims). Meta-analytic pooling via metafor::rma().
# Note: the archive pools with rmeta::meta.summaries() and passes no method, so it takes
#   that function's default, which is FIXED effects. metafor::rma(method = "FE") is the
#   same estimator. rmeta has not been updated since March 2018 and survives on CRAN by
#   the archiving policy rather than by maintenance, which is why the call is ported
#   rather than kept; the estimator is not changed along with it. Substituting a random
#   effects estimator here would move every cell of table 6.3.
# Note: archive used melt()/dcast() from reshape2; replaced with pivot_longer()/pivot_wider().

source(here::here("maintained", "helpers.R"))

set.seed(script_seed(20230126))

opeds_long <- read_rds(path_original("data", "opeds_long.rds"))

opeds_longer <-
  opeds_long |>
  filter(!is.na(Y_w1), !is.na(Y_w2), sample_label != "Policy Professional Sample") |>
  pivot_longer(cols = c(Y_w1_s, Y_w2_s, Y_w3_s), names_to = "wave", values_to = "dv") |>
  filter(!is.na(dv)) |>
  mutate(
    treatment = factor(Z == "control", levels = c(TRUE, FALSE), labels = c("Control", "Treatment")),
    days = recode(wave, "Y_w1_s" = 0L, "Y_w2_s" = 10L, "Y_w3_s" = 30L),
    topic_label = recode(topic,
      "amtrak" = "Amtrak op-ed",
      "climate" = "Climate op-ed",
      "flat" = "Flat tax op-ed",
      "veterans" = "Veterans Administration op-ed",
      "wallstreet" = "Wall Street op-ed"
    ),
    pid_3 = recode(pid_3, "Republican" = "Republicans", "Democrat" = "Democrats"),
    Z = as.character(Z)
  )

gg_df <-
  opeds_longer |>
  group_by(topic_label, days) |>
  reframe(tidy(lm_robust(dv ~ treatment + pid_7, data = pick(everything())))) |>
  filter(!term %in% c("(Intercept)", "pid_7"))

figure_6.3 <-
  ggplot(gg_df, aes(days, estimate)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
  geom_line(position = position_dodge(width = 4), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~topic_label, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Days since exposure to treatment op-eds") +
  ylab("Standardized average treatment effect")

ggsave(path_output("figure_6.3_oped_persistence.pdf"),
       plot = figure_6.3, width = 7, height = 5)
ggsave(path_output("figure_6.3_oped_persistence.png"),
       plot = figure_6.3, width = 7, height = 5, dpi = 300)

write_csv(gg_df, path_output("figure_6.3_oped_persistence.csv"))

# Bootstrap persistence estimates ----

estimation_function <- function(data, rhs) {
  model_1 <- lm_robust(formula(paste0("Y_w1_s", rhs)), weights = weights, data = data)
  model_2 <- lm_robust(formula(paste0("Y_w2_s", rhs)), weights = weights, data = data)
  model_3 <- lm_robust(formula(paste0("Y_w3_s", rhs)), weights = weights, data = data)
  tibble(
    term = names(coef(model_2)),
    estimate_w1 = coef(model_1),
    estimate_w2 = coef(model_2),
    estimate_w3 = coef(model_3),
    estimate_ratio_w2 = coef(model_2) / coef(model_1),
    estimate_ratio_w3 = coef(model_3) / coef(model_1)
  ) |>
    pivot_longer(cols = -term, names_to = "model", values_to = "estimate")
}

tidy_estimator <- function(data, rhs, sims = 500) {
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

message("Bootstrapping oped estimates (overall)...")
oped_ests <-
  opeds_long |>
  filter(sample_label == "Mechanical Turk Sample") |>
  group_by(topic) |>
  reframe(tidy_estimator(pick(everything()), "~ Z + pid_7")) |>
  filter(!term %in% c("(Intercept)", "pid_7")) |>
  pivot_wider(
    id_cols = c(term, topic),
    names_from = model,
    values_from = c(estimate, std.error, conf.low, conf.high),
    names_sep = "_"
  )

overall_w2 <-
  tidy(rma(yi = estimate_estimate_ratio_w2, sei = std.error_estimate_ratio_w2,
           data = oped_ests, method = "FE")) |>
  mutate(pid_3 = "Overall")

overall_w3 <-
  tidy(rma(yi = estimate_estimate_ratio_w3, sei = std.error_estimate_ratio_w3,
           data = oped_ests, method = "FE")) |>
  mutate(pid_3 = "Overall")

message("Bootstrapping oped estimates (by party)...")
oped_ests_pid <-
  opeds_long |>
  filter(sample_label == "Mechanical Turk Sample", pid_3 != "Independent") |>
  group_by(topic, pid_3) |>
  reframe(tidy_estimator(pick(everything()), "~ Z + pid_7")) |>
  filter(!term %in% c("(Intercept)", "pid_7")) |>
  pivot_wider(
    id_cols = c(pid_3, term, topic),
    names_from = model,
    values_from = c(estimate, std.error, conf.low, conf.high),
    names_sep = "_"
  )

pid_w2 <-
  oped_ests_pid |>
  split(~pid_3) |>
  map_df(~tidy(rma(yi = .x$estimate_estimate_ratio_w2,
                   sei = .x$std.error_estimate_ratio_w2, method = "FE")),
         .id = "pid_3")

pid_w3 <-
  oped_ests_pid |>
  split(~pid_3) |>
  map_df(~tidy(rma(yi = .x$estimate_estimate_ratio_w3,
                   sei = .x$std.error_estimate_ratio_w3, method = "FE")),
         .id = "pid_3")

w2 <-
  bind_rows(overall_w2, pid_w2) |>
  transmute(pid_3,
            w2_est = make_se_entry(estimate, std.error),
            w2_ci = make_interval_entry(conf.low, conf.high))

w3 <-
  bind_rows(overall_w3, pid_w3) |>
  transmute(pid_3,
            w3_est = make_se_entry(estimate, std.error),
            w3_ci = make_interval_entry(conf.low, conf.high))

table_6.3 <- left_join(w2, w3, by = "pid_3")
write_csv(table_6.3, path_output("table_6.3_oped_persistence.csv"))
