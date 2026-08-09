# coppock_2022/maintained/in_text_claims.R
# Output: none; it prints an audit trail
# Depends on: helpers.R, everything in maintained/output/,
#   ground_truth/published_claims.csv, ground_truth/published_maintext_tables.csv
# Description: The second instrument. Every number the book states in prose is paired here
#   with the sentence that states it and recomputed from the pipeline's own output.
#
#   This file reads maintained/output/ and never refits. Where the ground-truth build
#   reaches a quantity one way, this file reaches it another where a second path exists,
#   because two derivations that disagree have found something and two copies of one
#   derivation have not. It reads ground_truth/published_claims.csv for the precision each
#   sentence prints at and for nothing else; it does not read the ground truth itself.
#
#   Each block prints CLAIM <id> = <value> || <label>. That printed line is the only link
#   the coverage gate reads, so a block that errors or prints nothing is a failure rather
#   than a pass.

source(here::here("maintained", "helpers.R"))

options(width = 200)

claims_spec <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(
    value_paper = col_character(),
    digits = col_integer(),
    needs_block = col_logical(),
    .default = col_character()
  )
)

emit <- function(claim_id, value) {
  spec <- claims_spec |> filter(.data$claim_id == !!claim_id)
  stopifnot("claim_id is not declared in published_claims.csv" = nrow(spec) == 1)
  rendered <- sprintf(paste0("%.", spec$digits, "f"), value)
  rendered <- str_replace(rendered, "^-(0\\.?0*)$", "\\1")
  cat("CLAIM ", claim_id, " = ", rendered, " || ", spec$claim,
      " (book: ", spec$value_paper, ")\n", sep = "")
}

read_output <- function(file) {
  read_csv(here::here("maintained", "output", file), show_col_types = FALSE)
}

# Chapter 1: Persuasion in Polarized America ----

# "Despite these baseline differences, both Republicans and Democrats on MTurk change
#  their minds in response to the op-ed by similar amounts: 1.13 (robust standard error:
#  0.20) for Republicans and 0.54 points (0.15) for Democrats. A similar pattern holds for
#  the policy professionals: 0.52 (0.25) for Republicans and 0.29 (0.20) for Democrats."
#  (p. 8, repeated in the note to figure 1.2)
#
# The point estimates are recovered here by differencing the two condition means the
# figure itself plots, which is a different route from the build's, and the standard
# errors come from the fit, which has only one route.
flat_tax_means_w1 <- read_output("figure_1.2_flat_tax_immediate.csv")
flat_tax_means_w2 <- read_output("figure_1.3_flat_tax_10day.csv")
flat_tax_effects <- read_output("figure_1.2_and_1.3_flat_tax_effects.csv")

mean_difference <- function(means, sample_label, party) {
  cells <- means |> filter(sample == sample_label, pid_3_cat == party)
  stopifnot(nrow(cells) == 2)
  cells$estimate[cells$Z_label == "Pro-flat tax op-ed"] -
    cells$estimate[cells$Z_label == "No op-ed"]
}

standard_error <- function(wave_label, sample_label, party) {
  cell <- flat_tax_effects |>
    filter(wave == wave_label, sample == sample_label, pid_3_cat == party)
  stopifnot(nrow(cell) == 1)
  cell$std.error
}

emit("c1_flat_tax_mt_rep_est", mean_difference(flat_tax_means_w1, "Mechanical Turk Sample", "Republican"))
emit("c1_flat_tax_mt_rep_se", standard_error("Immediate", "Mechanical Turk Sample", "Republican"))
emit("c1_flat_tax_mt_dem_est", mean_difference(flat_tax_means_w1, "Mechanical Turk Sample", "Democrat"))
emit("c1_flat_tax_mt_dem_se", standard_error("Immediate", "Mechanical Turk Sample", "Democrat"))
emit("c1_flat_tax_pp_rep_est", mean_difference(flat_tax_means_w1, "Policy Professional Sample", "Republican"))
emit("c1_flat_tax_pp_rep_se", standard_error("Immediate", "Policy Professional Sample", "Republican"))
emit("c1_flat_tax_pp_dem_est", mean_difference(flat_tax_means_w1, "Policy Professional Sample", "Democrat"))
emit("c1_flat_tax_pp_dem_se", standard_error("Immediate", "Policy Professional Sample", "Democrat"))

# "On MTurk, the average effects after 10 days were 0.908 (robust standard error: 0.22)
#  for Republicans and 0.32 points (0.15) for Democrats. Among policy professionals, these
#  values were 0.18 (0.25) for Republicans and 0.13 (0.20) for Democrats." (p. 10, and the
#  note to figure 1.3)
emit("c1_flat_tax_w2_mt_rep_est", mean_difference(flat_tax_means_w2, "Mechanical Turk Sample", "Republican"))
emit("c1_flat_tax_w2_mt_rep_se", standard_error("10-day follow-up", "Mechanical Turk Sample", "Republican"))
emit("c1_flat_tax_w2_mt_dem_est", mean_difference(flat_tax_means_w2, "Mechanical Turk Sample", "Democrat"))
emit("c1_flat_tax_w2_mt_dem_se", standard_error("10-day follow-up", "Mechanical Turk Sample", "Democrat"))
emit("c1_flat_tax_w2_pp_rep_est", mean_difference(flat_tax_means_w2, "Policy Professional Sample", "Republican"))
emit("c1_flat_tax_w2_pp_rep_se", standard_error("10-day follow-up", "Policy Professional Sample", "Republican"))
emit("c1_flat_tax_w2_pp_dem_est", mean_difference(flat_tax_means_w2, "Policy Professional Sample", "Democrat"))
emit("c1_flat_tax_w2_pp_dem_se", standard_error("10-day follow-up", "Policy Professional Sample", "Democrat"))

# "Note: Survey experimental data from 860 MTurk respondents and 518 policy professionals
#  who provided immediate and 10-day follow-up responses (Coppock, Ekins, and Kirby 2018)."
#  (notes to figures 1.2 and 1.3)
flat_tax_n <- read_output("figure_1.2_and_1.3_flat_tax_n.csv")
emit("c1_n_mturk", flat_tax_n$n[flat_tax_n$sample == "Mechanical Turk Sample"])
emit("c1_n_policy_professionals", flat_tax_n$n[flat_tax_n$sample == "Policy Professional Sample"])

# "Among the MTurk subjects, effects persist at 80 percent of the original magnitude for
#  Republicans and 58 percent for Democrats." (p. 9)
#
# Derived here from the two waves of condition means rather than from the persistence file
# the build reads.
persistence <- function(party) {
  100 * mean_difference(flat_tax_means_w2, "Mechanical Turk Sample", party) /
    mean_difference(flat_tax_means_w1, "Mechanical Turk Sample", party)
}
emit("c1_persistence_rep", persistence("Republican"))
emit("c1_persistence_dem", persistence("Democrat"))

# "On MTurk, partisans in the control group differ on average by over a full point on the
#  1 to 7 scale. Among the policy professionals, the gap is closer to 2.5 points." (p. 8)
control_gap <- function(sample_label) {
  cells <- flat_tax_means_w1 |> filter(sample == sample_label, Z_label == "No op-ed")
  stopifnot(nrow(cells) == 2)
  cells$estimate[cells$pid_3_cat == "Republican"] - cells$estimate[cells$pid_3_cat == "Democrat"]
}
emit("c1_baseline_gap_mt", as.integer(control_gap("Mechanical Turk Sample") > 1))
emit("c1_baseline_gap_pp", control_gap("Policy Professional Sample"))

# "Overall, the average change (or slope with respect to time) is about 1.7 percentage
#  points per year. The slopes for some groups are slightly larger (Democrats 2.1 points
#  per year, White mainline Protestants 2.1 points per year) than for others (Republicans
#  1.2 points per year, White evangelical Protestants 1.2 points per year), but the overall
#  pattern is very similar from one subgroup to the next." (p. 11)
gm_slopes <- read_output("figure_1.4_gay_marriage_slopes.csv")
slope <- function(label) {
  row <- gm_slopes |> filter(method == label)
  stopifnot(nrow(row) == 1)
  row$estimate
}
emit("c1_gm_slope_overall", slope("Random effects pooling (DerSimonian and Laird)"))
emit("c1_gm_slope_democrat", slope("Group slope: Partisanship, Democrat"))
emit("c1_gm_slope_white_mainline", slope("Group slope: Religion, White mainline Protestants"))
emit("c1_gm_slope_republican", slope("Group slope: Partisanship, Republican"))
emit("c1_gm_slope_white_evangelical", slope("Group slope: Religion, White evangelical Protestants"))

# "In all twenty-five, the proportion favoring gay marriage was higher in 2019 than it was
#  in 2001, the beginning of data collection." (p. 11)
group_slopes <- gm_slopes |> filter(str_starts(method, "Group slope: "))
emit("c1_gm_all_groups_rose", sum(group_slopes$estimate > 0))

# "Whereas Democrats saw a greater percentage point change (28 points) than Republicans
#  (16 points), Republicans experienced a larger percent change (76 percent increase) than
#  Democrats (65 percent increase)." (p. 12)
gm_changes <- read_output("figure_1.4_gay_marriage_trends.csv")
change <- function(key_label, column) {
  row <- gm_changes |> filter(key == key_label)
  stopifnot(nrow(row) == 1)
  row[[column]]
}
emit("c1_gm_dem_points", change("Democrat", "change_in_points"))
emit("c1_gm_rep_points", change("Republican", "change_in_points"))
emit("c1_gm_rep_percent", 100 * change("Republican", "percent_change"))
emit("c1_gm_dem_percent", 100 * change("Democrat", "percent_change"))

# Chapter 2: Reinterpreting a Social Psychology Classic ----

# "We recruited 682 subjects to participate in our study from Mechanical Turk (MTurk), a
#  common source of online convenience samples." (p. 22, repeated in the note to figure 2.3)
emit("c2_n_replication", read_output("figure_2.3_gc_replication_n.csv")$n)

# "Focusing on the 'Combined' rows, we see that proponents report a total of 2.7 points of
#  change in the pro-capital punishment direction and opponents report exactly the
#  opposite: 2.7 points of change in the anti-capital punishment direction." (p. 24)
table_2.2 <- read_output("table_2.2_lrl_reanalysis.csv")
combined <- table_2.2 |> filter(outcome_variable == "attitude", content_factor == "Combined")
stopifnot(nrow(combined) == 1)
emit("c2_combined_proponents", combined$`Capital Punishment Proponents`)
emit("c2_combined_opponents", combined$`Capital Punishment Opponents`)

# Table 2.2's two study rows, verbatim as printed on page 26:
#
#   "After pro-capital punishment study      0.0    -2.3"
#   "After anti-capital punishment study     2.7    -0.4"
#
# The claim a row label makes is that the cells beside it are that study's means. Counted
# here by rendering both the pro-study and the con-study means at the page's one decimal
# and asking which of the two the printed row is. The build reaches the same count by
# joining on the label; this block reaches it by testing both readings, which also shows
# that the row is not merely wrong but is specifically the other study's.
published_maintext <- read_csv(
  here::here("ground_truth", "published_maintext_tables.csv"),
  col_types = cols(value_paper = col_character(), .default = col_character())
)

printed_pro_row <-
  published_maintext |>
  filter(table == "table_2.2", row_label == "After pro-capital punishment study") |>
  arrange(panel, group, .locale = "en")

study_means <- function(content_label) {
  table_2.2 |>
    filter(content_factor == content_label) |>
    pivot_longer(c(`Capital Punishment Proponents`, `Capital Punishment Opponents`),
                 names_to = "group", values_to = "value") |>
    transmute(panel = outcome_variable, group = str_remove(group, "Capital Punishment "),
              rendered = sprintf("%.1f", value)) |>
    arrange(panel, group, .locale = "en") |>
    pull(rendered)
}

stopifnot(nrow(printed_pro_row) == 4)
print(tibble(
  claim = "Table 2.2 'After pro' row against both readings",
  printed = printed_pro_row$value_paper,
  as_pro_study = study_means("After pro capital punishment study"),
  as_con_study = study_means("After anti capital punishment study")
))

emit("t2_2_row_labels",
     sum(printed_pro_row$value_paper == study_means("After pro capital punishment study")))

# "The pro-versus-con difference is statistically significant at p < 0.05 in all four
#  opportunities." (p. 26)
figure_2.2 <- read_output("figure_2.2_lrl_biased_assimilation.csv")
pro_con <-
  figure_2.2 |>
  filter(content_factor %in% c("Pro Study", "Con Study")) |>
  select(content_factor, predisposition, outcome_text, estimate, std.error) |>
  pivot_wider(names_from = content_factor, values_from = c(estimate, std.error)) |>
  mutate(z = (`estimate_Pro Study` - `estimate_Con Study`) /
           sqrt(`std.error_Pro Study`^2 + `std.error_Con Study`^2))
emit("c2_pro_con_significant", sum(2 * pnorm(-abs(pro_con$z)) < 0.05))

# Chapter 3: Definitions and Distinctions ----

# "More precisely, the op-eds move opinion on target attitudes by an average of about 0.4
#  standard units (SDs), but the average effect on non-target attitudes is tiny (0.02
#  SDs)." (p. 36)
figure_3.1 <- read_output("figure_3.1_oped_target_nontarget.csv")
emit("c3_target_sd", mean(figure_3.1$estimate[figure_3.1$target == "Target attitude"]))
emit("c3_nontarget_sd", mean(figure_3.1$estimate[figure_3.1$target == "Nontarget attitude"]))

# "Note: The 1,007 subjects in the 1982 California poll experiment were quasi-randomly
#  assigned to hear which governor ... had appointed each justice." (note to figure 3.2)
emit("c3_ca_poll_n", read_output("figure_3.2_ca_party_cues_n.csv")$n)

# Chapter 5: Persuasion Experiments ----

# "For proponents, the two pro videos (relative to the two anti videos) increased the
#  preferred minimum wage by $3.24 (robust standard error: $0.40) and the support for
#  raising it by 0.84 scale points (SE: 0.19 points). For opponents, the effect on the
#  preferred minimum wage was smaller ($1.04, SE: $0.65), but the effect on support for
#  raising the minimum wage was almost identical, at 0.88 scale points (SE: 0.32 points)."
#  (p. 76)
mw <- read_output("figure_5.1_minimum_wage_pro_vs_anti.csv")
mw_cell <- function(outcome, position, column) {
  row <- mw |> filter(dv == outcome, initial_position == position)
  stopifnot(nrow(row) == 1)
  row[[column]]
}
emit("c5_mw_pro_amount_est", mw_cell("Amount", "Proponents", "estimate"))
emit("c5_mw_pro_amount_se", mw_cell("Amount", "Proponents", "std.error"))
emit("c5_mw_pro_favor_est", mw_cell("Favor", "Proponents", "estimate"))
emit("c5_mw_pro_favor_se", mw_cell("Favor", "Proponents", "std.error"))
emit("c5_mw_opp_amount_est", mw_cell("Amount", "Opponents", "estimate"))
emit("c5_mw_opp_amount_se", mw_cell("Amount", "Opponents", "std.error"))
emit("c5_mw_opp_favor_est", mw_cell("Favor", "Opponents", "estimate"))
emit("c5_mw_opp_favor_se", mw_cell("Favor", "Opponents", "std.error"))

# "When they see the anti-gun control study, they become less supportive of stricter gun
#  laws, by 7.4 percentage points (SE: 2.2 points). ... The largest effect for opponents is
#  the effect of the pro study, which comes in at 3.1 points. The standard error around
#  that estimate is 4.1 points, so the estimate is not statistically significant." (p. 78)
#
# The book states these on the percentage point scale, so the proportions are multiplied by
# a hundred here rather than read back out of the appendix table, whose cells are already
# rounded to two decimals and would give 7.0 rather than 7.4.
gun <- read_output("table_a2_gun_control_unrounded.csv")
gun_cell <- function(study, group, column) {
  row <- gun |> filter(term == study, proponent == group)
  stopifnot(nrow(row) == 1)
  100 * row[[column]]
}
emit("c5_gun_anti_prop_est", abs(gun_cell("Anti Gun Control Study", "Gun control proponents", "estimate")))
emit("c5_gun_anti_prop_se", gun_cell("Anti Gun Control Study", "Gun control proponents", "std.error"))
emit("c5_gun_pro_opp_est", gun_cell("Pro Gun Control Study", "Gun control opponents", "estimate"))
emit("c5_gun_pro_opp_se", gun_cell("Pro Gun Control Study", "Gun control opponents", "std.error"))

# "The estimated correlations are 0.86 (partisanship), 0.80 (ideology), 0.62 (race), 0.92
#  (gender), 0.62 (age), and 0.70 (education)." (p. 97)
#
# Recomputed here from the plotted pairs rather than read out of the correlations file,
# which is the build's route.
cate_pairs <- read_output("figure_5.16_cate_pairs.csv")
facet_correlation <- function(facet_label) {
  pairs <- cate_pairs |> filter(facet == facet_label)
  cor(pairs$estimate_group_1, pairs$estimate_group_2, use = "complete.obs")
}
emit("c5_cate_corr_partisanship", facet_correlation("Partisanship"))
emit("c5_cate_corr_ideology", facet_correlation("Ideology"))
emit("c5_cate_corr_race", facet_correlation("Race"))
emit("c5_cate_corr_gender", facet_correlation("Gender"))
emit("c5_cate_corr_age", facet_correlation("Age"))
emit("c5_cate_corr_education", facet_correlation("Education"))

# "As shown in the left facet, the average effect of the two-sided message on support for
#  capital punishment is very close to zero for both proponents (0.28 scale points, SE:
#  0.18) and opponents (0.10, SE: 0.11). The right facet shows a very similar effect on
#  belief in the deterrent effect for opponents (0.30 points, SE: 0.17) and for proponents
#  (0.50, SE: 0.18)." (p. 101)
gc_effects <- read_output("figure_5.17_gc_pro_con_effects.csv")
two_sided <- function(outcome, group, column) {
  row <- gc_effects |> filter(dv == outcome, predisposition == group, term == "Pro Con")
  stopifnot(nrow(row) == 1)
  row[[column]]
}
emit("c5_twosided_support_prop_est", two_sided("support_recode_change", "Capital Punishment Proponents", "estimate"))
emit("c5_twosided_support_prop_se", two_sided("support_recode_change", "Capital Punishment Proponents", "std.error"))
emit("c5_twosided_support_opp_est", two_sided("support_recode_change", "Capital Punishment Opponents", "estimate"))
emit("c5_twosided_support_opp_se", two_sided("support_recode_change", "Capital Punishment Opponents", "std.error"))
emit("c5_twosided_deter_opp_est", two_sided("deter_recode_change", "Capital Punishment Opponents", "estimate"))
emit("c5_twosided_deter_opp_se", two_sided("deter_recode_change", "Capital Punishment Opponents", "std.error"))
emit("c5_twosided_deter_prop_est", two_sided("deter_recode_change", "Capital Punishment Proponents", "estimate"))
emit("c5_twosided_deter_prop_se", two_sided("deter_recode_change", "Capital Punishment Proponents", "std.error"))

# Chapter 6: Persistence and Decay ----
#
# Every number in this section comes from a bootstrap the deposit runs without a seed, so
# what is printed below is one draw. The ground truth scores these against the range the
# estimator takes across seeds rather than against a single value.

# "Across all 12 studies, the average persistence ratio was about one-third, or 34 percent;
#  that figure is 50 percent in the stronger persistence group and 20 percent in the weaker
#  persistence group (see table 6.2)." (p. 114)
table_6.2 <- read_output("table_6.2_persistence_ratio.csv")
pooled_ratio <- function(group_label) {
  row <- table_6.2 |> filter(group == group_label)
  stopifnot(nrow(row) == 1)
  100 * as.numeric(str_extract(row$est, "^[-0-9.]+"))
}
emit("c6_persistence_overall", pooled_ratio("Overall"))
emit("c6_persistence_stronger", pooled_ratio("Prediction: stronger persistence"))
emit("c6_persistence_weaker", pooled_ratio("Prediction: weaker persistence"))

# "On the basis of the twelve panel survey experiments analyzed in this chapter, we can say
#  that durable means that treatment effects are, on average, approximately one-third their
#  original magnitudes after ten days." (p. 118)
emit("c6_decay_one_third", pooled_ratio("Overall") / 100)

# "Curiously, the different replications of the Hiscox (2006) 'expert' treatment generated
#  very different persistence estimates: 49 percent on MTurk but 0 percent on Lucid."
#  (p. 114)
#
# There is no Lucid sample in the persistence analysis. Table 6.1 lists twelve panel
# experiments and the Hiscox rows of table 6.4 are Mechanical Turk and GfK, so the zero
# printed here is the GfK estimate, which is what the book's own table 6.4 gives.
table_6.4 <- read_output("table_6.4_all_persistence.csv")
hiscox <- function(sample_label) {
  row <- table_6.4 |>
    filter(str_starts(study_label, "Hiscox"), Sample == sample_label, Treatment == "Expert cue")
  stopifnot(nrow(row) == 1)
  100 * as.numeric(str_extract(row$`Persistence Ratio`, "^[-0-9.]+"))
}
emit("c6_hiscox_mturk", hiscox("MTurk"))
emit("c6_hiscox_lucid", hiscox("GfK"))

# "The pro-minimum wage videos, the flat tax op-ed, and the pro-capital punishment studies
#  all had persistence ratios above 60 percent." (p. 114)
above_sixty <-
  table_6.4 |>
  filter(str_detect(Treatment, "pro-minimum wage|Flat tax|Two pro studies")) |>
  mutate(ratio = as.numeric(str_extract(`Persistence Ratio`, "^[-0-9.]+")))
emit("c6_above_sixty", sum(above_sixty$ratio > 0.6))

# "After ten days treatment effects decay to 46 percent (SE = 5 percent) of their original
#  magnitudes. ... After thirty days, average persistence declines to 44 percent (SE = 6
#  percent)." (p. 117)
table_6.3 <- read_output("table_6.3_oped_persistence.csv")
oped <- function(group, column, part) {
  row <- table_6.3 |> filter(pid_3 == group)
  stopifnot(nrow(row) == 1)
  if (part == "estimate") {
    100 * as.numeric(str_extract(row[[column]], "^[-0-9.]+"))
  } else {
    100 * as.numeric(str_remove_all(str_extract(row[[column]], "\\([0-9.]+\\)"), "[()]"))
  }
}
emit("c6_oped_ten_days", oped("Overall", "w2_est", "estimate"))
emit("c6_oped_ten_days_se", oped("Overall", "w2_est", "se"))
emit("c6_oped_thirty_days", oped("Overall", "w3_est", "estimate"))
emit("c6_oped_thirty_days_se", oped("Overall", "w3_est", "se"))

# "For Republicans, persistence after ten days averaged 57 percent; this figure was 43
#  percent for Democrats. But an inspection of the confidence intervals reveals that these
#  two estimates are not statistically significantly different from each other." (p. 118)
emit("c6_oped_rep_ten_days", oped("Republican", "w2_est", "estimate"))
emit("c6_oped_dem_ten_days", oped("Democrat", "w2_est", "estimate"))

group_gap <- oped("Republican", "w2_est", "estimate") - oped("Democrat", "w2_est", "estimate")
group_gap_se <- sqrt(oped("Republican", "w2_est", "se")^2 + oped("Democrat", "w2_est", "se")^2)
emit("c6_oped_groups_not_different", as.integer(abs(group_gap / group_gap_se) < qnorm(0.975)))
