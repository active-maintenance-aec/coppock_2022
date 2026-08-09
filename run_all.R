# coppock_2022/run_all.R
# Output: everything in maintained/output/, plus ground_truth/coppock_2022_ground_truth.csv
#   and ground_truth/float_coverage.csv
# Depends on: original_manifest.csv, original_members_manifest.csv
# Description: The repository's entry point. Open coppock_2022.Rproj and source this file.
#   It fetches and verifies the deposit, runs every analysis script in book order, builds
#   the ground truth, prints the in-text claims, and then re-verifies the deposit.
#
#   The deposit is verified twice on purpose. The check inside download_original.R is a
#   precondition when sourced first, so a script that damaged original/ or
#   original_extracted/ midway through would go uncaught until the next run, which might
#   be months away. Re-sourcing the file at the end turns that into a postcondition of
#   this run.

library(here)
library(tidyverse)

here::i_am("run_all.R")

# Fetch and verify the deposit ----
source(here::here("download_original.R"))

# Analysis scripts, in book order ----
scripts <- c(
  "figure_1.2_and_1.3_flat_tax_immediate_and_delayed.R",
  "figure_1.4_gay_marriage_trends.R",
  "figure_1.5_gss_abortion.R",
  "figure_2.2_table_2.2_lrl_biased_assimilation.R",
  "figure_2.3_table_2.3_gc_replication.R",
  "figure_3.1_oped_target_nontarget.R",
  "figure_3.2_ca_party_cues.R",
  "figure_3.3_elite_endorsements.R",
  "figure_5.1_table_a4_minimum_wage.R",
  "figure_5.2_table_a2_gun_control.R",
  "figure_5.3_other_four_opeds.R",
  "figure_5.4_table_a6_patriot_act.R",
  "figure_5.5_table_a7_immigration.R",
  "figure_5.6_free_trade_expert.R",
  "figure_5.7_free_trade_valence.R",
  "figure_5.8_table_a9_expert_economists.R",
  "figure_5.9_table_a10_framing.R",
  "figure_5.10_table_a12_gash_murakami.R",
  "figure_5.11_table_a14_flavin.R",
  "figure_5.12_table_a16_kreps_wallace.R",
  "figure_5.13_table_a18_mutz.R",
  "figure_5.15_table_a19_trump_white.R",
  "figure_5.16_cate_correlations.R",
  "figure_5.17_gc_pro_con.R",
  "figure_5.18_patriot_act_both.R",
  "figure_5.19_free_trade_both.R",
  "figure_6.1_and_6.2_table_6.2_and_6.4_persistence.R",
  "figure_6.3_table_6.3_oped_persistence.R"
)

stopifnot(all(file.exists(here::here("maintained", scripts))))

# Each script runs in its own environment. A script that silently depends on an object
# another one left behind would otherwise pass here and fail for a reader running it on
# its own, which the README tells them they may do.
timings <- map_df(scripts, function(script) {
  message("=== ", script, " ===")
  started <- Sys.time()
  source(here::here("maintained", script), local = new.env())
  tibble(script = script, seconds = as.numeric(difftime(Sys.time(), started, units = "secs")))
})

# Timings are a property of the machine on the day, not of the pipeline, so they are
# written outside maintained/output/ and are excluded from the acceptance test that
# requires that directory to come back unchanged.
write_csv(timings, here::here("run_timings.csv"))
print(timings |> arrange(desc(seconds)), n = Inf)
print(str_glue("Total: {round(sum(timings$seconds) / 60, 1)} minutes across ",
               "{nrow(timings)} scripts."))

# Figure timestamps ----
# R's pdf() device stamps a wall-clock /CreationDate and /ModDate into every figure it
# writes, and those two fields are the only reason two runs of this pipeline produce
# differing files. Blanking them lets the determinism check cover every file the
# pipeline writes rather than all but the figures.
source(here::here("maintained", "helpers.R"))
walk(
  list.files(here::here("maintained", "output"), pattern = "\\.pdf$", full.names = TRUE),
  blank_pdf_timestamps
)

# Ground truth ----
# build_ground_truth.R also sources in_text_claims.R under capture.output() and runs the
# coverage gate over what it printed, so the gate is checked here as well as by the
# human-readable pass below.
source(here::here("ground_truth", "build_ground_truth.R"), local = new.env())

# In-text claims, for a human to read ----
source(here::here("maintained", "in_text_claims.R"), local = new.env())

# Re-verify the deposit ----
# This is the postcondition. It halts if any script wrote into original/ or
# original_extracted/ during the run above.
source(here::here("download_original.R"), local = new.env())

print("run_all.R complete.")
