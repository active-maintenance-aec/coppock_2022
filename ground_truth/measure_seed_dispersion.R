# coppock_2022/ground_truth/measure_seed_dispersion.R
# Output: ground_truth/seed_dispersion.csv
# Depends on: maintained/figure_6.1_and_6.2_table_6.2_and_6.4_persistence.R,
#   maintained/figure_6.3_table_6.3_oped_persistence.R
# Description: Measure how far the book's chapter 6 persistence estimates could have
#   landed, by re-running the two persistence scripts at a range of seeds.
#
#   The deposit's two chapter 6 scripts call rsample::bootstraps() and set no seed at
#   all, so every number in tables 6.2, 6.3 and 6.4, in figures 6.1 and 6.2, and in the
#   persistence sentences of chapter 6 is one unlabelled draw. A published value from an
#   unseeded procedure cannot be matched by any single run, so the ground truth compares
#   it against this distribution rather than against one number.
#
#   The bootstrap enters the point estimates as well as the standard errors, which is
#   easy to miss: metafor::rma() weights each study by the inverse of its squared
#   standard error, and those standard errors are bootstrapped, so the pooled persistence
#   ratio moves with the seed too.
#
#   This script is deliberately NOT part of run_all.R. It re-runs a 237-second script
#   once per seed, and what it measures is a property of the estimator rather than of any
#   one run, so it is measured when the ground truth is built and committed as evidence.
#   Set SEED_DISPERSION_SEEDS to change how many seeds are used.

library(here)
library(tidyverse)

here::i_am("ground_truth/measure_seed_dispersion.R")

n_seeds <- as.integer(Sys.getenv("SEED_DISPERSION_SEEDS", unset = "20"))
stopifnot(n_seeds >= 2)

scratch <- file.path(tempdir(), "coppock_2022_seed_dispersion")

# Run one script at one seed ----
# The run happens in a separate process with MAINTAINED_OUTPUT_DIR pointed at a scratch
# directory, so that measuring the dispersion cannot overwrite the committed outputs.
run_at_seed <- function(script, seed) {
  out_dir <- file.path(scratch, paste0("seed_", seed))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  status <- system2(
    "Rscript",
    c("--vanilla", "-e", shQuote(str_glue('source(here::here("maintained", "{script}"))'))),
    env = c(paste0("MAINTAINED_OUTPUT_DIR=", out_dir), paste0("MAINTAINED_SEED=", seed)),
    stdout = FALSE, stderr = FALSE
  )
  stopifnot(status == 0)
  out_dir
}

seeds <- seq_len(n_seeds)

# Table 6.3: the op-ed persistence estimates ----
table_6.3 <- map_df(seeds, function(seed) {
  out_dir <- run_at_seed("figure_6.3_table_6.3_oped_persistence.R", seed)
  read_csv(file.path(out_dir, "table_6.3_oped_persistence.csv"), show_col_types = FALSE) |>
    mutate(seed = seed)
})

table_6.3_long <-
  table_6.3 |>
  pivot_longer(c(w2_est, w3_est), names_to = "horizon", values_to = "entry") |>
  transmute(
    float = "table_6.3",
    quantity = str_glue("{pid_3}, {if_else(horizon == 'w2_est', '10 days', '30 days')}"),
    seed,
    value = as.numeric(str_extract(entry, "^[-0-9.]+"))
  )

# Table 6.2: the pooled persistence ratios ----
table_6.2 <- map_df(seeds, function(seed) {
  out_dir <- run_at_seed("figure_6.1_and_6.2_table_6.2_and_6.4_persistence.R", seed)
  read_csv(file.path(out_dir, "table_6.2_persistence_ratio.csv"), show_col_types = FALSE) |>
    mutate(seed = seed)
})

table_6.2_long <-
  table_6.2 |>
  transmute(
    float = "table_6.2",
    quantity = group,
    seed,
    value = as.numeric(str_extract(est, "^[-0-9.]+"))
  )

# Record ----
# Only the distribution is committed, not the seed-by-seed draws: the draws are a
# property of this run and would leave the file dirty after every regeneration, while
# the summary is a property of the estimator.
dispersion <-
  bind_rows(table_6.2_long, table_6.3_long) |>
  summarize(
    n_seeds = n(),
    minimum = min(value),
    maximum = max(value),
    median = median(value),
    .by = c(float, quantity)
  ) |>
  arrange(float, quantity, .locale = "en")

write_csv(dispersion, here::here("ground_truth", "seed_dispersion.csv"))

print(dispersion, n = Inf)
print(str_glue("Wrote ground_truth/seed_dispersion.csv ({nrow(dispersion)} rows) ",
               "from {n_seeds} seeds."))
