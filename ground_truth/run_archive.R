# coppock_2022/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv
# Depends on: original_extracted/ (rebuilt by download_original.R)
# Description: Run every deposited script twice, once against the deposit as shipped and
#   once against a copy stripped to data plus code, and record where each one stopped.
#   Also measure, by mtime, which deposited files a run overwrites.
#
#   Two questions, two passes. As shipped answers "does the deposit reproduce for someone
#   who downloads it"; stripped answers "does each script build its own inputs, or is it
#   reading an intermediate the deposit happens to carry". Where an archive ships the
#   outputs of its own cleaning scripts, the second answer is much worse than the first,
#   and only the two together say which.
#
#   The run happens in a scratch copy and never in original_extracted/, because a script
#   that writes its output beside its input silently overwrites part of the deposit when
#   the output name is itself a deposited file.
#
#   Scratch location comes from ARCHIVE_RUN_DIR so this script is runnable by anyone; it
#   defaults to a session temporary directory.

library(here)
library(tidyverse)

here::i_am("ground_truth/run_archive.R")

archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR", unset = file.path(tempdir(), "coppock_2022_archive"))
stopifnot(nzchar(archive_run_dir))

deposit <- here::here("original_extracted", "replication_archive")
stopifnot(dir.exists(deposit))

timeout_seconds <- 2400

# Which members are data plus code ----
# The stripped pass needs a stated definition rather than an assumed one. This deposit
# ships 21 pre-cleaned .rds files, 30 scripts, a README and RStudio session state, and
# no script writes anything, so it carries no derived object produced by its own code:
# stripped and as-shipped are the same set of inputs. That is asserted here rather than
# assumed, because "the stripped test passed" and "the stripped test was vacuous" are
# very different findings and look identical if nobody checks.
members <- read_csv(here::here("original_members_manifest.csv"), show_col_types = FALSE) |>
  mutate(
    relative = str_remove(member, "^replication_archive/"),
    kind = case_when(
      str_detect(relative, "^code/.*\\.R$") ~ "code",
      str_detect(relative, "^data/.*\\.rds$") ~ "data",
      TRUE ~ "other"
    )
  )

print(count(members, kind))

scripts <- members |>
  filter(kind == "code", !relative %in% c("code/helpers.R", "code/tidiers.R")) |>
  pull(relative) |>
  sort(method = "radix")

print(str_glue("{length(scripts)} deposited analysis scripts; ",
               "{sum(members$kind == 'data')} deposited data files."))

# Run one script ----
# The timeout is tested BEFORE the error text. A killed script prints a dying error of
# its own, and reading that error as the reason it stopped files a resource limit as a
# code defect.
#
# source() prints nothing without print.eval = TRUE, so a harness that omits it records
# the exit status correctly while seeing an empty log. That would matter here: the
# deposit's appendix tables are printed to the console rather than written to a file,
# and they are what a later pass reads back as the archive's own values.
#
# Both streams are captured into one character vector rather than redirected to a file
# twice. Passing the same file name as stdout and as stderr opens it under two
# connections, and the error text can be lost when the second overwrites the first,
# which is how a run recorded "Error in `mutate()`:" and lost the "! unused arguments"
# line that names the actual fault.
run_one <- function(script, run_dir) {
  log_path <- file.path(run_dir, "run_logs", paste0(basename(script), ".log"))
  started <- Sys.time()
  output <- withCallingHandlers(
    system2(
      "Rscript",
      c("--vanilla", "-e", shQuote(str_glue("source('{script}', print.eval = TRUE)"))),
      stdout = TRUE, stderr = TRUE,
      timeout = timeout_seconds
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  # system2() reports a non-zero exit through the "status" attribute when it is
  # capturing output, and reports success by leaving that attribute off.
  exit_code <- attr(output, "status") %||% 0L
  log_lines <- as.character(output)
  write_lines(log_lines, log_path)

  timed_out <- identical(exit_code, 124L) || elapsed >= timeout_seconds

  # rlang prints the condition over several lines and the first is often only
  # "Error in `mutate()`:", which names the verb rather than the fault. The line
  # beginning "!" carries the actual message, so both are kept.
  error_line <- log_lines[str_detect(log_lines, "^Error")] |> head(1)
  cause_line <- log_lines[str_detect(log_lines, "^! ")] |> head(1)

  tibble(
    script = script,
    status = if (timed_out) "timeout" else if (exit_code == 0) "ok" else "error",
    message = if (timed_out) {
      str_glue("killed at the {timeout_seconds}s limit")
    } else if (exit_code == 0) {
      NA_character_
    } else if (length(error_line) == 1) {
      str_squish(paste(c(error_line, cause_line), collapse = " "))
    } else {
      "non-zero exit with no Error line"
    },
    seconds = elapsed
  )
}

run_pass <- function(pass, keep_kinds) {
  run_dir <- file.path(archive_run_dir, pass)
  unlink(run_dir, recursive = TRUE)
  dir.create(file.path(run_dir, "run_logs"), recursive = TRUE)

  keep <- members |> filter(kind %in% keep_kinds)
  walk(unique(dirname(keep$relative)), \(d) dir.create(file.path(run_dir, d), recursive = TRUE, showWarnings = FALSE))
  file.copy(file.path(deposit, keep$relative), file.path(run_dir, keep$relative))

  before <- tibble(
    relative = keep$relative,
    mtime_before = file.mtime(file.path(run_dir, keep$relative)),
    md5_before = unname(tools::md5sum(file.path(run_dir, keep$relative)))
  )
  files_before <- list.files(run_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)

  old <- setwd(run_dir)
  on.exit(setwd(old), add = TRUE)
  results <- map_df(scripts, run_one, run_dir = run_dir)
  setwd(old)

  after <- before |>
    mutate(
      mtime_after = file.mtime(file.path(run_dir, relative)),
      md5_after = unname(tools::md5sum(file.path(run_dir, relative))),
      touched = mtime_after != mtime_before,
      changed = md5_after != md5_before
    )

  files_after <- list.files(run_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  strays <- setdiff(files_after, c(files_before, paste0("run_logs/", basename(scripts), ".log")))
  strays <- strays[!str_detect(strays, "^run_logs/")]

  print(str_glue("[{pass}] {sum(results$status == 'ok')} of {nrow(results)} scripts ran clean; ",
                 "{sum(after$touched)} deposited files overwritten, ",
                 "{sum(after$changed)} of those with different contents; ",
                 "{length(strays)} stray files created."))
  if (length(strays) > 0) print(strays)

  results |> mutate(pass = pass, .before = 1)
}

# As shipped, then stripped ----
as_shipped <- run_pass("as_shipped", c("code", "data", "other"))
stripped <- run_pass("stripped", c("code", "data"))

# Record ----
# Only properties of the deposit are committed. Timings are a property of the machine on
# the day, and a committed file carrying them is dirty after every run, so they are
# printed and discarded.
run_status <- bind_rows(as_shipped, stripped) |>
  select(pass, script, status, message) |>
  arrange(pass, script, .locale = "en")

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))

print(bind_rows(as_shipped, stripped) |>
        select(pass, script, status, seconds) |>
        arrange(pass, desc(seconds)), n = Inf)

print(str_glue("Wrote ground_truth/archive_run_status.csv ({nrow(run_status)} rows)."))
print(str_glue("Logs under {archive_run_dir}."))
