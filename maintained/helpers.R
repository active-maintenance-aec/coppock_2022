# coppock_2022/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Packages, paths and shared helpers for every script in maintained/.
#   Sourced first by each script; no analysis script loads a package of its own.

library(here)
library(tidyverse)
library(estimatr)
library(patchwork)
library(rsample)
library(metafor)
library(vayr)
library(ggbrace)

here::i_am("maintained/helpers.R")

# Paths ----
# original_extracted/ is the unpacked deposit, rebuilt by download_original.R.
path_original <- function(...) here::here("original_extracted", "replication_archive", ...)

# Output goes to maintained/output/ in every ordinary run. The directory is overridable
# only so that ground_truth/measure_seed_dispersion.R can run an analysis script at a
# different seed without writing over the committed outputs. Nothing else sets it.
output_dir <- Sys.getenv("MAINTAINED_OUTPUT_DIR", unset = here::here("maintained", "output"))
path_output <- function(...) file.path(output_dir, ...)

# Seeds ----
# The deposit's two chapter 6 scripts set no seed at all, so the published persistence
# estimates are one unlabelled draw of an unseeded bootstrap. The rewrite fixes a seed so
# that its own output is reproducible, and exposes an override so the dispersion of the
# published quantities across seeds can be measured rather than assumed. The default is
# the seed the script names, and that is what every ordinary run uses.
script_seed <- function(default) {
  override <- Sys.getenv("MAINTAINED_SEED", unset = "")
  if (nzchar(override)) as.integer(override) else default
}

# Formatting helpers ----

add_parens <- function(x, digits = 3) {
  x <- as.numeric(x)
  paste0("(", sprintf(paste0("%.", digits, "f"), x), ")")
}

format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  sprintf(paste0("%.", digits, "f"), x)
}

make_se_entry <- function(est, se, digits = 2) {
  paste0(format_num(est, digits = digits), " ", add_parens(se, digits = digits))
}

make_interval_entry <- function(conf.low, conf.high, digits = 2) {
  paste0(
    "[",
    format_num(conf.low, digits = digits),
    ", ",
    format_num(conf.high, digits = digits),
    "]"
  )
}

# Color palettes ----

bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")
pro_con_colors <- c("#C67800", "#205C8A")
grays <- c(gray(0), gray(0.5))

# Custom tidy methods ----
# The deposit's tidiers.R also defines tidy.robu() and tidy.deming(), and calls neither
# robu() nor deming() anywhere. They are dead in the deposit and are not carried here.

# rma() returns beta as a 1 by 1 matrix. Left as it comes, the tidied tibble carries a
# matrix column, which write_csv() refuses, so it is coerced here.
tidy.rma.uni <- function(fit) {
  tibble(
    estimate = as.numeric(fit$beta),
    std.error = as.numeric(fit$se),
    conf.low = as.numeric(fit$ci.lb),
    conf.high = as.numeric(fit$ci.ub)
  )
}

# Sunflower helper ----
# The archive calls vayr::sunflower(x, y, width, height), where width and height are
# the semi-axes of the ellipse the points are laid out on. vayr 1.0.0 replaced those
# two arguments with density and aspect_ratio and computes
#   width  = 1 / sqrt(100 * density / n)
#   height = width / aspect_ratio
# so the archive's call is recovered exactly by inverting those two lines. n is the
# number of points in the group, which is why density has to be computed per call
# rather than fixed: the archive's width and height do not depend on n and the new
# API's density does.
sunflower_compat <- function(x = NULL, y = NULL, width, height) {
  n <- length(if (is.null(x)) y else x)
  vayr::sunflower(
    x = x,
    y = y,
    density = n / (100 * width^2),
    aspect_ratio = width / height
  )
}

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
