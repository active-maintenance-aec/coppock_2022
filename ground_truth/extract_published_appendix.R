# coppock_2022/ground_truth/extract_published_appendix.R
# Output: ground_truth/published_appendix_values.csv
# Depends on: the published book PDF (BOOK_PDF), and poppler's pdftotext on the PATH
# Description: Parse every cell of the book's appendix treatment-effect tables out of the
#   published page and commit them, so the appendix's 1,172 published numbers have a
#   transcription the ground truth can join against.
#
#   The book is not redistributed in this repository, so the PDF's location comes from
#   the BOOK_PDF environment variable. The committed CSV is the artifact; this script is
#   here so the transcription is auditable rather than hand-typed.
#
#   Parsing is positional, via `pdftotext -bbox-layout`, and that is not a preference.
#   In reading order pdftotext loses the numeric columns of Table A.2 entirely: its six
#   rows come back carrying their labels and nothing else, which reads as a four-row
#   table with a couple of stray numbers rather than as a failure.
#
#   Three artifacts of this particular PDF, each of which silently drops rows if it is
#   not handled:
#     - a negative number is typeset as a minus sign followed by a soft hyphen, and
#       sometimes by a space as well, so "-0.25" arrives as "- ­0.25"
#     - the minus sign is U+2212, not a hyphen
#     - the small-capital T of "Table" is sometimes dropped, so Table A.10's caption
#       arrives as "a ble A.10", and letter-spacing puts a space inside "Ta ble"

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_published_appendix.R")

book_pdf <- Sys.getenv("BOOK_PDF", unset = "")
stopifnot(
  "Set BOOK_PDF to the path of the published book PDF." = nzchar(book_pdf),
  file.exists(book_pdf)
)

bbox_path <- file.path(tempdir(), "coppock_2022_bbox.html")
status <- system2("pdftotext", c("-bbox-layout", shQuote(book_pdf), shQuote(bbox_path)),
                  stdout = FALSE, stderr = FALSE)
stopifnot("pdftotext failed" = status == 0, file.exists(bbox_path))

# Rebuild lines from word boxes ----
# Words are grouped by their vertical position and ordered by their horizontal one, which
# is what recovers a table row as a row.
raw <- read_file(bbox_path)

page_blocks <- str_split_1(raw, fixed("<page "))[-1]

words_on_page <- function(block, page) {
  m <- str_match_all(
    block,
    '<word xMin="([0-9.]+)" yMin="([0-9.]+)" xMax="[0-9.]+" yMax="[0-9.]+">([^<]*)</word>'
  )[[1]]
  if (nrow(m) == 0) return(tibble())
  tibble(page = page, x = as.numeric(m[, 2]), y = as.numeric(m[, 3]), word = m[, 4])
}

normalize <- function(x) {
  x |>
    str_remove_all("­") |>
    str_replace_all("[−‐‑‒–]", "-") |>
    str_replace_all("-\\s+(?=[0-9.])", "-") |>
    str_squish()
}

# Word baselines within one typeset line differ by a fraction of a point, and a caption
# set in small capitals differs by more, so bins are merged when they are closer together
# than the leading. Without the merge a caption splits in two and stops being recognized
# as a caption, which silently files the next table's rows under the previous table:
# Table A.9's thirty rows land in Table A.8, and the row-count check below is what
# catches it.
lines_df <-
  imap(page_blocks, words_on_page) |>
  list_rbind() |>
  mutate(bin = round(y * 2) / 2) |>
  arrange(page, bin, x) |>
  mutate(row = cumsum(page != lag(page, default = -1L) | bin - lag(bin, default = -Inf) >= 2)) |>
  summarize(page = first(page), text = normalize(str_c(word, collapse = " ")), .by = row) |>
  arrange(row)

# Assign each line to the appendix table above it ----
# Table A.9's caption comes back with "Ta ble A.9" at the end of the line rather than the
# start, because the small-capital label sits on a slightly different baseline from the
# rest of the caption, so the number can be the last thing on the line.
caption <- str_match(lines_df$text,
                     "(?:T\\s?a|a)\\s?b\\s?l\\s?e\\s+(A\\.[0-9]+)(?:\\s|$)")[, 2]
lines_df$table <- vctrs::vec_fill_missing(caption, direction = "down")
lines_df$is_caption <- !is.na(caption)

# Parse the estimate rows ----
# Every cell of every treatment-effect table is one of these lines: some label columns,
# then "estimate (standard error)", then a bracketed 95 per cent interval.
row_pattern <- "^(.*?)\\s(-?[0-9]+\\.[0-9]+)\\s\\((-?[0-9]+\\.[0-9]+)\\)\\s\\[\\s*(-?[0-9]+\\.[0-9]+),\\s*(-?[0-9]+\\.[0-9]+)\\s*\\]$"

published_cells <-
  lines_df |>
  filter(!is.na(table), !is_caption) |>
  mutate(m = map(text, ~str_match(.x, row_pattern))) |>
  filter(map_lgl(m, ~!is.na(.x[1, 1]))) |>
  transmute(
    table,
    pdf_page = page,
    row_label = map_chr(m, ~str_squish(.x[1, 2])),
    estimate = map_chr(m, ~.x[1, 3]),
    std_error = map_chr(m, ~.x[1, 4]),
    conf_low = map_chr(m, ~.x[1, 5]),
    conf_high = map_chr(m, ~.x[1, 6])
  ) |>
  mutate(cell_id = str_c(table, "_row", row_number()), .by = table) |>
  relocate(cell_id)

# The parse is checked against the book's own inventory rather than trusted. Fourteen of
# the nineteen appendix tables report treatment effects; the other five (A.3, A.11, A.13,
# A.15, A.17) set out treatments and outcomes in words and contain no estimates.
expected <- c(A.1 = 27, A.2 = 6, A.4 = 12, A.5 = 30, A.6 = 18, A.7 = 12, A.8 = 48,
              A.9 = 30, A.10 = 36, A.12 = 33, A.14 = 11, A.16 = 11, A.18 = 8, A.19 = 11)

found <- table(published_cells$table)
stopifnot(
  "An appendix table came back with a row count the published page does not have." =
    identical(sort(names(expected)), sort(names(found))) &&
    all(expected[names(found)] == as.integer(found))
)

write_csv(published_cells, here::here("ground_truth", "published_appendix_values.csv"))

print(published_cells |> count(table) |> mutate(cells = n * 4), n = Inf)
print(str_glue("Wrote ground_truth/published_appendix_values.csv: ",
               "{nrow(published_cells)} rows, {nrow(published_cells) * 4} published cells ",
               "across {n_distinct(published_cells$table)} appendix tables."))
