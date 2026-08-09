# coppock_2022/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
#   and original_extracted/ (the unpacked contents of the deposited zip)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse, verify it, and unpack
#   it. Run this once before running anything in maintained/. Re-running is free: a file
#   already present with the right checksum is not downloaded again.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes
#   Dataverse returns for `?format=original`, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. The one file here agrees,
#   but they do not always: other deposits in this program carry published checksums that
#   verify neither the original nor the derived tabular file, so verification runs against
#   md5_served and any disagreement is reported.
#
#   This deposit is a single zip rather than a set of loose files, so original/ holds the
#   zip alone and the unpacked tree lives beside it in original_extracted/. Both are
#   rebuilt from Dataverse and neither is redistributed here. Unpacking is done fresh
#   every time so that a stale tree cannot survive a change to the deposit, and the
#   __MACOSX resource-fork directory the zip carries is discarded.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/I9GSKI"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than the tabular
# representation Dataverse derives for ingested files.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
# md5_served is the MD5 of the bytes `?format=original` returns, and it is the gate.
# md5_published is what Dataverse displays for those same bytes: where the two agree it
# adds nothing a check could fail on, and where they disagree the archive's own metadata
# is wrong and no local copy could satisfy both, so it is reported rather than enforced.
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    bytes_on_disk = file.size(path),
    md5_ok = md5_downloaded == md5_served,
    bytes_ok = bytes_on_disk == bytes,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, bytes_on_disk, bytes_ok, md5_served, md5_downloaded, md5_ok,
         published_agrees)

print(verified |> select(file, bytes, bytes_ok, md5_ok, published_agrees), n = nrow(verified))

if (!all(verified$md5_ok)) {
  stop("Checksum mismatch in original/: ",
       paste(verified$file[!verified$md5_ok], collapse = ", "),
       ". Delete the offending files and re-run to refetch them from Dataverse.")
}

if (!all(verified$bytes_ok)) {
  stop("Byte size mismatch in original/: ",
       paste(verified$file[!verified$bytes_ok], collapse = ", "), ".")
}

# original/ must hold the deposit and nothing else. A name check alone would pass a
# renamed file, so the check above is by checksum and this one is for strays.
# all.files = TRUE is not optional: without it a stray dotfile passes unseen, and a
# deposit can ship dotfiles of its own, which the manifest then has to list.
strays <- setdiff(
  list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE),
  manifest$file
)
if (length(strays) > 0) {
  stop("original/ holds files the manifest does not list: ",
       paste(strays, collapse = ", "),
       ". Move them elsewhere; original/ is the deposit and only the deposit.")
}

print(str_glue("All {nrow(verified)} files match on MD5 and byte size, and original/ holds ",
               "nothing else. {sum(!verified$published_agrees)} carry a published checksum ",
               "that disagrees with what Dataverse serves."))

# Members manifest ----
# original/ holds one file, so the emptiness check above is vacuous on its own: it
# proves the zip is the deposited zip and says nothing about what came out of it.
# original_members_manifest.csv pins every member of the unpacked tree by checksum
# and byte size, and the stray check makes the pair complete.
members <- read_csv(here::here("original_members_manifest.csv"), show_col_types = FALSE)

extracted_dir <- here::here("original_extracted")

# A helper rather than two copies of the same twelve lines: the tree is audited twice
# per run, once before it is rebuilt and once after, and the two audits must ask
# exactly the same question.
#
# all.files = TRUE is not optional: this deposit ships a .DS_Store and a whole
# .Rproj.user tree, so a check that skipped dotfiles would both miss real members and
# let a stray dotfile through unseen.
audit_extracted <- function() {
  checked <- members |>
    mutate(
      path = file.path(extracted_dir, member),
      md5_extracted = unname(tools::md5sum(path)),
      bytes_extracted = file.size(path),
      md5_ok = !is.na(md5_extracted) & md5_extracted == md5,
      bytes_ok = !is.na(bytes_extracted) & bytes_extracted == bytes
    )
  strays <- setdiff(
    list.files(extracted_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
    members$member
  )
  list(
    damaged = checked$member[!checked$md5_ok | !checked$bytes_ok],
    strays = strays,
    n_ok = sum(checked$md5_ok & checked$bytes_ok)
  )
}

# Audit the tree BEFORE rebuilding it ----
# This is the audit that can fail, and the ordering is the whole point of it. The
# rebuild below wipes original_extracted/ and unpacks it again, so a check placed
# only after the rebuild is answering a question about the zip rather than about the
# tree, and it passes however badly a run damaged the tree. Nothing but this script
# may write into original_extracted/; run_all.R re-sources this file as its last
# step, so a script that wrote there mid-run is caught here at the end of that run
# rather than months later.
if (dir.exists(extracted_dir)) {
  before <- audit_extracted()
  if (length(before$damaged) > 0) {
    stop("original_extracted/ was modified since it was last unpacked: ",
         paste(before$damaged, collapse = ", "),
         ". Something wrote into the unpacked deposit; find it before re-running, ",
         "because re-running rebuilds the tree and erases the evidence.")
  }
  if (length(before$strays) > 0) {
    stop("original_extracted/ holds files the members manifest does not list: ",
         paste(before$strays, collapse = ", "),
         ". Nothing but this script may write there; find what put them there ",
         "before re-running.")
  }
  print(str_glue("original_extracted/ audited before rebuild: all {before$n_ok} ",
                 "members intact and nothing else present."))
}

# Unpack ----
# Everything in maintained/ reads from original_extracted/, never from original/, so
# that original/ holds the deposit and nothing else. Unpacking into the deposit
# directory is how another archive in this program lost five deposited files: a
# script that writes its output beside its input silently overwrites part of the
# deposit when the output name is itself a deposited file.
#
# The tree is rebuilt from the zip on every run, so a stale member cannot survive a
# change to the deposit.
unlink(extracted_dir, recursive = TRUE)
dir.create(extracted_dir)
unzip(here::here("original", "replication_archive.zip"), exdir = extracted_dir)
unlink(file.path(extracted_dir, "__MACOSX"), recursive = TRUE)

# Verify the members ----
# This audit checks the zip rather than the tree, since the tree was written from the
# zip a line ago. It is what catches a deposit that has been revised underneath the
# manifest.
after <- audit_extracted()
if (length(after$damaged) > 0) {
  stop("The unpacked archive does not match the members manifest: ",
       paste(after$damaged, collapse = ", "),
       ". The deposited zip and original_members_manifest.csv disagree.")
}
if (length(after$strays) > 0) {
  stop("The deposited zip unpacks files the members manifest does not list: ",
       paste(after$strays, collapse = ", "), ".")
}

print(str_glue("All {after$n_ok} members unpacked into original_extracted/ and ",
               "verified on MD5 and byte size; the tree holds nothing else."))
print(str_glue("Archive: {dataset_doi}"))
