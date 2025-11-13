

# Wipe global environment (use with care in interactive sessions)
rm(list = ls(all = TRUE))

# Regenerate Rd/NAMESPACE from Roxygen comments
devtools::document()

output_directory <- "data-raw/releases/harmonized_data"

# Verify auth first (nice sanity check)
if (requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

# Official Pasture, Rangeland, Forage Pilot Insurance Program Data
piggyback::pb_release_create(
  repo = "ftsiboe/GHAgricProductivityLab",
  tag  = "hh_data",
  name = "Harmonized household Datasets",
  body = readr::read_file(file.path(output_directory,"README.md"))
)

# Upload the assets
asset_list <- list.files(output_directory, full.names = TRUE, recursive = TRUE)
asset_list <- asset_list[!grepl("README",asset_list)]
piggyback::pb_upload(
  asset_list,
  repo  = "ftsiboe/GHAgricProductivityLab",
  tag   = "hh_data",
  overwrite = TRUE
)