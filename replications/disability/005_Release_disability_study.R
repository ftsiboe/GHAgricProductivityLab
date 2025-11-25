

# Wipe global environment (use with care in interactive sessions)
rm(list = ls(all = TRUE))

# Regenerate Rd/NAMESPACE from Roxygen comments
devtools::document()

project_name  <- "disability"
project_title <- "Disability studies"

output_directory <- file.path("releases",project_name)

# Verify auth first (nice sanity check)
if (requireNamespace("gh", quietly = TRUE)) try(gh::gh_whoami(), silent = TRUE)

# Official Pasture, Rangeland, Forage Pilot Insurance Program Data
piggyback::pb_release_create(
  repo = "ftsiboe/GHAgricProductivityLab",
  tag  = project_name,
  name = project_title,
  body = readr::read_file(file.path(output_directory,"legacy_studies.md"))
)
file.path("replications",project_name,"output","estimations")

# Upload the assets
asset_list <- list.files(output_directory, full.names = TRUE, recursive = TRUE)
asset_list <- asset_list[grepl(".zip",asset_list)]
piggyback::pb_upload(
  asset_list,
  repo  = "ftsiboe/GHAgricProductivityLab",
  tag   = "legacy_studies",
  overwrite = TRUE
)

