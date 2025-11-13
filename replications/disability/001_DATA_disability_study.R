# =============================================================================
#  DATA and SETUP - DISABILITY STUDY 
# =============================================================================
#  General Description:
#  ---------------------------------------------------------------------------
#  This script prepares analysis-ready data for the disability study within the
#  GHAgricProductivityLab project. It:
#    - Initializes a study-specific environment (folders, paths, metadata),
#    - Loads harmonized farm/household and disability modules,
#    - Merges them at the household-member level,
#    - Restricts the sample to recent GLSS waves (GLSS6 and GLSS7),
#    - Constructs a composite "disabled" indicator (any disability in household),
#    - Cleans sub-indicator variables to avoid false zeros when overall
#      disability = 1, and
#    - Saves both the processed study dataset and the study environment object
#      to disk for downstream analysis (e.g., regression, matching, etc.).
# =============================================================================

# ---- Housekeeping: clear workspace and run garbage collection ----------------
rm(list = ls(all = TRUE)); gc()              

# ---- Rebuild package documentation (if this is part of a package) -----------
# This calls roxygen2 via devtools to regenerate .Rd docs and NAMESPACE.
devtools::document()                         

# ---- Define study name and initialize study environment ---------------------
project_name <- "disability"

# study_setup() is assumed to:
#   - create / verify directories,
#   - define paths (e.g., wd$home, wd$data, wd$output),
#   - and return a list-like "study_environment" object
#     containing configuration for this specific project.
study_environment <- study_setup(project_name = project_name)

# ---- Load harmonized household / farmer-level data --------------------------
# Wrapper that downloads (via piggyback) and caches Stata .dta files from
# the GHAgricProductivityLab GitHub repo, then reads them with haven.
farmer_data     <- get_household_data("harmonized_crop_farmer_data")
disability_data <- get_household_data("harmonized_disability_data")

# ---- Merge farmer and disability data at the household-member level ---------
# Merge keys:
#   - Surveyx : survey round (e.g., GLSS6, GLSS7)
#   - EaId    : enumeration area
#   - HhId    : household ID
#   - Mid     : member ID
study_data <- dplyr::inner_join(
  farmer_data,
  disability_data,
  by = c("Surveyx", "EaId", "HhId", "Mid")
)

# ---- Restrict to GLSS6 and GLSS7 and drop certain variables -----------------
# Keep only GLSS6 and GLSS7, i.e., more recent rounds with richer disability data.
# Then drop any variables whose names match the patterns "LndFrgMid", "EduWhyNo", or "RentHa".
study_data <- study_data[
  study_data$Surveyx %in% c("GLSS6","GLSS7"),
  names(study_data)[!grepl("LndFrgMid|EduWhyNo|RentHa", names(study_data))]
]

# ---- Construct composite disability indicator --------------------------------
# Create 'disabled' = 1 if ANY of the disability indicators equals 1.
# This captures whether there is any reported disability linked to the respondent
# or a close relation.
study_data$disabled <- as.integer(
  study_data$disabled == 1 |
    study_data$disabled_self == 1 |
    study_data$disabled_spouse == 1 |
    study_data$disabled_child == 1 |
    study_data$disabled_close == 1 |
    study_data$disabled_member == 1
)

# ---- Clean sub-disability indicators when overall disabled = 1 --------------
# For households / individuals flagged as disabled (disabled == 1),
# replace 0s in the sub-disability fields with NA. This avoids
# interpreting zeros as "explicitly no disability" when we only know
# that *someone* is disabled but not exactly which sub-category applies.
vars <- c("disabled_self", "disabled_spouse", "disabled_child",
          "disabled_close", "disabled_member")

for (v in vars) {
  study_data[[v]][study_data[[v]] == 0 & study_data$disabled == 1] <- NA
}
# ---- Attach raw data to study environment (potential issue) ------------------
study_environment$study_raw_data <- study_data

# ---- Save study environment object ------------------------------------------
# Save the entire study environment configuration (paths, metadata, etc.)
# for reproducibility and to simplify subsequent scripts.
saveRDS(
  study_environment,
  file.path(study_environment$wd$output, paste0(project_name,"_study_environment.rds"))
)
