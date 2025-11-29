# =============================================================================
#  DATA and SETUP - EDUCATION STUDY 
# =============================================================================
#  General Description:
#  ---------------------------------------------------------------------------
#  This script prepares analysis-ready data for the education study within the
#  GHAgricProductivityLab project. It:
#    - Initializes a study-specific environment (folders, paths, metadata),
#    - Loads harmonized farm/household and education modules,
#    - Merges them at the household-farmer level,
#    - Restricts the sample to relevant GLSS waves,
#    - Saves both the processed study dataset and the study environment object
#      to disk for downstream analysis.
# =============================================================================

# ---- Housekeeping: clear workspace and run garbage collection
rm(list = ls(all = TRUE)); gc()              

# ---- Rebuild package documentation (if this is part of a package) 
# This calls roxygen2 via devtools to regenerate .Rd docs and NAMESPACE.
devtools::document()                         

run_only_for(id = 3, allowed_jobnames = "run_all")

# ---- Define study name and initialize study environment
project_name <- "education"

# study_setup() is assumed to:
#   - create / verify directories,
#   - define paths (e.g., wd$home, wd$data, wd$output),
#   - and return a list-like "study_environment" object
#     containing configuration for this specific project.
study_environment <- study_setup(project_name = project_name)

# ---- Load harmonized household / farmer-level data
# Wrapper that downloads (via piggyback) and caches Stata .dta files from
# the GHAgricProductivityLab GitHub repo, then reads them with haven.
farmer_data <- get_household_data("harmonized_crop_farmer_data")

# education_data <- get_household_data("harmonized_education_data")
education_data  <- as.data.frame(haven::read_dta("data-raw/releases/harmonized_data/harmonized_education_data.dta"))
education_data  <- education_data[names(education_data)[!grepl("YerEdu|EduLevel|EduWhyNo", names(education_data))]]

# ---- Merge farmer and education data at the household-member level
# Merge keys:
#   - Surveyx : survey round 
#   - EaId    : enumeration area
#   - HhId    : household ID
#   - Mid     : member ID
study_data <- dplyr::inner_join(
  farmer_data,
  education_data,
  by = c("Surveyx", "EaId", "HhId", "Mid")
)

# ---- Restrict to relevant survey rounds and drop certain variables
study_data <- study_data[
  study_data$Surveyx %in% c("GLSS6","GLSS7"),
]

# ---- Attach raw data to study environment (potential issue)
study_environment$study_raw_data <- study_data

# ---- Save study environment object
# Save the entire study environment configuration (paths, metadata, etc.)
# for reproducibility and to simplify subsequent scripts.
saveRDS(
  study_environment,
  file.path(study_environment$wd$output, paste0(project_name,"_study_environment.rds"))
)
