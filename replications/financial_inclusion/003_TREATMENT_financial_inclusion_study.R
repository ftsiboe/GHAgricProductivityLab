# =============================================================================
#  TREATMENT EFFECT WORKFLOW - FINANCIAL INCLUSION STUDY 
# =============================================================================
#  General Description:
# -----------------------------------------------------------------------------
# This script automates the estimation and summarization of
# treatment effects for the project replication.
# 
# It performs two main tasks depending on the runtime context:
# 1. **Treatment Effect Estimation **:
#    - Loads harmonized survey data and matching specifications
#    - Iterates through all (or assigned) matching specifications
#    - Computes log-linear treatment effects (ATE, ATET, ATEU)
#      using `treatment_effect_calculation()`
#    - Saves one RDS result per specification (e.g., "te_0001.rds")
#
# 2. **Treatment Effect Summary (te_sum)**:
#    - Re-reads the environment and treatment effect outputs
#    - Aggregates all `.rds` results using
#      `treatment_effect_summary()`
#    - Produces a summarized table ("te_summary.rds")
# =============================================================================

rm(list = ls(all = TRUE)); gc()  

devtools::document()  

run_only_for(id = 4, allowed_jobnames = "run_all")

project_name = "financial_inclusion"

# Detect operating system to determine runtime environment
sysname <- toupper(as.character(Sys.info()[["sysname"]]))

# Load saved study environment (directories, specifications, etc.)
study_environment <- readRDS(
  file.path(paste0("replications/", project_name, "/output"),
            paste0(project_name,"_study_environment.rds")))

# --- Data ingest & harmonization
# Load harmonized survey data stored in the study environment
data <- study_environment[["estimation_data"]]

# Focus only on “Pooled” CropID entries for cross-crop analysis
data <- data[as.character(data$CropID) %in% "Pooled", ]

# Set key directories and specifications for downstream routines
matching_output_directory <- study_environment$wd$matching
match_specifications      <- study_environment$match_specifications

# Build formula objects for matching (exact, scalar, and factor covariates)
match_formulas <- write_match_formulas(
  match_variables_exact  = study_environment$match_variables_exact,
  match_variables_scaler = study_environment$match_variables_scaler,
  match_variables_factor = study_environment$match_variables_factor
)

# --- Treatment Effect Estimation
# Create progress bar for visual feedback
idx <- cli::cli_progress_along(seq_len(nrow(match_specifications)), name = paste0( "Computing log-linear treatment effects for ",project_name," study"))
lapply(idx, function(i) {
  # Compute treatment effects using the pre-matched samples
  res <- treatment_effect_calculation(
    data                      = data,
    outcome_variables         = c("Area", "HrvstKg", "SeedKg","HHLaborAE","HirdHr","FertKg", "PestLt"),
    normalize                 = TRUE,
    i                         = i,
    matching_output_directory = matching_output_directory,
    match_specifications      = match_specifications,
    match_formulas            = match_formulas
  )
  # Save results for each ARRAY index as an individual .rds file
  saveRDS(
    res,
    file.path(
      study_environment$wd$treatment_effects,
      paste0("te_", stringr::str_pad(match_specifications$ARRAY[i], 4, pad = "0"), ".rds")
    )
  )
  invisible()
})

cli::cli_progress_done()
res <- treatment_effect_summary(study_environment$wd$treatment_effects)
saveRDS(res, file = file.path(study_environment$wd$output, "te_summary.rds"))