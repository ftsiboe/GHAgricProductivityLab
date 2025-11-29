# =============================================================================
#  MATCHING WORKFLOW - EDUCATION STUDY
# =============================================================================
#  General Description:
#  ---------------------------------------------------------------------------
#  This workflow executes a multi-stage matching and evaluation pipeline for
#  the project. It performs the following main tasks:
#
#    1. **Project Setup & Environment Creation**:
#         - Initializes a reproducible study environment with consistent
#           directory paths and random seed using `study_setup()`.
#
#    2. **Data Preparation**:
#         - Loads and harmonizes the raw Stata dataset.
#         - Filters for the pooled crop sample.
#         - Defines treatment status 
#         - Builds covariate lists for exact, continuous (scaler), and
#           categorical (factor) matching variables.
#
#    3. **Specification & Sample Drawing**:
#         - Randomly generates 100 matching specifications and sample draw
#           lists for replication.
#         - Saves the resulting specifications and environment metadata.
#
#    4. **Covariate Balance Evaluation**:
#         - When run as a `cov_bal` SLURM job, computes covariate balance
#           statistics using `cobalt::bal.tab()` across all specifications,
#           produces a composite balance rate, and ranks the specifications.
#
#    5. **Outputs**:
#         - Matching results per specification (`matching/*.rds`)
#         - Balance tables, ranked and optimal specifications
#           (`output/match_specification_*.rds`)
# =============================================================================

# --- Session hygiene
rm(list = ls(all = TRUE)); gc()              

devtools::document()                         

run_only_for(id = 3, allowed_jobnames = "run_all")

project_name <- "education"

study_environment <- readRDS(
  file.path(paste0("replications/", project_name, "/output"),
            paste0(project_name,"_study_environment.rds")))

# --- Data ingest & harmonization
DATA <- harmonized_data_prep(study_environment$study_raw_data)           

# Focus analysis sample: pooled crop only; define treatment indicator
DATA$Treat <- as.integer(as.numeric(DATA$educated %in% 1)) # logical treated flag
data <- DATA[as.character(DATA$CropID) %in% "Pooled", ]

# --- Matching variable sets
# Continuous/scalar covariates (plus dynamic crop area columns discovered from data)
crop_area_list         <- get_crop_area_list(data)
match_variables_scaler <- c("AgeYr","HHSizeAE","FmleAERt","Depend","CrpMix", crop_area_list)

# Categorical covariates used as factors in matching distance
match_variables_factor <- c("Credit","OwnLnd","Ethnic","Marital","Religion","Head")

# Exact-match strata (must match identically)
match_variables_exact  <- c("Survey", "Region", "Ecozon", "Locality", "Female")

# --- Complete-case restriction (ensures no NAs in any matching fields)
required_cols <- c("Surveyx", "EaId", "HhId", "Mid", "UID", "Weight", "Treat",
                   match_variables_scaler, match_variables_exact, match_variables_factor)
data <- data[complete.cases(data[required_cols]),]

# Quick sanity summary of covariates used in matching
summary(data[c(match_variables_scaler, match_variables_exact, match_variables_factor)])

# --- Draw matching specifications & sample indices
m.specs <- match_sample_specifications(data = data, myseed = study_environment$myseed)
# Expected structure:
#   m.specs$m.specs  : data.frame of matching “recipes” (ARRAY ids, method, distance, link, boot, etc.)
#   m.specs$drawlist : index sets for resampling / bootstraps

# Local convenience bindings
match_specifications <- m.specs$m.specs #[1:8,]
sample_draw_list     <- as.data.frame(m.specs$drawlist) #[1:3,]

# Persist key objects in the study_environment container
study_environment[["match_specifications"]]   <- match_specifications
study_environment[["sample_draw_list"]]       <- sample_draw_list
study_environment[["crop_area_list"]]         <- crop_area_list
study_environment[["match_variables_exact"]]  <- match_variables_exact
study_environment[["match_variables_factor"]] <- match_variables_factor
study_environment[["match_variables_scaler"]] <- match_variables_scaler
study_environment[["estimation_data"]]        <- DATA

# --- Matching stage 
idx <- cli::cli_progress_along(seq_len(nrow(match_specifications)), name = paste0( "Drawing matched samples for ",project_name," study"))

lapply(
  idx,
  function(i, data) {
    tryCatch({
      # Produce matched sample & (optionally) matching object for spec i
      sampels <- draw_matched_samples(
        i,
        data,
        match_variables_exact,
        match_variables_scaler,
        match_variables_factor,
        match_specifications,
        sample_draw_list
      )
      
      # For bootstrap specs (boot != 0), drop the heavy m.out object to save space
      if (!match_specifications$boot[i] %in% 0) { sampels[["m.out"]] <- NULL }
      
      # Persist result: one RDS per ARRAY (zero-padded)
      saveRDS(
        sampels,
        file.path(
          study_environment$wd$matching,
          paste0("match_",stringr::str_pad(match_specifications$ARRAY[i], 4, pad = "0"), ".rds")
        )
      )
    }, error = function(e) {})
    return(i)
  },
  data = data
)

cli::cli_progress_done()

# --- Covariate balance stage 
res <- covariate_balance(
  matching_output_directory = study_environment$wd$matching,
  match_specifications      = study_environment$match_specifications
)

# Save: full ranking, top spec, and detailed long-format balance table
study_environment[["match_specification_ranking"]] <- res$rate
study_environment[["match_specification_optimal"]] <- res$rate[nrow(res$rate),]
study_environment[["balance_table"]]               <- res$bal_tab

# Save environment snapshot for downstream stages
saveRDS(
  study_environment,
  file.path(study_environment$wd$output, paste0(project_name,"_study_environment.rds"))
)

