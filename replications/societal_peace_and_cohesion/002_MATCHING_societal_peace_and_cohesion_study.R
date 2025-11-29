# =============================================================================
#  MATCHING WORKFLOW - SOCIETAL PEACE AND COHESION STUDY
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

project_name <- "societal_peace_and_cohesion"

study_environment <- readRDS(
  file.path(paste0("replications/", project_name, "/output"),
            paste0(project_name,"_study_environment.rds")))

# --- Data ingest & harmonization
DATA <- harmonized_data_prep(study_environment$study_raw_data)           

# Focus analysis sample: pooled crop only; define treatment indicator
data <- DATA[as.character(DATA$CropID) %in% "Pooled", ]

# --- Matching variable sets
# Continuous/scalar covariates (plus dynamic crop area columns discovered from data)
crop_area_list         <- get_crop_area_list(data)

# --- Draw matching specifications & sample indices
m.specs <- match_sample_specifications(data = data, myseed = study_environment$myseed)
# Expected structure:
#   m.specs$m.specs  : data.frame of matching “recipes” (ARRAY ids, method, distance, link, boot, etc.)
#   m.specs$drawlist : index sets for resampling / bootstraps

# Local convenience bindings
match_specifications <- m.specs$m.specs[1:8,]
sample_draw_list     <- as.data.frame(m.specs$drawlist)[1:3,]

# Persist key objects in the study_environment container
study_environment[["sample_draw_list"]]       <- sample_draw_list
study_environment[["crop_area_list"]]         <- crop_area_list
study_environment[["estimation_data"]]        <- DATA

saveRDS(
  study_environment,
  file.path(study_environment$wd$output, paste0(project_name,"_study_environment.rds"))
)

