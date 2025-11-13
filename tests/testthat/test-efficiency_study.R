test_that("efficiency study has no issues", {
  
  # =============================================================================
  #  DATA and SETUP - TEST 
  # =============================================================================
  project_name <- "test"
  study_environment <- study_setup(project_name = project_name)
 
  # ---- Load harmonized household / farmer-level data --------------------------
  farmer_data     <- get_household_data("harmonized_crop_farmer_data")
  disability_data <- get_household_data("harmonized_disability_data")
  
  # ---- Merge farmer and disability data at the household-member level ---------
  study_data <- dplyr::inner_join(
    farmer_data,
    disability_data,
    by = c("Surveyx", "EaId", "HhId", "Mid")
  )
  
  # ---- Restrict to GLSS6 and GLSS7 and drop certain variables -----------------
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
  vars <- c("disabled_self", "disabled_spouse", "disabled_child",
            "disabled_close", "disabled_member")
  
  for (v in vars) {
    study_data[[v]][study_data[[v]] == 0 & study_data$disabled == 1] <- NA
  }
  # ---- Attach raw data to study environment (potential issue) ------------------
  study_environment$study_raw_data <- study_data
  
  # ---- Save study environment object ------------------------------------------
  saveRDS(
    study_environment,
    file.path(study_environment$wd$output, paste0(project_name,"_study_environment.rds"))
  )
  
  expect_true(all(names(study_environment) %in% c("wd", "myseed","study_raw_data")))
  expect_true(all(list.files(study_environment$wd$output) %in% c("estimations","figure","figure_data","matching","test_study_environment.rds","treatment_effects")))
  
  # =============================================================================
  #  MATCHING WORKFLOW - TEST 
  # =============================================================================
  rm(list = ls(all = TRUE)); gc()    
  
  project_name <- "test"
  
  study_environment <- readRDS(
    file.path(paste0("replications/", project_name, "/output"),
              paste0(project_name,"_study_environment.rds")))
  
  # Detect operating system to determine runtime environment
  sysname <- toupper(as.character(Sys.info()[["sysname"]]))
  
  # --- Data ingest & harmonization
  DATA <- harmonized_data_prep(study_environment$study_raw_data)           
  
  # Focus analysis sample: pooled crop only; define treatment indicator
  DATA$Treat <- as.integer(as.numeric(DATA$disabled %in% 1)) # logical treated flag
  data <- DATA[as.character(DATA$CropID) %in% "Pooled", ]
  
  # --- Matching variable sets
  # Continuous/scalar covariates (plus dynamic crop area columns discovered from data)
  crop_area_list         <- get_crop_area_list(data)
  match_variables_scaler <- c("AgeYr", "YerEdu", "HHSizeAE", "FmleAERt", "Depend", "CrpMix", crop_area_list)
  
  # Categorical covariates used as factors in matching distance
  match_variables_factor <- c("Credit", "OwnLnd", "Ethnic", "Marital", "Religion", "Head")
  
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
  match_specifications <- m.specs$m.specs[1:8,]
  sample_draw_list     <- as.data.frame(m.specs$drawlist)
  
  # Persist key objects in the study_environment container
  study_environment[["match_specifications"]]   <- match_specifications
  study_environment[["sample_draw_list"]]       <- sample_draw_list
  study_environment[["crop_area_list"]]         <- crop_area_list
  study_environment[["match_variables_exact"]]  <- match_variables_exact
  study_environment[["match_variables_factor"]] <- match_variables_factor
  study_environment[["match_variables_scaler"]] <- match_variables_scaler
  study_environment[["estimation_data"]]        <- DATA
  
  # Save environment snapshot for downstream stages
  saveRDS(
    study_environment,
    file.path(study_environment$wd$output, paste0(project_name,"_study_environment.rds"))
  )
  
  # --- SLURM array subsetting 
  # Executed if running locally (Windows) or launched as an array job, run only the row indexed by SLURM_ARRAY_TASK_ID
  if (!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))) {
    match_specifications <- match_specifications[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")), ]
  }
  
  # --- Matching stage 
  # Executed if running locally (Windows) or on SLURM jobs named match_all or match_disa
  if(grepl("WINDOWS",sysname) || Sys.getenv("SLURM_JOB_NAME") %in% c("match_all", "match_disa")) {
    idx <- cli::cli_progress_along(seq_len(nrow(match_specifications)), name = "Drawing matched samples")
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
  }
  
  # --- Covariate balance stage 
  # Executed if running locally (Windows) or on SLURM jobs named cov_bal
  if(grepl("WINDOWS",sysname) || Sys.getenv("SLURM_JOB_NAME") %in% c("cov_bal")) {
    
    # Reload environment (paths/specs) to ensure clean context
    project_name <- "test"
    
    study_environment <- readRDS(
      file.path(paste0("replications/", project_name, "/output"),
                paste0(project_name,"_study_environment.rds")))
    
    # Compute balance tables and spec-level composite balance “rate”
    res <- covariate_balance(
      matching_output_directory = study_environment$wd$matching,
      match_specifications      = study_environment$match_specifications
    )
    
    # Save: full ranking, top spec, and detailed long-format balance table
    
    study_environment[["match_specification_ranking"]] <- res$rate
    study_environment[["match_specification_optimal"]] <- res$rate[nrow(res$rate),]
    study_environment[["balance_table"]]               <- res$bal_tab
    
    saveRDS(
      study_environment,
      file.path(study_environment$wd$output, paste0(project_name,"_study_environment.rds"))
    )
  }
  
  expect_true(
    all(names(study_environment)
                  %in% c("wd","myseed","study_raw_data","match_specifications","sample_draw_list","crop_area_list",
                         "match_variables_exact","match_variables_factor","match_variables_scaler",
                         "match_specification_ranking","match_specification_optimal","balance_table","estimation_data")))
  expect_true(all(list.files(study_environment$wd$output) %in% c("estimations","figure","figure_data","matching","test_study_environment.rds","treatment_effects")))
  expect_true(all(list.files(study_environment$wd$matching) %in% paste0("match_",stringr::str_pad(1:8, 4, pad = "0"), ".rds")))
  
  # =============================================================================
  #  TREATMENT EFFECT WORKFLOW - TEST
  # =============================================================================
  
  rm(list = ls(all = TRUE)); gc()  

  project_name = "test"
  
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
  
  # --- Block 1: Treatment Effect Estimation
  # Executed if running locally (Windows) or on SLURM jobs named “te_all” or “te_disa”
  if (grepl("WINDOWS", sysname) || Sys.getenv("SLURM_JOB_NAME") %in% c("te_all", "te_disa")) {
    
    # Restrict to a single specification if SLURM_ARRAY_TASK_ID is set
    if (!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))) {
      match_specifications <- match_specifications[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")), ]
    }
    
    # Create progress bar for visual feedback
    idx <- cli::cli_progress_along(seq_len(nrow(match_specifications)),
                                   name = "Computing log-linear treatment effects")
    
    # Iterate through each matching specification and compute treatment effects
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
  }
  
  # --- Block 2: Treatment Effect Summary
  # Executed if running locally (Windows) or on SLURM jobs named “te_sum”
  if (grepl("WINDOWS", sysname) || Sys.getenv("SLURM_JOB_NAME") %in% c("te_sum")) {
    
    # Reload environment to ensure clean references (paths, specs, etc.)
    project_name <- "test"
    study_environment <- readRDS(
      file.path(paste0("replications/", project_name, "/output"),
                paste0(project_name,"_study_environment.rds")))
    
    # Summarize all treatment effect estimates across specifications
    res <- treatment_effect_summary(study_environment$wd$treatment_effects)
    
    # Save the combined summary table
    saveRDS(res, file = file.path(study_environment$wd$output, "te_summary.rds"))
  }
  
  expect_true(
    all(names(study_environment)
                  %in% c("wd","myseed","study_raw_data","match_specifications","sample_draw_list","crop_area_list",
                         "match_variables_exact","match_variables_factor","match_variables_scaler",
                         "match_specification_ranking","match_specification_optimal","balance_table","estimation_data")))
  expect_true(all(list.files(study_environment$wd$output) %in% c("estimations","figure","figure_data","matching",
                                                                 "test_study_environment.rds","te_summary.rds","treatment_effects")))
  expect_true(all(list.files(study_environment$wd$treatment_effects) %in% paste0("te_",stringr::str_pad(1:8, 4, pad = "0"), ".rds")))
  
})
