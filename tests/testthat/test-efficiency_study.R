test_that("efficiency study has no issues", {

  library(dplyr);library(sfaR) ;library(micEcon);library(frontier)
  library(purrr);library(data.table);library(MatchIt);library(randomForest);library(CBPS);library(dbarts)
  library(optmatch);library(Matching);library(rgenoud);library(quadprog);library(car)
  
  # =============================================================================
  #  DATA and SETUP - TEST
  # =============================================================================
  project_name <- "test"
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
  sample_draw_list     <- as.data.frame(m.specs$drawlist)[1:3,]

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
  #if(grepl("WINDOWS",sysname) || Sys.getenv("SLURM_JOB_NAME") %in% c("match_all", "match_disa")) {

    idx <- cli::cli_progress_along(seq_len(nrow(match_specifications)), name = paste0( "Drawing matched samples for ",project_name," study"))

    lapply(
      idx,
      function(i, data) {
        #tryCatch({
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
          #}, error = function(e) {})
        return(i)
      },
      data = data
    )

    cli::cli_progress_done()
    #}

  # --- Covariate balance stage
  # Executed if running locally (Windows) or on SLURM jobs named cov_bal
    #if(grepl("WINDOWS",sysname) || Sys.getenv("SLURM_JOB_NAME") %in% c("cov_bal")) {

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
    #}

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
  #if (grepl("WINDOWS", sysname) || Sys.getenv("SLURM_JOB_NAME") %in% c("te_all", "te_disa")) {

    # Restrict to a single specification if SLURM_ARRAY_TASK_ID is set
    if (!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))) {
      match_specifications <- match_specifications[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")), ]
    }

    # Create progress bar for visual feedback
    idx <- cli::cli_progress_along(seq_len(nrow(match_specifications)), name = paste0( "Computing log-linear treatment effects for ",project_name," study"))

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

    cli::cli_progress_done()
    #}
  
  # --- Block 2: Treatment Effect Summary
  # Executed if running locally (Windows) or on SLURM jobs named “te_sum”
  #if (grepl("WINDOWS", sysname) || Sys.getenv("SLURM_JOB_NAME") %in% c("te_sum")){

    # Reload environment to ensure clean references (paths, specs, etc.)
    project_name <- "test"
    study_environment <- readRDS(
      file.path(paste0("replications/", project_name, "/output"),
                paste0(project_name,"_study_environment.rds")))

    # Summarize all treatment effect estimates across specifications
    res <- treatment_effect_summary(study_environment$wd$treatment_effects)

    # Save the combined summary table
    saveRDS(res, file = file.path(study_environment$wd$output, "te_summary.rds"))
    #}

  expect_true(
    all(names(study_environment)
                  %in% c("wd","myseed","study_raw_data","match_specifications","sample_draw_list","crop_area_list",
                         "match_variables_exact","match_variables_factor","match_variables_scaler",
                         "match_specification_ranking","match_specification_optimal","balance_table","estimation_data")))
  expect_true(all(list.files(study_environment$wd$output) %in% c("estimations","figure","figure_data","matching",
                                                                 "test_study_environment.rds","te_summary.rds","treatment_effects")))
  expect_true(all(list.files(study_environment$wd$treatment_effects) %in% paste0("te_",stringr::str_pad(1:8, 4, pad = "0"), ".rds")))

  obj <- readRDS(
    file.path(paste0("replications/", project_name, "/output"),
              paste0(project_name,"_study_environment.rds")))
  
  expect_true(nrow(obj$match_specification_ranking) >= 8)
  expect_true(nrow(obj$balance_table) >= 8)
  
  # =============================================================================
  #  MULTI-STAGE FRONTIER ESTIMATION WORKFLOW – DISABILITY STUDY
  # =============================================================================

  rm(list = ls(all = TRUE)); gc()

  project_name = "test"

  # Detect operating system to determine runtime environment
  sysname <- toupper(as.character(Sys.info()[["sysname"]]))

  # Load saved study environment (directories, specifications, etc.)
  study_environment <- readRDS(
    file.path(paste0("replications/", project_name, "/output"),
              paste0(project_name,"_study_environment.rds")))

  # Data ingest & basic harmonization
  estimation_data <- study_environment[["estimation_data"]]
  estimation_data$EduCat <- as.character(estimation_data$EduCat)
  distforms   <- sf_functional_forms()$distforms
  fxnforms    <- sf_functional_forms()$fxnforms

  # Build table of model specifications for multi–stage frontier estimation
  model_specifications <- sf_model_specifications(
    distforms = distforms,
    fxnforms = fxnforms,
    data = study_environment$estimation_data,
    technology_variables = c("disabled","disabled_self","disabled_spouse","disabled_child","disabled_close","disabled_member"))

  # Drop specifications that use disaggregation variables you do NOT want
  model_specifications <- model_specifications[!model_specifications$disasg %in% c( "Female","Region","Ecozon","EduCat","EduLevel","AgeCat"),]
  model_specifications <- model_specifications[model_specifications$level %in% c( "Pooled"),]

  row.names(model_specifications) <- 1:nrow(model_specifications)

  # If running on a cluster with SLURM array jobs:
  # pick a single row of model_specifications based on SLURM_ARRAY_TASK_ID
  if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
    model_specifications <- model_specifications[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
  }

  # Create progress bar for visual feedback
  idx <- 1

  lapply(
    idx,
    function(fit){
      # fit is the row index in model_specifications
      fit <- 2;matching_type <- "optimal"

      # Pull out this specification’s functional form, distribution, etc.
      f <- model_specifications$f[fit]
      d <- model_specifications$d[fit]
      disaggregate_variable <- model_specifications$disasg[fit]
      disaggregate_level    <- model_specifications$level[fit]
      technology_variable   <- model_specifications$TechVar[fit]
      matching_type         <- model_specifications$matching_type[fit]

      # Construct a unique name for this estimation scenario
      est_name <- paste0(
        disaggregate_variable,"_",disaggregate_level,
        "_",technology_variable,"_",names(fxnforms)[f],"_",
        names(distforms)[d],"_",matching_type)

      # Output path for saving results
      out_path <- file.path(
        study_environment$wd$estimations,
        paste0(est_name,".rds"))

      # Only run estimation if this result does NOT already exist
      if(!file.exists(out_path)){
        #tryCatch({

          # Data Preparation for this specification
          data <- estimation_data[estimation_data[,model_specifications$disasg[fit]] %in% model_specifications$level[fit],]

          # Drop observations with missing technology variable
          data <- data[!data[,model_specifications$TechVar[fit]] %in% NA,]

          # Create numeric technology index 'Tech' based on the technology variable
          data$Tech <- as.numeric(as.integer(as.factor(as.character(data[,model_specifications$TechVar[fit]]))))

          # If we are not disaggregating by CropID, restrict to pooled CropID
          if(!model_specifications$disasg[fit] %in% "CropID") data <- data[data[,"CropID"] %in% "Pooled",]

          # Legend for technology categories (Tech code vs original label)
          technology_legend <- unique(data[c("Tech",model_specifications$TechVar[fit])])
          technology_legend <- technology_legend[order(technology_legend$Tech),]

          # List of crop-area variables to normalize by total area
          crop_area_list <- study_environment$crop_area_list

          # Convert each crop area to a share of total area
          for(crop in crop_area_list){data[,crop] <- data[,crop]/data[,"Area"]}

          # Keep only crops with average area share > 3%
          crop_area_list <- apply(data[names(data)[names(data) %in% crop_area_list]],2,mean) > 0.03
          crop_area_list <- names(crop_area_list)[crop_area_list %in% TRUE]

          # Construct indicator variables for crops (CROP_*)
          for(crop in gsub("Area_","",crop_area_list)){data[,paste0("CROP_",crop)] <- ifelse(data[,paste0("Area_",crop)] > 0, crop,NA)}

          # Add residual area category 'Area_Other'
          crop_area_list <- unique(c(crop_area_list,"Area_Other"))
          if(length(crop_area_list)>0){
            data$Area_Other <- 1 - rowSums(data[crop_area_list[!crop_area_list %in% "Area_Maize"]],na.rm=T)
            crop_area_list  <- unique(c(crop_area_list,"Area_Other"))
          }

          # Draw-based estimation setup
          # Pre-computed sample draws for matching / bootstrapping
          drawlist = study_environment$sample_draw_list

          # By default, no disaggregated scores list
          disagscors_list <- NULL

          # For one specific core scenario, compute disaggregated scores
          if(technology_variable %in% "disabled" &
             matching_type %in% "optimal" &
             disaggregate_level %in% "Pooled" &
             disaggregate_variable %in% "CropID" &
             f %in% 2 & d %in% 1){
            disagscors_list <- c("Ecozon","Region","AgeCat","EduLevel","Female","disability",names(data)[grepl("CROP_",names(data))])
          }

          # Multi-stage frontier estimation over sample draws
          res <- lapply(
            unique(drawlist$ID)[1],
            draw_msf_estimations,
            data                    = data,
            surveyy                 = FALSE,
            intercept_shifters      = list(scalar_variables=crop_area_list,factor_variables=c("Survey","Ecozon")),
            intercept_shifters_meta = list(scalar_variables=crop_area_list,factor_variables=c("Survey","Ecozon")),
            drawlist                = drawlist,
            weight_variable         = "Weight",
            output_variable         = "HrvstKg",
            input_variables         = c("Area", "SeedKg", "HHLaborAE","HirdHr","FertKg","PestLt"),
            inefficiency_covariates = list(scalar_variables=c("lnAgeYr","lnYerEdu","CrpMix"),factor_variables=c("Female","Survey","Ecozon","Extension","Credit","EqipMech","OwnLnd")),
            adoption_covariates     = list(scalar_variables=c("lnAgeYr","lnYerEdu","CrpMix"),factor_variables=c("Female","Survey","Ecozon","Extension","Credit","EqipMech","OwnLnd")),
            identifiers             = c("unique_identifier", "Survey", "CropID", "HhId", "EaId", "Mid"),
            disagscors_list         = disagscors_list,
            f                       = f,
            d                       = d,
            technology_variable     = technology_variable,
            matching_type           = matching_type)

          # Summarize results across draws (means, stats, etc.)
          res <- list(res[[1]],res[[2]],res[[2]],res[[2]],res[[2]])
          res <- draw_msf_summary(res=res,technology_legend=technology_legend)

          # Attach metadata (functional form, distribution, tech labels)
          for(i in 1:length(res)){
            # tryCatch({
              res[[i]][,"FXN"]     <- names(fxnforms)[f]
              res[[i]][,"DIS"]     <- names(distforms)[d]
              res[[i]][,"disasg"]  <- disaggregate_variable
              res[[i]][,"level"]   <- disaggregate_level
              res[[i]][,"TCH"]     <- technology_variable
              res[[i]][,"TCHLvel"] <- factor(res[[i]][,"Tech"],levels = c(-999,technology_legend$Tech,999),labels = c("National",technology_legend[,2],"Meta"))
              # }, error=function(e){})
          }

          # (Optional) Quick extraction of key efficiency results (not saved)
          function(){
            Main <- res$ef_mean
            Main <- Main[Main$Survey %in% "GLSS0",]
            Main <- Main[!Main$sample %in% "unmatched",]
            Main <- Main[Main$stat %in% "wmean",]
            Main <- Main[Main$CoefName %in% "efficiencyGap_lvl",]
            Main <- Main[Main$restrict %in% "Restricted",]
            Main <- Main[Main$estType %in% "teBC",]
            Main[Main$type %in% "TGR",c("sample","type","Tech","Estimate")]
            Main[Main$type %in% "TE",c("sample","type","Tech","Estimate")]
            Main[Main$type %in% "MTE",c("sample","type","Tech","Estimate")]
          }

          # Add the estimation name to the result list
          res[["names"]] <- est_name

          # Save full results for this model specification
          saveRDS(res,file=out_path)

          # }, error=function(e){ invisible()})
      }
      invisible()
    })

  expect_true(all(list.files(study_environment$wd$estimations) %in% "CropID_Pooled_disabled_TL_hnormal_optimal.rds"))
  res <- readRDS(file.path(study_environment$wd$estimations,"CropID_Pooled_disabled_TL_hnormal_optimal.rds"))
  expect_true(all(names(res) %in% c("sf_estm","el_mean","ef_mean","rk_mean","ef_dist","rk_dist","disagscors","el_samp","ef_samp","rk_samp","names")))
})
