# =============================================================================
#  MULTI-STAGE FRONTIER ESTIMATION WORKFLOW – DISABILITY STUDY
# =============================================================================
#  General Description:
# -----------------------------------------------------------------------------
# This script automates the full multi-stage stochastic frontier (MSF)
# estimation workflow for the “disability” project replication.
#
# It performs the following core tasks:
#
# 1. **Environment & Specification Setup**
#    - Loads the pre-built study environment (.rds) containing directories,
#      harmonized survey microdata, sampling draws, and crop-area lists.
#    - Imports functional and distributional forms for stochastic frontier
#      models (`sf_functional_forms()`).
#    - Constructs the full model specification grid using
#      `sf_model_specifications()`, then filters to the desired
#      disaggregation levels and technology variables.
#    - Supports automated model assignment when running on SLURM array jobs.
#
# 2. **Data Preparation for Each Specification**
#    - Filters the harmonized microdata to the appropriate disaggregation level.
#    - Cleans and encodes the technology variable into numeric categories.
#    - Normalizes crop-area variables, constructs CROP_* indicators, and
#      creates residual “Area_Other” shares.
#    - Sets up intercept shifters and inefficiency shifters for MSF estimation.
#
# 3. **MSF Estimation Across Bootstrap / Matching Draws**
#    - For each unique draw ID in the sample draw list:
#        * Runs `draw_msf_estimations()` to estimate the production frontier,
#          inefficiency effects, and matching-adjusted technology gaps.
#    - Aggregates results across draws via `draw_msf_summary()`, producing
#      efficiency scores, TGR/TE/MTE metrics, and parameter summaries.
#
# 4. **Output Management**
#    - Appends metadata (functional form, distribution, disaggregation level,
#      technology category) to each result object.
#    - Saves each model’s full result set as an `.rds` file named using a
#      standardized pattern:  
#      `<disagg>_<level>_<tech>_<fxn>_<dist>_<matching>.rds`
#    - Skips estimation automatically if an output file already exists.
#
# The script is designed to run seamlessly on:
#    • **Local Windows/Mac/Linux machines** (full specification sweep)  
#    • **HPC clusters via SLURM arrays** (each task runs one specification)
#
# This workflow produces the full library of MSF frontier estimations used in
# the empirical analysis of disability and agricultural productivity.
# =============================================================================
rm(list = ls(all = TRUE)); gc()  

library(dplyr);library(sfaR) ;library(micEcon);library(frontier)
library(rgenoud);library(quadprog);library(car)

devtools::document()  

project_name = "disability"

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
idx <- cli::cli_progress_along(seq_len(nrow(model_specifications)), name = paste0( "Estimating frontier models for ",project_name," study"))

lapply(
  idx,
  function(fit){
    # fit is the row index in model_specifications
    # fit <- 2;matching_type <- "optimal"
    
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
      tryCatch({ 
        
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
          unique(drawlist$ID),
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
        res <- draw_msf_summary(res=res,technology_legend=technology_legend)
        
        # Attach metadata (functional form, distribution, tech labels)
        for(i in 1:length(res)){
          tryCatch({
            res[[i]][,"FXN"]     <- names(fxnforms)[f]
            res[[i]][,"DIS"]     <- names(distforms)[d]
            res[[i]][,"disasg"]  <- disaggregate_variable
            res[[i]][,"level"]   <- disaggregate_level
            res[[i]][,"TCH"]     <- technology_variable
            res[[i]][,"TCHLvel"] <- factor(res[[i]][,"Tech"],levels = c(-999,technology_legend$Tech,999),labels = c("National",technology_legend[,2],"Meta"))
          }, error=function(e){})
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
        
      }, error=function(e){ invisible()})
    }
    invisible()
  })

# Mark progress bar as fully completed
cli::cli_progress_done()

# unlink(list.files(getwd(),pattern =paste0(".out"),full.names = T))



