#' Mode of a Vector (Internal)
#'
#' Computes the most frequent value in a vector. Used internally.
#'
#' @param x Vector.
#' @param na.rm Logical; remove NA values. Default TRUE.
#'
#' @return The modal value.
#'
#' @keywords internal
mode <- function(x, na.rm = TRUE) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Prepare Data for Agricultural Productivity Analysis
#'
#' Cleans and transforms a dataset by creating new variables, applying 
#' log transformations, converting selected variables to factors or 
#' characters, and recoding education levels. The function is designed 
#' to standardize inputs for further analysis of agricultural productivity.
#'
#' @param data A `data.frame` or `data.table` containing household- 
#'   and farm-level variables such as weights, demographic information, 
#'   and agricultural inputs.
#'
#' @return A cleaned and transformed `data.frame` or `data.table` 
#'   with additional variables ready for analysis.
#' @family helpers
#' @importFrom haven as_factor
#' @export
harmonized_data_prep <- function(data){
  
  # Copy the WeightHH column to Weight
  data$Weight <- data$WeightHH
  
  # Log-transform the AgeYr column and store this transformation in a new column called lnAgeYr
  for(vv in c("AgeYr")) {
    data[,paste0("ln",vv)] <- log(data[,vv])
  }
  
  # Log-transform the YerEdu column with a small constant added to avoid log(0) issues, and store in a new column called lnYerEdu
  for(vv in c("YerEdu")) {
    data[,paste0("ln",vv)] <- log(data[,vv] + 0.00001)
  }
  
  # Convert EduLevel to numeric levels for analysis
  tryCatch({
    data$EduLevel <- ifelse(as.character((data$EduLevel)) %in% "None","0",as.character((data$EduLevel)))
    data$EduLevel <- ifelse(data$EduLevel %in% "Primary","1",data$EduLevel)
    data$EduLevel <- ifelse(data$EduLevel %in% "JSS","2",data$EduLevel)
    data$EduLevel <- ifelse(data$EduLevel %in% "SSS","3",data$EduLevel)
    data$EduLevel <- ifelse(data$EduLevel %in% "Post SSS","4",data$EduLevel)
    data$EduLevel <- as.numeric(data$EduLevel)
  }, error=function(e){})
  
  # Convert specified columns to character type
  data$CropID <- as.character(data$CropID)
  data$Ecozon <- as.character(data$Ecozon)
  data$Survey <- as.character(data$Survey)
  data$Region <- as.character(data$Region)
  data$unique_identifier <- 1:nrow(data)
  # Return the cleaned and transformed data
  return(data)
}


#' Extract Matching Crop Area Columns from a Dataset
#'
#' @description
#' Identifies and returns the column names in a dataset corresponding to crop area variables
#' for a specified set of crops. The function looks for column names that start with `"Area_"`
#' and match any of the crops provided in `selected_crops`.
#'
#' @param data A `data.frame` or `data.table` containing crop-related variables.
#'   Column names are expected to include fields prefixed with `"Area_"`, such as `"Area_Maize"`.
#' @param selected_crops A character vector specifying the crop names to filter.
#'   Defaults to a common set of crops including `"Beans"`, `"Cassava"`, `"Cocoa"`, `"Cocoyam"`,
#'   `"Maize"`, `"Millet"`, `"Okra"`, `"Palm"`, `"Peanut"`, `"Pepper"`, `"Plantain"`,
#'   `"Rice"`, `"Sorghum"`, `"Tomatoe"`, and `"Yam"`.
#'
#' @return
#' A character vector containing the names of columns in `data` that correspond
#' to the specified crop area variables (e.g., `"Area_Maize"`, `"Area_Rice"`).
#' Returns an empty vector if no matching columns are found.
#' @family helpers
#' @export
get_crop_area_list <- function(
    data,
    selected_crops = c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                       "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam")){
  crop_area_list <- names(data)[grepl("Area_", names(data))]
  return(crop_area_list[crop_area_list %in% paste0("Area_", selected_crops)])
}


#' Restrict Execution of an R Script to Allowed SLURM Job Conditions
#'
#' @description
#' `run_only_for()` controls when an R script is allowed to run based on:
#' - the SLURM array index (`SLURM_ARRAY_TASK_ID`)
#' - the SLURM job name (`SLURM_JOB_NAME`)
#' - whether the script is being run locally on Windows
#'
#' The function is designed for workflows where multiple R scripts run in a
#' single SLURM array job; each script decides independently whether it should
#' run.
#'
#' @details
#' Execution logic:
#'
#' 1. **Windows override**  
#'    If running on Windows and `run_on_windows = TRUE`, return immediately.
#'
#' 2. **Allowed job names**  
#'    If `allowed_jobnames` is set, the script runs only when SLURM job name is
#'    in that vector.
#'
#' 3. **Array ID restriction**  
#'    - If `id = NULL`, skip array-index filtering.  
#'    - If numeric, match against `SLURM_ARRAY_TASK_ID`.
#'
#' Any violation -> `quit(save = "no")`.
#'
#' @param id Integer or `NULL`. Expected SLURM array index, or `NULL` to skip
#'   checking (useful for Windows/local runs).
#' @param run_on_windows Logical. When `TRUE` (default), do not restrict when
#'   running on Windows.
#' @param allowed_jobnames Character vector of SLURM job names allowed to run
#'   this script. If `NULL`, job-name filtering is skipped.
#'
#' @return Invisibly returns `NULL` when allowed; otherwise exits R.
#'
#' @export
run_only_for <- function(id = NULL,
                         run_on_windows = TRUE,
                         allowed_jobnames = NULL) {
  
  # ---- Windows override ----
  sysname <- Sys.info()[["sysname"]]
  if (is.null(sysname)) sysname <- ""
  sysname <- tolower(sysname)
  
  if (run_on_windows && identical(sysname, "windows")) {
    return(invisible(NULL))
  }
  
  # ---- Check allowed SLURM job names ----
  current_jobname <- Sys.getenv("SLURM_JOB_NAME", unset = NA)
  
  if (!is.null(allowed_jobnames) &&
      !is.na(current_jobname) &&
      !(current_jobname %in% allowed_jobnames)) {
    
    cat(
      "Skipping script. Job name =", current_jobname,
      "\nAllowed job names:", paste(allowed_jobnames, collapse = ", "), "\n"
    )
    quit(save = "no")
  }
  
  # ---- Check SLURM array index (only if id is NOT NULL) ----
  if (!is.null(id)) {
    task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", NA))
    
    if (!is.na(task_id) && task_id != id) {
      cat(
        "Skipping script. Task ID =", task_id,
        "Expected =", id, "\n"
      )
      quit(save = "no")
    }
  }
  
  invisible(NULL)
}


#' Compute Jenks-Binned Shares by Aggregation Groups
#'
#' @description
#' This function bins a numeric variable using Jenks (Fisher) natural breaks
#' and computes the relative share of *counts* and/or *weights* within each
#' aggregation group across the binned ranges.
#'
#' It is useful for summarizing the distribution of a variable (e.g., dealer
#' density, yield, acreage, distances) across spatial or categorical groups.
#'
#' @param dt A `data.table` containing the data.
#' @param output_variable Character name of the numeric column to bin.
#' @param aggregation_points Character vector of grouping variables
#'        (e.g., `"region"`, `"district"`, `"grid_id"`).
#' @param jenks Optional numeric vector of breakpoints.  
#'        If `NULL`, Jenks breaks are computed automatically.
#' @param jenks_number Integer number of Jenks classes to compute
#'        when `jenks` is `NULL`.
#' @param weight_variable Optional character name of a weighting column.
#'        If `NULL`, a weight of `1` is assumed for each row.
#'
#' @return A `data.table` with:
#' \itemize{
#'   \item grouping variables
#'   \item `binned_range_name` - character label of the Jenks bin
#'   \item `binned_range_level` - numeric factor level of the bin
#'   \item `estimate_count` - share of counts within the group
#'   \item `estimate_weight` - share of weights within the group
#' }
#'
#' @details
#' Internally the function:
#' \enumerate{
#'   \item Computes Jenks natural breaks (if not provided)
#'   \item Creates `count` and `weight` totals within each bin by group
#'   \item Merges with total counts/weights per group
#'   \item Computes shares
#' }
#' @family helpers
#' @export
compute_jenks_binned_shares <- function(
    dt,
    output_variable,
    aggregation_points,
    jenks = NULL,
    jenks_number = NULL,
    weight_variable = NULL
) {
  
  dt <- data.table::copy(dt)
  
  # 1. Compute Jenks intervals if needed
  if (is.null(jenks)) {
    jenks <- classInt::classIntervals(
      dt[[output_variable]],
      n     = jenks_number,
      style = "fisher"
    )$brks
  }
  
  # 2. Construct weight variable if missing
  if (is.null(weight_variable)) {
    dt[, weight_tmp := 1]
    weight_variable <- "weight_tmp"
  }
  
  # 3. Bin numeric variable into Jenks categories
  dt[, binned_range_name := cut(get(output_variable), jenks)]
  dt[, count := 1]
  
  # 4. Counts/weights within each group by bin
  cnt_dt <- dt[
    ,
    .(
      count   = sum(count, na.rm = TRUE),
      weights = sum(get(weight_variable), na.rm = TRUE)
    ),
    by = c(aggregation_points, "binned_range_name")
  ]
  
  # 5. Total counts/weights by group
  sum_dt <- dt[
    ,
    .(
      count_sum   = sum(count, na.rm = TRUE),
      weights_sum = sum(get(weight_variable), na.rm = TRUE)
    ),
    by = c(aggregation_points)
  ]
  
  # 6. Merge & compute shares
  res <- merge(cnt_dt, sum_dt, by = aggregation_points, allow.cartesian = TRUE)
  
  res[, estimate_weight := weights / weights_sum]
  res[, estimate_count  := count   / count_sum]
  
  # 7. Add bin labels & clean output
  res[, binned_range_level := as.integer(binned_range_name)]
  res[, binned_range_name  := as.character(binned_range_name)]
  
  data.table::setcolorder(
    res,
    c(
      aggregation_points,
      "binned_range_name",
      "binned_range_level",
      "estimate_count",
      "estimate_weight"
    )
  )
  
  return(res[])
}
