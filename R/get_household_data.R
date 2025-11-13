#' Download and Load Harmonized Household Data from the GHAgricProductivityLab Repository
#'
#' @description
#' Retrieves a harmonized household- or farm-level dataset from the 
#' **GHAgricProductivityLab** GitHub repository using **piggyback**, stores it 
#' in a package-specific cache directory, and returns the dataset as a 
#' `data.frame`. The file is downloaded only once and reused from the local 
#' cache on future calls.
#'
#' @details
#' The function downloads a Stata `.dta` file associated with the chosen dataset 
#' from the GitHub release labeled `hh_data`. It uses the package-specific 
#' cache directory determined by:
#' 
#' \preformatted{
#' tools::R_user_dir("GHAgricProductivityLab", which = "cache")
#' }
#' 
#' If the file already exists locally, it is not re-downloaded.
#' 
#' **GitHub Authentication**
#'
#' - If `github_token` is supplied, it is used.
#' - Otherwise, the function looks for environment variable `GHProdLab_TOKEN`.
#' - If neither is available, the function falls back to default GitHub 
#'   credentials (e.g., from `gh` CLI or cached credentials).
#'
#' @param dataset Character string.  
#'   Base name of the dataset to retrieve (without file extension).  
#'   Must correspond to a `.dta` file in the `hh_data` GitHub release  
#'   (e.g., `"harmonized_crop_farmer_data"`).
#'
#' @param github_token Optional GitHub personal access token (PAT).  
#'   If `NULL`, the function checks the environment variable `GHProdLab_TOKEN`.  
#'   If that is also missing, the piggyback download will use default 
#'   authentication behavior.
#'
#' @return
#' A `data.frame` containing the requested harmonized dataset.
#' @import piggyback 
#' @export
get_household_data <- function(
    dataset = "harmonized_crop_farmer_data",
    github_token = NULL){
  
  # Handle GitHub token: use supplied token, then env var, then default credentials
  if (is.null(github_token)) {
    github_token <- Sys.getenv("GHProdLab_TOKEN", unset = NA)
    if (is.na(github_token)) {
      github_token <- NULL  # Use cached GitHub credentials
    }
  }
  
  # Define package-specific cache directory
  temporary_dir <- tools::R_user_dir("GHAgricProductivityLab", which = "cache")
  
  if (!dir.exists(temporary_dir)) {
    dir.create(temporary_dir, recursive = TRUE)
  }
  
  file_path <- file.path(temporary_dir, paste0(dataset, ".dta"))
  
  if (!file.exists(file_path)) {
    message(paste0("Downloading ", dataset, " ..."))
    
    piggyback::pb_download(
      file      = paste0(dataset, ".dta"),
      dest      = temporary_dir,
      repo      = "ftsiboe/GHAgricProductivityLab",
      tag       = "hh_data",
      overwrite = TRUE,
      .token    = github_token
    )
  }
  
  data <- as.data.frame(haven::read_dta(file_path))
  
  # Convert specified columns to factors using the haven::as_factor function
  for( vv in c("EduLevel", "Survey", "Region", "Ecozon", "Locality", "Ethnic", "Season", 
               "EduCat", "Head", "Religion", "Marital", "CropID")){
    tryCatch({
      data[,vv] <- haven::as_factor(data[,vv])
    }, error=function(e){})
  }
  
  data
}
