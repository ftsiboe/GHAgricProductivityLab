#' Initialize Study Environment and Directory Structure
#'
#' @param myseed Numeric/integer scalar seed. Will be coerced to integer. Default: 1980632.
#' @param project_name Length-1, non-NA character project name (required).
#' @param local_directories Named list of character paths to create. Defaults use `project_name`.
#' @return List with `wd` (directories) and `seed`.
#' @export
study_setup <- function(
    myseed = 1980632,
    project_name,
    local_directories = list(
      home             = file.path("replications", project_name),
      output           = file.path("replications", project_name, "output"),
      matching         = file.path("replications", project_name, "output", "matching"),
      treatment_effects= file.path("replications", project_name, "output", "treatment_effects"),
      estimations      = file.path("replications", project_name, "output", "estimations"),
      figure_data      = file.path("replications", project_name, "output", "figure_data"),
      figure           = file.path("replications", project_name, "output", "figure")
    )
){
  # project_name validation
  if (missing(project_name) || !is.character(project_name) || length(project_name) != 1 ||
      is.na(project_name) || !nzchar(project_name)) {
    stop("`project_name` must be a non-empty character(1).", call. = FALSE)
  }
  
  # seed validation -> coerce to safe integer
  if(is.null(myseed)){
    myseed <- GHAgricProductivityLab_control()$myseed
  }

  # local_directories validation
  if (!is.list(local_directories) || length(local_directories) == 0L) {
    stop("`local_directories` must be a non-empty list of paths.", call. = FALSE)
  }
  if (is.null(names(local_directories)) || any(names(local_directories) == "")) {
    stop("`local_directories` must be a *named* list.", call. = FALSE)
  }
  if (!all(vapply(local_directories, is.character, logical(1)) &
           vapply(local_directories, function(x) length(x) == 1 && !is.na(x) && nzchar(x), logical(1)))) {
    stop("All `local_directories` entries must be non-empty character(1) paths.", call. = FALSE)
  }
  
  # create directories
  invisible(lapply(local_directories, dir.create, recursive = TRUE, showWarnings = FALSE))
  
  # set seed
  set.seed(myseed)
  
  # return
  list(
    wd   = local_directories,
    myseed = myseed
  )
}

