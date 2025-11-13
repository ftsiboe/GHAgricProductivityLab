#' Control Settings (Internal)
#'
#' Creates a control list for simulation settings. Used internally.
#'
#' @param number_of_draws Integer(1). Number of draws.
#' @param myseed Numeric(1). Random seed.
#'
#' @return A list containing control parameters.
#'
#' @keywords internal
GHAgricProductivityLab_control <- function(
    number_of_draws = 100,
    myseed = 11122025
){
  
  if (!is.numeric(myseed) || length(myseed) != 1 || !is.finite(myseed) || is.na(myseed)) {
    stop("`myseed` must be a finite numeric(1).", call. = FALSE)
  }
  seed_int <- as.integer(myseed)
  
  if (length(number_of_draws) != 1L || !is.numeric(number_of_draws) || is.na(number_of_draws) || number_of_draws < 0 || number_of_draws != as.integer(number_of_draws)) {
    stop("'number_of_draws' must be a single non-negative integer.")
  }
  
  list(
    number_of_draws = number_of_draws,
    myseed          = seed_int
  )
}

