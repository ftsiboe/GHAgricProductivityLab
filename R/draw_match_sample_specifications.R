#' Draw / Match Sample Specifications
#'
#' Generates (i) a draw list by sampling \code{EaId} within each unique
#' \code{Survey} group and (ii) a grid of matching specifications for each
#' bootstrap draw.
#'
#' @param drawN Integer. The number of draws to perform per \code{Survey}.
#' @param data A data.frame or data.table containing at least the columns
#'   \code{Survey} and \code{EaId}.
#' @param myseed Integer. Seed for random number generation (default \code{03242025}).
#'
#' @details
#' \strong{Draw list:}
#' For each unique value of \code{Survey}, the function samples \code{drawN}
#' \code{EaId} values with replacement and prepends a 0 row (ID = 0) for the
#' baseline. The result is then spread to wide format with one column per
#' \code{Survey}.
#'
#' \strong{Matching specs:}
#' For each draw \code{ID}, creates a set of matching model specifications
#' that include:
#' \itemize{
#'   \item Nearest neighbor with distances:
#'         \code{"euclidean"}, \code{"scaled_euclidean"},
#'         \code{"mahalanobis"}, \code{"robust_mahalanobis"}.
#'   \item Nearest neighbor with distance \code{"glm"} and links:
#'         \code{"logit"}, \code{"probit"}, \code{"cloglog"}, \code{"cauchit"}.
#' }
#' An \code{ARRAY} index is added for convenience.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{m.specs}}{A data.frame of matching specifications with columns
#'         \code{boot}, \code{method}, \code{distance}, \code{link}, \code{ARRAY}.}
#'   \item{\code{drawlist}}{A data.frame in wide format where each \code{Survey}
#'         is a column and rows correspond to draw \code{ID} (0:\code{drawN}).}
#' }
#'
#' @note Requires that \code{data} contain \code{Survey} and \code{EaId}.
#' \code{tidyr::spread()} is used for wide reshaping (consider
#' \code{tidyr::pivot_wider()} in new code).
#'
#' @importFrom data.table rbindlist
#' @export
draw_match_sample_specifications <- function(
    drawN, data, myseed = 03242025) {
  
  # ---- basic validation ----
  if (!is.data.frame(data)) stop("'data' must be a data.frame or data.table.")
  if (!all(c("Survey","EaId") %in% names(data))) {
    stop("`data` must contain columns 'Survey' and 'EaId'.")
  }
  if (length(drawN) != 1L || !is.numeric(drawN) || is.na(drawN) || drawN < 0 || drawN != as.integer(drawN)) {
    stop("'drawN' must be a single non-negative integer.")
  }
  if (length(myseed) != 1L || !is.numeric(myseed) || is.na(myseed)) {
    stop("'myseed' must be a single numeric seed.")
  }
  
  # Generate a draw list by sampling EaId values within each unique Survey group.
  # (Seed set once for reproducibility across all groups.)
  set.seed(as.integer(myseed))
  
  surveys <- as.character(unique(data$Survey))
  drawlist_long <- as.data.frame(
    data.table::rbindlist(
      lapply(
        surveys,
        function(glss) {
          pool <- data$EaId[data$Survey == glss]
          pool <- pool[!is.na(pool)]
          # if group empty, fill with NA; else sample with replacement
          sampled <- if (length(pool) == 0L) rep(NA, drawN) else sample(pool, size = drawN, replace = TRUE)
          data.frame(
            glss = glss,
            ID   = 0:drawN,
            EaId = c(NA, sampled),             # prepend baseline row with NA (not 0) for EaId
            stringsAsFactors = FALSE
          )
        }), 
      fill = TRUE
    )
  )
  
  # Spread the draw list to create a wide format with each Survey as a column.
  drawlist <- tidyr::pivot_wider(drawlist_long, names_from = glss, values_from = EaId)
  
  # Generate matching specifications for each unique draw ID.
  # (Removed tryCatch that could hide errors.)
  m.specs <- as.data.frame(
    data.table::rbindlist(
      lapply(
        unique(drawlist$ID),
        function(w) {
          data.frame(
            boot = w,
            rbind(
              # Nearest Neighbor Matching
              data.frame(method = "nearest",
                         distance = c("euclidean", "scaled_euclidean", "mahalanobis", "robust_mahalanobis"),
                         link = NA_character_,
                         stringsAsFactors = FALSE),
              data.frame(
                method = "nearest",
                distance = "glm",
                link = c("logit", "probit", "cloglog", "cauchit"),
                stringsAsFactors = FALSE
              )
            ),
            stringsAsFactors = FALSE
          )
        }), 
      fill = TRUE
    )
  )
  
  # Add an ARRAY column to the matching specifications dataframe.
  m.specs$ARRAY <- seq_len(nrow(m.specs))
  
  # Return a list containing the matching specifications and the draw list.
  return(list(m.specs = m.specs, drawlist = drawlist))
}

