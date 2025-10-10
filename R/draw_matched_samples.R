#' Draw matched samples (stratified bootstrap + matching)
#'
#' Performs stratified resampling at the \code{Surveyx, EaId} level (excluding EaIds
#' listed for the current bootstrap \code{ID}) and computes adjusted sampling weights
#' used for matching. Then fits a matching model per \code{m.specs[i, ]} using
#' \pkg{MatchIt}, with exact matching on \code{Emch} and distance/link from \code{m.specs}.
#'
#' @param data A data.frame/data.table containing at least:
#'   \code{Surveyx, EaId, HhId, Mid, UID, Weight, Treat}, plus columns named in \code{Emch, Scle, Fixd}.
#' @param Emch Character vector of covariate names used for exact matching.
#' @param Scle Character vector of covariate names used in the distance model (scaling).
#' @param Fixd Character vector of additional fixed covariates in the distance model.
#' @param m.specs Data frame of matching specifications with columns
#'   \code{boot}, \code{method}, \code{distance}, and optionally \code{link}.
#' @param i Integer index selecting the row of \code{m.specs} to use.
#' @param drawlist Data frame where column \code{ID} identifies the bootstrap draw,
#'   and each remaining column corresponds to a \code{Survey} containing the sampled \code{EaId}.
#' @param verbose Logical; if \code{TRUE}, prints the chosen matching method and timing. Default \code{FALSE}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{m.specs}}{The selected row from \code{m.specs}.}
#'   \item{\code{m.out}}{The \code{matchit} object.}
#'   \item{\code{md}}{Matched data: \code{Surveyx, EaId, HhId, Mid, UID, weights, pWeight}.}
#'   \item{\code{df}}{The analysis data with adjusted weights: \code{Surveyx, EaId, HhId, Mid, UID, pWeight}.}
#' }
#'
#' @details
#' Adjusted weights are computed as \eqn{pWeight = Weight \times (alloc/allocj)},
#' where \code{alloc} is the pre-exclusion sum of \code{Weight} by \code{Surveyx, EaId}
#' and \code{allocj} is the post-exclusion sum.
#'
#' Exact matching is performed on variables in \code{Emch}. The distance model
#' formula is constructed as \code{Treat ~ Scle + Fixd}. When \code{distance == "glm"},
#' \code{m.order = "largest"} is used; otherwise \code{"closest"}.
#'
#' @import MatchIt
#' @export
draw_matched_samples <- function(data, Emch, Scle, Fixd, m.specs, i, drawlist, verbose = FALSE) {
  
  # helper: `%||%` returns y when x is NULL, otherwise x
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # ---- validation ----
  must_have <- c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat")
  if (!is.data.frame(data)) stop("'data' must be a data.frame or data.table.")
  if (!all(must_have %in% names(data))) {
    stop("`data` must contain columns: ", paste(must_have, collapse = ", "))
  }
  if (!is.data.frame(m.specs) || nrow(m.specs) < 1L) stop("`m.specs` must be a non-empty data.frame.")
  if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(m.specs)) stop("`i` must select a valid row of `m.specs`.")
  if (!is.data.frame(drawlist) || !"ID" %in% names(drawlist)) stop("`drawlist` must be a data.frame with an 'ID' column.")
  
  # Coerce vectors (allow empty)
  Emch <- as.character(Emch %||% character(0))
  Scle <- as.character(Scle %||% character(0))
  Fixd <- as.character(Fixd %||% character(0))
  
  # Ensure columns referenced actually exist
  needed <- unique(c(must_have, Emch, Scle, Fixd))
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
  
  # ---- Select relevant columns from the dataset. ----
  m.data <- data[, c("Surveyx", "EaId", "HhId", "Mid", "UID", "Weight", "Treat", Emch, Scle, Fixd)]
  
  # ---- Remove rows with missing values in the specified columns. ----
  keep_cols <- c("Surveyx", "EaId", "HhId", "Mid", "UID", "Weight", "Treat", Emch, Scle, Fixd)
  m.data <- m.data[stats::complete.cases(m.data[, keep_cols, drop = FALSE]), , drop = FALSE]
  if (!nrow(m.data)) stop("No complete cases available after filtering required columns.")
  
  # ---- Summarize the weight by Surveyx and EaId and merge with the original data. ----
  alloc_tbl <- dplyr::group_by(m.data, .data$Surveyx, .data$EaId) |>
    dplyr::summarise(alloc = sum(.data$Weight, na.rm = TRUE), .groups = "drop")
  m.data <- dplyr::inner_join(alloc_tbl, m.data, by = c("Surveyx", "EaId"))
  
  # ---- Drawing a stratified bootstrap sample by excluding certain EaId values. ----
  cur_id <- m.specs$boot[i]
  if (is.na(cur_id)) stop("`m.specs$boot[i]` is NA; cannot select a bootstrap draw.")
  row_idx <- which(drawlist$ID == cur_id)
  if (length(row_idx) != 1L) stop("Bootstrap ID not found or duplicated in `drawlist$ID`.")
  # flatten the row (excluding ID col) to a vector of EaId to exclude
  exc_ids <- unlist(drawlist[row_idx, setdiff(names(drawlist), "ID"), drop = FALSE], use.names = FALSE)
  exc_ids <- unique(exc_ids[!is.na(exc_ids)])
  if (length(exc_ids)) {
    m.data <- m.data[!m.data$EaId %in% exc_ids, , drop = FALSE]
  }
  if (!nrow(m.data)) stop("All rows were excluded by `drawlist` for the selected bootstrap ID.")
  
  # ---- Summarize the weight by Surveyx and EaId again and merge with the sampled data. ----
  allocj_tbl <- dplyr::group_by(m.data, .data$Surveyx, .data$EaId) |>
    dplyr::summarise(allocj = sum(.data$Weight, na.rm = TRUE), .groups = "drop")
  m.data <- dplyr::inner_join(allocj_tbl, m.data, by = c("Surveyx", "EaId"))
  
  # ---- Calculate the adjusted weights for matching. ----
  # Guard against division by zero: if allocj == 0, set pWeight = 0
  m.data$pWeight <- with(m.data, ifelse(allocj > 0, Weight * (alloc / allocj), 0))
  
  # ---- Assign row names and matching IDs. ----
  row.names(m.data) <- NULL
  m.data$MtchId <- seq_len(nrow(m.data))
  
  # ---- Build formulas from inputs ----
  # exact formula (~ Emch1 + Emch2 + ...)
  Emch.formula <- if (length(Emch)) paste(Emch, collapse = " + ") else "1"
  # distance formula Treat ~ Scle + Fixd  (if none provided, use 1)
  rhs_terms <- c(Scle, Fixd)
  rhs <- if (length(rhs_terms)) paste(rhs_terms, collapse = " + ") else "1"
  Match.formula <- paste0("Treat ~ ", rhs)
  
  # ---- Perform matching using the specified method and distance. ----
  dist_i <- as.character(m.specs$distance[i])
  link_i <- m.specs$link[i]
  method_i <- as.character(m.specs$method[i])
  m_order <- ifelse(identical(dist_i, "glm"), "largest", "closest")
  
  if (verbose) {
    if (is.na(link_i)) {
      cat(crayon::green("method =", method_i, ", distance =", dist_i, Sys.time()), fill = TRUE)
    } else {
      cat(crayon::green("method =", method_i, ", distance =", dist_i, "link =", link_i, Sys.time()), fill = TRUE)
    }
  }
  
  if (is.na(link_i)) {
    m.out <- MatchIt::matchit(
      stats::as.formula(Match.formula),
      exact     = stats::as.formula(paste0("~", Emch.formula)),
      data      = m.data,
      method    = method_i,
      distance  = dist_i,
      estimand  = "ATT",
      s.weights = "pWeight",
      ratio     = 1,
      m.order   = m_order,
      replace   = TRUE,
      reuse.max = 1,
      tol       = 1e-7
    )
  } else {
    m.out <- MatchIt::matchit(
      stats::as.formula(Match.formula),
      exact     = stats::as.formula(paste0("~", Emch.formula)),
      data      = m.data,
      method    = method_i,
      distance  = dist_i,
      link      = as.character(link_i),
      estimand  = "ATT",
      s.weights = "pWeight",
      ratio     = 1,
      # caliper  = 0.10,
      # discard  = "both",
      m.order   = m_order,
      replace   = TRUE,
      reuse.max = 1,
      tol       = 1e-7
    )
  }
  
  # ---- Extract matched data and return the results. ----
  md <- MatchIt::match.data(m.out)[c("Surveyx", "EaId", "HhId", "Mid", "UID", "weights", "pWeight")]
  DONE <- list(
    m.specs = m.specs[i, , drop = FALSE],
    m.out   = m.out,
    md      = md,
    df      = m.data[c("Surveyx", "EaId", "HhId", "Mid", "UID", "pWeight")]
  )
  return(DONE)
}

