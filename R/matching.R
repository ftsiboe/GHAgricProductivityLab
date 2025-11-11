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


#' Construct Matching Formulas for Exact and General Matching
#'
#' @description
#' Builds two types of matching formulas commonly used in propensity score
#' or covariate matching workflows:
#' * An **exact match** formula specifying categorical variables to match exactly.
#' * A **general match** formula specifying numeric and factor covariates for distance-based matching.
#'
#' The function returns both formulas as character strings that can be used
#' in matching functions such as `MatchIt::matchit()` or `Matching::Match()`.
#'
#' @param match_variables_exact A character vector of variable names to be included
#'   in the exact match component (e.g., `"gender"`, `"region"`).
#' @param match_variables_scaler A character vector of continuous or scalar variable names
#'   to include as covariates in the general match formula (e.g., `"age"`, `"income"`).
#' @param match_variables_factor A character vector of factor variable names
#'   to be included as dummy-coded categorical covariates in the general match formula.
#'
#' @return
#' A named list with two elements:
#' \describe{
#'   \item{`exact_match`}{A string representing the exact match formula (factor variables only).}
#'   \item{`general_match`}{A string representing the general match formula, including both
#'   continuous and factor covariates on the right-hand side of `Treat ~`.}
#' }
#' @export
write_match_formulas <- function(
    match_variables_exact,
    match_variables_scaler,
    match_variables_factor){
  
  # Construct exact match formula
  exact_match <- paste0(paste0("factor(", match_variables_exact, ")"), collapse = "+")
  
  # Start general match formula with numeric/scalar variables
  general_match <- paste0("Treat~", paste0(match_variables_scaler, collapse = "+"))
  
  # Append factor variables (converted to factors)
  if (length(match_variables_factor) > 0) {
    factor_terms <- paste0("+factor(", match_variables_factor, ")", collapse = "")
    general_match <- paste0(general_match, factor_terms)
  }
  
  # Return both formulas
  return(list(
    exact_match   = stats::as.formula(paste0("~", exact_match)),
    general_match = stats::as.formula(general_match)
  ))
}


#' Draw matched samples (stratified bootstrap + matching)
#'
#' Performs stratified resampling at the \code{Surveyx, EaId} level (excluding EaIds
#' listed for the current bootstrap \code{ID}) and computes adjusted sampling weights
#' used for matching. Then fits a matching model per \code{match_specifications[i, ]} using
#' \pkg{MatchIt}, with exact matching on \code{Emch} and distance/link from \code{match_specifications}.
#'
#' @param data A data.frame/data.table containing at least:
#'   \code{Surveyx, EaId, HhId, Mid, UID, Weight, Treat}, plus columns named in \code{Emch, Scle, Fixd}.
#' @param match_variables_exact A character vector of variable names to be included
#'   in the exact match component (e.g., `"gender"`, `"region"`).
#' @param match_variables_scaler A character vector of continuous or scalar variable names
#'   to include as covariates in the general match formula (e.g., `"age"`, `"income"`).
#' @param match_variables_factor A character vector of factor variable names
#'   to be included as dummy-coded categorical covariates in the general match formula.
#' @param match_specifications Data frame of matching specifications with columns
#'   \code{boot}, \code{method}, \code{distance}, and optionally \code{link}.
#' @param i Integer index selecting the row of \code{match_specifications} to use.
#' @param sample_draw_list Data frame where column \code{ID} identifies the bootstrap draw,
#'   and each remaining column corresponds to a \code{Survey} containing the sampled \code{EaId}.
#' @param verbose Logical; if \code{TRUE}, prints the chosen matching method and timing. Default \code{FALSE}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{match_specifications}}{The selected row from \code{match_specifications}.}
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
draw_matched_samples <- function(
    i,
    data, 
    match_variables_exact,
    match_variables_scaler,
    match_variables_factor, 
    match_specifications, 
    sample_draw_list, 
    verbose = FALSE) {
  
  # helper: `%||%` returns y when x is NULL, otherwise x
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # ---- validation
  must_have <- c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat")
  if (!is.data.frame(data)) stop("'data' must be a data.frame or data.table.")
  if (!all(must_have %in% names(data))) {
    stop("`data` must contain columns: ", paste(must_have, collapse = ", "))
  }
  if (!is.data.frame(match_specifications) || nrow(match_specifications) < 1L) stop("`match_specifications` must be a non-empty data.frame.")
  if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(match_specifications)) stop("`i` must select a valid row of `match_specifications`.")
  if (!is.data.frame(sample_draw_list) || !"ID" %in% names(sample_draw_list)) stop("`sample_draw_list` must be a data.frame with an 'ID' column.")
  
  # Coerce vectors (allow empty)
  match_variables_exact <- as.character(match_variables_exact %||% character(0))
  match_variables_scaler <- as.character(match_variables_scaler %||% character(0))
  match_variables_factor <- as.character(match_variables_factor %||% character(0))
  
  # Ensure columns referenced actually exist
  needed <- unique(c(must_have, match_variables_exact, match_variables_scaler, match_variables_factor))
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
  
  # ---- Select relevant columns from the dataset.
  m.data <- data[, c("Surveyx", "EaId", "HhId", "Mid", "UID", "Weight", "Treat", match_variables_exact, match_variables_scaler, match_variables_factor)]
  
  # ---- Remove rows with missing values in the specified columns.
  keep_cols <- c("Surveyx", "EaId", "HhId", "Mid", "UID", "Weight", "Treat", match_variables_exact, match_variables_scaler, match_variables_factor)
  m.data <- m.data[stats::complete.cases(m.data[, keep_cols, drop = FALSE]), , drop = FALSE]
  if (!nrow(m.data)) stop("No complete cases available after filtering required columns.")
  
  # ---- Summarize the weight by Surveyx and EaId and merge with the original data.
  alloc_tbl <- dplyr::group_by(m.data, .data$Surveyx, .data$EaId) |>
    dplyr::summarise(alloc = sum(.data$Weight, na.rm = TRUE), .groups = "drop")
  m.data <- dplyr::inner_join(alloc_tbl, m.data, by = c("Surveyx", "EaId"))
  
  # ---- Drawing a stratified bootstrap sample by excluding certain EaId values.
  cur_id <- match_specifications$boot[i]
  if (is.na(cur_id)) stop("`match_specifications$boot[i]` is NA; cannot select a bootstrap draw.")
  row_idx <- which(sample_draw_list$ID == cur_id)
  if (length(row_idx) != 1L) stop("Bootstrap ID not found or duplicated in `sample_draw_list$ID`.")
  # flatten the row (excluding ID col) to a vector of EaId to exclude
  exc_ids <- unlist(sample_draw_list[row_idx, setdiff(names(sample_draw_list), "ID"), drop = FALSE], use.names = FALSE)
  exc_ids <- unique(exc_ids[!is.na(exc_ids)])
  if (length(exc_ids)) {
    m.data <- m.data[!m.data$EaId %in% exc_ids, , drop = FALSE]
  }
  if (!nrow(m.data)) stop("All rows were excluded by `sample_draw_list` for the selected bootstrap ID.")
  
  # ---- Summarize the weight by Surveyx and EaId again and merge with the sampled data.
  allocj_tbl <- dplyr::group_by(m.data, .data$Surveyx, .data$EaId) |>
    dplyr::summarise(allocj = sum(.data$Weight, na.rm = TRUE), .groups = "drop")
  m.data <- dplyr::inner_join(allocj_tbl, m.data, by = c("Surveyx", "EaId"))
  
  # ---- Calculate the adjusted weights for matching.
  # Guard against division by zero: if allocj == 0, set pWeight = 0
  m.data$pWeight <- with(m.data, ifelse(allocj > 0, Weight * (alloc / allocj), 0))
  
  # ---- Assign row names and matching IDs.
  row.names(m.data) <- NULL
  m.data$MtchId <- seq_len(nrow(m.data))
  
  # ---- Build match formulas from inputs
  match_formulas <- write_match_formulas(
    match_variables_exact  = match_variables_exact,
    match_variables_scaler = match_variables_scaler,
    match_variables_factor = match_variables_factor
  )

  # ---- Perform matching using the specified method and distance.
  dist_i <- as.character(match_specifications$distance[i])
  link_i <- match_specifications$link[i]
  method_i <- as.character(match_specifications$method[i])
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
      match_formulas$general_match ,
      exact     = match_formulas$exact_match,
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
      match_formulas$general_match ,
      exact     = match_formulas$exact_match,
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
  return(list(
    match_specifications = match_specifications[i, , drop = FALSE],
    m.out   = m.out,
    md      = MatchIt::match.data(m.out,data=m.data)[c("Surveyx", "EaId", "HhId", "Mid", "UID", "weights", "pWeight")],
    df      = m.data[c("Surveyx", "EaId", "HhId", "Mid", "UID", "pWeight")]
  ))
}



#' Compute Covariate Balance Summaries Across Matching Specs
#'
#' @param match_specifications data.frame/data.table with at least columns:
#'   \code{boot}, \code{ARRAY}, and the spec fields stored in RDS (e.g., method, distance, link).
#' @param matching_output_directory Directory containing RDS files named as "0001.rds", "0002.rds", etc.
#'
#' @return A list with:
#'   \item{rate}{data.frame of mean balance metrics by spec (Adj sample only) with a composite \code{rate}.}
#'   \item{bal_tab}{long-format balance table per covariate/stat/sample/spec.}
#'
#' @details
#' Reads each matching result RDS (expected to contain \code{m.out} and \code{match_specifications}),
#' extracts balance via \code{cobalt::bal.tab}, reshapes, computes a composite balance
#' \eqn{rate = mean( (Diff-0)^2, (KS-0)^2, (V_Ratio-1)^2 )}, and averages by
#' \code{ARRAY, method, distance, link, sample}.
#'
#' @export
covariate_balance <- function(
    match_specifications,
    matching_output_directory){
  
  # ---- Input checks
  if (!all(c("boot", "ARRAY") %in% names(match_specifications))) {
    stop("`match_specifications` must include columns: boot, ARRAY.")
  }
  if (!dir.exists(matching_output_directory)) {
    stop("`matching_output_directory` does not exist.")
  }
  
  # ---- Specs: only boot == 0
  m.specs <- match_specifications[match_specifications$boot == 0, , drop = FALSE]
  if (nrow(m.specs) == 0L) {
    return(list(rate = data.frame(), bal_tab = data.frame()))
  }
  
  
  # Initialize the balance table by applying balance checks for each matching specification.
  bal_tab <- as.data.frame(
    data.table::rbindlist(
      lapply(
        m.specs$ARRAY,
        function(i) {
          #tryCatch({ 
          # Read in the matching results.
          m.out <- readRDS(file.path(matching_output_directory,paste0(stringr::str_pad(m.specs$ARRAY[i],4,pad="0"),".rds")))
          
          # Calculate the balance statistics using the cobalt package.
          bal_tab <- cobalt::bal.tab(m.out$m.out, un = TRUE, abs = TRUE, stats = c("m", "v", "ks"))$Balance
          bal_tab$Coef <- rownames(bal_tab)
          
          # Reshape the balance table for easier analysis.
          bal_tab <- bal_tab |> tidyr::gather(stat, value, c("Diff.Un", "V.Ratio.Un", "KS.Un", "Diff.Adj", "V.Ratio.Adj", "KS.Adj"))
          bal_tab$stat <- gsub("V.Ratio", "V_Ratio", bal_tab$stat)
          bal_tab <- tidyr::separate(bal_tab, "stat", into = c("stat", "sample"), sep = "[.]")
          return(data.frame(m.out$match_specifications, bal_tab))
          # }, error = function(e){NULL})
        }), fill = TRUE)
  )
  
  # Reshape the balance table to spread the statistics into separate columns.
  rate <- bal_tab |> tidyr::spread(stat, value)
  
  # Calculate a composite balance rate.
  rate$rate <- ((rate$Diff - 0)^2 + (rate$KS - 0)^2 + (rate$V_Ratio - 1)^2) / 3
  
  # Summarize the balance statistics by method, distance, link, and sample.
  rate <- doBy::summaryBy(rate + Diff + V_Ratio + KS ~ ARRAY + method + distance + link + sample, data = rate, FUN = mean, na.rm = T)
  
  # Order the rate table by the composite balance rate.
  rate <- rate[order(-rate$rate, rate$ARRAY),]
  
  # Filter the rate table to only include adjusted samples.
  rate <- rate[rate$sample %in% "Adj",]
  
  # Add a descriptive name for each combination of method, distance, and link.
  rate$name <- ifelse(rate$link %in% "logit", "Logit [PS]", NA)
  rate$name <- ifelse(rate$link %in% "cauchit", "Cauchit [PS]", rate$name)
  rate$name <- ifelse(rate$link %in% "probit", "Probit [PS]", rate$name)
  rate$name <- ifelse(rate$link %in% "cloglog", "Complementary Log-Log [PS]", rate$name)
  rate$name <- ifelse(rate$distance %in% "euclidean", "Euclidean", rate$name)
  rate$name <- ifelse(rate$distance %in% "robust_mahalanobis", "Robust Mahalanobis", rate$name)
  rate$name <- ifelse(rate$distance %in% "scaled_euclidean", "Scaled Euclidean", rate$name)
  rate$name <- ifelse(rate$distance %in% "mahalanobis", "Mahalanobis", rate$name)
  
  # Add an ID column to the rate table.
  rate$ID <- 1:nrow(rate)
  
  return(  list(rate=rate,bal_tab=bal_tab))
}


