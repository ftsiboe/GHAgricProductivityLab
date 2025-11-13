#' Draw / Match Sample Specifications
#'
#' Generates (i) a draw list by sampling \code{EaId} within each unique
#' \code{Survey} group and (ii) a grid of matching specifications for each
#' bootstrap draw.
#'
#' @param number_of_draws Integer. The number of draws to perform per \code{Survey}.
#' @param data A data.frame or data.table containing at least the columns
#'   \code{Survey} and \code{EaId}.
#' @param myseed Integer. Seed for random number generation (default \code{03242025}).
#'
#' @details
#' \strong{Draw list:}
#' For each unique value of \code{Survey}, the function samples \code{number_of_draws}
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
#'         is a column and rows correspond to draw \code{ID} (0:\code{number_of_draws}).}
#' }
#'
#' @note Requires that \code{data} contain \code{Survey} and \code{EaId}.
#' \code{tidyr::spread()} is used for wide reshaping (consider
#' \code{tidyr::pivot_wider()} in new code).
#' @family matching
#' @importFrom data.table rbindlist
#' @export
match_sample_specifications <- function(
    number_of_draws = NULL, data, myseed = NULL) {
  
  if(is.null(myseed)){
    myseed <- GHAgricProductivityLab_control()$myseed
  }
  
  if(is.null(number_of_draws)){
    number_of_draws <- GHAgricProductivityLab_control()$number_of_draws
  }
  
  # ---- basic validation ----
  if (!is.data.frame(data)) stop("'data' must be a data.frame or data.table.")
  if (!all(c("Survey","EaId") %in% names(data))) {
    stop("`data` must contain columns 'Survey' and 'EaId'.")
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
          sampled <- if (length(pool) == 0L) rep(NA, number_of_draws) else sample(pool, size = number_of_draws, replace = TRUE)
          data.frame(
            glss = glss,
            ID   = 0:number_of_draws,
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
#' @family matching
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
#'   \code{Surveyx, EaId, HhId, Mid, unique_identifier, Weight, Treat}, plus columns named in \code{Emch, Scle, Fixd}.
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
#'   \item{\code{md}}{Matched data: \code{Surveyx, EaId, HhId, Mid, unique_identifier, weights, pWeight}.}
#'   \item{\code{df}}{The analysis data with adjusted weights: \code{Surveyx, EaId, HhId, Mid, unique_identifier, pWeight}.}
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
#' @family matching
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
  must_have <- c("Surveyx","EaId","HhId","Mid","unique_identifier","Weight","Treat")
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
  m.data <- data[, c("Surveyx", "EaId", "HhId", "Mid", "unique_identifier", "Weight", "Treat", match_variables_exact, match_variables_scaler, match_variables_factor)]
  
  # ---- Remove rows with missing values in the specified columns.
  keep_cols <- c("Surveyx", "EaId", "HhId", "Mid", "unique_identifier", "Weight", "Treat", match_variables_exact, match_variables_scaler, match_variables_factor)
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
    md      = MatchIt::match.data(m.out,data=m.data)[c("Surveyx", "EaId", "HhId", "Mid", "unique_identifier", "weights", "pWeight")],
    df      = m.data[c("Surveyx", "EaId", "HhId", "Mid", "unique_identifier", "pWeight")]
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
#' @family matching
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
          m.out <- readRDS(file.path(matching_output_directory,paste0("match_",stringr::str_pad(m.specs$ARRAY[i],4,pad="0"),".rds")))
          
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



#' Compute log-linear treatment effects (ATE/ATET/ATEU) for multiple outcomes
#'
#' @description
#' For a given matching specification index \code{i}, this function:
#' \enumerate{
#'   \item Loads the corresponding matched weights file from \code{matching_output_directory},
#'   \item Merges those weights into the analysis \code{data} via \code{unique_identifier},
#'   \item Optionally normalizes each outcome by the first element of \code{outcome_variables}
#'         (e.g., to construct per-hectare or per-unit measures),
#'   \item Fits a weighted log-linear model for each outcome with interactions between
#'         \code{Treat} and the supplied matching covariate formulas,
#'   \item Transforms fitted differences into percentage treatment effects, trims extreme
#'         values, and computes weighted ATE, ATET, and ATEU alongside model fit statistics.
#' }
#'
#' @param data A \code{data.frame} containing at least:
#'   \itemize{
#'     \item \code{unique_identifier}: unit identifier used for merging matched weights,
#'     \item \code{Treat}: treatment indicator (logical or 0/1),
#'     \item \code{Area}: plot size (positive; used for input checks and typical scaling),
#'     \item all variables named in \code{outcome_variables},
#'     \item any covariates referenced in \code{match_formulas$general_match} and
#'           \code{match_formulas$exact_match}.
#'   }
#' @param outcome_variables Character vector of outcome variable names
#'   (e.g., \code{c("HrvstKg","Area","SeedKg","HHLaborAE","HirdHr","FertKg","PestLt")}).
#' @param normalize Logical scalar. If \code{TRUE}, each outcome in
#'   \code{outcome_variables} is divided by the first element of \code{outcome_variables}
#'   in the merged data (e.g., to obtain outcomes per hectare or per unit).
#' @param i Integer index selecting the row of \code{match_specifications} and the
#'   corresponding matching result file (zero-padded via \code{ARRAY} to
#'   \code{"NNNN.rds"}).
#' @param match_specifications A \code{data.frame} with at least a column
#'   \code{ARRAY}. The \code{i}-th row is used to locate the matching output file
#'   and is cbind-ed to the returned results.
#' @param matching_output_directory Directory containing per-specification matching
#'   results named \code{"match_0001.rds"}, \code{"match_0002.rds"}, etc. Each RDS
#'   is expected to contain an element \code{$md} with columns
#'   \code{unique_identifier} and \code{weights}.
#' @param match_formulas A list with:
#'   \itemize{
#'     \item \code{general_match}: a formula; the RHS is accessed via
#'           \code{as.character(...)[3]},
#'     \item \code{exact_match}: a formula; the RHS is accessed via
#'           \code{as.character(...)[2]}.
#'   }
#'
#' @details
#' For each outcome \code{oc} in \code{outcome_variables}, the function:
#' \enumerate{
#'   \item Filters to observations with non-missing \code{Treat}, the outcome, and
#'         \code{weights},
#'   \item Constructs a log-linear model of the form
#' \preformatted{
#' log(oc + eps) ~ Treat * (general_match_rhs + exact_match_rhs)
#' }
#'         where \code{eps = min(oc[oc > 0]) * 1/100} in the estimation sample,
#'   \item Fits the model using \code{lm()} with weights given by the matched
#'         \code{weights},
#'   \item Predicts fitted log outcomes for treated (\code{Treat = 1}) and untreated
#'         (\code{Treat = 0}) counterfactuals,
#'   \item Computes individual percentage treatment effects
#' \deqn{TE_OLS = \{ \exp(\hat{y}_1 - \hat{y}_0) - 1 \} \times 100,}
#'         trims them to the interval \eqn{[-100, 100]}, and then forms:
#'         \itemize{
#'           \item ATE: weighted mean of \code{TE_OLS} over all units,
#'           \item ATET: weighted mean of \code{TE_OLS} among treated units,
#'           \item ATEU: weighted mean of \code{TE_OLS} among untreated units.
#'         }
#' }
#' In addition, for each outcome the function records model diagnostics:
#' AIC, log-likelihood, \eqn{R^2}, adjusted \eqn{R^2}, F-statistic, and the sample size
#' used in the regression.
#'
#' @return A \code{data.frame} (\code{atet_scalar}) with the columns from
#'   \code{match_specifications[i, ]} repeated across rows, and the additional columns:
#'   \itemize{
#'     \item \code{outcome}: outcome variable name,
#'     \item \code{treatment}: name of the treatment indicator (always \code{"Treat"}),
#'     \item \code{level}: one of
#'           \code{"ATE"}, \code{"ATET"}, \code{"ATEU"},
#'           \code{"aic"}, \code{"ll"}, \code{"R2"}, \code{"N"}, \code{"Ft"}, \code{"R2a"},
#'     \item \code{est}: numeric estimate corresponding to \code{level}.
#'   }
#'   One block of rows is returned per outcome in \code{outcome_variables}. If a model
#'   fails for a given outcome, that outcome is skipped.
#' @family matching
#' @import stats
#' @export
treatment_effect_calculation <- function(
    data,
    outcome_variables,
    normalize = NULL,
    i,
    match_specifications,
    matching_output_directory,
    match_formulas){
  
  # 1. Validate inputs
  # Ensure formulas are actually formulas (not character strings)
  if (!inherits(match_formulas$general_match, "formula") ||
      !inherits(match_formulas$exact_match, "formula")) {
    stop("`match_formulas$general_match` and `$exact_match` must be formulas.")
  }
  
  # Convert Treat to numeric (1/0) for consistent filtering
  if (is.logical(data$Treat)) data$Treat <- as.integer(data$Treat)
  
  # Check Area for non-positive or missing values to prevent division by zero
  if (any(!is.finite(data$Area) | data$Area <= 0, na.rm = TRUE)) {
    stop("Non-positive or missing `Area` values found. Clean before running.")
  }
  
  # Verify that each outcome has at least some positive observations
  for (oc in outcome_variables) {
    if (!any(data[[oc]] > 0, na.rm = TRUE)) {
      stop(sprintf("Outcome '%s' has no positive values.", oc))
    }
  }
  
  # 2. Load matched dataset for the current specification
  # Expected file: matching_output_directory/0001.rds, 0002.rds, etc.
  file_path <- file.path(
    matching_output_directory,
    paste0("match_",stringr::str_pad(match_specifications$ARRAY[i], 4, pad = "0"), ".rds")
  )
  
  # Read in matched data; expect object with `$md` containing unique_identifier and weights
  matched_file <- readRDS(file_path)
  if (!("md" %in% names(matched_file)) ||
      !all(c("unique_identifier", "weights") %in% names(matched_file$md))) {
    stop("Matching file is missing `$md` or required columns (unique_identifier, weights).")
  }
  
  # Extract matched weights and merge into analysis data
  md <- matched_file$md[c("unique_identifier", "weights")]
  md <- dplyr::inner_join(data, md, by = "unique_identifier")
  
  # 3. Normalize key variables by area (e.g, per-hectare metrics)
  if(normalize){
    for (oc in outcome_variables) {
      md[,oc] <- md[,oc]/md[,outcome_variables[1]]
    }
  }
  
  treatment <- "Treat"
  
  # 4. Fit outcome-specific models and compute ATE/ATET/ATEU
  atet_scalar <- as.data.frame(
    data.table::rbindlist(
      lapply(outcome_variables, function(outcome) {
        tryCatch({
          
          # Filter to complete observations for this outcome
          data <- md[!(is.na(md[, treatment]) |
                         is.na(md[, outcome])   |
                         is.na(md[, "weights"])), ]
          
          # Build log-linear model formula dynamically
          Fit.formula <- as.formula(
            paste0(
              "log(", outcome, "+",
              min(data[, outcome][data[, outcome] > 0], na.rm = TRUE) * (1/100),
              ") ~ ", treatment, "*(",
              as.character(match_formulas$general_match)[3], "+",
              as.character(match_formulas$exact_match)[2], ")"
            )
          )
          
          # Weighted linear model
          fit_lm <- lm(Fit.formula, weights = weights, data = data)
          
          # Predict counterfactuals for treated and untreated cases
          TREATED <- data[names(data)[!names(data) %in% treatment]]
          TREATED[, treatment] <- 1
          TREATED <- predict(fit_lm, TREATED)
          
          UNTREATED <- data[names(data)[!names(data) %in% treatment]]
          UNTREATED[, treatment] <- 0
          UNTREATED <- predict(fit_lm, UNTREATED)
          
          # Treatment effect in percent change
          data$TE_OLS  <- (exp(TREATED - UNTREATED) - 1) * 100
          data$outcome <- outcome
          
          # Trim implausible effects outside [-100, 100]
          TE <- data[!(data$TE_OLS <= -100 | data$TE_OLS >= 100), ]
          
          # Compute weighted means for ATE, ATET, and ATEU
          data.table::setDT(TE)
          ATE  <- data.frame(TE[, .(est = weighted.mean(TE_OLS, w = weights, na.rm = TRUE)), by = .(outcome)])$est
          ATET <- data.frame(TE[Treat == 1][, .(est = weighted.mean(TE_OLS, w = weights, na.rm = TRUE)), by = .(outcome)])$est
          ATEU <- data.frame(TE[Treat == 0][, .(est = weighted.mean(TE_OLS, w = weights, na.rm = TRUE)), by = .(outcome)])$est
          
          # Model diagnostics
          aic <- as.numeric(AIC(fit_lm))
          ll  <- as.numeric(logLik(fit_lm))
          R2  <- summary(fit_lm)$r.squared
          R2a <- summary(fit_lm)$adj.r.squared
          Ft  <- summary(fit_lm)$fstatistic[1]
          N   <- nrow(fit_lm$model)
          
          # Store all results in a tidy frame
          fit_lm_res <- data.frame(
            outcome = outcome,
            treatment = treatment,
            level = c("ATE", "ATET", "ATEU", "aic", "ll", "R2", "N", "Ft", "R2a"),
            est   = c(ATE, ATET, ATEU, aic, ll, R2, N, Ft, R2a)
          )
          
          return(fit_lm_res)
        }, error = function(e) {
          # Return NULL for this outcome if any model fails
          return(NULL)
        })
      }), fill = TRUE)
  )
  
  # Attach specification metadata to results (not returned)
  atet_scalar <- data.frame(match_specifications[i, ], atet_scalar)
  
  # 5. Return specification index to indicate completion
  return(atet_scalar)
  
}



#' Summarize Treatment Effect Estimates Across Matching Specifications
#'
#' @description
#' Aggregates and summarizes treatment effect results stored as `.rds` files
#' in a specified output directory. The function reads each RDS file, combines
#' all results into a single data frame, filters out invalid estimates, and
#' computes jackknife-style summary statistics (mean, standard error, sample size,
#' t-value, and p-value) by matching specification and outcome.
#'
#' @param treatment_effects_output_directory
#' Character string giving the path to the directory containing
#' `.rds` files with treatment effect results (e.g., outputs of
#' \code{treatment_effect_calculation()} saved across specifications).
#'
#' @details
#' The function proceeds through the following steps:
#' \enumerate{
#'   \item Lists and reads all `.rds` files in \code{treatment_effects_output_directory}.
#'   \item Combines them into a single long-format data frame using
#'         \code{data.table::rbindlist()}.
#'   \item Removes rows with invalid estimates (\code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf}).
#'   \item Separates base (non-bootstrap) results where \code{boot == 0} and joins these
#'         with jackknife summary statistics computed via
#'         \code{doBy::summaryBy()} across method, distance, link, outcome, and level.
#'   \item Renames summary columns to:
#'         \itemize{
#'           \item \code{jack_mean}: mean estimate
#'           \item \code{jack_se}: standard error
#'           \item \code{jack_n}: sample size (number of replicates)
#'           \item \code{jack_tvl}: t-value (\code{jack_mean / jack_se})
#'           \item \code{jack_pvl}: two-sided p-value
#'         }
#' }
#'
#' @return
#' A \code{data.frame} containing summarized treatment effect estimates with
#' columns:
#' \itemize{
#'   \item \code{method, distance, link, outcome, level}
#'   \item \code{est} (original estimate for non-bootstrap sample)
#'   \item \code{jack_mean, jack_se, jack_n, jack_tvl, jack_pvl}
#' }
#' @family matching
#' @export
treatment_effect_summary <- function(
    treatment_effects_output_directory){
  
  files <- list.files(treatment_effects_output_directory, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0L) return(data.frame())
  
  estim <- as.data.frame(
    data.table::rbindlist(
      lapply(files, function(file) {
        out <- NULL
        tryCatch({ out <- readRDS(file) }, error = function(e) {})
        out
      }),
      fill = TRUE
    )
  )
  
  # Required columns present?
  req <- c("method","distance","link","outcome","level","est","boot")
  miss <- setdiff(req, names(estim))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))
  
  # Keep finite estimates only
  estim <- estim[is.finite(estim$est), , drop = FALSE]
  
  # Base (boot==0), deduplicated
  base0 <- unique(estim[estim$boot %in% 0, c("method","distance","link","outcome","level","est")])
  
  # Jackknife summaries across specs (or boots)
  jk <- doBy::summaryBy(est ~ method + distance + link + outcome + level,
                        data = estim, FUN = c(length, mean, sd))
  
  # Merge
  estim <- dplyr::full_join(base0, jk, by = c("method","distance","link","outcome","level"))
  
  # Rename & inferential stats
  names(estim)[names(estim) == "est.mean"]   <- "jack_mean"
  names(estim)[names(estim) == "est.sd"]     <- "jack_se"
  names(estim)[names(estim) == "est.length"] <- "jack_n"
  
  estim$jack_tvl <- ifelse(estim$jack_se > 0, estim$jack_mean / estim$jack_se, NA_real_)
  df <- pmax(estim$jack_n - 2L, 1L)
  estim$jack_pvl <- 2 * (1 - stats::pt(abs(estim$jack_tvl), df))
  
  
  return(estim)
}
