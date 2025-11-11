#' Clear the package cache of downloaded data files
#'
#' Deletes the entire cache directory used by the **rfcipCalcPass** package to store
#' downloaded data files. Useful if you need to force re-download of data,
#' or free up disk space.
#' @family helpers
#' @return Invisibly returns `NULL`. A message is printed indicating which
#'   directory was cleared.
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove all cached data files so they will be re-downloaded on next use
#' clear_rfcipCalcPass_cache()
#' }
clear_rfcipCalcPass_cache <- function(){
  dest_dir <- tools::R_user_dir("rfcipCalcPass", which = "cache")
  if (dir.exists(dest_dir)) {
    unlink(dest_dir, recursive = TRUE, force = TRUE)
  }
  message("Cleared cached files in ", dest_dir)
  invisible(NULL)
}

#' Create a control list of adjustment factors for PASS Calculators
#'
#' This function initializes a named list of control parameters (adjustment factors)
#' used throughout the farm policy simulation pipeline. Each element has a sensible
#' default but can be overridden to customize behavior.
#'
#' @param revenue_lookup_adjustment_factor Numeric scalar. Multiplier applied to revenue look ups. (Default = 1)
#' @param unit_structure_discount_factor Numeric scalar. Discount factor for unit structure. (Default = 1)
#' @param additive_optional_rate_adjustment_factor Numeric scalar. Additive adjustment to optional rates. (Default = 0)
#' @param multiplicative_optional_rate_adjustment_factor Numeric scalar. Multiplicative adjustment to optional rates. (Default = 1)
#' @param capped_revenue_add_on_factor Numeric scalar. Add-on factor applied to capped revenue. (Default = 0)
#' @param liability_adjustment_factor Numeric scalar. Multiplier applied to liability coverage. (Default = 1)
#' @param multiple_commodity_adjustment_factor Numeric scalar. Adjustment factor when multiple commodities are insured. (Default = 1)
#' @param reported_acres Numeric scalar. Number of acres reported for insurance purposes. (Default = 1)
#' @param insured_share_percent Numeric scalar. Share of the crop insured (0-1). (Default = 1)
#' @param price_election_percent Numeric scalar. Proportion of the elected price used (0-1). (Default = 1)
#' @param damage_area_rate Numeric scalar. Rate applied to damage-area calculation.(Default= 1)
#' @param harvest_price_inclusion_plans Vector of plan codes that include harvest price in guarantee calculation.
#' @param non_price_risk_plans Vector of plan codes that do not adjust revenue based on harvest price.
#' @param rma_rounding logical(1) or numeric(1) If FALSE, rounds only to integer. Otherwise multiplies the
#'   number of digits by this factor (mimics round(x, n * rma_rounding)). Defaults to TRUE.
#' @param yield_ratio_cup_and_cap logical(1) If TRUE, enforces a 0.50-1.50 `cup & cap` on yield ratios. Defaults to TRUE.
#' @param continuous_integration_session logical(1). If TRUE, a small deterministic subset of the Actuarial Data Master (ADM)
#' YTD ZIP archive is used. This is designed to be safe and fast for use in continuous integration sessions. see `build_min_adm()`
#' @param adm_decoy_state_abb tate abbreviation indicating which state's decoy ADM to use (default is ND).
#' @return A named list of all control parameters, ready to be passed to other simulation functions.
#' @family helpers
#' @export
#' @examples
#' \dontrun{
#' # Use all defaults:
#' ctrl <- rfcipCalcPass_control()
#'
#' # Override a couple of factors:
#' ctrl2 <- rfcipCalcPass_control(
#'   free_acres_factor = 0.15,
#'   liability_adjustment_factor = 0.9
#' )
#'
#'}
rfcipCalcPass_control <- function(
    revenue_lookup_adjustment_factor               = 1,
    unit_structure_discount_factor                 = 1,
    additive_optional_rate_adjustment_factor       = 0,
    multiplicative_optional_rate_adjustment_factor = 1,
    capped_revenue_add_on_factor                   = 0,
    liability_adjustment_factor                    = 1,
    multiple_commodity_adjustment_factor           = 1,
    reported_acres                                 = 1,
    insured_share_percent                          = 1,
    price_election_percent                         = 1,
    damage_area_rate                               = 1,
    harvest_price_inclusion_plans                  = c(2,5,16,32,88),
    non_price_risk_plans                           = c(1,90,4,31,87),
    rma_rounding                                   = TRUE,
    yield_ratio_cup_and_cap                        = TRUE,
    continuous_integration_session                 = FALSE,
    adm_decoy_state_abb                            = "ND"
){
  list(
    revenue_lookup_adjustment_factor               = revenue_lookup_adjustment_factor,
    unit_structure_discount_factor                 = unit_structure_discount_factor,
    additive_optional_rate_adjustment_factor       = additive_optional_rate_adjustment_factor,
    multiplicative_optional_rate_adjustment_factor = multiplicative_optional_rate_adjustment_factor,
    capped_revenue_add_on_factor                   = capped_revenue_add_on_factor,
    liability_adjustment_factor                    = liability_adjustment_factor,
    multiple_commodity_adjustment_factor           = multiple_commodity_adjustment_factor,
    reported_acres                                 = reported_acres,
    insured_share_percent                          = insured_share_percent,
    price_election_percent                         = price_election_percent,
    damage_area_rate                               = damage_area_rate,
    harvest_price_inclusion_plans                  = harvest_price_inclusion_plans,
    non_price_risk_plans                           = non_price_risk_plans,
    rma_rounding                                   = rma_rounding,
    yield_ratio_cup_and_cap                        = yield_ratio_cup_and_cap,
    continuous_integration_session                 = continuous_integration_session,
    adm_decoy_state_abb                            = adm_decoy_state_abb
  )
}


#' Ensure specified fields exist in a data.table, filling in defaults
#'
#' Given a set of field names and a data.frame or data.table, this function
#' checks for any missing columns and adds them, pulling default values
#' from a control list (e.g. created by \code{\link{rfcipCalcPass_control}}).
#'
#' @param fields Character vector. Names of columns that must be present in `data`.
#' @param data A `data.frame` or `data.table` to operate on. Converted by reference.
#' @param control A named list of default column values; typically from \code{\link{rfcipCalcPass_control}}.
#'
#' @return The input `data` as a `data.table`, with any missing `fields` added
#'   and populated by the corresponding element in `control`.
#' @family helpers
#' @import data.table
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' # Sample data with only one column
#' df <- data.frame(id = 1:3)
#'
#' # Ensure insurance fields exist
#' out <- apply_controls(
#'   fields = c("insured_share_percent", "damage_area_rate"),
#'   data   = df,
#'   control = rfcipCalcPass_control(insured_share_percent = 0.8)
#' )
#'
#' }
#'
#' # Now out has id, insured_share_percent, and damage_area_rate
apply_controls <- function(
    fields,
    data,
    control = rfcipCalcPass_control()) {
  setDT(data)
  missing <- setdiff(fields, names(data))
  if (length(missing)) {
    data[, (missing) := control[missing]]
  }
  return(data)
}



