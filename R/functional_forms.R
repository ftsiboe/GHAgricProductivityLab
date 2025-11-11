#' Generate Functional and Distribution Forms
#'
#' Creates symbolic production function strings for stochastic frontier analysis (SFA),
#' including Cobb-Douglas (CD) and Translog (TL) specifications, plus a set of
#' candidate distributions for the inefficiency term.
#'
#' @param nX Integer. Number of input variables. Default is 5.
#' @param trend Logical. If \code{TRUE}, adjusts the (optional) transcendental
#'   form (\code{TP}) by removing the last linear input term. Default is \code{FALSE}.
#'
#' @details
#' The returned \code{fxnforms} list includes:
#' \itemize{
#'   \item \code{CD}: sum of log inputs (\code{lnI1 + lnI2 + ...}).
#'   \item \code{TL}: CD terms plus second-order and pairwise interaction terms
#'         (e.g., \code{0.5 * lnIi^2} and \code{lnIi * lnIj} for \code{i < j}).
#' }
#' Additional forms (Linear, Quadratic, Generalized, Transcendental) are shown in
#' the code as commented examples; uncomment to include them. The \code{trend}
#' argument only affects \code{TP} when that form is enabled.
#'
#' \code{distforms} lists common inefficiency distributions (e.g., half-normal,
#' truncated normal, exponential, lognormal, Weibull) and whether a scaling
#' property is assumed for each.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{fxnforms}}{Named character strings of functional forms.}
#'   \item{\code{distforms}}{Named list of inefficiency distributions with a \code{scaling} flag.}
#' }
#'
#' @export
functional_forms <- function(nX=5, trend=FALSE) {
  
  # Initialize strings for different functional forms.
  lnInputs  <- 0
  Inputs    <- 0
  Quadratic <- 0
  Translog  <- 0
  
  # Construct the functional forms by iterating over the number of input variables.
  for(i in 1:nX) {
    Inputs    <- paste0(Inputs, "+I", i)
    lnInputs  <- paste0(lnInputs, "+lnI", i)
    Quadratic <- paste0(Quadratic, "+I(1/2*I", i, "*I", i, ")")
    Translog  <- paste0(Translog, "+I(1/2*lnI", i, "*lnI", i, ")")
    for(j in 1:nX) {
      if(i < j) Translog <- paste0(Translog, "+lnI", i, "*lnI", j)
      if(i < j) Quadratic <- paste0(Quadratic, "+I", i, "*I", j)
    }
  }
  
  # Create a list of functional forms.
  fxnforms <- list(
    CD = gsub("0[+]", "", lnInputs),                                            # Cobb-Douglas Production Function
    TL = paste0(gsub("0[+]", "", lnInputs), "+", gsub("0[+]", "", Translog)),   # Translog Production Function
    # LN = gsub("0[+]", "", Inputs),                                            # Linear Production Function
    # QD = paste0(gsub("0[+]", "", Inputs), "+", gsub("0[+]", "", Quadratic)),  # Quadratic Production Function
    # GP = paste0(gsub("0[+]", "", lnInputs), "+Y"),                            # Generalized Production Function
    # TP = paste0(gsub("0[+]", "", lnInputs), "+", gsub("0[+]", "", Inputs)),   # Transcendental Production Function
    NULL
  )
  
  # Remove any NULL entries in the list.
  fxnforms <- fxnforms[lengths(fxnforms) != 0]
  
  # Modify the Transcendental Production Function if trend is TRUE.
  if (isTRUE(trend) && !is.null(fxnforms$TP)) fxnforms$TP <- gsub(paste0("[+]I", nX), "", fxnforms$TP)
  
  # Create a list of distribution forms.
  distforms <- list(
    hnormal        = list("hnormal", scaling=FALSE),        # the half normal distribution (Aigner et al. 1977, Meeusen and Vandenbroeck 1977)
    tnormal        = list("tnormal", scaling=FALSE),        # the truncated normal distribution (Stevenson 1980)
    exponential    = list("exponential", scaling=FALSE),    # the exponential distribution
    tslaplace      = list("tslaplace", scaling=FALSE),      # the truncated skewed Laplace distribution (Wang 2012)
    genexponential = list("genexponential", scaling=FALSE), # the generalized exponential distribution (Papadopoulos 2020)
    tnormal_scaled = list("tnormal", scaling=TRUE),         # the truncated normal distribution with the scaling property model (Wang and Schmidt 2002)
    rayleigh       = list("rayleigh", scaling=FALSE),       # the Rayleigh distribution (Hajargasht 2015)
    uniform        = list("uniform", scaling=FALSE),        # the uniform distribution (Li 1996, Nguyen 2010)
    gamma          = list("gamma", scaling=FALSE),          # the Gamma distribution (Greene 2003)
    lognormal      = list("lognormal", scaling=FALSE),      # the log normal distribution (Migon and Medici 2001, Wang and Ye 2020)
    weibull        = list("weibull", scaling=FALSE)         # the Weibull distribution (Tsionas 2007)
  )
  
  # Return the list of functional forms and distribution forms.
  return(list(fxnforms=fxnforms, distforms=distforms))
}
