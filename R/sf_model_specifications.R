#' Build Model Specifications
#'
#' Construct a specification table for multi-stage frontier (MSF) analysis by
#' combining alternative production-function forms and distributional
#' assumptions with technology choices and disaggregation levels.
#' The function first creates a pooled grid of functional forms (\code{fxnforms})
#' and distributions (\code{distforms}), then augments it with crop-specific and
#' demographic splits, and finally expands technology choices.
#'
#' @param data data.frame or data.table.
#'   A dataset that (ideally) contains the columns named in \code{demographic_variables}.
#'   **Note:** The current implementation references a global object \code{DATA}
#'   (not \code{data}) when deriving demographic levels; ensure \code{DATA} exists
#'   with those columns or adapt your environment accordingly.
#' @param distforms named \code{list} of distributional forms.
#'   If \code{NULL}, defaults to \code{functional_forms()$distforms}.
#' @param fxnforms named \code{list} of functional forms.
#'   If \code{NULL}, defaults to \code{functional_forms()$fxnforms}.
#' @param technology_variables \code{character} vector of technology variables.
#'   The first element is taken as the default/primary technology.
#' @param mainF \code{integer} index of the preferred functional form (in \code{fxnforms}).
#'   Default is \code{2}.
#' @param mainD \code{integer} index of the preferred distribution (in \code{distforms}).
#'   Default is \code{1}.
#' @param demographic_variables \code{character} vector of column names used for
#'   demographic disaggregation. Defaults to
#'   \code{c("Female","Region","Ecozon","EduCat","EduLevel","AgeCat")}.
#' @param crop_list \code{character} vector of crop names for crop-level
#'   disaggregation. Defaults to a selection of common crops.
#'
#' @details
#' \strong{Procedure (high level):}
#' \enumerate{
#'   \item Build a pooled grid of all \code{fxnforms} * \code{distforms}.
#'   \item Keep rows matching \code{mainF} and \code{mainD} and tag pooled specs.
#'   \item Add crop-specific rows for \code{crop_list} (using the main form/dist).
#'   \item Add demographic splits for each variable in \code{demographic_variables},
#'         using unique levels in \code{DATA[, var]} (see note in \emph{Warning}).
#'   \item Set \code{TechVar} to the first element of \code{technology_variables};
#'         if additional technology variables are provided, add pooled rows for them
#'         with the main form/dist.
#'   \item Reorder to prioritize \code{mainD} and create an \code{nnm} flag with
#'         values \code{"fullset"} and \code{"optimal"}.
#'   \item Remove demographic rows for the default demographic list and retain only
#'         \code{level == "Pooled"} in the final output.
#' }
#'
#' \strong{Returned columns} (final table):
#' \itemize{
#'   \item \code{disasg} - disaggregation variable (e.g., \code{"CropID"}).
#'   \item \code{level} - disaggregation level (e.g., \code{"Pooled"} or a crop/level).
#'   \item \code{TechVar} - selected technology variable.
#'   \item \code{f} - index of functional form (position in \code{fxnforms}).
#'   \item \code{d} - index of distribution (position in \code{distforms}).
#'   \item \code{nnm} - marker column (\code{"fullset"} / \code{"optimal"}).
#' }
#'
#' @return A \code{data.frame} (data.table-compatible) of model specifications with
#'   columns \code{disasg}, \code{level}, \code{TechVar}, \code{f}, \code{d}, and \code{nnm}.
#'
#' @section Warning:
#' The function calls \code{unique(DATA[, w])} inside a \code{tryCatch}; this
#' requires a global object \code{DATA} containing the demographic columns.
#' If \code{DATA} is absent or lacks a given variable, that split is skipped.
#'
#' @section Assumptions:
#' \itemize{
#'   \item \code{technology_variables} is non-empty; its first element is the default.
#'   \item \code{mainF} and \code{mainD} are valid indices within \code{fxnforms}
#'         and \code{distforms}, respectively.
#' }
#'
#' @import data.table
#' @export
sf_model_specifications <- function(
    data,
    distforms = sf_functional_forms()$distforms,
    fxnforms  = sf_functional_forms()$fxnforms,
    technology_variables,
    mainF = 2,
    mainD = 1,
    demographic_variables = c("Female","Region","Ecozon","EduCat","EduLevel","AgeCat"),
    crop_list = c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                  "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam")){

  # Create a dataframe with all combinations of fxnforms and distforms, labeled as "Pooled".
  SPECS <- as.data.frame(
    data.table::rbindlist(
      lapply(1:length(fxnforms), function(f) {
        data.frame(level="Pooled", f=f, d=1:length(distforms))
      }), fill = TRUE
    )
  )
  
  # Filter the dataframe to only include rows where f and d match mainF and mainD, respectively.
  SPECS <- rbind(SPECS[SPECS$f %in% mainF,], SPECS[SPECS$d %in% mainD,])
  
  # Add a column for disaggregation variable, initially set to "CropID".
  SPECS$disasg <- "CropID"
  
  # Remove duplicate rows in the dataframe.
  SPECS <- unique(SPECS)
  
  # Add additional rows for different levels of disaggregation based on crop types and demographic variables.
  SPECS <- unique(
    rbind(SPECS,
          data.frame(disasg="CropID", level= crop_list, 
                     SPECS[(SPECS$f %in% mainF & SPECS$d %in% mainD), c("f","d")]),
          as.data.frame(
            data.table::rbindlist(
              lapply(
                demographic_variables,
                function(w) {
                  tryCatch({
                    DONE <- data.frame(disasg=w, level=unique(DATA[,w]), 
                                       SPECS[(SPECS$f %in% mainF & SPECS$d %in% mainD), c("f","d")])
                    return(DONE)
                  }, error = function(e) { return(NULL) })
                }), fill = TRUE)
          )
    )
  )
  
  # Set the first technical variable from technology_variables as the TechVar column.
  SPECS$TechVar <- technology_variables[1]
  
  # If there are more technical variables, add them to the dataframe for rows where disasg is "CropID" and level is "Pooled".
  if (length(technology_variables) > 1) {
    SPECS <- unique(
      rbind(SPECS,
            data.frame(TechVar=technology_variables[2:length(technology_variables)], 
                       SPECS[(SPECS$disasg %in% "CropID" & SPECS$level %in% "Pooled" & SPECS$f %in% mainF & SPECS$d %in% mainD), c("disasg","level","f","d")])
      )
    )
  }
  
  # Reorder the dataframe to prioritize rows where d matches mainD.
  SPECS <- unique(rbind(SPECS[SPECS$d %in% mainD,], SPECS[!SPECS$d %in% mainD,]))
  
  
  SPECS <- rbind(
    data.frame(SPECS[ (SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% technology_variables[1] & SPECS$level %in% "Pooled"),], nnm="fullset"),
    data.frame(SPECS[ (SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% technology_variables[1] & SPECS$level %in% "Pooled"),], nnm="optimal"),
    data.frame(SPECS[!(SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% technology_variables[1] & SPECS$level %in% "Pooled"),], nnm="optimal"))

  SPECS

}


#' Generate Functional and Distribution Forms for MSF Models
#'
#' Builds algebraic strings for common production-function specifications and
#' assembles a catalog of distributional assumptions for one-sided inefficiency
#' terms used in stochastic frontier / multi-stage frontier (MSF) analysis.
#'
#' @param number_of_inputs Integer. Number of input variables to enumerate
#'   in the functional forms (creates symbols \code{I1..Ik} and \code{lnI1..lnIk}).
#'   Default \code{5}.
#' @param include_trend Logical. If \code{TRUE}, adjusts the Transcendental
#'   production form (\code{TP}) by dropping the last linear input term.
#'   See \emph{Note} regarding when \code{TP} is present. Default \code{FALSE}.
#'
#' @details
#' \strong{Functional forms (returned as character strings):}
#' \itemize{
#'   \item \code{CD}: Cobb-Douglas - sum of logs of inputs, e.g.,
#'         \code{lnI1 + lnI2 + ... + lnIk}.
#'   \item \code{TL}: Translog - \code{CD} plus all second-order log terms
#'         (squares and cross-products), e.g.
#'   % \item \code{LN}: Linear (commented out in current code).
#'   % \item \code{QD}: Quadratic (commented out).
#'   % \item \code{GP}: Generalized (commented out).
#'   % \item \code{TP}: Transcendental (commented out; see Note).
#' }
#'
#' Terms are generated programmatically for \code{k = number_of_inputs}, with:
#' \code{Ii} denoting input levels and \code{lnIi} their logarithms.
#'
#' \strong{Distributional forms (list of lists):}
#' Each entry is named and contains \code{list(name, scaling = <logical>)}.
#' Included options: \code{hnormal}, \code{tnormal}, \code{exponential},
#' \code{tslaplace}, \code{genexponential}, \code{tnormal_scaled},
#' \code{rayleigh}, \code{uniform}, \code{gamma}, \code{lognormal}, \code{weibull}.
#' The \code{scaling} flag indicates whether the Wang-Schmidt (2002) scaling
#' property is used in that specification.
#'
#' @return A \code{list} with two components:
#' \itemize{
#'   \item \code{fxnforms}: named list of character strings for functional forms
#'         (e.g., \code{$CD}, \code{$TL}). Forms that are commented out in the
#'         implementation will not appear.
#'   \item \code{distforms}: named list of distribution specifications; each is
#'         a list with elements \code{[1]} = distribution name and
#'         \code{scaling} = logical.
#' }
#'
#' @note
#' The current implementation defines \code{CD} and \code{TL} and removes NULL
#' entries before returning. Other forms (\code{LN}, \code{QD}, \code{GP}, \code{TP})
#' are commented out. If you plan to set \code{include_trend = TRUE}, ensure
#' \code{TP} is actually included in \code{fxnforms}; otherwise, modifying
#' \code{fxnforms$TP} will not have any effect (and would error if \code{TP}
#' is not defined).
#' @export
sf_functional_forms <- function(number_of_inputs=5, include_trend=FALSE) {
  
  # ---- validation
  if (!is.numeric(number_of_inputs) || length(number_of_inputs) != 1 || number_of_inputs < 1) {
    stop("`number_of_inputs` must be a single numeric value >= 1.")
  }
  number_of_inputs <- as.integer(number_of_inputs)
  
  # Initialize strings for different functional forms.
  lnInputs  <- 0
  Inputs    <- 0
  Quadratic <- 0
  Translog  <- 0
  
  # Construct the functional forms by iterating over the number of input variables.
  for(i in 1:number_of_inputs) {
    Inputs    <- paste0(Inputs, "+I", i)
    lnInputs  <- paste0(lnInputs, "+lnI", i)
    Quadratic <- paste0(Quadratic, "+I(1/2*I", i, "*I", i, ")")
    Translog  <- paste0(Translog, "+I(1/2*lnI", i, "*lnI", i, ")")
    for(j in 1:number_of_inputs) {
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
  
  # Modify the Transcendental Production Function if include_trend is TRUE.
  if(include_trend) fxnforms$TP <- gsub(paste0("[+]I", number_of_inputs), "", fxnforms$TP)
  
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

