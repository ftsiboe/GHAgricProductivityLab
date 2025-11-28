#' Build Model Specifications
#'
#' Construct a specification table for frontier analysis by
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
#'   \item Reorder to prioritize \code{mainD} and create an \code{matching_type} flag with
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
#'   \item \code{matching_type} - marker column (\code{"fullset"} / \code{"optimal"}).
#' }
#'
#' @return A \code{data.frame} (data.table-compatible) of model specifications with
#'   columns \code{disasg}, \code{level}, \code{TechVar}, \code{f}, \code{d}, and \code{matching_type}.
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
#' @import data.table
#' @family frontier analysis
#' @keywords internal
#' @noRd
sf_model_specifications <- function(
    data,
    distforms             = sf_functional_forms()$distforms,
    fxnforms              = sf_functional_forms()$fxnforms,
    technology_variables,
    mainF                 = 2,
    mainD                 = 1,
    demographic_variables = c("Female","Region","Ecozon","EduCat","EduLevel","AgeCat"),
    crop_list             = c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
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
                    DONE <- data.frame(disasg=w, level=unique(data[,w]), 
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
    data.frame(SPECS[ (SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% technology_variables[1] & SPECS$level %in% "Pooled"),], matching_type="fullset"),
    data.frame(SPECS[ (SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% technology_variables[1] & SPECS$level %in% "Pooled"),], matching_type="optimal"),
    data.frame(SPECS[!(SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% technology_variables[1] & SPECS$level %in% "Pooled"),], matching_type="optimal"))
  
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
#' @family frontier analysis
#' @keywords internal
#' @noRd
sf_functional_forms <- function(
    number_of_inputs = 5, 
    include_trend    = FALSE) {
  
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



#' Meta stochastic frontier (MSF) workhorse
#'
#' Performs Meta Stochastic Frontier (MSF) analysis in several stages:
#' \enumerate{
#'   \item \strong{Naive SF:} Fits a pooled stochastic frontier and computes
#'     baseline technical efficiency (TE) measures.
#'   \item \strong{Group SF:} If a technology variable \code{technology_variable} is supplied,
#'     splits the sample into technology groups and fits group-specific SF
#'     models.
#'   \item \strong{Meta SF (unmatched and matched):} Constructs a meta-frontier
#'     using fitted values from group models and, optionally, nearest-neighbor
#'     matching specifications. From this, it derives technology gap ratios
#'     (TGR) and meta-technical efficiency (MTE).
#'   \item \strong{Summaries and distributions:} Produces weighted and
#'     unweighted summaries and distributional statistics for efficiency,
#'     elasticity, and risk across technologies, samples (unmatched/matched),
#'     and surveys.
#' }
#'
#' The function is designed as a high-level orchestrator around
#' \code{\link{sf_workhorse}}, assembling naive, group-level, and meta-frontier
#' results into a unified output structure suitable for MSF analysis.
#'
#' @param data A data.frame or data.table containing the estimation sample.
#'   Must include the dependent variable \code{output_variable}, the inputs in \code{input_variables},
#'   the weight variable \code{weight_variable}, unique ID variables in \code{identifiers}, any
#'   inefficiency and risk covariates in \code{inefficiency_covariates} and \code{risk_covariates}, the
#'   technology variable \code{technology_variable} (if used), and any variables required by
#'   matching objects on disk (e.g., \code{"Surveyx"}, \code{"EaId"}, \code{"HhId"},
#'   \code{"Mid"}, \code{"unique_identifier"}).
#' @param output_variable Character scalar. Name of the dependent (output) variable used
#'   in the production frontier.
#' @param input_variables Character vector of input variable names (e.g., land, labor,
#'   capital) passed through to \code{\link{sf_workhorse}}.
#' @param inefficiency_covariates Optional named list specifying variables in the inefficiency
#'   function for the naive and group frontiers. Typical structure:
#'   \code{list(scalar_variables = c(...), factor_variables = c(...))}.
#' @param risk_covariates Optional named list specifying variables in the production
#'   risk (noise) function for the naive and group frontiers. Same structure
#'   as \code{inefficiency_covariates}. If \code{NULL}, a homoskedastic noise term is usually
#'   implied.
#' @param weight_variable Character scalar. Name of the sampling/observation weight
#'   variable in \code{data}. Observations with zero weight are dropped.
#' @param production_slope_shifters Character scalar giving the name of a slope-shifter
#'   variable in \code{data} (e.g., technology shifter, policy dummy). Defaults
#'   to \code{NULL} for no slope shifter.
#' @param intercept_shifters Optional named list of intercept shifter variables
#'   for the naive and group frontiers. Typical structure:
#'   \code{list(scalar_variables = c(...), factor_variables = c(...))}, passed to
#'   \code{\link{sf_workhorse}}.
#' @param f Functional form index for the production frontier (e.g.,
#'   Cobb-Douglas, translog, quadratic). The index is passed to
#'   \code{\link{sf_workhorse}} and ultimately to the internal form-selection
#'   utilities (e.g., \code{sf_functional_forms}).
#' @param d Distributional form index for the inefficiency term (e.g.,
#'   half-normal, truncated-normal). Passed to \code{\link{sf_workhorse}}.
#' @param identifiers Character vector of variable names that uniquely identify
#'   observations (e.g., \code{c("unique_identifier","Survey","CropID","HhId","EaId","Mid")}).
#'   These IDs are used when merging scores and summarizing by technology or
#'   sample.
#' @param include_trend Logical; if \code{TRUE}, the last element in \code{input_variables} is
#'   treated as a trend/technology variable in the production function.
#'   Passed through to \code{\link{sf_workhorse}}. Defaults to \code{FALSE}.
#' @param technology_variable Optional character scalar naming the technology variable (e.g.,
#'   region, system, period) used to define groups for group SF and meta-frontier
#'   estimation. If \code{NULL}, no technology groups are formed and only the
#'   naive frontier and TE are computed.
#' @param matching_type Optional character scalar controlling which nearest-neighbor
#'   matching specifications to use for meta-frontier estimation. When not
#'   \code{NULL}, the function reads matching specifications and matched
#'   samples from \code{"results/matching/"} and related RDS files. Setting
#'   \code{matching_type = "optimal"} restricts attention to the subset of matching
#'   specifications labeled as optimal.
#' @param adoption_covariates Optional named list specifying inefficiency-function covariates
#'   for the meta-frontier (matched/unmatched) estimation. If \code{NULL},
#'   defaults to \code{inefficiency_covariates}.
#' @param intercept_shifters_meta Optional named list of intercept shifters for the
#'   meta-frontier estimation. If \code{NULL}, defaults to
#'   \code{intercept_shifters}.
#' @param match_specifications match specifications
#' @param match_specification_optimal match specifications
#' @param match_path match path
#' 
#' @details
#' The workflow can be summarized as:
#' \itemize{
#'   \item \strong{Naive SFA (TE0):} Calls \code{\link{sf_workhorse}} on the full
#'     sample to obtain baseline efficiencies (\code{teBC}, \code{teJLMS},
#'     \code{teMO}). If no technology variable is specified (\code{technology_variable = NULL}),
#'     these naive efficiencies are the final scores and only TE-related
#'     summaries are produced.
#'
#'   \item \strong{Group SFA (TE):} When \code{technology_variable} is provided, the sample
#'     is recoded into numeric technology groups (\code{Tech}), and
#'     \code{\link{sf_workhorse}} is applied separately to each group. 
#'     Group-level efficiencies are combined into TE measures by group and survey.
#'
#'   \item \strong{Meta-frontier (TGR and MTE):} Using fitted group-frontier
#'     values (\code{Yhat}) from the restricted models, the function fits
#'     meta-frontiers (unmatched and, optionally, matched) again via
#'     \code{\link{sf_workhorse}}. Technology gap ratios (TGR) are derived
#'     from these meta-frontiers, and meta-technical efficiency (MTE) is
#'     computed as \code{TE * TGR}.
#'
#'   \item \strong{Matching layer (optional):} If \code{matching_type} is given, the
#'     function loads matching specifications (\code{mspecs}, \code{mspecs_optimal})
#'     and corresponding matched samples from disk (e.g., \code{"results/matching/"}),
#'     fits additional meta-frontiers by matching specification, and augments
#'     the set of scores and summaries with these matched samples.
#'
#'   \item \strong{Summaries and distributions:}
#'     \describe{
#'       \item{Scores:}{TE0, TE, TGR, and MTE are reshaped into long form and
#'         aggregated to compute weighted/unweighted means, medians, modes, and
#'         distribution histograms (both count-based and weight-based) across
#'         surveys, samples, technologies, and estimation types. When
#'         \code{technology_variable} is not \code{NULL}, the function also computes
#'         technology gaps (level and percent) relative to the minimum
#'         technology in \code{  technology_legend}.}
#'       \item{Elasticities:}{Elasticities from the underlying SF models
#'         (naive, group, meta) are combined and summarized in a similar way,
#'         including technology-gap metrics for elasticities when \code{technology_variable}
#'         is provided.}
#'       \item{Risk:}{Risk measures derived from \code{sf_workhorse} are
#'         combined, summarized, and used to build distributional statistics
#'         analogous to those for efficiency.}
#'     }
#'
#'   \item \strong{LR tests:} When \code{technology_variable} is not \code{NULL}, the function
#'     builds likelihood ratio test statistics comparing a naive pooled
#'     frontier to the combination of group and meta-frontiers, storing these
#'     as rows with \code{CoefName = "LRT"} in the \code{sf_estm} output.
#' }
#'
#' Throughout, the function uses \pkg{dplyr}, \pkg{tidyr}, \pkg{data.table},
#' \pkg{doBy}, and related helper functions for reshaping and summarizing the
#' output of \code{\link{sf_workhorse}}.
#'
#' @return
#' A named list with the following components:
#' \itemize{
#'   \item \code{sf_estm}: Data.frame of coefficient-level summaries from all
#'     naive, group, and meta-frontiers (including LR tests), augmented with
#'     technology identifiers (\code{Tech}) and \code{sample} labels
#'     (e.g., \code{"unmatched"}, matching-spec names).
#'   \item \code{ef_mean}: Data.frame of aggregated efficiency statistics
#'     (TE0, TE, TGR, MTE), including weighted/unweighted means, medians, and
#'     modes, by survey, sample, technology, type, estimation type, and
#'     restriction status. When \code{technology_variable} is supplied, also includes
#'     efficiency gap levels and percentages.
#'   \item \code{ef_dist}: Data.frame of efficiency distributions (histogram
#'     counts and weights) over efficiency ranges, by survey, sample,
#'     technology, type, estimation type, and restriction.
#'   \item \code{rk_dist}: Data.frame of risk distributions (histogram counts
#'     and weights) analogous to \code{ef_dist}, with \code{type = "risk"}.
#'   \item \code{el_mean}: Data.frame of aggregated elasticity statistics
#'     (weighted/unweighted means, medians, modes) by input, survey, sample,
#'     technology, and restriction, including elasticity-gap measures when
#'     \code{technology_variable} is provided.
#'   \item \code{rk_mean}: Data.frame of aggregated risk statistics (weighted
#'     and unweighted) by survey, sample, technology, and restriction, with
#'     risk-gap measures when \code{technology_variable} is provided.
#'   \item \code{el_samp}: Data.frame of observation-level elasticities for
#'     all models and samples (excluding the synthetic \code{"GLSS0"} survey
#'     rows).
#'   \item \code{ef_samp}: Data.frame of observation-level efficiency scores
#'     (TE0, TE, TGR, MTE) including IDs, weights, technologies, and sample
#'     labels.
#'   \item \code{rk_samp}: Data.frame of observation-level risk measures for
#'     all models and samples (excluding the synthetic \code{"GLSS0"} survey
#'     rows).
#' }
#' @family frontier analysis
#' @import data.table
#' @export
msf_workhorse <- function(
    data ,
    output_variable,
    input_variables,
    inefficiency_covariates   = NULL,
    risk_covariates           = NULL,
    weight_variable           = NULL,
    production_slope_shifters = NULL,
    intercept_shifters        = NULL,
    f,
    d,
    identifiers,
    include_trend             = FALSE,
    technology_variable       = NULL,
    matching_type             = NULL,
    adoption_covariates       = NULL,
    intercept_shifters_meta   = NULL,
    match_path,
    match_specifications,
    match_specification_optimal){
 
  #---------------------------------------------------
  # Preliminaries                                  ####
  data <- data[!data[,weight_variable] %in% 0,]
  
  if(!is.null(technology_variable)){
    data$Tech <- as.numeric(as.integer(as.factor(as.character(data[,technology_variable]))))
    TechList  <- unique(data$Tech)
    
      technology_legend <- unique(data[c("Tech",technology_variable)])
      technology_legend <- technology_legend[order(  technology_legend$Tech),]
  }
  #---------------------------------------------------
  # SF Estimation [Naive TE]                       ####
  cat(crayon::green("SF Estimation [Naive TE]",Sys.time()),fill=T)
  sf_Naive <- sf_workhorse(
    data=data,
    output_variable=output_variable,
    input_variables=input_variables,weight_variable=weight_variable,d=d,f=f,
    production_slope_shifters=production_slope_shifters,
    intercept_shifters=intercept_shifters,
    inefficiency_covariates=inefficiency_covariates,
    risk_covariates=risk_covariates,
    identifiers=identifiers,
    include_trend=include_trend)
  if(is.null(technology_variable)){ 
    score <- sf_Naive$ef[names(sf_Naive$ef)[names(sf_Naive$ef) %in% c(identifiers,"teBC","teJLMS","teMO","restrict")]]
    score <- score |> tidyr::gather(estType, TE, names(score)[!names(score) %in% c(identifiers,"restrict")])
    score$sample <- "unmatched"
    score <- dplyr::inner_join(data[c(identifiers,weight_variable)],score,by=identifiers)
    score$Tech <- -999
    names(score)[names(score) %in% weight_variable] <- "weights"
  }
  # cc <- sf_Naive$ef
  # cc <- cc[cc$CoefName %in% "Nobs",]
  # cc
  #---------------------------------------------------
  # MSF Estimation                                 ####
  sf_Group <- NULL;mflist<-NULL
  if(!is.null(technology_variable)){
    #-------------------------------------------------
    # Group SF Estimation [Group TE]               ####
    
    sf_Group <- lapply(
      TechList,
      function(tech){
        DONE <- NULL
        #tryCatch({ 
        # tech <- 1
        cat(crayon::green("Group SF",tech,Sys.time()),fill=T)
        
        sfi <- sf_workhorse(
          data=data[data$Tech %in% tech,],
          output_variable=output_variable,
          input_variables=input_variables,weight_variable=weight_variable,d=d,f=f,
          production_slope_shifters=production_slope_shifters,
          intercept_shifters=intercept_shifters,
          inefficiency_covariates=inefficiency_covariates,
          risk_covariates=risk_covariates,
          identifiers=identifiers,
          include_trend=include_trend)
        
        DONE <- sfi
        #}, error=function(e){})
        return(DONE)})
    
    sf_Group <- Filter(Negate(is.null), sf_Group)
    
    #-------------------------------------------------
    # Meta SF Estimation unmatched [TGR]           ####
    
    cat(crayon::green("unmatched Meta SF",Sys.time()),fill=T)
    
    mf.data <- as.data.frame(
      data.table::rbindlist(
        lapply(
          1:length(sf_Group),
          function(tech){
            # tech <- 1
            mf.data <- sf_Group[[tech]]$ef
            mf.data$Yhat <- mf.data$mlFitted
            mf.data <- mf.data[mf.data$restrict %in% "Restricted",]
            return(mf.data[c(identifiers,"Yhat")])
          }), fill = TRUE))
    
    mf.data <- dplyr::inner_join(data,mf.data,by=identifiers)
    
    if(is.null(adoption_covariates)){
      adoption_covariates <- inefficiency_covariates 
    }
    
    if(is.null(intercept_shifters_meta)){
      intercept_shifters_meta <- intercept_shifters
    }
    
    mflist <- list(
      unmatched=sf_workhorse(
        data                      = mf.data,
        output_variable           = "Yhat",
        input_variables           = input_variables,
        weight_variable           = weight_variable,
        d                         = d,
        f                         = f,
        production_slope_shifters = production_slope_shifters,
        intercept_shifters        = intercept_shifters_meta,
        inefficiency_covariates   = adoption_covariates,
        identifiers               = identifiers,
        include_trend             = include_trend))
    
    #-------------------------------------------------
    # Meta SF Estimation matched [TGR]             ####
    if(!is.null(matching_type)){
    
      m.specs      <- match_specifications
      m.specs$name <- ifelse(m.specs$link %in% NA,m.specs$distance,m.specs$link)
      
      if(matching_type %in% "optimal"){
        m.specs <- m.specs[m.specs$method %in% match_specification_optimal$method,]
        m.specs <- m.specs[m.specs$distance %in% match_specification_optimal$distance,]
        m.specs <- m.specs[m.specs$link %in% match_specification_optimal$link,]
      }
      
      for(mm in c(1:nrow(m.specs))){
        tryCatch({
          # mm <- 1
          cat(crayon::green("matched Meta SF-",as.character(m.specs$name[mm]),Sys.time()),fill=T)
          mfm.data <- dplyr::inner_join(unique(readRDS(file.path(match_path,paste0("match_",stringr::str_pad(m.specs$ARRAY[mm],4,pad="0"),".rds")))$md),
                                        mf.data,by=c("Surveyx","EaId", "HhId", "Mid","unique_identifier"))
          mfm <- sf_workhorse(
            data=as.data.frame(mfm.data),
            output_variable="Yhat",
            input_variables=input_variables,weight_variable=weight_variable,d=d,f=f,
            production_slope_shifters=production_slope_shifters,
            intercept_shifters=intercept_shifters_meta,
            inefficiency_covariates=inefficiency_covariates,
            identifiers=identifiers,
            include_trend=include_trend)
          
          mflist[[as.character(m.specs$name[mm])]] <- mfm
          rm(mfm,mfm.data)
        }, error=function(e){})
      }
    }
    #-------------------------------------------------
    # Calculate Scores                             ####
    cat(crayon::green("Calculate scors",Sys.time()),fill=T)
    TE0 <- sf_Naive$ef[names(sf_Naive$ef)[names(sf_Naive$ef) %in% c(identifiers,"restrict","teBC","teJLMS","teMO")]]
    TE0 <- TE0 |> tidyr::gather(estType, TE0, names(TE0)[!names(TE0) %in% c(identifiers,"restrict")])
    
    TE <- as.data.frame(data.table::rbindlist(lapply(1:length(sf_Group),function(tech){sf_Group[[tech]]$ef}), fill = TRUE))
    TE <- TE[names(TE)[names(TE) %in% c(identifiers,"restrict","teBC","teJLMS","teMO")]]
    TE <- TE |> tidyr::gather(estType, TE, names(TE)[!names(TE) %in% c(identifiers,"restrict")])
    
    score <- as.data.frame(
      data.table::rbindlist(
        lapply(
          names(mflist),
          function(sample){
            DONE <- NULL
            tryCatch({ 
              # sample <- "euclidean"
              smf_score <- mflist[[sample]]$ef[names( mflist[[sample]]$ef)[names(mflist[[sample]]$ef) %in% c(identifiers,"restrict","teBC","teJLMS","teMO","weights")]]
              smf_score <- smf_score[smf_score$restrict %in% "Restricted",names(smf_score)[names(smf_score) %in% c(identifiers,"weights","teBC","teJLMS","teMO")]]
              smf_score <- smf_score |> tidyr::gather(estType, TGR, names(smf_score)[!names(smf_score) %in% c(identifiers,"weights")])
              
              smf_score <- dplyr::inner_join(dplyr::inner_join(TE0,TE,by=c(identifiers,"restrict","estType")) ,smf_score,by=c(identifiers,"estType"))
              smf_score$MTE <- smf_score$TE*smf_score$TGR
              smf_score$sample <- sample
              DONE <- smf_score
            }, error=function(e){})
            return(DONE)
          }), fill = TRUE))
    
    score <- dplyr::inner_join(data[c(identifiers,"Tech")],score,by=identifiers)
    
    #-------------------------------------------------
  }
  #---------------------------------------------------
  # Summary Estimates                              ####
  cat(crayon::green("Summary Estimates",Sys.time()),fill=T)
  Estimates <- sf_Naive$sf
  Estimates$Tech <- -999
  Estimates$sample <- "unmatched"
  
  if(!is.null(technology_variable)){
    Estimates <- list(Estimates)
    Estimates[[length(Estimates)+1]] <- as.data.frame(data.table::rbindlist(
      lapply(1:length(sf_Group),function(tech){
        sf <- sf_Group[[tech]]$sf;sf$Tech <- TechList[tech];sf$sample <- "unmatched"
        return(sf)
      }), fill = TRUE))
    
    Estimates[[length(Estimates)+1]] <- data.frame(
      data.table::rbindlist(
        lapply(names(mflist),function(sample){
          DONE <- NULL
          tryCatch({ 
            smf <- mflist[[sample]]$sf; smf$Tech <- 999
            smf$sample <- sample; DONE <- smf
          }, error=function(e){})
          return(DONE)}), fill = TRUE))
    
    Estimates <- as.data.frame(data.table::rbindlist(Estimates, fill = TRUE))
    Estimates$Estimate <- as.numeric(as.character(Estimates$Estimate))
    lrtest <- Estimates[Estimates$CoefName %in% c("Nobs","nXvar","nuZUvar","nvZVvar","mlLoglik"),]
    lrtest$CoefName <- ifelse(lrtest$CoefName %in% c("nXvar","nuZUvar","nvZVvar"),"npar",lrtest$CoefName)
    lrtest <- doBy::summaryBy(Estimate~Tech+CoefName+sample+restrict,data=lrtest,FUN=sum,keep.names = T,na.rm=T)
    lrtest <- lrtest[order(lrtest$sample,lrtest$Tech,lrtest$CoefName),]
    
    LL_Naive <- lrtest[(lrtest$CoefName %in% "mlLoglik" & lrtest$Tech %in% -999),c("restrict","Estimate")]
    DF_Naive <- lrtest[(lrtest$CoefName %in% "npar"     & lrtest$Tech %in% -999),c("restrict","Estimate")]
    
    LL_Group <- doBy::summaryBy(Estimate~type+restrict,FUN=sum,keep.names = T,na.rm=T,
                                data=lrtest[(lrtest$CoefName %in% "mlLoglik" & lrtest$Tech %in% TechList),])
    DF_Group <- doBy::summaryBy(Estimate~type+restrict,FUN=sum,keep.names = T,na.rm=T,
                                data=lrtest[(lrtest$CoefName %in% "npar" & lrtest$Tech %in% TechList),])
    
    LL_Meta <- lrtest[(lrtest$CoefName %in% "mlLoglik" & lrtest$Tech %in% 999),c("restrict","Estimate","sample")]
    DF_Meta <- lrtest[(lrtest$CoefName %in% "npar"     & lrtest$Tech %in% 999),c("restrict","Estimate","sample")]
    
    names(LL_Naive)[names(LL_Naive) %in% "Estimate"] <- "LL_Naive"
    names(DF_Naive)[names(DF_Naive) %in% "Estimate"] <- "DF_Naive"
    names(LL_Group)[names(LL_Group) %in% "Estimate"] <- "LL_Group"
    names(DF_Group)[names(DF_Group) %in% "Estimate"] <- "DF_Group"
    names(LL_Meta)[names(LL_Meta) %in% "Estimate"]   <- "LL_Meta"
    names(DF_Meta)[names(DF_Meta) %in% "Estimate"]   <- "DF_Meta"
    
    lrtest <- dplyr::inner_join(LL_Meta,DF_Meta,by=c("sample","restrict"))
    lrtest <- dplyr::inner_join(LL_Group,lrtest,by=c("restrict"))
    lrtest <- dplyr::inner_join(DF_Group,lrtest,by=c("restrict"))
    lrtest <- dplyr::inner_join(LL_Naive,lrtest,by=c("restrict"))
    lrtest <- dplyr::inner_join(DF_Naive,lrtest,by=c("restrict"))
    
    lrtest$LL1 <- lrtest$LL_Group + lrtest$LL_Meta
    lrtest$DF1 <- lrtest$DF_Group + lrtest$DF_Meta
    lrtest$LL0 <- lrtest$LL_Naive
    lrtest$DF0 <- lrtest$DF_Naive
    lrtest$Estimate <- -2*(lrtest$LL0-lrtest$LL1)
    lrtest$DF <- lrtest$DF1-lrtest$DF0
    lrtest$Pvalue <- 1-pchisq(lrtest$Estimate, df=lrtest$DF)
    
    lrtest <- lrtest[c("sample","restrict","Estimate","Pvalue")]
    lrtest$CoefName <- "LRT"
    lrtest$Tech <- 999
    
    Estimates <- data.table::rbindlist(list(Estimates,lrtest), fill = TRUE)
    
  }
  
  # check <- unique(Estimates[c("sample","Tech","CoefName")]);table(check$CoefName,check$sample)
  
  #---------------------------------------------------
  # Summary Elasticity                             ####
  cat(crayon::green("Summary Elasticity",Sys.time()),fill=T)
  Elasticity <- sf_Naive$el
  Elasticity$Tech <- -999
  Elasticity$sample <- "unmatched"
  
  if(!is.null(technology_variable)){
    Elasticity <- list(Elasticity)
    Elasticity[[length(Elasticity)+1]] <- as.data.frame(data.table::rbindlist(
      lapply(1:length(sf_Group),function(tech){
        sf <- sf_Group[[tech]]$el;sf$Tech <- TechList[tech];sf$sample <- "unmatched"
        return(sf)
      }), fill = TRUE))
    
    Elasticity[[length(Elasticity)+1]] <- as.data.frame(
      data.table::rbindlist(
        lapply(names(mflist),function(sample){
          DONE <- NULL
          tryCatch({ 
            # sample <- "euclidean"
            smf <- mflist[[sample]]$el; smf$Tech <- 999
            
            sfx <- as.data.frame(data.table::rbindlist(
              lapply(1:length(sf_Group),function(tech){sf <- sf_Group[[tech]]$el;sf$Tech <- TechList[tech]
              return(sf)
              }), fill = TRUE))
            
            sfx <- dplyr::inner_join(smf[c(identifiers,"weights")],sfx[c(identifiers,"restrict","Tech",names(sfx)[grepl("el",names(sfx))])],by=identifiers)
            
            smf <- as.data.frame(data.table::rbindlist(list(smf,sfx), fill = TRUE))
            
            smf$sample <- sample; DONE <- smf
          }, error=function(e){})
          return(DONE)}), fill = TRUE))
    
    Elasticity <- as.data.frame(data.table::rbindlist(Elasticity, fill = TRUE))
  }
  
  Elasticity00 <- Elasticity
  Elasticity00$Survey <- "GLSS0"
  Elasticity <- rbind(Elasticity,Elasticity00)
  rm(Elasticity00)
  
  Elasticity <- Elasticity |> 
    tidyr::gather(
      input, value, names(Elasticity)[grepl("el",names(Elasticity))])
  Elasticity$value <- as.numeric(as.character(Elasticity$value))
  Elasticity <- Elasticity[!Elasticity$value %in% c(NA,Inf,-Inf,NaN),]
  
  Elasticity_sample <- Elasticity
  
  Elasticity <- as.data.table(Elasticity)
  
  Elasticity <- sf_levels_and_gaps(
    dt                  = Elasticity,
    output_variable     = "value",
    aggregation_points  = c("Survey", "sample", "input", "restrict"),
    stub_name           = "elasticity",
    weight_variable     = "weights",
    technology_variable = technology_variable,
    technology_legend   = technology_legend
  )

  #---------------------------------------------------
  # Summary Scores                                 ####
  cat(crayon::green("Summary Scores",Sys.time()),fill=T)
  if(!is.null(technology_variable)){
    Estescors <- score |> tidyr::gather(type, value, c("TE0","TE","TGR","MTE"))
    Estescors <- Estescors[!Estescors$value %in% c(NA,Inf,-Inf,NaN),]
    Estescors00 <- Estescors
    Estescors00$Tech <- -999
    Estescors <- rbind(Estescors,Estescors00)
  }else{
    Estescors <- score |> tidyr::gather(type, value, c("TE"))
    Estescors <- Estescors[!Estescors$value %in% c(NA,Inf,-Inf,NaN),]
  }
  
  Estescors00 <- Estescors
  Estescors00$Survey <- "GLSS0"
  Estescors <- rbind(Estescors,Estescors00)
  rm(Estescors00)
  
  Estescors <- as.data.table(Estescors)
  
  Estescors <- sf_levels_and_gaps(
    dt                  = Estescors,
    output_variable     = "value",
    aggregation_points  = c("Survey", "sample", "type", "estType", "restrict"),
    stub_name           = "efficiency",
    weight_variable     = "weights",
    technology_variable = technology_variable,
    technology_legend   = technology_legend
  )

  #---------------------------------------------------
  # Summary Risk                                   ####
  cat(crayon::green("Summary Risk",Sys.time()),fill=T)
  Risk <- sf_Naive$rk
  Risk$Tech <- -999
  Risk$sample <- "unmatched"
  
  if(!is.null(technology_variable)){
    Risk <- list(Risk)
    Risk[[length(Risk)+1]] <- as.data.frame(data.table::rbindlist(
      lapply(1:length(sf_Group),function(tech){
        sf <- sf_Group[[tech]]$rk;sf$Tech <- TechList[tech];sf$sample <- "unmatched"
        return(sf)
      }), fill = TRUE))
    
    Risk[[length(Risk)+1]] <- as.data.frame(
      data.table::rbindlist(
        lapply(names(mflist)[!names(mflist) %in% "unmatched"],function(sample){
          DONE <- NULL
          tryCatch({ 
            # sample <- "unmatched"
            smf <- mflist[[sample]]$ef[identifiers]
            smf <- dplyr::inner_join(smf,Risk[[2]],by=identifiers)
            smf$sample <- sample; DONE <- smf
          }, error=function(e){})
          return(DONE)}), fill = TRUE))
    
    Risk <- as.data.frame(data.table::rbindlist(Risk, fill = TRUE))
    
  }
  
  Risk00 <- Risk
  Risk00$Survey <- "GLSS0"
  Risk <- rbind(Risk,Risk00)
  rm(Risk00)
  
  Risk <- Risk[!Risk$risk %in% c(NA,NaN,Inf,-Inf),]
  
  Risk_sample <- Risk
  
  Risk <- as.data.table(Risk)
  
  Risk <- sf_levels_and_gaps(
    dt                  = Risk,
    output_variable     = "risk",
    aggregation_points  = c("Survey", "sample", "restrict"),
    stub_name           = "risk",
    weight_variable     = "weights",
    technology_variable = technology_variable,
    technology_legend   = technology_legend
  )

  #---------------------------------------------------
  # Distribution bars- Scores                      ####
  cat(crayon::green("Distribution bars- Scores",Sys.time()),fill=T)
  if(!is.null(technology_variable)){
    dataFrq <- score |> tidyr::gather(type, value, c("TE0","TE","TGR","MTE"))
    dataFrq <- dataFrq[!dataFrq$value %in% c(NA,Inf,-Inf,NaN),]
    dataFrq00 <- dataFrq
    dataFrq00$Tech <- -999
    dataFrq <- rbind(dataFrq,dataFrq00)
  }else{
    dataFrq <- score |> tidyr::gather(type, value, c("TE"))
    dataFrq <- dataFrq[!dataFrq$value %in% c(NA,Inf,-Inf,NaN),]
  }
  dataFrq <- as.data.table(dataFrq)
  dataFrq <- rbind(dataFrq,copy(dataFrq)[,Survey := "GLSS0"])
  ef_dist <- compute_jenks_binned_shares(
    dt                 = dataFrq,
    output_variable    = "value",
    aggregation_points = c("Survey","sample","Tech","type","estType","restrict"),
    jenks              = seq(0,1,0.05),
    weight_variable    = "weights")
  rm(dataFrq)
  #---------------------------------------------------
  # Distribution bars- Risk                        ####
  cat(crayon::green("Distribution bars- Risk",Sys.time()),fill=T)
  dataFrq <- Risk_sample[!Risk_sample$risk %in% c(NA,Inf,-Inf,NaN),]
  dataFrq <- as.data.table(dataFrq)
  dataFrq <- rbind(dataFrq,copy(dataFrq)[,Survey := "GLSS0"])
  rk_dist <- compute_jenks_binned_shares(
    dt                 = dataFrq,
    output_variable    = "risk",
    aggregation_points = c("Survey","sample","Tech","restrict"),
    jenks              = seq(0,2,0.05),
    weight_variable    = "weights")
  rk_dist[,type := "risk"]
  rm(dataFrq)
  #---------------------------------------------------
  # Summary and export                             ####
  cat(crayon::green("Summary and export",Sys.time()),fill=T)
  res <- list(
    sf_estm = as.data.frame(Estimates),
    ef_mean = as.data.frame(Estescors),
    ef_dist = as.data.frame(ef_dist),
    rk_dist = as.data.frame(rk_dist),
    el_mean = as.data.frame(Elasticity),
    rk_mean = as.data.frame(Risk),
    el_samp = as.data.frame(Elasticity_sample[!Elasticity_sample$Survey %in% "GLSS0",]),
    ef_samp = as.data.frame(score),
    rk_samp = as.data.frame(Risk_sample[!Risk_sample$Survey %in% "GLSS0",]))
  #---------------------------------------------------
  return(res)
  #---------------------------------------------------
}



#' Stochastic frontier workhorse 
#'
#' Fits a stochastic frontier model for a given production specification and,
#' when monotonicity is sufficiently violated, re-estimates a
#' shape-constrained (restricted) frontier using minimum-distance methods.
#' The function wraps the underlying \code{sfaR} estimation routines and a set
#' helper utilities (e.g., \code{sf_functional_forms()}, \code{equation_editor()},
#' \code{fit_organizer()}, \code{translogEla()}, curvature/monotonicity
#' checks) to produce:
#' \enumerate{
#'   \item Unrestricted stochastic frontier estimates and diagnostics.
#'   \item Shape-constrained estimates that enforce monotonicity (and related
#'         regularity conditions) when needed.
#'   \item Observation-level efficiencies, fitted values, and risk measures.
#'   \item Observation-level elasticities, including a "returns-to-scale" term.
#' }
#'
#' @param data A data.frame or data.table containing all variables required for
#'   estimation, including the dependent variable \code{output_variable}, inputs
#'   \code{input_variables}, the weight variable \code{weight_variable} (if used), the unique ID
#'   variables listed in \code{identifiers}, and any variables referenced in
#'   \code{production_slope_shifters}, \code{intercept_shifters}, \code{inefficiency_covariates}, or
#'   \code{risk_covariates}.
#' @param output_variable Character scalar. Name of the dependent/output variable in
#'   \code{data} used for the production frontier.
#' @param input_variables Character vector of input variable names (e.g., land, labor,
#'   capital). The order of variables in \code{input_variables} determines how they map
#'   into the generic input labels \code{I1}, \code{I2}, \ldots used in the
#'   functional-form utilities.
#' @param weight_variable Optional character scalar. Name of the sampling/observation
#'   weight variable. If \code{NULL}, all observations are given unit weight.
#' @param d Distributional form index for the inefficiency term. This is passed
#'   to \code{sf_functional_forms()} and ultimately to \code{sfacross()} (e.g., to
#'   select half-normal, truncated-normal, etc.). The exact mapping is
#'   determined by \code{sf_functional_forms()}.
#' @param f Functional form index for the production frontier (e.g.,
#'   Cobb-Douglas, translog, quadratic), used to select from the list returned
#'   by \code{sf_functional_forms()}. The name of the chosen form (e.g., \code{"CD"},
#'   \code{"TL"}, \code{"QD"}, \code{"LN"}) influences logging of variables and
#'   regularity checks.
#' @param production_slope_shifters Character scalar giving the name of a slope-shifter
#'   variable in \code{data} (e.g., technology shift, policy dummy). Defaults
#'   to \code{NULL} for no slope shifter and is passed to
#'   \code{equation_editor()}.
#' @param intercept_shifters Optional named list of intercept shifter variables
#'   for the production function. Typical structure:
#'   \code{list(scalar_variables = c(...), factor_variables = c(...))}, where the specific
#'   interpretation is handled inside \code{equation_editor()}.
#' @param inefficiency_covariates Optional named list describing the inefficiency-function
#'   covariates. Typical structure:
#'   \code{list(scalar_variables = c(...), factor_variables = c(...))}, passed as
#'   \code{uhet} and \code{muhet} to \code{sfacross()} when non-\code{NULL}.
#' @param risk_covariates Optional named list describing the production-risk (noise)
#'   covariates. When non-\code{NULL}, used as \code{vhet} in
#'   \code{sfacross()} to allow heteroskedasticity of the noise term.
#' @param identifiers Character vector of variable names that uniquely identify
#'   observations (e.g., \code{c("unique_identifier","Survey","CropID","HhId","EaId","Mid")}).
#'   These IDs are carried through to efficiency, elasticity, and risk
#'   outputs.
#' @param include_trend Logical; if \code{TRUE}, the last element in \code{input_variables} is
#'   treated as a trend/technology variable and handled accordingly in the
#'   functional-form and elasticity calculations. Defaults to \code{FALSE}.
#'
#' @details
#' The function proceeds in several steps:
#' \enumerate{
#'   \item \strong{Setup and functional form selection:}
#'     Using \code{sf_functional_forms()}, the function determines the appropriate
#'     production functional form and distributional specification based on
#'     the number of inputs (\code{number_of_inputs}), the \code{f} and \code{d} indices,
#'     and the \code{include_trend} flag. This includes whether the model is specified
#'     in logs (e.g., CD/TL/GP/TP) or levels.
#'
#'   \item \strong{Variable construction and equation building:}
#'     Inputs in \code{input_variables} are mapped to generic labels (\code{I1}, \code{I2},
#'     \dots) and their log transforms are created when needed. The outcome
#'     variable is set to \code{Y} or \code{lnY}. The function then calls
#'     \code{equation_editor()} to construct the production, inefficiency,
#'     and risk equations used by \code{sfacross()}.
#'
#'   \item \strong{Unrestricted stochastic frontier estimation:}
#'     The function iterates over a grid of optimization methods
#'     (\code{nr}, \code{nm}, \code{bfgs}, \code{bhhh}, \dots) and tolerance
#'     settings, calling \code{sfacross()} until a successful convergence is
#'     reported. It extracts observation-level efficiencies via
#'     \code{sfaR::efficiencies()}, constructs fitted values, and builds an
#'     observation-level risk metric based on squared deviations from mean
#'     output.
#'
#'   \item \strong{Elasticities and regularity checks:}
#'     Using \code{fit_organizer()} and \code{translogEla()}, the function
#'     computes elasticities and returns-to-scale-type measures, and evaluates
#'     monotonicity and curvature via \code{translogCheckMono()} and
#'     \code{translogCheckCurvature()} (or a coefficient-sign check for CD/LN
#'     forms).
#'
#'   \item \strong{Shape-constrained frontier (if needed):}
#'     If the monotonicity measure \code{mono} falls below 0.80, the function
#'     constructs a set of linear restrictions using \code{translogMonoRestr()}
#'     and solves a quadratic programming problem (via \code{quadprog::solve.QP}
#'     and fall-back matrix inversions using \pkg{Matrix}, \pkg{MASS},
#'     \pkg{corpcor}) to obtain constrained coefficients. For TL/QD forms a
#'     constrained frontier (\code{lcFitted}) is computed with
#'     \code{translogCalc()}; for CD/LN forms, constrained coefficients are
#'     obtained via a SEM representation using \pkg{lavaan}.
#'
#'   \item \strong{Re-estimation under constraints:}
#'     Given the constrained frontier, the function rebuilds the production
#'     equation with \code{equation_editor()} and re-estimates a
#'     stochastic frontier (\code{sfc}) using the same optimization grid.
#'     Constrained efficiencies, risks, and elasticities are computed and
#'     summarized, and regularity checks are repeated.
#' }
#'
#' Unrestricted and restricted summaries are then stacked, with a
#' \code{restrict} flag (\code{"Unrestricted"} vs. \code{"Restricted"}), for
#' downstream comparison.
#'
#' @return
#' A named list with four data.frames:
#' \itemize{
#'   \item \code{sf}: Coefficient-level results combining unrestricted and,
#'     when applicable, restricted SFA estimates, plus monotonicity/curvature
#'     diagnostics. Columns include (at minimum) \code{CoefName},
#'     \code{Estimate}, \code{StdError}, \code{Zvalue}, \code{Pvalue}, and
#'     \code{restrict}.
#'   \item \code{ef}: Observation-level efficiency results (unrestricted and
#'     restricted), including the ID variables in \code{identifiers}, weights,
#'     efficiency measures (e.g., \code{u}), model-fitted values
#'     (\code{mlFitted}), and \code{restrict}.
#'   \item \code{el}: Observation-level elasticities and
#'     returns-to-scale-type measures. Includes IDs, weights, elasticity
#'     columns (e.g., \code{el1}, \code{el2}, \dots, and the summed
#'     elasticity), and \code{restrict}.
#'   \item \code{rk}: Observation-level risk measures (unrestricted and
#'     restricted), including IDs, weights, and \code{risk}, plus
#'     \code{restrict}.
#' }
#' @family frontier analysis
#' @import sfaR
#' @export
sf_workhorse <- function(
    data, 
    output_variable, 
    input_variables, 
    weight_variable           = NULL, 
    d, 
    f,
    production_slope_shifters = NULL,
    intercept_shifters        = NULL,
    inefficiency_covariates   = NULL,
    risk_covariates           = NULL,
    identifiers,
    include_trend             = FALSE) {
  
  # Number of independent variables
  number_of_inputs <- length(input_variables)
  xNames <- paste0("I", 1:number_of_inputs)
  
  # Get the functional forms and distribution forms
  SF_forms <- sf_functional_forms(number_of_inputs=number_of_inputs, include_trend=include_trend)
  FXN <- SF_forms$fxnforms[f]
  udist <- SF_forms$distforms[d][[1]][[1]]
  scaling <- SF_forms$distforms[d][[1]][[2]]
  
  # Determine if the dependent variable should be logged
  logDepVar <- names(FXN) %in% c("CD", "TL", "GP", "TP")
  if(logDepVar) xNames <- paste0("ln", xNames)
  
  # Set weights
  if(is.null(weight_variable)) data$weights <- 1
  if(!is.null(weight_variable)) data$weights <- data[, weight_variable]
  
  # Set the dependent variable
  data$Y <- data[, output_variable]
  if(logDepVar) data$lnY <- log(data$Y)
  
  # Create the independent variables
  for(i in 1:number_of_inputs) {
    data[, paste0("I", i)] <- data[, input_variables[i]]  
    if(logDepVar) data[, paste0("lnI", i)] <- log(data[, paste0("I", i)] + 0.00001)
  }
  
  # Include trend variable if specified
  if(include_trend) {
    data[, paste0("I", number_of_inputs)] <- data[, input_variables[number_of_inputs]]
    if(logDepVar) data[, paste0("lnI", number_of_inputs)] <- data[, input_variables[number_of_inputs]]
  }
  
  # Create the equations for the production function, inefficiency function, and risk function
  equations <- equation_editor(
    outcome=ifelse(logDepVar, "lnY", "Y"),
    data=data, FXN=FXN, production_slope_shifters=production_slope_shifters,
    intercept_shifters=intercept_shifters, inefficiency_covariates=inefficiency_covariates, risk_covariates=risk_covariates
  )
  
  # Attempt Stochastic Frontier Convergence via Multi-Step Search
  sf <- .sf_try_convergence(
    equations = equations,
    udist     = udist,
    scaling   = scaling,
    logDepVar = logDepVar,
    data      = data
  )
  
  # Extract efficiencies and fitted values
  ef <- sfaR::efficiencies(sf)
  ef <- data.frame(data[row.names(ef), c(identifiers, "weights")], ef)
  ef$mlFitted <- sf$dataTable$mlFitted
  if(logDepVar) { ef$mlFitted <- exp(ef$mlFitted) }

  data <- data[row.names(ef),]
  
  rk <- data.frame(data[c(identifiers, "weights")])
  if(logDepVar) {
    rk$ybar <- ef$mlFitted * exp(-ef$u)
    rk$vrbr <- (rk$ybar - exp(sf$dataTable$lnY))^2
  }
  if(!logDepVar) {
    rk$ybar <- ef$mlFitted - ef$u
    rk$vrbr <- (rk$ybar - sf$dataTable$Y)^2
  }
  rk$risk <- sqrt(rk$vrbr) / rk$ybar
  rk <- rk[c(identifiers, "weights", "risk")]
  
  # Summarize the results
  sf_res <- sfaR_summary(sf)
  fit_organizer_out <- fit_organizer(fit=sf, number_of_inputs=number_of_inputs, FXN=FXN)
  est_coef <- fit_organizer_out$est_coef
  est_vcov <- fit_organizer_out$est_vcov
  est_list <- fit_organizer_out$est_list
  el <- translogEla(xNames=xNames, data=data, coef=est_coef, dataLogged=logDepVar)
  names(el) <- paste0("el", 1:ncol(el))
  if(!include_trend) { el[, paste0("el", (ncol(el)+1))] <- rowSums(el) }
  if(include_trend) { el[, paste0("el", (ncol(el)+1))] <- rowSums(el[1:(ncol(el)-1)]) }
  el <- data.frame(data[c(identifiers, "weights")], el)
  
  # Check curvature and monotonicity for different functional forms
  if(names(FXN) %in% c("TL", "QD")) {
    mono_obs <- translogCheckMono(xNames = xNames, data=data, coef=est_coef, dataLogged=logDepVar)$obs
    curv_obs <- translogCheckCurvature(xNames = xNames, data=data, est_coef, convexity=F, quasi=TRUE, dataLogged=logDepVar)$obs
    curv <- mean(curv_obs, na.rm=T)
    mono <- mean(mono_obs, na.rm=T)
  }
  
  if(names(FXN) %in% c("CD", "LN")) {
    curv <- mono <- as.numeric(mean(as.numeric(est_coef[2:(number_of_inputs+1)] > 0), na.rm=T) %in% 1)
  }
  
  mc <- data.frame(CoefName=c("mono", "curv"), Estimate=c(mono, curv), StdError=NA, Zvalue=NA, Pvalue=NA)
  
  if(mono < 0.80) {
    # Use minimum distance estimation of restricted production function coefficients
    if(names(FXN) %in% c("TL", "QD")) {
      monoRestr <- translogMonoRestr(xNames=xNames, data=data, dataLogged=logDepVar)
      monoRestr <- monoRestr[rowMeans(is.finite(monoRestr)) %in% 1,]
      sf_minDist <- NULL
      
      if(is.null(sf_minDist)) {
        tryCatch({
          inv_est_vcov <- solve(est_vcov)
          sf_minDist <- quadprog::solve.QP(Dmat=inv_est_vcov, dvec=rep(0, length(est_coef)), Amat=t(monoRestr), bvec=-monoRestr %*% est_coef)
          if(is.null(sf_minDist)) {
            tryCatch({
              inv_est_vcov <- corpcor::make.positive.definite(inv_est_vcov)
              sf_minDist <- quadprog::solve.QP(Dmat=inv_est_vcov, dvec=rep(0, length(est_coef)), Amat=t(monoRestr), bvec=-monoRestr %*% est_coef)
            }, error=function(e){})
          }
        }, error=function(e){})
      }
      
      if(is.null(sf_minDist)) {
        tryCatch({
          inv_est_vcov <- Matrix::solve(est_vcov)
          sf_minDist <- quadprog::solve.QP(Dmat=inv_est_vcov, dvec=rep(0, length(est_coef)), Amat=t(monoRestr), bvec=-monoRestr %*% est_coef)
          if(is.null(sf_minDist)) {
            tryCatch({
              inv_est_vcov <- corpcor::make.positive.definite(inv_est_vcov)
              sf_minDist <- quadprog::solve.QP(Dmat=inv_est_vcov, dvec=rep(0, length(est_coef)), Amat=t(monoRestr), bvec=-monoRestr %*% est_coef)
            }, error=function(e){})
          }
        }, error=function(e){})
      }
      
      if(is.null(sf_minDist)) {
        tryCatch({
          inv_est_vcov <- MASS::ginv(est_vcov)
          sf_minDist <- quadprog::solve.QP(Dmat=inv_est_vcov, dvec=rep(0, length(est_coef)), Amat=t(monoRestr), bvec=-monoRestr %*% est_coef)
          if(is.null(sf_minDist)) {
            tryCatch({
              inv_est_vcov <- corpcor::make.positive.definite(inv_est_vcov)
              sf_minDist <- quadprog::solve.QP(Dmat=inv_est_vcov, dvec=rep(0, length(est_coef)), Amat=t(monoRestr), bvec=-monoRestr %*% est_coef)
            }, error=function(e){})
          }
        }, error=function(e){})
      }
      
      if(is.null(sf_minDist)) {
        tryCatch({
          inv_est_vcov <- matrix(0, nrow(est_vcov), nrow(est_vcov))
          diag(inv_est_vcov) <- 1
          sf_minDist <- quadprog::solve.QP(Dmat=inv_est_vcov, dvec=rep(0, length(est_coef)), Amat=t(monoRestr), bvec=-monoRestr %*% est_coef)
        }, error=function(e){})
      }
      
      if(is.null(sf_minDist)) {
        stop("Minimum-distance QP failed for all attempts; cannot construct constrained coefficients.")
      }
      
      est_coefc <- sf_minDist$solution + est_coef
      
      # Fitted frontier output of the restricted model (assuming efficiency == 1)
      lcFitted <- translogCalc(xNames=xNames, data=data, coef=est_coefc, dataLogged=logDepVar)
    }
    
    if(names(FXN) %in% c("CD", "LN")) {
      Y <- all.vars(equation_editor(outcome=ifelse(logDepVar, "lnY", "Y"), data=data, FXN=FXN)$prodfxn)[1]
      X <- all.vars(equation_editor(outcome=ifelse(logDepVar, "lnY", "Y"), data=data, FXN=FXN)$prodfxn[-2])
      data.sem <- data
      data.sem[, X] <- 0
      fit.sem <- lm(equations$prodfxn, data=data)
      fit.sem$coefficients <- coef(sf)
      data.sem$const <- predict(fit.sem, data.sem)
      data.sem[, X] <- data[, X]
      fit.sem <- lavaan::lavaan(
        model = paste(paste0(Y, ' ~ a_c*const+ ', paste0(paste0(paste0("a_", 1:length(X)), "^2*", X), collapse = "+")),
                      paste0(Y, ' ~ a_0*1'), paste0(Y, ' ~~ ', Y), sep = ' \n '), 
        constraints="a_c==1",
        data=data.sem, model.type="sem", estimator="ML"
      )
      lcFitted <- lavaan::lavPredictY(fit.sem) - data.sem$const
      fit.sem <- lavaan::summary(fit.sem, standardized=TRUE)
      fit.sem <- fit.sem$pe[grepl("a_", fit.sem$pe$label), c("label", "est")]
      fit.sem <- fit.sem[!fit.sem$label %in% "a_c",]
      fit.sem <- fit.sem[order(fit.sem$label),]
      est_coefc <- est_coef
      for(xx in fit.sem$label) {
        est_coefc[xx] <- fit.sem[fit.sem$label %in% xx, "est"]^2
      }
    }
    
    # Estimate stochastic frontier model with the constrained frontier
    data$lcFitted <- lcFitted
    equations <- equation_editor(
      outcome=ifelse(logDepVar, "lnY", "Y"),
      data=data, FXN="lcFitted", production_slope_shifters=production_slope_shifters,
      intercept_shifters=intercept_shifters, inefficiency_covariates=inefficiency_covariates, risk_covariates=risk_covariates
    )
    
    # Attempt Stochastic Frontier Convergence via Multi-Step Search
    sfc <- .sf_try_convergence(
      equations = equations,
      udist     = udist,
      scaling   = scaling,
      logDepVar = logDepVar,
      data      = data
    )
    
    est_coefca <- est_coefc * coef(sfc)["lcFitted"]
    est_coefca[1] <- est_coefca[1] + coef(sfc)["(Intercept)"]
    
    efc <- sfaR::efficiencies(sfc)
    efc <- data.frame(data[row.names(efc), c(identifiers, "weights")], efc)
    efc$mlFitted <- sfc$dataTable$mlFitted
    if(logDepVar) { efc$mlFitted <- exp(efc$mlFitted) }
    
    data <- data[row.names(efc),]
    
    rkc <- data.frame(data[c(identifiers, "weights")])
    if(logDepVar) {
      rkc$ybar <- efc$mlFitted * exp(-efc$u)
      rkc$vrbr <- (rkc$ybar - exp(sf$dataTable$lnY))^2
    }
    if(!logDepVar) {
      rkc$ybar <- efc$mlFitted - efc$u
      rkc$vrbr <- (rkc$ybar - sf$dataTable$Y)^2
    }
    rkc$risk <- sqrt(rkc$vrbr) / rkc$ybar
    rkc <- rkc[c(identifiers, "weights", "risk")]
    
    elc <- translogEla(xNames=xNames, data=data, coef=est_coefca, dataLogged=logDepVar)
    names(elc) <- paste0("el", 1:ncol(elc))
    if(!include_trend %in% TRUE){elc[,paste0("el",(ncol(elc)+1))] <- rowSums(elc)}
    if(include_trend %in% TRUE ){elc[,paste0("el",(ncol(elc)+1))] <- rowSums(elc[1:(ncol(elc)-1)])}
    elc <- data.frame(data[c(identifiers,"weights")],elc)
    
    sfc_res <- dplyr::full_join(data.frame(CoefName=est_list,est_coefca=names(est_coefca)[1:length(est_list)],Estimate_c=est_coefca[1:length(est_list)]),
                                sfaR_summary(sfc),by="CoefName")
    sfc_res <- sfc_res[!sfc_res$CoefName %in% c("lcFitted"),]
    sfc_res$Estimate_c <- ifelse(sfc_res$Estimate_c %in% NA,sfc_res$Estimate,sfc_res$Estimate_c)
    sfc_res <- sfc_res[c("CoefName","Estimate_c","Pvalue")]
    names(sfc_res) <- c("CoefName","Estimate","Pvalue")
    
    if(names(FXN) %in% c("TL","QD")){
      curv_c <- mean(translogCheckCurvature(xNames = xNames,data=data, est_coefca, convexity = F, quasi = TRUE ,dataLogged =logDepVar)$obs,na.rm=T)
      mono_c <- mean(translogCheckMono(xNames = xNames,data = data, coef = est_coefca,dataLogged =logDepVar)$obs,na.rm=T)
    }
    
    if(names(FXN) %in% c("CD","LN")){
      curv_c <- mono_c <- as.numeric(mean(as.numeric(est_coefca[2:(number_of_inputs+1)] > 0),na.rm=T) %in% 1)
    }
    
    mc_c <- data.frame(CoefName=c("mono","curv"),Estimate=c(mono_c,curv_c),StdError=NA,Zvalue=NA,Pvalue=NA)
    
  }else{
    sfc_res <- sf_res
    mc_c    <- mc
    elc     <- el
    efc     <- ef
    rkc     <- rk
  }
  
  sf_res$restrict  <- "Unrestricted"
  sfc_res$restrict <- "Restricted"
  mc$restrict   <- "Unrestricted"
  mc_c$restrict <- "Restricted"
  final_sf <- as.data.frame(data.table::rbindlist(list(sf_res,sfc_res,mc,mc_c), fill = TRUE))
  
  el$restrict  <- "Unrestricted"
  elc$restrict <- "Restricted"
  final_el <- as.data.frame(data.table::rbindlist(list(el,elc), fill = TRUE))
  
  ef$restrict  <- "Unrestricted"
  efc$restrict <- "Restricted"
  final_ef <- as.data.frame(data.table::rbindlist(list(ef,efc), fill = TRUE))
  
  rk$restrict  <- "Unrestricted"
  rkc$restrict <- "Restricted"
  final_rk <- as.data.frame(data.table::rbindlist(list(rk,rkc), fill = TRUE))
  
  res <- list(sf=final_sf,ef=final_ef,el=final_el,rk=final_rk)
  
  return(res)
}



#' Build production, inefficiency, and risk equations for SFA/MSF
#'
#' Constructs formula objects for the production function, the inefficiency
#' function, and the production risk function used in stochastic frontier and
#' meta-stochastic frontier estimations. The function takes a generic
#' functional form (\code{FXN}) and conditionally augments it with intercept
#' and slope shifters, as well as covariates for the inefficiency (\eqn{u})
#' and noise (\eqn{v}) equations.
#'
#' Continuous covariates are only included when their coefficient of variation
#' exceeds a small threshold (|CV| > 0.001), and categorical covariates are
#' included only if they have more than one level and each level has at least
#' a minimal share of the sample (>\code{0.01\%} of observations). This helps
#' avoid numerical issues from nearly-constant or extremely sparse regressors.
#'
#' @param data Data.frame or similar object containing all variables referenced
#'   in \code{FXN}, \code{outcome}, \code{production_slope_shifters},
#'   \code{intercept_shifters}, \code{inefficiency_covariates}, and \code{risk_covariates}.
#' @param FXN Functional form of the production function. Typically a named
#'   object where \code{FXN[[1]]} is a character string representing the
#'   production frontier in terms of input variables (e.g.,
#'   \code{"lnI1 + lnI2 + I(1/2 * lnI1^2) + lnI1:lnI2"} for a translog).
#' @param outcome Character scalar giving the dependent variable name for the
#'   production equation (e.g., \code{"Y"} or \code{"lnY"}).
#' @param production_slope_shifters Character scalar naming a variable used as a slope
#'   shifter in the production function. If equal to \code{NULL} (default),
#'   no slope shifter is applied. Otherwise, the production equation is
#'   specified as an interaction between \code{factor(production_slope_shifters)} and
#'   \code{FXN[[1]]}, net of the baseline \code{FXN[[1]]}.
#' @param intercept_shifters Optional list of intercept shifters for the
#'   production function with elements:
#'   \itemize{
#'     \item \code{scalar_variables}: character vector of continuous shifter variables.
#'     \item \code{factor_variables}: character vector of categorical shifter variables
#'           (included as \code{factor()} terms).
#'   }
#'   If \code{intercept_shifters} is \code{NULL} or both components are
#'   \code{NULL}, the production function only uses \code{FXN[[1]]}.
#' @param inefficiency_covariates Optional list of covariates for the inefficiency (\eqn{u})
#'   equation with elements:
#'   \itemize{
#'     \item \code{scalar_variables}: character vector of continuous covariates.
#'     \item \code{factor_variables}: character vector of categorical covariates
#'           (included as \code{factor()} terms).
#'   }
#'   If \code{inefficiency_covariates} is \code{NULL} or both components are \code{NULL},
#'   the inefficiency equation is set to \code{NULL}.
#' @param risk_covariates Optional list of covariates for the production risk (\eqn{v})
#'   equation with elements:
#'   \itemize{
#'     \item \code{scalar_variables}: character vector of continuous covariates.
#'     \item \code{factor_variables}: character vector of categorical covariates
#'           (included as \code{factor()} terms).
#'   }
#'   If \code{risk_covariates} is \code{NULL} or both components are \code{NULL},
#'   the risk equation is set to \code{NULL}.
#'
#' @details
#' The function builds three formula objects:
#' \describe{
#'   \item{Production function (\code{prodfxn})}{
#'     Starts from \code{outcome ~ FXN[[1]]}. If intercept shifters are
#'     supplied, they are appended as additive terms (continuous variables
#'     directly, categorical variables as \code{factor()}). When a
#'     \code{production_slope_shifters} is specified (not \code{NULL}), the main
#'     production part is reparameterized as:
#'     \preformatted{
#'     outcome ~ factor(production_slope_shifters) * (FXN[[1]]) - (FXN[[1]]) + shifters
#'     }
#'   }
#'   \item{Inefficiency function (\code{uequ})}{
#'     Built as \code{~ 1 + ...} using continuous and/or categorical variables
#'     specified in \code{inefficiency_covariates}. If neither \code{scalar_variables} nor
#'     \code{factor_variables} are supplied (or all are filtered out), \code{uequ} is
#'     set to \code{NULL}.
#'   }
#'   \item{Risk function (\code{vequ})}{
#'     Built analogously to \code{uequ} using \code{risk_covariates}. If no valid
#'     covariates remain, \code{vequ} is set to \code{NULL}.
#'   }
#' }
#'
#' In all three parts, continuous and categorical candidate variables are
#' screened to avoid near-constant or extremely sparse regressors:
#' \itemize{
#'   \item Continuous variables are retained only if
#'     \eqn{| sd(x) / mean(x) | > 0.001}.
#'   \item Categorical variables are retained only if they have more than one
#'     level and the smallest category represents at least \code{0.01\%} of the
#'     sample.
#' }
#'
#' @return A list with three components:
#' \itemize{
#'   \item \code{prodfxn}: a \code{\link[stats]{formula}} for the production
#'     frontier.
#'   \item \code{uequ}: a \code{formula} for the inefficiency equation, or
#'     \code{NULL} if no inefficiency covariates were retained.
#'   \item \code{vequ}: a \code{formula} for the production risk equation, or
#'     \code{NULL} if no risk covariates were retained.
#' }
#'
#' @importFrom stats as.formula sd
#' @family frontier analysis
#' @keywords internal
#' @noRd
equation_editor <- function(
    data,
    FXN,
    outcome,
    production_slope_shifters = NULL,
    intercept_shifters        = NULL,
    inefficiency_covariates   = NULL,
    risk_covariates           = NULL){
  
  #---------------------------------------------------
  # Production function                            ####
  # Initialize the shifters formula with an intercept term
  shifters <- "~1"
  
  # Loop through the continuous intercept shifters (scalar_variables) and add them to the shifters formula
  if(!is.null(intercept_shifters$scalar_variables)){
    for(sv in intercept_shifters$scalar_variables){
      # Only add the variable if its coefficient of variation is greater than a small threshold
      if(abs((sd(data[,sv],na.rm=T)/mean(data[,sv],na.rm=T))) > 0.001){ 
        shifters <- paste0(shifters,"+",sv)
      }
    }
  }
  
  # Loop through the categorical intercept shifters (factor_variables) and add them to the shifters formula as factors
  if(!is.null(intercept_shifters$factor_variables)){
    for(fv in intercept_shifters$factor_variables){
      # Only add the variable if it has more than one unique value and each category has a sufficient number of observations
      if(length(unique(as.character(data[,fv]))) > 1 & min(table(as.character(data[,fv]))/nrow(data))>0.0001){
        shifters <- paste0(shifters,"+",paste0("factor(",fv,")"))
      } 
    }
  }
  
  # If no intercept shifters are provided, create the production function formula using only the FXN
  if(is.null(intercept_shifters$scalar_variables) & is.null(intercept_shifters$factor_variables)){
    prodfxn <- as.formula(paste0(outcome," ~",FXN[[1]]))
  } else {
    # Remove the initial intercept term if other shifters are present
    shifters <- gsub("~1[+]","",shifters)
    # Create the production function formula with the FXN and shifters
    prodfxn <- as.formula(paste0(outcome,"~",FXN[[1]],"+",shifters))
    
    # If a slope shifter is provided, modify the production function formula accordingly
    if(!is.null(production_slope_shifters)){
      prodfxn <- as.formula(paste0(paste0(outcome,"~factor(",production_slope_shifters,")*(",FXN[[1]],") - (",FXN[[1]],")"),"+",shifters))
    }
  }
  
  #---------------------------------------------------
  # Inefficiency function                          ####
  # Initialize the inefficiency function formula with an intercept term
  uequ <- "~1"
  
  # Loop through the continuous variables in inefficiency_covariates (scalar_variables) and add them to the inefficiency function formula
  if(!is.null(inefficiency_covariates$scalar_variables)){
    for(sv in inefficiency_covariates$scalar_variables){
      # Only add the variable if its coefficient of variation is greater than a small threshold
      if(abs((sd(data[,sv],na.rm=T)/mean(data[,sv],na.rm=T))) > 0.001){ 
        uequ <- paste0(uequ,"+",sv)
      }
    }
  }
  
  # Loop through the categorical variables in inefficiency_covariates (factor_variables) and add them to the inefficiency function formula as factors
  if(!is.null(inefficiency_covariates$factor_variables)){
    for(fv in inefficiency_covariates$factor_variables){
      # Only add the variable if it has more than one unique value and each category has a sufficient number of observations
      if(length(unique(as.character(data[,fv]))) > 1 & min(table(as.character(data[,fv]))/nrow(data))>0.0001){
        uequ <- paste0(uequ,"+",paste0("factor(",fv,")"))
      } 
    }
  }
  
  # If no variables are provided for the inefficiency function, set uequ to NULL
  if(is.null(inefficiency_covariates$scalar_variables) & is.null(inefficiency_covariates$factor_variables)){
    uequ <- NULL
  } else {
    # Remove the initial intercept term if other variables are present
    uequ <- gsub("~1[+]","~",uequ)
    # Convert the inefficiency function formula to a formula object
    uequ <- as.formula(uequ)
  }
  
  #---------------------------------------------------
  # Production Risk function                       ####
  # Initialize the production risk function formula with an intercept term
  vequ <- "~1"
  
  # Loop through the continuous variables in risk_covariates (scalar_variables) and add them to the production risk function formula
  if(!is.null(risk_covariates$scalar_variables)){
    for(sv in risk_covariates$scalar_variables){
      # Only add the variable if its coefficient of variation is greater than a small threshold
      if(abs((sd(data[,sv],na.rm=T)/mean(data[,sv],na.rm=T))) > 0.001){ 
        vequ <- paste0(vequ,"+",sv)
      }
    }
  }
  
  # Loop through the categorical variables in risk_covariates (factor_variables) and add them to the production risk function formula as factors
  if(!is.null(risk_covariates$factor_variables)){
    for(fv in risk_covariates$factor_variables){
      # Only add the variable if it has more than one unique value and each category has a sufficient number of observations
      if(length(unique(as.character(data[,fv]))) > 1 & min(table(as.character(data[,fv]))/nrow(data))>0.0001){
        vequ <- paste0(vequ,"+",paste0("factor(",fv,")"))
      } 
    }
  }
  
  # If no variables are provided for the production risk function, set vequ to NULL
  if(is.null(risk_covariates$scalar_variables) & is.null(risk_covariates$factor_variables)){
    vequ <- NULL
  } else {
    # Remove the initial intercept term if other variables are present
    vequ <- gsub("~1[+]","~",vequ)
    # Convert the production risk function formula to a formula object
    vequ <- as.formula(vequ)
  }
  
  #---------------------------------------------------
  # Return a list containing the formulas for the production function, inefficiency function, and production risk function
  return(list(prodfxn=prodfxn, uequ=uequ, vequ=vequ))
}


#' Organize frontier coefficients and variance-covariance matrix
#'
#' Extracts and structures the coefficients and variance-covariance matrix
#' from a fitted stochastic frontier (or related) model, mapping them into a
#' generic parameterization compatible with flexible functional forms such as
#' Cobb-Douglas, translog, quadratic, and linear frontiers.
#'
#' For translog and quadratic specifications, the function identifies and
#' orders both first-order and second-order (interaction) terms. For
#' Cobb-Douglas and linear forms, it augments the estimated coefficients with
#' zero-valued second-order terms and builds a conformable variance-covariance
#' matrix (filled with zeros for the non-estimated interactions), so that
#' downstream routines can work with a common parameter vector and matrix.
#'
#' @param fit Fitted model object (typically the result of a call to
#'   \code{sfacross()} or a similar frontier regression). The object must
#'   support \code{coef()} and \code{vcov()} methods.
#' @param number_of_inputs Integer. Number of input variables in the production function.
#'   This determines how many first-order and second-order terms are expected.
#' @param FXN Named object describing the functional form of the production
#'   function. The name (i.e., \code{names(FXN)}) is used to branch between
#'   supported forms:
#'   \itemize{
#'     \item \code{"TL"}: translog.
#'     \item \code{"QD"}: quadratic.
#'     \item \code{"CD"}: Cobb-Douglas.
#'     \item \code{"LN"}: linear.
#'   }
#'   For \code{"TL"} and \code{"CD"}, inputs are expected to enter in logged
#'   form as \code{"lnI1"}, \code{"lnI2"}, \dots; for \code{"QD"} and
#'   \code{"LN"}, in levels as \code{"I1"}, \code{"I2"}, \dots.
#'
#' @details
#' The function constructs three key objects:
#' \itemize{
#'   \item \strong{est_list}: character vector of coefficient names as they
#'     appear in the fitted model object (e.g., \code{"(Intercept)"},
#'     \code{"lnI1"}, \code{"lnI2"}, interaction terms such as
#'     \code{"I(1/2 * lnI1 * lnI1)"} or \code{"lnI1:lnI2"}, etc.).
#'   \item \strong{est_name}: generic parameter labels corresponding to the
#'     structural coefficients, such as \code{"a_0"}, \code{"a_1"}, \dots for
#'     first-order terms and \code{"b_i_j"} for second-order terms.
#'   \item \strong{est_coef} and \strong{est_vcov}: numeric vector of
#'     coefficients and conformable variance-covariance matrix, reordered and
#'     expanded (for CD/LN) to match the generic parameterization in
#'     \code{est_name}.
#' }
#'
#' For Cobb-Douglas and linear specifications (\code{"CD"}, \code{"LN"}), only
#' first-order terms are typically estimated directly. The function therefore:
#' \enumerate{
#'   \item Copies the estimated coefficients for first-order terms.
#'   \item Appends zeros for all second-order coefficients so that the length
#'         matches the full parameterization implied by \code{number_of_inputs}.
#'   \item Builds a larger variance-covariance matrix with the original
#'         submatrix in the upper-left corner and zeros elsewhere.
#' }
#'
#' This unified representation is useful for elasticities, curvature checks,
#' and minimum-distance procedures that require a full set of parameters and a
#' matching variance-covariance matrix regardless of the underlying functional
#' form.
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{est_coef}: Numeric named vector of organized coefficients.
#'     Names follow the generic parameter labels \code{a_0}, \code{a_1},
#'     \dots, \code{b_i_j}.
#'   \item \code{est_vcov}: Square variance-covariance matrix corresponding to
#'     \code{est_coef}, with row/column names matching the coefficient names.
#'   \item \code{est_list}: Character vector of coefficient names as they were
#'     extracted from \code{fit} (i.e., the original model coefficient labels
#'     used to build \code{est_coef} and \code{est_vcov}).
#' }
#' @family frontier analysis
#' @keywords internal
#' @noRd
fit_organizer <- function(fit, number_of_inputs, FXN) {
  
  # Check if the functional form is one of the specified types
  if(names(FXN) %in% c("TL", "QD", "CD", "LN")) {
    # Determine the prefix for the input variables based on the functional form
    tf <- ifelse(names(FXN) %in% c("TL", "CD"), "lnI", "I")
    
    # Initialize the list of estimated coefficients and their names
    est_list <- c("(Intercept)", names(coef(fit))[names(coef(fit)) %in% paste0(tf, 1:number_of_inputs)])
    est_name <- c(paste0("a_", 0:number_of_inputs))
    
    # Initialize the list of input variable names
    xNames <- paste0(tf, 1:number_of_inputs)
    
    # Loop through all pairs of input variables to construct the interaction terms
    for(i in 1:number_of_inputs) {
      for(j in 1:number_of_inputs) {
        # For diagonal elements (interaction of a variable with itself)
        if(i == j) {
          est_name <- c(est_name, paste0("b_", i, "_", j))
          if(names(FXN) %in% c("TL", "QD")) est_list <- c(est_list, paste0("I(1/2 * ", tf, i, " * ", tf, j, ")"))
        }
        # For off-diagonal elements (interaction of different variables)
        if(i < j) {
          est_name <- c(est_name, paste0("b_", i, "_", j))
          if(names(FXN) %in% c("TL", "QD")) est_list <- c(est_list, paste0(tf, i, ":", tf, j))
        }
      }
    }
  }
  
  # Extract the coefficients and the variance-covariance matrix for the estimated terms
  est_coef <- coef(fit)[est_list]
  est_vcov <- vcov(fit)[est_list, est_list]
  
  # If the functional form is Cobb-Douglas or Linear, handle the zero coefficients for interaction terms
  if(names(FXN) %in% c("CD", "LN")) {
    # Add zero coefficients for interaction terms
    est_coef <- c(est_coef, rep(0, (length(est_name) - length(est_coef))))
    
    # Initialize a zero matrix for the variance-covariance matrix
    est_vcov_cdLN <- matrix(ncol=length(est_coef), nrow=length(est_coef), data=0)
    
    # Loop through the variance-covariance matrix to fill in the values
    for(ii in 1:nrow(est_vcov)) {
      for(jj in 1:nrow(est_vcov)) {
        est_vcov_cdLN[[ii, jj]] <- est_vcov[[ii, jj]]
      }
    }
    
    # Update the variance-covariance matrix with the new matrix
    est_vcov <- est_vcov_cdLN
    rm(est_vcov_cdLN)
  }
  
  # Set the column and row names of the variance-covariance matrix and the names of the coefficients
  colnames(est_vcov) <- rownames(est_vcov) <- names(est_coef) <- est_name
  
  # Return the organized coefficients and variance-covariance matrix
  return(list(est_coef=est_coef, est_vcov=est_vcov, est_list=est_list))
}



#' Summarize a stochastic frontier model fitted with sfaR
#'
#' Builds a tidy coefficient-and-test table from a fitted stochastic frontier
#' model (typically from \pkg{sfaR}). The output combines:
#' \itemize{
#'   \item Maximum likelihood estimates of the frontier and auxiliary
#'         parameters (coefficients, standard errors, z-values, p-values), and
#'   \item Model-level diagnostics and tests, including variance components,
#'         skewness tests for OLS residuals, and a likelihood-ratio test for
#'         the presence of inefficiency.
#' }
#'
#' This function is intended as a convenient post-estimation summarizer whose
#' output can be merged with other model summaries or used directly in tables
#' and figures.
#'
#' @param fit A fitted stochastic frontier model object from \pkg{sfaR}. The
#'   object must support \code{summary()} and contain elements such as
#'   \code{mlLoglik}, \code{olsSkew}, \code{olsM3Okay}, \code{CoelliM3Test},
#'   and \code{AgostinoTest} (as produced by \pkg{sfaR} fits).
#'
#' @details
#' The function proceeds in several steps:
#' \enumerate{
#'   \item Extracts the maximum-likelihood coefficient table from
#'     \code{summary(fit)$mlRes} and renames columns to
#'     \code{Estimate}, \code{StdError}, \code{Zvalue}, \code{Pvalue}.
#'
#'   \item Constructs a one-row data.frame (\code{TEST}) containing
#'     model-level statistics returned by \code{summary(fit)}, such as:
#'     \code{Nobs}, \code{nXvar}, \code{nuZUvar}, \code{nvZVvar},
#'     \code{mlLoglik}, \code{AIC}, \code{BIC}, \code{HQIC}, \code{sigmavSq},
#'     \code{sigmauSq}, \code{Varu}, \code{Eu}, and \code{Expu}. From these,
#'     it computes:
#'     \itemize{
#'       \item \code{Sigma = sqrt(sigmauSq + sigmavSq)}, and
#'       \item \code{Gamma = sigmauSq / (Sigma^2)}.
#'     }
#'     It also overwrites \code{mlLoglik} with \code{fit$mlLoglik}, adds OLS
#'     skewness (\code{olsSkew}), a flag for the expected skewness
#'     (\code{olsM3Okay}), and reshapes everything into a
#'     \code{CoefName/Estimate} table with \code{Pvalue = NA}.
#'
#'   \item Appends additional tests:
#'     \describe{
#'       \item{Coelli's M3 test}{Stored as \code{CoefName = "CoelliM3Test"} with
#'         statistic and p-value taken from \code{fit$CoelliM3Test}.}
#'       \item{D'Agostino tests}{Three rows are added for
#'         \code{"AgostinoOmn"}, \code{"AgostinoSkw"}, and \code{"AgostinoKrt"},
#'         using \code{fit$AgostinoTest@test$statistic} and
#'         \code{fit$AgostinoTest@test$p.value}.}
#'       \item{LR test of inefficiency}{One row with
#'         \code{CoefName = "LRInef"}, where the test statistic is
#'         \code{summary(fit)$chisq}. The p-value is computed using a
#'         chi-bar-square distribution via \code{emdbook::qchibarsq()} at
#'         0.90, 0.95, and 0.99 quantiles, and then discretized to
#'         \code{0.10}, \code{0.05}, \code{0.01}, or \code{1}.}
#'     }
#' }
#'
#' Finally, the ML coefficient table and the TEST rows are stacked (using
#' \code{data.table::rbindlist()}) into a single data.frame with consistent
#' columns.
#'
#' @return A data.frame with at least the following columns:
#' \itemize{
#'   \item \code{CoefName}: Name of the parameter or diagnostic/statistic.
#'   \item \code{Estimate}: Point estimate or test statistic.
#'   \item \code{StdError}: Standard error (for ML coefficients; \code{NA} for
#'     many diagnostics).
#'   \item \code{Zvalue}: z-statistic for ML coefficients (\code{NA} otherwise).
#'   \item \code{Pvalue}: p-value for ML coefficients and tests when
#'     available (\code{NA} if not applicable).
#' }
#' @family frontier analysis
#' @keywords internal
#' @noRd
sfaR_summary <- function(fit){
  
  # Extract the maximum likelihood results from the fit summary and convert to a dataframe
  mlRes <- as.data.frame(summary(fit)$mlRes)
  # Rename the columns of the dataframe
  names(mlRes) <- c("Estimate", "StdError", "Zvalue", "Pvalue")
  # Add a column for the coefficient names
  mlRes$CoefName <- rownames(mlRes)
  
  # Create a dataframe to hold various statistics and test results
  TEST <- data.frame(
    Nobs     = ifelse(is.null(summary(fit)$Nobs)      , NA, summary(fit)$Nobs),
    nXvar    = ifelse(is.null(summary(fit)$nXvar)     , NA, summary(fit)$nXvar),
    nuZUvar  = ifelse(is.null(summary(fit)$nuZUvar)   , NA, summary(fit)$nuZUvar),
    nvZVvar  = ifelse(is.null(summary(fit)$nvZVvar)   , NA, summary(fit)$nvZVvar),
    mlLoglik = ifelse(is.null(summary(fit)$mlLoglik)  , NA, summary(fit)$mlLoglik),
    AIC      = ifelse(is.null(summary(fit)$AIC)       , NA, summary(fit)$AIC),
    BIC      = ifelse(is.null(summary(fit)$BIC)       , NA, summary(fit)$BIC),
    HQIC     = ifelse(is.null(summary(fit)$HQIC)      , NA, summary(fit)$HQIC),
    sigmavSq = ifelse(is.null(summary(fit)$sigmavSq)  , NA, summary(fit)$sigmavSq),
    sigmauSq = ifelse(is.null(summary(fit)$sigmauSq)  , NA, summary(fit)$sigmauSq),
    Varu     = ifelse(is.null(summary(fit)$Varu)      , NA, summary(fit)$Varu),
    Eu       = ifelse(is.null(summary(fit)$Eu)        , NA, summary(fit)$Eu),
    Expu     = ifelse(is.null(summary(fit)$Expu)      , NA, summary(fit)$Expu),
    optStatus= ifelse(is.null(summary(fit)$optStatus) , NA, summary(fit)$optStatus)
  )

  # Calculate additional statistics
  TEST$Sigma <- sqrt(TEST$sigmauSq + TEST$sigmavSq)
  TEST$Gamma <- TEST$sigmauSq / (TEST$Sigma^2)
  # Log-likelihood value of the M(S)L estimation
  TEST$mlLoglik <- fit$mlLoglik
  # Skewness of the residuals of the OLS estimation
  TEST$olsSkew <- fit$olsSkew    
  # Indicate whether the residuals of the OLS estimation have the expected skewness
  TEST$olsM3Okay <- as.numeric(fit$olsM3Okay %in% "Residuals have the expected skewness olsM3Okay")
  # Transpose the TEST dataframe
  TEST <- data.frame(coef = t(TEST[1,]), name = names(TEST))
  # Rename the columns of the TEST dataframe
  names(TEST) <- c("Estimate", "CoefName")
  # Add a column for p-values, initially set to NA
  TEST$Pvalue <- NA
  
  # Coelli's test for OLS residuals skewness
  TEST <- rbind(TEST, data.frame(CoefName = "CoelliM3Test", Estimate = fit$CoelliM3Test[1], Pvalue = fit$CoelliM3Test[2]))
  
  # D'Agostino's test for OLS residuals skewness
  TEST <- rbind(TEST, data.frame(
    CoefName = c("AgostinoOmn", "AgostinoSkw", "AgostinoKrt"),
    Estimate = fit$AgostinoTest@test$statistic,
    Pvalue = fit$AgostinoTest@test$p.value
  ))
  
  # Likelihood Ratio Test of Inefficiency
  TEST <- rbind(TEST, data.frame(
    CoefName = c("LRInef"),
    Estimate = summary(fit)$chisq,
    Pvalue = ifelse(summary(fit)$chisq >= emdbook::qchibarsq(0.99, df = summary(fit)$df), 0.01,
                    ifelse(summary(fit)$chisq >= emdbook::qchibarsq(0.95, df = summary(fit)$df), 0.05,
                           ifelse(summary(fit)$chisq >= emdbook::qchibarsq(0.90, df = summary(fit)$df), 0.10, 1)))
  ))
  
  # Combine the maximum likelihood results and the test results into one dataframe
  mlRes <- as.data.frame(data.table::rbindlist(list(mlRes, TEST), fill = TRUE))
  
  # Return the combined dataframe
  return(mlRes)
}



#' Attempt Stochastic Frontier Convergence via Multi-Step Search
#'
#' @description
#' Internal utility that repeatedly attempts to fit a stochastic frontier model
#' using `sfacross()` by cycling over multiple optimization methods, gradient
#' tolerances, and convergence tolerances.
#'
#' This helper exists because frontier models frequently fail to converge under
#' a single optimization configuration. The function implements a systematic,
#' silent fallback strategy that tries a grid of solver configurations until a
#' model reports `"SUCCESSFUL CONVERGENCE"` in `sf$optStatus` or all
#' combinations have been attempted.
#'
#' @details
#' The search pattern loops over:
#'
#' - **Gradient tolerances:** default `c(1e-6, 1e-3)`  
#' - **Function tolerances:** default `c(1e-12, 1e-6)`  
#' - **Optimization methods:** default  
#'   `c("nr", "nm", "bfgs", "bhhh", "cg", "sann", "ucminf",
#'      "mla", "sr1", "sparse", "nlminb")`
#'
#' Custom grids can be supplied via `sf_gradtol_options`, `sf_tol_options` and
#' `sf_method_options` if a narrower or different search is desired.
#'
#' For each combination, the function calls `sfacross()` with the appropriate
#' specification of heterogeneity equations (`uequ`, `vequ`) found in the input
#' `equations` list.
#'
#' Once `"SUCCESSFUL CONVERGENCE"` is detected, the function stops trying new
#' combinations and returns the last successful `sfacross()` result.
#'
#' @param equations A named list containing:
#'   - `prodfxn`: the production function formula  
#'   - `uequ`: (optional) formula for u-heterogeneity  
#'   - `vequ`: (optional) formula for v-heterogeneity
#'
#' @param udist Character string specifying the inefficiency distribution used
#'   by `sfacross()`.
#'
#' @param scaling Logical or numeric control passed to `sfacross()` to enable
#'   scaling of the model.
#'
#' @param logDepVar Logical indicating whether the dependent variable in the
#'   production function is logged.
#'
#' @param data A data frame containing variables required for the production
#'   frontier and heterogeneity equations.
#'
#' @param sf_gradtol_options Optional numeric vector of gradient tolerances to
#'   try. If `NULL`, defaults to `c(1e-6, 1e-3)`.
#'
#' @param sf_tol_options Optional numeric vector of function tolerances to try.
#'   If `NULL`, defaults to `c(1e-12, 1e-6)`.
#'
#' @param sf_method_options Optional character vector of optimization methods to
#'   try. If `NULL`, defaults to
#'   `c("nr", "nm", "bfgs", "bhhh", "cg", "sann", "ucminf",
#'      "mla", "sr1", "sparse", "nlminb")`.
#'
#' @return
#' The final `sf` object returned by the last successful call to `sfacross()`.
#' If no configuration converges, the object will contain whatever the last
#' failed attempt returned (typically with `optStatus` not indicating
#' convergence).
#' @family frontier analysis
#' @keywords internal
#' @noRd
.sf_try_convergence <- function(
    equations,
    udist,
    scaling,
    logDepVar,
    data,
    sf_gradtol_options = NULL,
    sf_tol_options     = NULL,
    sf_method_options  = NULL) {
  
  if (is.null(sf_gradtol_options)) {
    sf_gradtol_options <- c(1e-6, 1e-3)
  }
  
  if (is.null(sf_tol_options)) {
    sf_tol_options <- c(1e-12, 1e-6)
  }

  if (is.null(sf_method_options)) {
    sf_method_options <- c(
      "nr", "nm", "bfgs", "bhhh", "cg", "sann",
      "ucminf", "mla", "sr1", "sparse", "nlminb"
    )
  }
  
  # initialize a dummy sf object with optStatus field
  sfx <- list(optStatus = "")
  
  # Loop over combinations of gradient tolerances, function tolerances, and optimization methods
  for (sf_gradtol in sf_gradtol_options) {
    
    for (sf_tol in sf_tol_options) {
      
      for (sf_method in sf_method_options) {
        
        # Stop trying if convergence already achieved
        if (!grepl("SUCCESSFUL CONVERGENCE", toupper(sfx$optStatus))) {
          
          tryCatch({
            
            # CASE 1: no u-het & no v-het
            if (is.null(equations$uequ) && is.null(equations$vequ)) {
              sfx <- sfacross(
                formula   = equations$prodfxn,
                udist     = udist,
                scaling   = scaling,
                S         = 1,
                method    = sf_method,
                logDepVar = logDepVar,
                data      = data,
                gradtol   = sf_gradtol,
                tol       = sf_tol
              )
            }
            
            # CASE 2: u-het only
            if (!is.null(equations$uequ) && is.null(equations$vequ)) {
              sfx <- sfacross(
                formula   = equations$prodfxn,
                udist     = udist,
                uhet      = equations$uequ,
                muhet     = equations$uequ,
                scaling   = scaling,
                S         = 1,
                method    = sf_method,
                logDepVar = logDepVar,
                data      = data,
                gradtol   = sf_gradtol,
                tol       = sf_tol
              )
            }
            
            # CASE 3: v-het only
            if (is.null(equations$uequ) && !is.null(equations$vequ)) {
              sfx <- sfacross(
                formula   = equations$prodfxn,
                udist     = udist,
                vhet      = equations$vequ,
                scaling   = scaling,
                S         = 1,
                method    = sf_method,
                logDepVar = logDepVar,
                data      = data,
                gradtol   = sf_gradtol,
                tol       = sf_tol
              )
            }
            
            # CASE 4: both u-het and v-het
            if (!is.null(equations$uequ) && !is.null(equations$vequ)) {
              sfx <- sfacross(
                formula   = equations$prodfxn,
                uhet      = equations$uequ,
                muhet     = equations$uequ,
                vhet      = equations$vequ,
                udist     = udist,
                scaling   = scaling,
                S         = 1,
                method    = sf_method,
                logDepVar = logDepVar,
                data      = data,
                gradtol   = sf_gradtol,
                tol       = sf_tol
              )
            }
            
          }, error = function(e) {
            # silently ignore — next combination will be tried
          })
          
        } 
        
      } 
      
    } 
    
  } 
  
  return(sfx)
}



#' Compute Outcome Levels and Technology Gaps
#'
#' @description
#' Computes weighted and unweighted outcome levels (mean, median, mode) for a
#' specified output variable across user-defined aggregation points, and—when
#' technology information is supplied—computes level gaps and percent gaps
#' between each technology group and a baseline technology (the minimum value
#' of `Tech` in `technology_legend`).
#'
#' @details
#' The function proceeds in two major stages:
#'
#' **(1) Outcome Levels**
#' * Converts the input to a `data.table`.
#' * If `technology_variable` is provided and `dt` does not already contain
#'   a `Tech` column, it is created as `dt[ , Tech := get(technology_variable)]`.
#' * Computes, for each grouping defined in `aggregation_points` (and `Tech`
#'   when present), the following statistics for `output_variable`:
#'   - Weighted mean (if a weight variable is provided)
#'   - Unweighted mean
#'   - Median
#'   - Statistical mode (small helper function defined inside)
#'
#' The statistics are returned in long format with a common column
#' `CoefName = stub_name`.
#'
#' **(2) Technology Gaps (optional)**
#' If both `technology_variable` and `technology_legend` are provided (and
#' `technology_legend` contains a column `Tech`):
#'
#' * Restricts calculations to technologies listed in `technology_legend$Tech`.
#' * Identifies the minimum technology code (`min(technology_legend$Tech)`)
#'   as the baseline.
#' * Computes baseline means for each statistic across aggregation points.
#' * For all non-baseline technologies, computes:
#'   - **Level gap:** difference in estimate relative to the baseline.
#'   - **Percent gap:** level gap divided by the absolute baseline level.
#'
#' Gap estimates are appended to the output in long format, where the gap
#' variable names (`<stub_name>Gap_lvl`, `<stub_name>Gap_pct`) are stored
#' in `CoefName`.
#'
#' When technology inputs are not supplied, the function returns only outcome
#' level statistics (no gap variables).
#'
#' @param dt A `data.frame` or `data.table` containing the variables needed
#'   for analysis.
#' @param output_variable Character. Name of the numeric variable whose levels
#'   and gaps will be computed.
#' @param aggregation_points Character vector of variable names used to group
#'   observations (e.g., district, year, gender).
#' @param stub_name Character. Label assigned to `CoefName` for the level
#'   estimates, and used to construct gap variable names
#'   (`<stub_name>Gap_lvl`, `<stub_name>Gap_pct`).
#' @param weight_variable Character or `NULL`. Optional weighting variable for
#'   computing the weighted mean. If `NULL`, all units receive equal weight.
#' @param technology_variable Character or `NULL`. Optional variable identifying
#'   the technology group of each unit. When supplied, it is used to construct
#'   or interpret the `Tech` column and is required for gap computations.
#' @param technology_legend A `data.frame` or `data.table` mapping technology
#'   codes. Must contain a column named `Tech`. Required for gap computations.
#'
#' @return A long `data.table` containing:
#' * Outcome levels (`wmean`, `mean`, `median`, `mode`) for each group.
#' * Gap levels (`<stub_name>Gap_lvl`) and gap percentages (`<stub_name>Gap_pct`)
#'   when technology inputs are supplied.
#'
#' Columns always include:
#' * Aggregation variables (`aggregation_points`)
#' * `stat` — the statistic type
#' * `CoefName` — variable name (levels or gaps)
#' * `Estimate` — numeric value
#'
#' @family frontier analysis
#' @keywords internal
#' @noRd
sf_levels_and_gaps <- function(
    dt,
    output_variable,
    aggregation_points,
    stub_name,
    weight_variable     = NULL,
    technology_variable = NULL,
    technology_legend   = NULL
    ) {
      
      dt <- data.table::as.data.table(dt)
      
      # Helper: statistical mode (not base::mode)
      stat_mode <- function(x) {
        x <- x[!is.na(x)]
        if (!length(x)) return(NA_real_)
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      
      # Compute level statistics
      res <- dt[
        ,
        {
          x <- get(output_variable)
          w <- if (is.null(weight_variable)) rep(1, .N) else get(weight_variable)
          
          list(
            wmean  = if (all(is.na(x))) NA_real_ else stats::weighted.mean(x, w, na.rm = TRUE),
            mean   = mean(x, na.rm = TRUE),
            median = stats::median(x, na.rm = TRUE),
            mode   = stat_mode(x)
          )
        },
        by = c(aggregation_points,"Tech")
      ]
      
      # Wide -> long: wmean/mean/median/mode
      res_long <- tidyr::gather(res, "stat", "Estimate", c("wmean", "mean", "median", "mode"))
      res_long$CoefName <- stub_name
      res_long <- data.table::as.data.table(res_long)
      
      # If no technology info, we are done (levels only)
      if (is.null(technology_variable) || is.null(technology_legend) || !"Tech" %in% names(res_long)) {
        return(res_long[])
      }
      
      # ---- Gap computation block
      # Technologies to consider (and baseline)
      tech_levels <- unique(technology_legend$Tech)
      base_tech   <- min(tech_levels)
      
      # Restrict to requested technologies
      res_gap <- res_long[Tech %in% tech_levels]
      
      # Baseline mean by group + stat
      res_base <- res_gap[Tech == base_tech,.(Estimate_mean = mean(Estimate, na.rm = TRUE)),by = c(aggregation_points, "stat")]
      res_gap  <- res_gap[!Tech %in% base_tech, c(aggregation_points,"Tech","stat","Estimate"), with = FALSE]
      res_gap  <- res_base[res_gap, on = c(aggregation_points, "stat")];rm(res_base)
      
      # Level and percent gaps
      gap_lvl_name <- paste0(stub_name, "Gap_lvl")
      gap_pct_name <- paste0(stub_name, "Gap_pct")
      
      res_gap[, (gap_lvl_name) := Estimate - Estimate_mean]
      res_gap[!is.na(Estimate_mean) & abs(Estimate_mean) > 0, (gap_pct_name) := (get(gap_lvl_name) / abs(Estimate_mean)) * 100]
      res_gap[abs(Estimate_mean) == 0 | is.na(Estimate_mean),(gap_pct_name) := NA_real_]
      
      res_gap <- res_gap[, c(aggregation_points,"Tech","stat",gap_lvl_name,gap_pct_name), with = FALSE]
      # Long format for gaps; CoefName will be the gap variable name
      res_gap_long <- tidyr::gather(
        res_gap,
        "CoefName",
        "Estimate",
        c(gap_lvl_name, gap_pct_name)
      )
      res_gap_long <- data.table::as.data.table(res_gap_long)
      
      # Stack levels and gaps
      res_out <- data.table::rbindlist(
        list(res_long, res_gap_long),
        use.names = TRUE,
        fill = TRUE
      )
      
      res_out[]
}

