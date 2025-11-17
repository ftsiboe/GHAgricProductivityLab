
#' Run a single MSF draw and summarize stochastic frontier results
#'
#' Performs one iteration of multi-stage stochastic frontier (MSF) estimation
#' for a given \code{draw}. The function (i) filters the analysis sample using
#' a draw-specific exclusion list, (ii) calls \code{msf_workhorse()} to
#' estimate production, inefficiency, and risk components by survey, and
#' (iii) optionally builds disaggregated efficiency score summaries.
#'
#' The main outputs include model estimates, mean/percentile summaries, and
#' (optionally) disaggregated efficiency statistics by user-specified grouping
#' variables. For draws other than zero, sample-level results are dropped to
#' reduce storage.
#'
#' @param draw Integer (or numeric coercible to integer). Draw iteration index.
#'   Typically \code{0} is used for the full baseline sample and positive
#'   integers for resampled or matched draws.
#' @param drawlist A data.frame or similar object containing exclusion
#'   information by draw. Rows correspond to draws, with column \code{ID}
#'   identifying the draw and subsequent columns containing \code{EaId}
#'   values to remove from \code{data} for that draw.
#' @param surveyy Logical; if \code{TRUE}, uses the survey labels stored in
#'   \code{data$Surveyx}. If \code{FALSE} (default), all observations are
#'   assigned to a synthetic survey labeled \code{"GLSS0"} for estimation.
#' @param data A data.frame or data.table containing the estimation sample.
#'   Must include, at minimum, the columns referenced in other arguments
#'   (e.g., \code{output_variable}, \code{input_variables}, \code{identifiers}, \code{weight_variable}, technology
#'   and matching variables, and any variables used in
#'   \code{disagscors_list}).
#' @param output_variable Character scalar. Name of the dependent (output) variable used
#'   in the production frontier.
#' @param input_variables Character vector of input (production) variables entering the
#'   stochastic frontier.
#' @param inefficiency_covariates Optional named list specifying variables in the inefficiency
#'   function. Typical structure:
#'   \code{list(Svarlist = c(...), Fvarlist = c(...))}.
#' @param risk_covariates Optional named list specifying variables in the production
#'   risk (noise) function. If \code{NULL}, a homoskedastic noise term is
#'   usually implied.
#' @param weight_variable Optional character scalar giving the name of the sampling
#'   weight variable in \code{data}. Used when computing weighted summaries
#'   (e.g., weighted means) of efficiency scores.
#' @param production_slope_shifters Character scalar naming a variable that shifts the
#'   production-function slope (e.g., technology shifter). Defaults to
#'   \code{NULL} to indicate no slope shifter.
#' @param intercept_shifters Optional named list of intercept shifter
#'   variables for the baseline (unmatched) sample. Typical structure:
#'   \code{list(Svarlist = c(...), Fvarlist = c(...))}.
#' @param f Functional form identifier passed to
#'   \code{msf_workhorse()} (e.g., Cobb-Douglas, translog). The exact
#'   meaning is handled by the workhorse function.
#' @param d Distributional form identifier for the inefficiency term
#'   (e.g., half-normal, truncated-normal), passed through to
#'   \code{msf_workhorse()}.
#' @param identifiers Character vector of variable names that uniquely identify units
#'   (e.g., household, plot, or observation IDs). These are used to merge
#'   efficiency scores back to \code{data} and for disaggregated summaries.
#' @param include_trend Logical; if \code{TRUE}, includes a technology trend variable
#'   (given by \code{technology_variable}) in the frontier. Defaults to \code{FALSE}.
#' @param technology_variable Optional character scalar naming the technology (trend)
#'   variable to be used when \code{include_trend = TRUE}.
#' @param matching_type Optional variable or object controlling nearest-neighbor
#'   matching; passed to \code{msf_workhorse()}. The expected type and
#'   structure depend on the implementation of that workhorse function.
#' @param adoption_covariates Optional named list specifying inefficiency-function
#'   variables for the matched sample (post-matching specification). Same
#'   structure as \code{inefficiency_covariates}.
#' @param intercept_shifters_meta Optional named list of intercept shifters for
#'   the matched sample. Same structure as \code{intercept_shifters}.
#' @param disagscors_list Optional character vector of variable names for
#'   which disaggregated efficiency score summaries should be computed.
#'   For each variable in this list, the function builds weighted and
#'   unweighted summaries of TE/TE0/TGR/MTE by survey, sample, and
#'   disaggregation level.
#' @param match_specifications match specifications
#' @param match_specification_optimal match specifications
#' @param match_path match path
#' @details
#' \enumerate{
#'   \item \strong{Draw-specific filtering:}
#'     Using \code{drawlist}, the function removes any \code{EaId} values
#'     associated with the current \code{draw} from \code{data}. This allows
#'     for bootstrap, jackknife, or matched-sample resampling.
#'
#'   \item \strong{Survey-specific estimation:}
#'     After setting the \code{Surveyy} label (either from \code{Surveyx} or
#'     collapsed to \code{"GLSS0"}), the function loops over unique survey
#'     labels and calls \code{msf_workhorse()} for each. The workhorse
#'     is expected to return a list with components such as
#'     \code{sf_estm}, \code{el_mean}, \code{ef_mean}, \code{rk_mean},
#'     \code{ef_dist}, \code{rk_dist}, \code{el_samp}, \code{ef_samp},
#'     and \code{rk_samp}.
#'
#'   \item \strong{Disaggregated efficiency summaries:}
#'     If \code{disagscors_list} is provided, the function constructs
#'     weighted and unweighted efficiency statistics (e.g., weighted mean,
#'     mean, median, mode) for TE/TE0/TGR/MTE by survey, sample, restriction
#'     status, technology, and disaggregation level. Results are stored in
#'     \code{res$disagscors}.
#'
#'   \item \strong{Draw-specific pruning:}
#'     When \code{draw != 0}, sample-level objects (\code{el_samp},
#'     \code{ef_samp}, \code{rk_samp}) are removed from the returned list
#'     to reduce memory usage, keeping only aggregate results.
#' }
#'
#' Internally, the function uses \pkg{data.table}, \pkg{dplyr},
#' \pkg{tidyr}, \pkg{doBy}, and \pkg{crayon} for data manipulation and
#' progress messages. All estimation is delegated to
#' \code{msf_workhorse()}.
#'
#' @return
#' A named list with components:
#' \itemize{
#'   \item \code{sf_estm} - Combined frontier parameter estimates across
#'     surveys (rows tagged with \code{Surveyy} and \code{draw}).
#'   \item \code{el_mean}, \code{ef_mean}, \code{rk_mean} - Mean/summary
#'     statistics for elasticities, efficiency, and risk metrics.
#'   \item \code{ef_dist}, \code{rk_dist} - Distributions of efficiency and
#'     risk quantities.
#'   \item \code{el_samp}, \code{ef_samp}, \code{rk_samp} - Sample-level
#'     results (only retained when \code{draw == 0}).
#'   \item \code{disagscors} - Optional disaggregated efficiency summary
#'     table if \code{disagscors_list} is not \code{NULL}; otherwise
#'     \code{NULL}.
#' }
#' If an error occurs anywhere in the pipeline, the function returns
#' \code{NULL}.
#'
#' @import data.table
#' @export
draw_msf_estimations <- function(
    draw, 
    drawlist, 
    surveyy                   = FALSE, 
    data, 
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
    disagscors_list           = NULL,
    match_specifications,
    match_specification_optimal,
    match_path) {
  
  tryCatch({ 
    function(){
      production_slope_shifters   = NULL
      include_trend               = FALSE
      technology_variable         = technology_variable
      draw                        = 0
      surveyy                     = FALSE
      intercept_shifters          = list(scalar_variables=crop_area_list,factor_variables=c("Survey","Ecozon"))
      intercept_shifters_meta     = list(scalar_variables=crop_area_list,factor_variables=c("Survey","Ecozon"))
      weight_variable             = "Weight"
      output_variable             = "HrvstKg"
      risk_covariates             = NULL
      input_variables             = c("Area", "SeedKg", "HHLaborAE","HirdHr","FertKg","PestLt")
      inefficiency_covariates     = list(scalar_variables=c("lnAgeYr","lnYerEdu","CrpMix"),factor_variables=c("Female","Survey","Ecozon","Extension","Credit","EqipMech","OwnLnd"))
      adoption_covariates         = list(scalar_variables=c("lnAgeYr","lnYerEdu","CrpMix"),factor_variables=c("Female","Survey","Ecozon","Extension","Credit","EqipMech","OwnLnd"))
      identifiers                 = c("unique_identifier", "Survey", "CropID", "HhId", "EaId", "Mid")
    }
    #---------------------------------------------
    # Data Preparation                         ####
    # Filter out rows based on drawlist
    data <- data[!data$EaId %in% c(t(drawlist[drawlist$ID %in% draw, 2:ncol(drawlist)])),]
    
    #---------------------------------------------
    # Survey estimations                       ####
    cat(crayon::green("Survey estimations", Sys.time()), fill=T)
    
    # Set survey variable
    data$Surveyy <- data$Surveyx
    if(surveyy %in% FALSE) {
      data$Surveyy <- "GLSS0"
    }
    
    # Perform survey estimations for each unique survey
    res <- lapply(
      unique(data$Surveyy),
      function(glss, draw) {
        tryCatch({ 
          # glss <-"GLSS0"
          res <- msf_workhorse(
            data=data[data[,"Surveyy"] %in% glss,], output_variable=output_variable, input_variables=input_variables,
            production_slope_shifters=production_slope_shifters, intercept_shifters=intercept_shifters, intercept_shifters_meta=intercept_shifters_meta,
            inefficiency_covariates=inefficiency_covariates, adoption_covariates=adoption_covariates, risk_covariates=risk_covariates, weight_variable=weight_variable, f=f, d=d, identifiers=identifiers, technology_variable=technology_variable, matching_type=matching_type, include_trend=include_trend)
          
          function() {
            Main <- res$ef_mean
            Main <- Main[Main$Survey %in% "GLSS0",]
            Main <- Main[!Main$sample %in% "unmatched",]
            Main <- Main[Main$stat %in% "wmean",]
            Main <- Main[Main$CoefName %in% "efficiencyGap_pct",]
            Main <- Main[Main$restrict %in% "Restricted",]
            Main <- Main[Main$estType %in% "teBC",]
            Main[Main$type %in% "TGR", c("sample", "type", "Tech", "Estimate")]
            Main[Main$type %in% "TE", c("sample", "type", "Tech", "Estimate")]
            Main[Main$type %in% "MTE", c("sample", "type", "Tech", "Estimate")]
          }
          
          # Add survey and draw information to the results
          for(outcome in c("sf_estm", "el_mean", "ef_mean", "rk_mean", "ef_dist", "rk_dist", "el_samp", "ef_samp", "rk_samp")) {
            tryCatch({ res[[outcome]][,"Surveyy"] <- glss; res[[outcome]][,"draw"] <- draw }, error=function(e){})
          }
          
          return(res)
        }, error = function(e) { return(NULL) })}, draw=draw)
    
    res <- lapply(
      c("sf_estm", "el_mean", "ef_mean", "rk_mean", "ef_dist", "rk_dist", "el_samp", "ef_samp", "rk_samp"),
      function(outcome) {
        return(as.data.frame(data.table::rbindlist(lapply(1:length(res), function(glss) { return(res[[glss]][[outcome]]) }), fill = TRUE)))
      })
    names(res) <- c("sf_estm", "el_mean", "ef_mean", "rk_mean", "ef_dist", "rk_dist", "el_samp", "ef_samp", "rk_samp")
    
    #---------------------------------------------
    # Disaggregated score summary              #### 
    cat(crayon::green("Disaggregated score summary", Sys.time()), fill=T)
    disagscors <- NULL
    if(!is.null(disagscors_list)) {
      disagscors <- as.data.frame(
        data.table::rbindlist(
          lapply(
            disagscors_list,
            function(disagscors_var, scors, data) {
              tryCatch({ 
                # disagscors_var <- disagscors_list[1];scors <- res$ef_samp
                #print(disagscors_var)
                scors00 <- scors
                scors00$Survey <- "GLSS0"
                scors <- rbind(scors, scors00)
                rm(scors00)
                
                disagscors <- scors |> tidyr::gather(input, value, c("TE0", "TE", "TGR", "MTE"))
                disagscors <- disagscors[!disagscors$value %in% c(NA, Inf, -Inf, NaN),]
                disagscors <- disagscors[disagscors$estType %in% "teBC",]
                
                disagscors <- dplyr::inner_join(disagscors, data[c("unique_identifier", "Survey", "CropID", "HhId", "EaId", "Mid", disagscors_var)],
                                                by=c("unique_identifier", "Survey", "CropID", "HhId", "EaId", "Mid"))
                disagscors$disagscors_level <- disagscors[, disagscors_var]
                disagscors <- disagscors[!disagscors$disagscors_level %in% c(NA, Inf, -Inf, NaN),]
                disagscors$disagscors_level <- as.character(disagscors$disagscors_level)
                
                disagscors0 <- disagscors[disagscors$input %in% "TE0",]
                disagscors0$Tech <- -999
                disagscors <- rbind(disagscors0, disagscors[!disagscors$input %in% "TE0",])
                rm(disagscors0)
                worklist <- unique(disagscors[c("sample", "estType", "Tech", "input", "restrict")])
                
                disagscors <- as.data.frame(
                  data.table::rbindlist(
                    lapply(
                      1:nrow(worklist),
                      function(kk, worklist, disagscors) {
                        score.data <- dplyr::inner_join(worklist[kk,], disagscors, by=names(worklist))
                        score.data <- score.data[!score.data$value %in% c(NA, NaN, Inf, -Inf),]
                        score.data0 <- score.data
                        score.data0$Survey <- "GLSS0"
                        score.data <- rbind(score.data0, score.data)
                        rm(score.data0)
                        score.data <- doBy::summaryBy(
                          list("value", c("Survey", "disagscors_level")),
                          FUN=function(x) {
                            w <- score.data$Weight
                            wmean <- sum(x * w) / sum(w)
                            mode <- function(x, na.rm=T) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}
                            mean <- mean(x, na.rm=T)
                            median <- median(x, na.rm=T)
                            mode <- mode(x, na.rm=T)
                            stat <- c(wmean, mean, median, mode)
                            names(stat) <- c("wmean", "mean", "median", "mode")
                            return(stat)
                          }, data=score.data)
                        names(score.data) <- gsub("value.", "", names(score.data))
                        score.data <- data.frame(worklist[kk,], score.data)
                        return(score.data)
                      }, worklist=worklist, disagscors=disagscors), fill = TRUE))
                
                disagscors$disagscors_var <- ifelse(grepl("CROP_", disagscors_var), "CROP", disagscors_var)
                disagscors$CoefName <- "disag_efficiency"
                return(disagscors)
              }, error = function(e) { return(NULL) })
              
              return(DONE)
            }, scors=res$ef_samp, data=data), fill = TRUE))
      disagscors$draw <- draw
    }
    res$disagscors <- disagscors
    
    #---------------------------------------------
    # Export                                   ####
    if(!draw %in% 0) {res[["el_samp"]] <- NULL; res[["ef_samp"]] <- NULL; res[["rk_samp"]] <- NULL}
    return(res)
    #---------------------------------------------
  }, error = function(e) {return(NULL)})
}



#' Summarize multi-draw Meta Stochastic Frontier (MSF) results
#'
#' Aggregates results from multiple stochastic frontier analysis (SFA) / MSF
#' draws (e.g., from a bootstrap or jackknife procedure) and computes
#' jackknife-style summary statistics for coefficients, efficiency scores,
#' elasticities, risk measures, and their distributions.
#'
#' The function assumes each element of \code{res} is the output of a single
#' draw from an MSF pipeline (e.g., \code{msf_workhorse()} followed by
#' \code{draw_msf_estimations()}), containing components such as:
#' \code{sf_estm}, \code{ef_mean}, \code{el_mean}, \code{rk_mean},
#' \code{ef_dist}, \code{rk_dist}, and (optionally) \code{disagscors}.
#'
#' @param res List of draw-level result objects. Each element is expected to be
#'   a list with (at least) the following components:
#'   \itemize{
#'     \item \code{sf_estm}: coefficient-level estimates and diagnostics for
#'       naive, group, and meta-frontiers, including a \code{draw} index.
#'     \item \code{ef_mean}: aggregated efficiency statistics (e.g., TE0, TE,
#'       TGR, MTE) by sample, technology, survey, and estimation type.
#'     \item \code{el_mean}: aggregated elasticity statistics by input,
#'       technology, and survey.
#'     \item \code{rk_mean}: aggregated risk statistics, if available.
#'     \item \code{ef_dist}: histogram-style efficiency distributions
#'       (counts/weights) over value ranges (\code{range}, \code{Frqlevel}).
#'     \item \code{rk_dist}: histogram-style risk distributions, if available.
#'     \item \code{disagscors}: optional disaggregated efficiency summaries
#'       by subgroup levels.
#'     \item \code{el_samp}, \code{ef_samp}, \code{rk_samp}: observation-level
#'       elasticities, efficiencies, and risk measures for at least the
#'       baseline draw (usually \code{draw == 0}).
#'   }
#'   Each component must include a \code{draw} column that distinguishes the
#'   baseline draw (typically \code{draw == 0}) from resampled draws
#'   (\code{draw != 0}).
#'
#' @param   technology_legend Data.frame providing the mapping between numeric technology
#'   codes and their labels, typically with at least:
#'   \itemize{
#'     \item \code{Tech}: numeric technology index used internally in MSF
#'       estimation, and
#'     \item a corresponding label column (e.g., the original \code{technology_variable}).
#'   }
#'   This is used to compute technology gaps (e.g., efficiency or
#'   disaggregated efficiency gaps) relative to the reference technology
#'   (smallest value in \code{  technology_legend$Tech}).
#'
#' @details
#' For each component, the function:
#' \enumerate{
#'   \item Stacks all draws using \code{data.table::rbindlist()}.
#'   \item Drops non-finite values (\code{NA}, \code{Inf}, \code{-Inf}, \code{NaN}).
#'   \item For each statistic (e.g., coefficient estimate, mean efficiency),
#'     identifies the baseline draw (\code{draw == 0}) and joins it with
#'     resampled draws (\code{draw != 0}) aggregated via
#'     \code{doBy::summaryBy()} to compute:
#'     \itemize{
#'       \item \code{Estimate.mean}: mean across non-baseline draws,
#'       \item \code{Estimate.sd}: standard deviation across non-baseline draws,
#'       \item \code{Estimate.length}: number of non-baseline draws.
#'     }
#'   \item Computes a jackknife-style test statistic and p-value:
#'     \itemize{
#'       \item \code{jack_zv = Estimate / Estimate.sd},
#'       \item \code{jack_pv = 2 * (1 - pt(|jack_zv|, df = Estimate.length))},
#'     }
#'     where \code{Estimate} is the baseline draw statistic and the
#'     distribution of resampled draws is treated as a reference distribution.
#' }
#'
#' For disaggregated scores (\code{disagscors}), the function also:
#' \itemize{
#'   \item Computes technology-specific disaggregated efficiency gaps
#'     (level and percent) relative to the reference technology in
#'     \code{  technology_legend}, and
#'   \item Aggregates these gaps across draws in the same jackknife fashion
#'     as for the main efficiency statistics.
#' }
#'
#' @return A list with the following components:
#' \itemize{
#'   \item \code{sf_estm}: Data.frame of jackknife summary statistics for
#'     frontier coefficients and diagnostics across draws, with columns such
#'     as \code{Estimate}, \code{Estimate.mean}, \code{Estimate.sd},
#'     \code{Estimate.length}, \code{jack_zv}, and \code{jack_pv}, indexed by
#'     \code{Survey}, \code{CoefName}, \code{Tech}, \code{sample}, and
#'     \code{restrict}.
#'   \item \code{ef_mean}: Jackknife summaries for efficiency statistics
#'     (TE0, TE, TGR, MTE), by sample, technology, survey, type, estimation
#'     type, statistic, and restriction.
#'   \item \code{el_mean}: Jackknife summaries for elasticity statistics,
#'     by input, technology, survey, statistic, and restriction.
#'   \item \code{rk_mean}: (If available) jackknife summaries for risk
#'     statistics, with \code{type = "risk"}.
#'   \item \code{ef_dist}: Jackknife summaries of efficiency distributions
#'     (counts and weights) over ranges, with \code{stat} indicating whether
#'     the row refers to counts or weights.
#'   \item \code{rk_dist}: (If available) jackknife summaries of risk
#'     distributions over ranges, with \code{type = "risk"}.
#'   \item \code{disagscors}: (If available) jackknife summaries for
#'     disaggregated efficiency statistics and their gaps by subgroup level
#'     and technology.
#'   \item \code{el_samp}: Observation-level elasticity outputs from the
#'     first draw in \code{res} (typically the baseline draw).
#'   \item \code{ef_samp}: Observation-level efficiency scores from the
#'     first draw in \code{res}.
#'   \item \code{rk_samp}: Observation-level risk measures from the first
#'     draw in \code{res}.
#' }
#'
#' @import data.table
#'
#' @export
draw_msf_summary <- function(res, technology_legend) {
  
  res_list <- list(el_samp=res[[1]]$el_samp, ef_samp=res[[1]]$ef_samp, rk_samp=res[[1]]$rk_samp)
  
  #---------------------------------------------------
  # Summary Estimates                              ####
  # Combine results from all draws for summary estimates
  sf_estm <- as.data.frame(data.table::rbindlist(lapply(1:length(res), function(draw) { return(res[[draw]]$sf_estm) }), fill = TRUE))
  sf_estm <- sf_estm[!sf_estm$Estimate %in% c(NA, Inf, -Inf, NaN),]
  sf_estm$Survey <- sf_estm$Surveyy
  
  # Add GLSS0 survey if multiple surveys are present
  if(length(unique(sf_estm$Survey)) > 1) {
    sf_estm00 <- sf_estm
    sf_estm00$Survey <- "GLSS0"
    sf_estm <- rbind(sf_estm00, sf_estm)
    rm(sf_estm00)
  }

  # Calculate summary statistics for the estimates
  sf_estm <- dplyr::inner_join(
    doBy::summaryBy(list(c("Estimate", "StdError", "Zvalue", "Pvalue"), 
                         c("Survey", "CoefName", "Tech", "sample", "restrict")),
                    data=sf_estm[sf_estm$draw %in% 0,], FUN=mean, keep.names = T),
    doBy::summaryBy(list(c("Estimate"), c("Survey", "CoefName", "Tech", "sample", "restrict")),
                    data=sf_estm, FUN=c(mean, sd, length)),
    by=c("Survey", "CoefName", "Tech", "sample", "restrict"))
  sf_estm$jack_zv <- sf_estm$Estimate / sf_estm$Estimate.sd
  sf_estm$jack_pv <- round(2 * (1 - pt(abs(sf_estm$jack_zv), df=sf_estm$Estimate.length)), 5)
  res_list[["sf_estm"]] <- sf_estm
  #---------------------------------------------------
  # Summary Scores                                 ####
  # Combine results from all draws for summary scores
  ef_mean <- as.data.frame(data.table::rbindlist(lapply(1:length(res), function(draw) { return(res[[draw]]$ef_mean) }), fill = TRUE))
  ef_mean <- ef_mean[!ef_mean$Estimate %in% c(NA, Inf, -Inf, NaN),]
  
  ef_mean <- doBy::summaryBy(list(c("Estimate"), c("sample", "Tech", "type", "estType", "Survey", "stat", "CoefName", "restrict", "draw")),
                             data=ef_mean, FUN=mean, keep.names = T)

  ef_mean <- dplyr::inner_join(
    doBy::summaryBy(list(c("Estimate"), 
                         c("sample", "Tech", "type", "estType", "Survey", "stat", "CoefName", "restrict")),
                    data=ef_mean[ef_mean$draw %in% 0,], FUN=mean, keep.names = T),
    doBy::summaryBy(list(c("Estimate"), c("sample", "Tech", "type", "estType", "Survey", "stat", "CoefName", "restrict")),
                    data=ef_mean, FUN=c(mean, sd, length)),
    by=c("sample", "Tech", "type", "estType", "Survey", "stat", "CoefName", "restrict"))
  
  ef_mean$jack_zv <- ef_mean$Estimate / ef_mean$Estimate.sd
  ef_mean$jack_pv <- round(2 * (1 - pt(abs(ef_mean$jack_zv), df=ef_mean$Estimate.length)), 5)
  res_list[["ef_mean"]] <- ef_mean
  #---------------------------------------------------
  # Summary Elasticity                             ####
  # Combine results from all draws for summary elasticity
  el_mean <- as.data.frame(data.table::rbindlist(lapply(1:length(res), function(draw) { return(res[[draw]]$el_mean) }), fill = TRUE))
  el_mean <- el_mean[!el_mean$Estimate %in% c(NA, Inf, -Inf, NaN),]
  
  el_mean <- doBy::summaryBy(list(c("Estimate"), c("Survey", "sample", "Tech", "input", "Survey", "stat", "CoefName", "restrict", "draw")),
                             data=el_mean, FUN=mean, keep.names = T)

  el_mean <- dplyr::inner_join(
    doBy::summaryBy(list(c("Estimate"), 
                         c("sample", "Tech", "input", "Survey", "stat", "CoefName", "restrict")),
                    data=el_mean[el_mean$draw %in% 0,], FUN=mean, keep.names = T),
    doBy::summaryBy(list(c("Estimate"), c("sample", "Tech", "input", "Survey", "stat", "CoefName", "restrict")),
                    data=el_mean, FUN=c(mean, sd, length)),
    by=c("sample", "Tech", "input", "Survey", "stat", "CoefName", "restrict"))
  
  el_mean$jack_zv <- el_mean$Estimate / el_mean$Estimate.sd
  el_mean$jack_pv <- round(2 * (1 - pt(abs(el_mean$jack_zv), df=el_mean$Estimate.length)), 5)
  res_list[["el_mean"]] <- el_mean
  #---------------------------------------------------
  # Distribution bars- Scores                      ####
  # Combine results from all draws for distribution bars of scores
  ef_dist <- as.data.frame(data.table::rbindlist(lapply(1:length(res), function(draw) { return(res[[draw]]$ef_dist) }), fill = TRUE)) |> 
    tidyr::gather(stat, Estimate, c("est_count", "est_weight")) 
  
  ef_dist <- ef_dist[!ef_dist$Estimate %in% c(NA, Inf, -Inf, NaN),]
  
  ef_dist <- doBy::summaryBy(list(c("Estimate"), c("Survey", "sample", "Tech", "type", "estType", "range", "Frqlevel", "stat", "restrict", "draw")),
                             data=ef_dist, FUN=mean, keep.names = T)
  
  ef_dist <- dplyr::inner_join(
    doBy::summaryBy(list(c("Estimate"), 
                         c("Survey", "sample", "Tech", "type", "estType", "range", "Frqlevel", "stat", "restrict")),
                    data=ef_dist[ef_dist$draw %in% 0,], FUN=mean, keep.names = T),
    doBy::summaryBy(list(c("Estimate"), c("Surveyy", "Survey", "sample", "Tech", "type", "estType", "range", "Frqlevel", "stat", "restrict")),
                    data=ef_dist, FUN=c(mean, sd, length)),
    by=c("Survey", "sample", "Tech", "type", "estType", "range", "Frqlevel", "stat", "restrict"))
  
  ef_dist$jack_zv <- ef_dist$Estimate / ef_dist$Estimate.sd
  ef_dist$jack_pv <- round(2 * (1 - pt(abs(ef_dist$jack_zv), df=ef_dist$Estimate.length)), 5)
  ef_dist$stat <- gsub("est_", "", ef_dist$stat)
  res_list[["ef_dist"]] <- ef_dist
  #---------------------------------------------------
  # Disaggregated score summary                    ####
  if(!is.null(res[[1]]$disagscors)) {
    disagscors <- as.data.frame(data.table::rbindlist(lapply(1:length(res), function(draw) { return(res[[draw]]$disagscors) }), fill = TRUE)) |> 
      tidyr::gather(stat, Estimate, c("wmean", "mean", "median", "mode")) 
    disagscors <- disagscors[!disagscors$Estimate %in% c(NA, Inf, -Inf, NaN),]
    
    disagscors <- doBy::summaryBy(list(c("Estimate"), c("sample", "estType", "Tech", "input", "Survey", "disagscors_level",
                                                        "disagscors_var", "CoefName", "stat", "restrict", "draw")),
                                  data=disagscors, FUN=mean, keep.names = T)
    
    # Calculate disaggregated efficiency gaps
    disagscorsGAP <- disagscors[disagscors$Tech %in%   technology_legend$Tech,]
    disagscorsGAP <- dplyr::inner_join(
      disagscorsGAP[!disagscorsGAP$Tech %in% min(  technology_legend$Tech), c("sample", "estType", "Survey", "CoefName", "input", "stat", "restrict", "disagscors_level", "disagscors_var", "Tech", "draw", "Estimate")],
      doBy::summaryBy(list("Estimate", c("sample", "estType", "Survey", "CoefName", "input", "stat", "restrict", "disagscors_level", "disagscors_var", "draw")),
                      data=disagscorsGAP[disagscorsGAP$Tech %in% min(  technology_legend$Tech),], FUN=c(mean), na.rm=T),
      by=c("sample", "estType", "Survey", "CoefName", "input", "stat", "restrict", "disagscors_level", "disagscors_var", "draw"))
    
    disagscorsGAP$disag_efficiencyGap_lvl <- disagscorsGAP$Estimate - disagscorsGAP$Estimate.mean
    disagscorsGAP$disag_efficiencyGap_pct <- ((disagscorsGAP$disag_efficiencyGap_lvl / abs(disagscorsGAP$Estimate.mean)))*100
    disagscorsGAP <- disagscorsGAP[c("sample", "estType", "Survey", "CoefName", "input", "stat", "restrict", "disagscors_level", "disagscors_var", "Tech", "draw",
                                     "disag_efficiencyGap_lvl", "disag_efficiencyGap_pct")]
    disagscorsGAP <- disagscorsGAP |> tidyr::gather(CoefName, Estimate, c("disag_efficiencyGap_lvl", "disag_efficiencyGap_pct"))
    disagscors <- as.data.frame(data.table::rbindlist(list(disagscors, disagscorsGAP), fill = TRUE))
    
    disagscors <- dplyr::inner_join(
      doBy::summaryBy(list(c("Estimate"), 
                           c("sample", "estType", "Tech", "input", "Survey", "disagscors_level", "disagscors_var", "CoefName", "stat", "restrict")),
                      data=disagscors[disagscors$draw %in% 0,], FUN=mean, keep.names = T),
      doBy::summaryBy(list(c("Estimate"), c("sample", "estType", "Tech", "input", "Survey", "disagscors_level", "disagscors_var", "CoefName", "stat", "restrict")),
                      data=disagscors, FUN=c(mean, sd, length)),
      by=c("sample", "estType", "Tech", "input", "Survey", "disagscors_level", "disagscors_var", "CoefName", "stat", "restrict"))
    
    disagscors$jack_zv <- disagscors$Estimate / disagscors$Estimate.sd
    disagscors$jack_pv <- round(2 * (1 - pt(abs(disagscors$jack_zv), df=disagscors$Estimate.length)), 5)
    res_list[["disagscors"]] <- disagscors
  }

  #---------------------------------------------------
  # Summary- Risk                                  ####
  rk_mean <- NULL
  if(!is.null(res[[1]]$rk_mean) & !nrow(res[[1]]$rk_mean) %in% 0) {
    rk_mean <- as.data.frame(data.table::rbindlist(lapply(1:length(res), function(draw) { return(res[[draw]]$rk_mean) }), fill = TRUE)) 
    rk_mean <- rk_mean[!rk_mean$Estimate %in% c(NA, Inf, -Inf, NaN),]
    
    rk_mean <- doBy::summaryBy(list(c("Estimate"), c("sample", "Tech", "Survey", "stat", "CoefName", "restrict", "draw")),
                               data=rk_mean, FUN=mean, keep.names = T)
    
    rk_mean <- dplyr::inner_join(
      doBy::summaryBy(list(c("Estimate"), 
                           c("sample", "Tech", "Survey", "stat", "CoefName", "restrict")),
                      data=rk_mean[rk_mean$draw %in% 0,], FUN=mean, keep.names = T),
      doBy::summaryBy(list(c("Estimate"), c("sample", "Tech", "Survey", "stat", "CoefName", "restrict")),
                      data=rk_mean, FUN=c(mean, sd, length)),
      by=c("sample", "Tech", "Survey", "stat", "CoefName", "restrict"))
    rk_mean$jack_zv <- rk_mean$Estimate / rk_mean$Estimate.sd
    rk_mean$jack_pv <- round(2 * (1 - pt(abs(rk_mean$jack_zv), df=rk_mean$Estimate.length)), 5)
    rk_mean$type <- "risk"
    res_list[["rk_mean"]] <- rk_mean
  }
  
  #---------------------------------------------------
  # Distribution bars- Risk                        ####
  rk_dist <- NULL
  if(!is.null(res[[1]]$rk_dist) & !nrow(res[[1]]$rk_dist) %in% 0) {
    rk_dist <- as.data.frame(data.table::rbindlist(lapply(1:length(res), function(draw) { return(res[[draw]]$rk_dist) }), fill = TRUE)) |> 
      tidyr::gather(stat, Estimate, c("est_count", "est_weight")) 
    rk_dist <- rk_dist[!rk_dist$Estimate %in% c(NA, Inf, -Inf, NaN),]
    
    rk_dist <- doBy::summaryBy(list(c("Estimate"), c("Survey", "sample", "Tech", "range", "Frqlevel", "stat", "restrict", "draw")),
                               data=rk_dist, FUN=mean, keep.names = T)

    rk_dist <- dplyr::inner_join(
      doBy::summaryBy(list(c("Estimate"), 
                           c("Survey", "sample", "Tech", "range", "Frqlevel", "stat", "restrict")),
                      data=rk_dist[rk_dist$draw %in% 0,], FUN=mean, keep.names = T),
      doBy::summaryBy(list(c("Estimate"), c("Survey", "sample", "Tech", "range", "Frqlevel", "stat", "restrict")),
                      data=rk_dist, FUN=c(mean, sd, length)),
      by=c("Survey", "sample", "Tech", "range", "Frqlevel", "stat", "restrict"))
    
    rk_dist$jack_zv <- rk_dist$Estimate / rk_dist$Estimate.sd
    rk_dist$jack_pv <- round(2 * (1 - pt(abs(rk_dist$jack_zv), df=rk_dist$Estimate.length)), 5)
    rk_dist$stat <- gsub("est_", "", rk_dist$stat)
    rk_dist$type <- "risk"
    res_list[["rk_dist"]] <- rk_dist
  }
  
  #---------------------------------------------------
  # Summary and export                             ####

  return(res_list)
  #---------------------------------------------------
}