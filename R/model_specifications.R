#' Build MSF Model Specifications
#'
#' Creates a specification table for multi-stage frontier (MSF) analysis by
#' combining production-function forms, distributional assumptions, technology
#' variables, and disaggregation levels (pooled, crops, demographics).
#'
#' @param data data.frame/data.table providing values for \code{demographic_variables}.
#' @param distforms Named list of distributions.
#' @param fxnforms  Named list of functional forms.
#' @param TechVarlist Character vector of technology variables (first is default).
#' @param mainF Integer index of preferred functional form (in fxnforms). Default 2.
#' @param mainD Integer index of preferred distribution (in distforms). Default 1.
#' @param demographic_variables Character vector of DATA column names for disaggregation.
#' @param crop_list Character vector of crop names for disaggregation.
#' @return A data.table with columns: disasg, level, TechVar, f, d.
#' @import data.table
#' @export
model_specifications <- function(
    data,
    distforms,
    fxnforms,
    TechVarlist,
    mainF = 2,
    mainD = 1,
    demographic_variables = c("Female","Region","Ecozon","EduCat","EduLevel","AgeCat"),
    crop_list = c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                  "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam")
) {
  # ---- validation ----
  if (missing(DATA) || is.null(DATA)) stop("'DATA' must be provided.")
  nF <- length(fxnforms); nD <- length(distforms)
  if (nF < 1L) stop("'fxnforms' must have at least one element.")
  if (nD < 1L) stop("'distforms' must have at least one element.")
  if (length(TechVarlist) < 1L) stop("'TechVarlist' must have at least one element.")
  if (!is.numeric(mainF) || mainF < 1L || mainF > nF) stop(sprintf("'mainF' must be in 1..%d", nF))
  if (!is.numeric(mainD) || mainD < 1L || mainD > nD) stop(sprintf("'mainD' must be in 1..%d", nD))
  
  # Coerce DATA to data.table
  if (!data.table::is.data.table(DATA)) DATA <- data.table::as.data.table(DATA)
  
  # Ensure demographic variables exist
  missing_demogs <- setdiff(demographic_variables, names(DATA))
  if (length(missing_demogs)) {
    stop(sprintf("The following 'demographic_variables' are missing in DATA: %s",
                 paste(missing_demogs, collapse = ", ")))
  }
  
  # ---- base pooled grid (all f x all d), keep f==mainF OR d==mainD ----
  base  <- data.table::CJ(level = "Pooled", f = seq_len(nF), d = seq_len(nD), unique = TRUE)
  SPECS <- unique(data.table::rbindlist(
    list(base[f == mainF], base[d == mainD]),
    use.names = TRUE, fill = TRUE
  ))
  SPECS[, disasg := "CropID"]
  
  # ---- preferred f,d pair ----
  pref_fd <- SPECS[f == mainF & d == mainD, .(f, d)]
  if (!nrow(pref_fd)) stop("Preferred (mainF, mainD) pair was not constructed; check inputs.")
  pf <- pref_fd$f[1]; pd <- pref_fd$d[1]
  
  # ---- crop disaggregation (preferred f,d) ----
  if (length(crop_list) > 0L) {
    crop_rows <- data.table::data.table(disasg = "CropID", level = crop_list, f = pf, d = pd)
    SPECS <- unique(data.table::rbindlist(list(SPECS, crop_rows), use.names = TRUE, fill = TRUE))
  }
  
  # ---- demographic disaggregation (preferred f,d) ----
  for (w in demographic_variables) {
    levs <- unique(DATA[[w]])
    levs <- levs[!is.na(levs)]
    if (length(levs)) {
      dem_rows <- data.table::data.table(disasg = w, level = as.character(levs), f = pf, d = pd)
      SPECS <- unique(data.table::rbindlist(list(SPECS, dem_rows), use.names = TRUE, fill = TRUE))
    }
  }
  
  # ---- technology variants ----
  SPECS[, TechVar := TechVarlist[1L]]
  base_tp <- SPECS[disasg == "CropID" & level == "Pooled" & f == pf & d == pd, .(disasg, level, f, d)]
  if (nrow(base_tp) && length(TechVarlist) > 1L) {
    add <- data.table::rbindlist(lapply(TechVarlist[-1L], function(tv) {
      cbind(data.table::data.table(TechVar = tv), base_tp)
    }), use.names = TRUE, fill = TRUE)
    SPECS <- unique(data.table::rbindlist(list(SPECS, add), use.names = TRUE, fill = TRUE))
  }
  
  # ---- order & select ----
  data.table::setorder(SPECS, (d != mainD), (f != mainF))
  SPECS <- SPECS[, .(disasg, level, TechVar, f, d)]
  data.table::setDT(SPECS)[]
}
