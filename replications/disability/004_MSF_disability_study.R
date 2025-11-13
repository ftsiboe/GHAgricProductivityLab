
rm(list = ls(all = TRUE)); gc()  

library(dplyr);library(sfaR) ;library(micEcon);library(frontier)
library(rgenoud);library(quadprog);library(car)

devtools::document()  

project_name = "disability"

# Detect operating system to determine runtime environment
sysname <- toupper(as.character(Sys.info()[["sysname"]]))

# Load saved study environment (directories, specifications, etc.)
study_environment <- readRDS(
  file.path(paste0("replications/", project_name, "/output"),
            paste0(project_name,"_study_environment.rds")))

# --- Data ingest & harmonization
# Load harmonized survey data stored in the study environment
estimation_data <- study_environment[["estimation_data"]]
estimation_data$EduCat <- as.character(estimation_data$EduCat)
distforms   <- sf_functional_forms()$distforms
fxnforms    <- sf_functional_forms()$fxnforms

SPECS <- sf_model_specifications(
  distforms = distforms,
  fxnforms = fxnforms,
  data = DATA,
  technology_variables = c("disabled","disabled_self","disabled_spouse","disabled_child","disabled_close","disabled_member"))

SPECS <- SPECS[!SPECS$disasg %in% c( "Female","Region","Ecozon","EduCat","EduLevel","AgeCat"),]
SPECS <- SPECS[SPECS$level %in% c( "Pooled"),]

row.names(SPECS) <- 1:nrow(SPECS)

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  SPECS <- SPECS[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}


lapply(
  c(1:nrow(SPECS)),
  function(fit){
    # fit <- 2;matching_type <- "optimal"
    f <- SPECS$f[fit]
    d <- SPECS$d[fit]
    disaggregate_variable <- SPECS$disasg[fit]
    disaggregate_level    <- SPECS$level[fit]
    technology_variable   <- SPECS$TechVar[fit]
    matching_type         <- SPECS$matching_type[fit]
    
    est_name <- paste0(
      disaggregate_variable,"_",disaggregate_level,
      "_",technology_variable,"_",names(fxnforms)[f],"_",
      names(distforms)[d],"_",matching_type)
    
    out_path <- file.path(
      study_environment$wd$estimations,
      paste0(est_name,".rds"))

    if(!file.exists(out_path)){
      #tryCatch({ 
      
      # Data Preparation
      data <- estimation_data[estimation_data[,SPECS$disasg[fit]] %in% SPECS$level[fit],]
      data <- data[!data[,SPECS$TechVar[fit]] %in% NA,]
      data$Tech <- as.numeric(as.integer(as.factor(as.character(data[,SPECS$TechVar[fit]]))))
      if(!SPECS$disasg[fit] %in% "CropID") data <- data[data[,"CropID"] %in% "Pooled",]
      technology_legend <- unique(data[c("Tech",SPECS$TechVar[fit])])
      technology_legend <- technology_legend[order(technology_legend$Tech),]
    
      crop_area_list <- study_environment$crop_area_list

      for(crop in crop_area_list){
        data[,crop] <- data[,crop]/data[,"Area"]
      }
      
      crop_area_list <- apply(data[names(data)[names(data) %in% crop_area_list]],2,mean) > 0.03
      crop_area_list <- names(crop_area_list)[crop_area_list %in% TRUE]
      
      for(crop in gsub("Area_","",crop_area_list)){
        data[,paste0("CROP_",crop)] <- ifelse(data[,paste0("Area_",crop)] > 0, crop,NA)
      }
      
      crop_area_list <- unique(c(crop_area_list,"Area_Other"))
      if(length(crop_area_list)>0){ 
        data$Area_Other <- 1 - rowSums(data[crop_area_list[!crop_area_list %in% "Area_Maize"]],na.rm=T)
        crop_area_list  <- unique(c(crop_area_list,"Area_Other"))
      }
      
      # draw estimations
      drawlist = study_environment$sample_draw_list
      
      # draw estimations
      disagscors_list <- NULL
      if(technology_variable %in% "disabled" &  
         matching_type %in% "optimal" & 
         disaggregate_level %in% "Pooled" & 
         disaggregate_variable %in% "CropID" & 
         f %in% 2 & d %in% 1){
        disagscors_list <- c("Ecozon","Region","AgeCat","EduLevel","Female","disability",names(data)[grepl("CROP_",names(data))])
      }
      
      res <- lapply(
        unique(drawlist$ID)[1:3],
        draw_msf_estimations,
        data                = data,
        surveyy             = FALSE,
        intercept_shifters  = list(Svarlist=crop_area_list,Fvarlist=c("Survey","Ecozon")),
        intercept_shiftersM = list(Svarlist=crop_area_list,Fvarlist=c("Survey","Ecozon")),
        drawlist            = drawlist,
        wvar                = "Weight",
        yvar                = "HrvstKg",
        xlist               = c("Area", "SeedKg", "HHLaborAE","HirdHr","FertKg","PestLt"),
        ulist               = list(Svarlist=c("lnAgeYr","lnYerEdu","CrpMix"),Fvarlist=c("Female","Survey","Ecozon","Extension","Credit","EqipMech","OwnLnd")),
        ulistM              = list(Svarlist=c("lnAgeYr","lnYerEdu","CrpMix"),Fvarlist=c("Female","Survey","Ecozon","Extension","Credit","EqipMech","OwnLnd")),
        identifiers         = c("unique_identifier", "Survey", "CropID", "HhId", "EaId", "Mid"),
        disagscors_list     = disagscors_list,
        f                   = f,
        d                   = d,
        tvar                = technology_variable,
        matching_type       = matching_type) 

      # draw summary 
      res <- draw_msf_summary(res=res,technology_legend=technology_legend)

      for(xx in 1:length(res)){
        tryCatch({
          res[[xx]][,"FXN"]     <- names(fxnforms)[f]
          res[[xx]][,"DIS"]     <- names(distforms)[d]
          res[[xx]][,"disasg"]  <- disaggregate_variable
          res[[xx]][,"level"]   <- disaggregate_level
          res[[xx]][,"TCH"]     <- technology_variable
          res[[xx]][,"TCHLvel"] <- factor(res[[xx]][,"Tech"],levels = c(-999,technology_legend$Tech,999),labels = c("National",technology_legend[,2],"Meta"))
        }, error=function(e){})
      }

      function(){
        Main <- res$ef_mean
        Main <- Main[Main$Survey %in% "GLSS0",]
        Main <- Main[!Main$sample %in% "unmatched",]
        Main <- Main[Main$stat %in% "wmean",]
        Main <- Main[Main$CoefName %in% "efficiencyGap_lvl",]
        Main <- Main[Main$restrict %in% "Restricted",]
        Main <- Main[Main$estType %in% "teBC",]
        Main[Main$type %in% "TGR",c("sample","type","Tech","Estimate")]
        Main[Main$type %in% "TE",c("sample","type","Tech","Estimate")]
        Main[Main$type %in% "MTE",c("sample","type","Tech","Estimate")]
      }
      
      res[["names"]] <- est_name

      saveRDS(res,file=out_path)

      #}, error=function(e){})
    }
    return(fit)
  })

# unlink(list.files(getwd(),pattern =paste0(".out"),full.names = T))
