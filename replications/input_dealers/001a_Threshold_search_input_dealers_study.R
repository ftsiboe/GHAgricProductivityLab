rm(list = ls(all = TRUE)); gc()              

devtools::document()                         

project_name <- "input_dealers"

study_environment <- readRDS(
  file.path(paste0("replications/", project_name, "/output"),
            paste0(project_name,"_study_environment.rds")))

DATA <- harmonized_data_prep(study_environment$study_raw_data)     
data <- DATA[as.character(DATA$CropID) %in% "Pooled", ]

# Area ≈ 30.7 km²
tauGrid <- data.table::rbindlist(
  lapply(
    names(data)[grepl("dealer_density_",names(data))],
    function(ds){
      tryCatch({ 
       
        taui <- unique(round(data[,ds],4))
        taui <- taui[order(taui)]
        
        tauGrid <- data.frame(lower=0,upper=1:length(taui))
        tauGrid$upper <- as.numeric(as.character(factor(tauGrid$upper,levels = 1:length(taui),labels = taui)))
        tauGrid <- tauGrid[tauGrid$lower<tauGrid$upper,]
        tauGrid <- tauGrid[tauGrid$upper>0,]
        
        tauGrid <- as.data.frame(
          data.table::rbindlist(
            lapply(
              1:nrow(tauGrid),
              function(pair) {
                DONE <- NULL
                tryCatch({ 
                  # pair <- 1
                  datai <- data[data$CropID %in% "Pooled",]
                  zone  <- ifelse(datai[,ds] <= tauGrid$lower[pair],1,2)
                  zone  <- ifelse(datai[,ds] > tauGrid$upper[pair],3,zone)
                  res   <- data.frame(lower=tauGrid$lower[pair],
                                      upper=tauGrid$upper[pair],
                                      groups=length(unique(zone)),
                                      zone_minprop=min(table(zone)/length(zone)))
                }, error=function(e){})
                return(res)
              }), fill = TRUE))
        
        tauGrid <- tauGrid[tauGrid$zone_minprop >= 0.10,]
        tauGrid <- tauGrid[tauGrid$groups %in% 3,c("lower","upper")]
        tauGrid$density_scope <- ds
        tauGrid
      }, error=function(e){NULL})
    }), fill = TRUE)










