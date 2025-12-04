rm(list = ls(all = TRUE)); gc()  
library('magrittr');library(ggplot2);library(gridExtra)
library(dplyr);library(gtable);library(stringr);library(cowplot)
devtools::document()  

project_name = "disability"
study_environment <- readRDS(
  file.path(paste0("replications/", project_name, "/output"),
            paste0(project_name,"_study_environment.rds")))

mspecs_optimal <- study_environment$match_specification_optimal

source("data-raw/scripts/figures_and_tables.R")

Keep.List<-c("Keep.List",ls())

# Main Specification   
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- tab_main_specification(study_environment)
wb <- openxlsx::loadWorkbook(file.path(study_environment$wd$output,paste0(project_name,"_results.xlsx")))
openxlsx::writeData(wb, sheet = "msf",res[res$Survey %in% "GLSS0",] , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,file.path(study_environment$wd$output,paste0(project_name,"_results.xlsx")),overwrite = T)

# Fig - Heterogeneity 
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS(file.path(study_environment$wd$estimations,"CropID_Pooled_disabled_TL_hnormal_optimal.rds"))$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level  <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_lvl",]
res <- res[c("disasg","level","fxnforms","distforms","Survey","input","technology_variable","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]

fig <- fig_heterogeneity00(res=res,y_title="Level difference (Disabled less non-Disabled)\n",study_environment=study_environment)
fig[["genderAge"]] <- fig[["genderAge"]] + theme(axis.text.x = element_text(size = 5.5))
ggsave(file.path(study_environment$wd$output,"figure","heterogeneity_crop_region.png"), fig[["crop_region"]],dpi = 600,width = 8, height = 5)
ggsave(file.path(study_environment$wd$output,"figure","heterogeneity_genderAge.png"), fig[["genderAge"]],dpi = 600,width = 8, height = 5)

# Fig - Robustness  
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_robustness(y_title="\nLevel difference [Disabled less non-Disabled]",
               res_list = c(file.path(study_environment$wd$output,"estimations","CropID_Pooled_disabled_CD_hnormal_optimal.rds"),
                            list.files(file.path(study_environment$wd$output,"estimations"),
                                       pattern = "CropID_Pooled_disabled_TL_",full.names = T)),
               study_environment=study_environment)

# Fig - Matching TE 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_input_te(
  y_title="\nDisability gap (%)",
  tech_lable=c("Full sample", "Disabled sample", "non-Disabled sample"),
  study_environment=study_environment)

# Fig - Covariate balance 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance(study_environment=study_environment)

bal_tab <- study_environment$balance_table
ranking <- study_environment$match_specification_ranking
mspecs  <- study_environment$match_specification_optimal
CovBalDATA <- rbind(bal_tab[(bal_tab$sample %in% "Un" & bal_tab$ARRAY %in% 5), ],
                    bal_tab[bal_tab$sample %in% "Adj", ])
CovBalDATA$sample <- ifelse(CovBalDATA$sample %in% "Un", CovBalDATA$sample,
                            ifelse(CovBalDATA$link %in% NA, CovBalDATA$distance, CovBalDATA$link))
CovBalDATA <- CovBalDATA[!CovBalDATA$value %in% NA, ]
CovBalDATA <- CovBalDATA[!CovBalDATA$Coef %in% NA, ]
CovBalDATA <- CovBalDATA[c("sample","stat","Coef","value")]

wb <- openxlsx::loadWorkbook(file.path(study_environment$wd$output,paste0(project_name,"_results.xlsx")))
openxlsx::writeData(wb, sheet = "CovBalDATA",CovBalDATA , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,file.path(study_environment$wd$output,paste0(project_name,"_results.xlsx")),overwrite = T)

wb <- openxlsx::loadWorkbook(file.path(study_environment$wd$output,paste0(project_name,"_results.xlsx")))
openxlsx::writeData(wb, sheet = "ranking",ranking[c("ID","name","Diff.mean",	"V_Ratio.mean",	"KS.mean","rate.mean")] , 
                    colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,file.path(study_environment$wd$output,paste0(project_name,"_results.xlsx")),overwrite = T)

# Fig - Distribution 
rm(list= ls()[!(ls() %in% c(Keep.List))])
dataFrq <- readRDS(file.path(study_environment$wd$output,"estimations/CropID_Pooled_disabled_TL_hnormal_fullset.rds"))
dataFrq <- dataFrq$ef_dist
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq <- dataFrq[dataFrq$stat %in% "estimate_weight",]
dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted",]
dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),levels = 0:1,labels = c("non-Disabled","Disabled"))
fig_dsistribution(dataFrq,study_environment=study_environment)

# Fig - Region and crop ranking text
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <-readRDS(file.path(study_environment$wd$output,"estimations/CropID_Pooled_disabled_TL_hnormal_optimal.rds"))$disagscors
res$disasg <- res$disagscors_var
res$level <- res$disagscors_level
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[res$input %in% "MTE",]

reg <- res[res$disagscors_var %in% "Region",]
reg <- reg[order(reg$Estimate),]
paste0(paste0(reg$level," (",round(reg$Estimate,2),"%)"),collapse = ", ")

CROP <- res[res$disagscors_var %in% "CROP",]
CROP <- CROP[order(CROP$Estimate),]
paste0(paste0(CROP$level," (",round(CROP$Estimate,2),"%)"),collapse = ", ")

