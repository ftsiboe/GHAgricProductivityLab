rm(list = ls(all = TRUE)); gc()  
library('magrittr');library(ggplot2);library(gridExtra)
library(dplyr);library(gtable);library(stringr);library(cowplot)
devtools::document()  

project_name = "education"
study_environment <- readRDS(
  file.path(paste0("replications/", project_name, "/output"),
            paste0(project_name,"_study_environment.rds")))

mspecs_optimal <- study_environment$match_specification_optimal

source("data-raw/scripts/figures_and_tables.R")

Keep.List<-c("Keep.List",ls())

# Main Specification   
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- tab_main_specification(study_environment)
wb <- openxlsx::loadWorkbook(file.path(study_environment$wd$output,paste0(project_name,"_results_smf.xlsx")))
openxlsx::writeData(wb, sheet = "msf",res[res$Survey %in% "GLSS0",] , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,file.path(study_environment$wd$output,paste0(project_name,"_results_smf.xlsx")),overwrite = T)


# Fig - Heterogeneity    
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS(file.path(study_environment$wd$estimations,"CropID_Pooled_educated_TL_hnormal_optimal.rds"))$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level  <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_lvl",]
res <- res[c("disasg","level","fxnforms","distforms","Survey","input","technology_variable","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]

fig <- fig_heterogeneity00(res=res,y_title="Level difference (Educated less Uneducated)\n",study_environment=study_environment)
fig[["genderAge"]] <- fig[["genderAge"]] + theme(axis.text.x = element_text(size = 5.5))
ggsave(file.path(study_environment$wd$output,"figure","heterogeneity_crop_region.png"), fig[["crop_region"]],dpi = 600,width = 8, height = 5)
ggsave(file.path(study_environment$wd$output,"figure","heterogeneity_genderAge.png"), fig[["genderAge"]],dpi = 600,width = 8, height = 5)


rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS("results/estimations/CropID_Pooled_educated_TL_hnormal_optimal.rds")$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_lvl",]
res <- res[c("disasg","level","FXN","DIS","Survey","input","TCH","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]
fig <- fig_heterogeneity00(res=res,y_title="Difference (Educated less Uneducated)\n")
ggsave("results/figures/heterogeneity_crop_region.png", fig[["crop_region"]],dpi = 600,width = 8, height = 5)
ggsave("results/figures/heterogeneity_genderAge.png", fig[["genderAge"]],dpi = 600,width = 5, height = 5)

# Fig - Robustness              
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_robustness(y_title="\nDifference (%) [Educated less Uneducated]",
               res_list = c("results/estimations/CropID_Pooled_educated_CD_hnormal_optimal.rds",
                             list.files("results/estimations/",pattern = "CropID_Pooled_educated_TL_",full.names = T)))

# Fig - Matching TE      
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_input_te(y_title="\nEducation gap (%)",tech_lable=c("Full sample", "Educated sample", "Uneducated sample"))

# Fig - Covariate balance 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance()

# Fig - Distribution 
dataFrq <- readRDS("results/estimations/CropID_Pooled_educated_TL_hnormal_fullset.rds")
dataFrq <- dataFrq$ef_dist
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq <- dataFrq[dataFrq$stat %in% "weight",]
dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted",]
dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),levels = 0:1,labels = c("Uneducated","Educated"))
fig_dsistribution(dataFrq)
