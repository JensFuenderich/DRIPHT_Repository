#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Sociometric status and well-being (Anderson, Kraus, Galinsky, & Keltner, 2012)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/1745691616652873
## Repository: https://osf.io/jymhe/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Anderson_fun <- function(data, variable_info = NULL, output_folder){

  # print variables for Anderson condition
  Anderson_vars <- variable_info[variable_info$study.analysis == "Anderson.1",]
  Anderson_vars <- eval(parse(text=Anderson_vars$study.vars[1]))
  # print(Anderson_vars)

  ## DV_Item_1 - DV_Item_25
  # select dependent variable (/items) of interest
  # multivariate selection
  Anderson_vars_treatment <- dplyr::select(data, Anderson_vars$High)
  Anderson_vars_control <- dplyr::select(data, Anderson_vars$Low)

  # selecting SWLS variables for treatment group
  dv_SWLS_treatment <- Anderson_vars_treatment[,1:5]

  # selecting SWLS variables for control group
  dv_SWLS_control <- Anderson_vars_control[,1:5]

  # selecting PANAS variables
  dv_PANAS_treatment <- Anderson_vars_treatment[,6:25]
  dv_PANAS_control <- Anderson_vars_control[,6:25]
  # selecting PANAS PA variables for treatment group
  dv_PA_treatment <- dv_PANAS_treatment[, c(1,4,5,8,9,12,14,17,18,19)]
  # selecting PANAS PA variables for control group
  dv_PA_control <- dv_PANAS_control[, c(1,4,5,8,9,12,14,17,18,19)]
  # selecting PANAS NA variables for treatment group
  dv_NA_treatment <- dv_PANAS_treatment[, c(2,3,6,7,10,11,13,15,16,20)]
  # selecting PANAS NA variables for control group
  dv_NA_control <- dv_PANAS_control[, c(2,3,6,7,10,11,13,15,16,20)]
  # negative recode
  dv_NAr_treatment <- 6 - dv_NA_treatment
  dv_NAr_control <- 6 - dv_NA_control

  # creating DV vectors
  dv_treatment <- (rowMeans(dv_SWLS_treatment) + rowMeans(dv_PA_treatment) + rowMeans(dv_NAr_treatment)) / 3
  dv_control <- (rowMeans(dv_SWLS_control) + rowMeans(dv_PA_control) + rowMeans(dv_NAr_control)) / 3

  # create final DV aggregate
  dv <- dv_treatment
  dv[is.na(dv)] = dv_control[is.na(dv)]

  # create data frame with all items
  Items_df <- cbind(dv_SWLS_treatment, dv_PA_treatment, dv_NAr_treatment)
  names(Items_df) <- paste("Item", as.character(1:ncol(Items_df)), sep = "_" )

  ## Group
  # select independent variable (/items) of interest
  treatment_group <- replace(dv_treatment, dv_treatment > 0, 1)
  control_group <- replace(dv_control, dv_control > 0, 2)
  treatment_group[is.na(treatment_group)] = control_group[is.na(treatment_group)]
  # treatment = 1, control = 0
  group <- replace(treatment_group, treatment_group == 2, 0)

  ## Data_Collection_Site
  Data_Collection_Site <- data$Source.Global

  ## create item level data
  # create df with columns for identification
  ID_df <- data.frame(MultiLab = rep("ML2", nrow(Items_df)),
                      MASC = rep("Anderson", nrow(Items_df)),
                      Data_Collection_Site = Data_Collection_Site)
  # create full df
  Item_Level_before_cleaning <- cbind(ID_df, Items_df, DV = dv, Group = group)

  # apply data cleaning
  # including cases according to ML2_masteRkey
  #ML2_masteRkey[ML2_masteRkey$study.analysis == "Anderson.1",]$study.cases.include
  Item_Level <-  Item_Level_before_cleaning %>% dplyr::filter(DV >= 0 & DV <= 100) %>% dplyr::filter(!is.na(DV)) %>% dplyr::select((!DV))

  # create full df
  IPD_Level_before_cleaning <- cbind(ID_df, DV = dv, Group = group)

  # create IPD level data (apply data cleaning)
  IPD_Level <- IPD_Level_before_cleaning %>% dplyr::filter(DV >= 0 & DV <= 100) %>% dplyr::filter(!is.na(DV))

  ## export item level data
  readr::write_csv(Item_Level,
                   file.path(output_folder,
                             paste(unique(Item_Level$MultiLab),
                                   unique(Item_Level$MASC),
                                   "Item_Level.csv",
                                   sep = "__")))

  ## export IPD level data
  readr::write_csv(IPD_Level,
                   file.path(output_folder,
                             paste(unique(IPD_Level$MultiLab),
                                   unique(IPD_Level$MASC),
                                   "IPD_Level.csv",
                                   sep = "__")))

  ## download codebook for IPD level data
  codebook <- readr::read_csv(url("https://raw.githubusercontent.com/JensFuenderich/MetaPipeX/main/Supplementary_Material/Table_Templates/lvl1_individual_participant_data/codebook_for_individual_participant_data.csv"))
  readr::write_csv(codebook,
                   file.path(output_folder, "/codebook_for_IPD_Level.csv")
  )
}
