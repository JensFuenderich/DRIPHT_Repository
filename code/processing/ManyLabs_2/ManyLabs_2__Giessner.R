#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Vertical position and power (Giessner & Schubert, 2007)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918810225
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

# selection of cleaning products only according to varfun.Giessner.1 in https://raw.githubusercontent.com/ManyLabsOpenScience/manylabRs/master/R/manylabRs_SOURCE.R

convert_raw_data_Giessner_fun <- function(data, variable_info = NULL, output_folder){

  # print variables for Giessner condition
  Giessner_vars <- variable_info[variable_info$study.analysis == "Giessner.1",]
  Giessner_vars <- eval(parse(text=Giessner_vars$study.vars[1]))
  # print(Giessner_vars)

  ## DV_Item_1 - DV_Item_5
  # select dependent variable (/items) of interest
  # multivariate selection

  # create data frame with all items
  Items_df <- dplyr::select(data, c(Giessner_vars$Long[-1]))
  names(Items_df) <- paste("Item", as.character(1:ncol(Items_df)), sep = "_" )
  dv <- rowMeans(Items_df)

  ## Group
  # select independent variable (/items) of interest
  treatment_group <- data$geis1.1
  # change all NA to 0
  treatment_group <- replace(treatment_group, treatment_group > 0, 1)
  # selecting all control group variables
  control_group <- data$geis2.1
  # change all NA to 0
  control_group <- replace(control_group, control_group > 0, 2)
  # merge the two groups by NA
  treatment_group[is.na(treatment_group)] = control_group[is.na(treatment_group)]
  group <- treatment_group
  # recode group variable
  # treatment = 1, control = 0
  group <- replace(group, group == 2, 0)

  ## Data_Collection_Site
  Data_Collection_Site <- data$Source.Global


  ## create item level data
  # create df with columns for identification
  ID_df <- data.frame(MultiLab = rep("ML2", nrow(Items_df)),
                      MASC = rep("Giessner", nrow(Items_df)),
                      Data_Collection_Site = Data_Collection_Site)
  # create full df
  Item_Level_before_cleaning <- cbind(ID_df, Items_df, DV = dv, Group = group)

  # apply data cleaning
  # including cases according to ML2_masteRkey
  #ML2_masteRkey[ML2_masteRkey$study.analysis == "Giessner.1",]$study.cases.include

  ## create item level data (apply data cleaning)
  Item_Level <- Item_Level_before_cleaning %>%
    dplyr::filter_at(.tbl = .,
                     .vars = dplyr::vars(names(Items_df)),
                     .vars_predicate = dplyr::all_vars(. != -99)) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV)) %>%
    dplyr::select(!DV)

  ## create IPD level data (apply data cleaning)
  IPD_Level <- Item_Level_before_cleaning %>%
    dplyr::filter_at(.tbl = .,
                     .vars = dplyr::vars(names(Items_df)),
                     .vars_predicate = dplyr::all_vars(. != -99)) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV)) %>%
    dplyr::select(MultiLab,
                  MASC,
                  Data_Collection_Site,
                  DV,
                  Group)

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
