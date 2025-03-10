#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Directionality and similarity (Tversky & Gati, 1978)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918810225
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

## The primary Data_Collection_Site of Tversky & Gati is a within design. We compared the average similarity ratings.

convert_raw_data_Gati_fun <- function(data, variable_info = NULL, output_folder){

  # print variables for Gati
  Gati_vars <- variable_info[variable_info$study.analysis == "Gati.2",]
  Gati_vars <- eval(parse(text=Gati_vars$study.vars[1]))
  # print(Gati_vars)

  ## DV_Item_1 - DV_Item_42
  # select dependent variable (/items) of interest
  # multivariate selection

  # create data frame with all items
  items_treatment <- dplyr::select(ML2_S2, Gati_vars$SimilarityA)
  items_control <- dplyr::select(ML2_S2, Gati_vars$SimilarityB)
  Items_df <- cbind(items_treatment, items_control)
  names(Items_df) <- paste("Item", as.character(1:ncol(Items_df)), sep = "_" )

  # aggregate DV
  dv_treatment <- rowMeans(items_treatment)
  dv_control <- rowMeans(items_control)
  dv <- dv_treatment
  dv[is.na(dv)] = dv_control[is.na(dv)]

  ## Group
  # create treatment/control group indication from the variables of both groups
  treatment_group <- replace(dv_treatment, dv_treatment > 0, 1)
  control_group <- replace(dv_control, dv_control > 0, 2)
  treatment_group[is.na(treatment_group)] = control_group[is.na(treatment_group)]
  # treatment = 1, control = 2
  group <- treatment_group
  # recode group variable
  # treatment = 1, control = 0
  group <- replace(group, group == 2, 0)

  ## Data_Collection_Site
  Data_Collection_Site <- data$Source.Global

  ## create item level data
  # create df with columns for identification
  ID_df <- data.frame(MultiLab = rep("ML2", nrow(Items_df)),
                      MASC = rep("Gati", nrow(Items_df)),
                      Data_Collection_Site = Data_Collection_Site)
  # create full df
  Item_Level_before_cleaning <- cbind(ID_df, Items_df, DV = dv, Group = group)

  # apply data cleaning
  # including cases according to ML2_masteRkey
  #ML2_masteRkey[ML2_masteRkey$study.analysis == "Gati.2",]$study.cases.include

  ## create item level data (apply data cleaning)
  Item_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV > 0) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV)) %>%
    dplyr::select(!DV)


  ## create IPD level data (apply data cleaning)
  IPD_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV > 0) %>%
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



