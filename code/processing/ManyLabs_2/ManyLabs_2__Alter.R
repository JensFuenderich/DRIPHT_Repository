#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Disfluency Engages Analytic Processing (Alter, Oppenheimer, Epley, & Eyre, 2007, study 4)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://psycnet.apa.org/record/2007-16657-003
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

## This version applies the subset used in the meta-analysis of the ML2 publication

convert_raw_data_Alter_fun <- function(data, variable_info = NULL, output_folder){

  # # print variables for Alter condition
  # Alter_vars <- ML2_masteRkey[ML2_masteRkey$study.analysis == "Alter.1",]
  # Alter_vars <- eval(parse(text=Alter_vars$study.vars[1]))
  # print(Alter_vars)

  ## construction of DV
  # combine items (scores on three syllogism tasks) to construct dependent variable

  # Control Group

  # multivariate selection
  response_item_1_control <- data$alt1.3
  response_item_5_control <- data$alt1.7
  response_item_6_control <- data$alt1.8

  # score the responses on items as correct or incorrect (retain NAs)
  score_item_1_control <- as.numeric(response_item_1_control == 7)
  score_item_5_control <- as.numeric(response_item_5_control == 3)
  score_item_6_control <- as.numeric(response_item_6_control == 8)

  # construct dependent variable as sum of correct responses
  dv_control <- rowSums(cbind(score_item_1_control, score_item_5_control, score_item_6_control))

  # Treatment Group

  # multivariate selection
  response_item_1_treatment <- data$alt2.3
  response_item_5_treatment <- data$alt2.7
  response_item_6_treatment <- data$alt2.8

  # score the responses on items as correct or incorrect (retain NAs)
  score_item_1_treatment <- as.numeric(response_item_1_treatment == 7)
  score_item_5_treatment <- as.numeric(response_item_5_treatment == 3)
  score_item_6_treatment <- as.numeric(response_item_6_treatment == 8)

  # construct dependent variable as sum of correct responses
  dv_treatment <- rowSums(cbind(score_item_1_treatment, score_item_5_treatment, score_item_6_treatment))


  # combine separate treatment & control files into one
  dv_treatment[is.na(dv_treatment)] = dv_control[is.na(dv_treatment)]
  dv <- dv_treatment

  score_item_1_treatment[is.na(score_item_1_treatment)] <- score_item_1_control[is.na(score_item_1_treatment)]
  score_item_5_treatment[is.na(score_item_5_treatment)] <- score_item_5_control[is.na(score_item_5_treatment)]
  score_item_6_treatment[is.na(score_item_6_treatment)] <- score_item_6_control[is.na(score_item_6_treatment)]
  dv_item_1 <- score_item_1_treatment
  dv_item_5 <- score_item_5_treatment
  dv_item_6 <- score_item_6_treatment

  # store raw values in a single vector per item for data cleaning
  response_item_1_treatment[is.na(response_item_1_treatment)] = response_item_1_control[is.na(response_item_1_treatment)]
  dv_raw_1 <- response_item_1_treatment
  response_item_5_treatment[is.na(response_item_5_treatment)] = response_item_5_control[is.na(response_item_5_treatment)]
  dv_raw_5 <- response_item_5_treatment
  response_item_6_treatment[is.na(response_item_6_treatment)] = response_item_6_control[is.na(response_item_6_treatment)]
  dv_raw_6 <- response_item_6_treatment

  ## Group
  # select independent variable (/items) of interest
  treatment_group <- data$alt2.2
  # change all NA to 0
  treatment_group <- replace(treatment_group, treatment_group > 0, 1)
  # selecting all control group variables
  control_group <- data$alt1.2
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
  Item_Level_before_cleaning <- data.frame(MultiLab = rep("ML2", length(dv)),
                                           MASC = rep("Alter", length(dv)),
                                           Data_Collection_Site = Data_Collection_Site,
                                           DV = dv,
                                           DV_Item_1 = dv_item_1,
                                           DV_Item_2 = dv_item_5,
                                           DV_Item_3 = dv_item_6,
                                           DV_raw_1 = dv_raw_1,
                                           DV_raw_2 = dv_raw_5,
                                           DV_raw_3 = dv_raw_6,
                                           Group = group,
                                           Language = data$Language,
                                           Setting = data$Setting
  )
  # apply data cleaning
  # including cases according to ML2_masteRkey
  #ML2_masteRkey[ML2_masteRkey$study.analysis == "Alter.1",]$study.cases.include

  ## create item level data (apply data cleaning)
  Item_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV_raw_1 > 0,
                  DV_raw_2 > 0,
                  DV_raw_3 > 0) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV_Item_1),
                  !is.na(DV_Item_2),
                  !is.na(DV_Item_3) # ,
                  # Language == "English",
                  # Setting == "In a lab"
    ) %>%
    dplyr::select(MultiLab,
                  MASC,
                  Data_Collection_Site,
                  DV_Item_1,
                  DV_Item_2,
                  DV_Item_3,
                  Group)

  ## create IPD level data (apply data cleaning)
  IPD_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV_raw_1 > 0,
                  DV_raw_2 > 0,
                  DV_raw_3 > 0) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV_Item_1),
                  !is.na(DV_Item_2),
                  !is.na(DV_Item_3) # ,
                  # Language == "English",
                  # Setting == "In a lab"
    ) %>%
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


