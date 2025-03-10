#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Cardinal direction and socioeconomic status (Huang, Tse, & Cho, 2014)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918810225
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

## This version uses a subset that is reduced to "selecting only those participants, across all samples, who indicated that wealth tended to be in the north in their hometown" to achieve a close(r) replication.

convert_raw_data_Huang_CR_fun <- function(data, variable_info = NULL, output_folder){

  # # print variables for Huang
  # Huang_vars <- variable_info[variable_info$study.analysis == "Huang.1",]
  # Huang_vars <- eval(parse(text=Huang_vars$study.vars[1]))
  # print(Huang_vars)

  ## DV_Item_1
  # select dependent variable (/items) of interest
  # univariate selection
  dv_item_1_treatment <- data$huan1.1_Y1
  dv_item_1_control <- data$huan2.1_Y1
  dv_item_1_treatment[is.na(dv_item_1_treatment)] = dv_item_1_control[is.na(dv_item_1_treatment)]
  dv_item_1 <- -1 * (dv_item_1_treatment - 238)

  ## Group
  # select independent variable (/items) of interest
  treatment_group <- data$huan1.1_Y1
  # change all NA to 0
  treatment_group <- replace(treatment_group, treatment_group > 0, 1)
  # selecting all control group variables
  control_group <- data$huan2.1_Y1
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

  # include only cases that clicked inside the map
  include_treatment <- data$huan1.1_R0
  include_control <- data$huan2.1_R0
  include_treatment[which(include_treatment == 0)] = include_control[which(include_treatment == 0)]
  include <- include_treatment

  ## create item level data
  Item_Level_before_cleaning <- data.frame(MultiLab = rep("ML2", length(dv_item_1)),
                                           MASC = rep("Huang_CR", length(dv_item_1)),
                                           Data_Collection_Site = Data_Collection_Site,
                                           DV_Item_1 = dv_item_1,
                                           Group = group,
                                           Include = include,
                                           Homewealth = data$homewealth
  )
  # apply data cleaning
  # include cases that clicked inside the map
  Item_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(Include == 1) %>%
    dplyr::filter(DV_Item_1 > 0) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV_Item_1))

  ## include only cases as suggested by original authors
  # "selecting only those participants, across all samples, who indicated that wealth tended to be in the north in their hometown"
  Item_Level <- Item_Level_before_cleaning %>% dplyr::filter(Homewealth == 1)

  # delete exclusion/inclusion columns
  Item_Level$Homewealth <- NULL
  Item_Level$Include <- NULL

  ## create IPD level data
  IPD_Level <- Item_Level %>% dplyr::rename(DV = DV_Item_1)

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
