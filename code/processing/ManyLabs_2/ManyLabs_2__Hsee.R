#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Less-is-better effect (Hsee, 1998)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918810225
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Hsee_fun <- function(data, variable_info = NULL, output_folder){

  # # print variables for Hsee condition
  # Hsee_vars <- ML2_masteRkey[ML2_masteRkey$study.analysis == "Hsee.1",]
  # Hsee_vars <- eval(parse(text=Hsee_vars$study.vars[1]))
  # print(Hsee_vars)

  ## DV_Item_1
  # select dependent variable (/items) of interest
  # univariate selection
  dv_item_1_treatment <- data$hsee1.1
  dv_item_1_control <- data$hsee2.1
  dv_item_1_treatment[is.na(dv_item_1_treatment)] = dv_item_1_control[is.na(dv_item_1_treatment)]
  dv_item_1 <- dv_item_1_treatment


  ## Group
  # select independent variable (/items) of interest
  treatment_group <- data$hsee1.1
  # change all NA to 0
  treatment_group <- replace(treatment_group, treatment_group > 0, 1)
  # selecting all control group variables
  control_group <- data$hsee2.1
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
  Item_Level_before_cleaning <- data.frame(MultiLab = rep("ML2", length(dv_item_1)),
                                           MASC = rep("Hsee", length(dv_item_1)),
                                           Data_Collection_Site = Data_Collection_Site,
                                           DV_Item_1 = dv_item_1,
                                           Group = group
  )
  # apply data cleaning
  # including cases according to ML2_masteRkey
  #ML2_masteRkey[ML2_masteRkey$study.analysis == "Hsee.1",]$study.cases.include

  ## create item level data (apply data cleaning)
  Item_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV_Item_1 > 0) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV_Item_1))

  ## create IPD level data (apply data cleaning)
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


