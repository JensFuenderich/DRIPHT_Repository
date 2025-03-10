#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Influence of incidental anchors on judgment (Critcher & Gilovich, 2008)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918810225
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

## accounting for problem 3 in this code: https://docs.google.com/document/d/1b7MTOAiB7NPWlYBnwkhNTsj9i-upDMODgQ9t_djjSz8/edit (Critcher stats are duplicated across columns for the pencil-and-paper sites (variables crit2.1 and crit1.1, which should only have a value for one or the other).)

convert_raw_data_Critcher_fun <- function(data, variable_info = NULL, output_folder){

  # # print variables for Critcher
  # Critcher_vars <- ML2_masteRkey[ML2_masteRkey$study.analysis == "Critcher.1",]
  # Critcher_vars <- eval(parse(text=Critcher_vars$study.vars[1]))
  # print(Critcher_vars)

  ## DV_Item_1
  # select dependent variable (/items) of interest
  # combine pencil labs & online labs
  # univariate selection
  dv_item_1_treatment_pen <- data$crit1.1_1_TEXT
  dv_item_1_control_pen <- data$crit2.1_1_TEXT
  dv_item_1_treatment_online <-  data$crit1.1
  dv_item_1_control_online <- data$crit2.1

  # insert online labs to NAs of pencil labs
  dv_item_1_treatment_pen[is.na(dv_item_1_treatment_pen)] = dv_item_1_treatment_online[is.na(dv_item_1_treatment_pen)]
  dv_item_1_control_pen[is.na(dv_item_1_control_pen)] = dv_item_1_control_online[is.na(dv_item_1_control_pen)]

  # store as treatment & control
  dv_item_1_treatment <- as.numeric(dv_item_1_treatment_pen)
  dv_item_1_control <-as.numeric(dv_item_1_control_pen)

  # insert control group data into treatment vector
  dv_item_1_treatment[is.na(dv_item_1_treatment)] = dv_item_1_control[is.na(dv_item_1_treatment)]
  dv_item_1 <- dv_item_1_treatment

  ## Group
  # select independent variable (/items) of interest
  treatment_group <- as.numeric(dv_item_1_treatment_pen)
  # change all non-NA to 1 (treatment)
  treatment_group[!is.na(treatment_group)] <- 1
  # selecting all control group variables
  control_group <- as.numeric(dv_item_1_control_pen)
  # change all non-NA to 0
  control_group[!is.na(control_group)] <- 0
  # merge the two groups by NA
  treatment_group[is.na(treatment_group)] = control_group[is.na(treatment_group)]
  group <- treatment_group

  ## Data_Collection_Site
  Data_Collection_Site <- data$Source.Global

  ## create item level data
  Item_Level_before_cleaning <- data.frame(MultiLab = rep("ML2", length(dv_item_1)),
                                           MASC = rep("Critcher", length(dv_item_1)),
                                           Data_Collection_Site = Data_Collection_Site,
                                           DV_Item_1 = dv_item_1,
                                           Group = group
  )
  # apply data cleaning
  # including cases according to ML2_masteRkey
  #ML2_masteRkey[ML2_masteRkey$study.analysis == "Critcher.1",]$study.cases.include
  Item_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV_Item_1 >= 0 & DV_Item_1 <= 100) %>%
    dplyr::filter(!is.na(Group), !is.na(DV_Item_1))

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






