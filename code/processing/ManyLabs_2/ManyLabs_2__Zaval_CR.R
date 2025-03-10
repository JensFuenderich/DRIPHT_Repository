#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Priming “heat” increases belief in global warming (Zaval, Keenan, Johnson, & Weber, 2014)
### Computational Reproducibility (subset used in meta-analysis of ML2 publication)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918810225
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

## This version uses a subset that is reduced to those sites that administered the test in English to achieve a close(r) replication.

convert_raw_data_Zaval_CR_fun <- function(data, variable_info = NULL, output_folder){

  # # print variables for Zaval
  # Zaval_vars <- ML2_masteRkey[ML2_masteRkey$study.analysis == "Zaval.1",]
  # Zaval_vars <- eval(parse(text=Zaval_vars$study.vars[1]))
  # print(Zaval_vars)

  ## DV_Item_1
  # select dependent variable (/items) of interest
  # univariate selection
  dv_item_1 <- data$zav.dv.3

  ## Group
  # select independent variable (/items) of interest
  group <- data$zav.condition
  # recode group variable
  # treatment = 1 (heat), control = 0 (cold)
  group <- group - 1



  ## Data_Collection_Site
  Data_Collection_Site <- data$Source.Global

  ## create item level data
  Item_Level_before_cleaning <- data.frame(MultiLab = rep("ML2", length(dv_item_1)),
                                           MASC = rep("Zaval_CR", length(dv_item_1)),
                                           Data_Collection_Site = Data_Collection_Site,
                                           DV_Item_1 = dv_item_1,
                                           Group = group,
                                           Include = data$zav.include.strict,
                                           Language = data$Language
  )
  # apply data cleaning
  # including cases according to ML2_masteRkey
  #ML2_masteRkey[ML2_masteRkey$study.analysis == "Zaval.1",]$study.cases.include
  Item_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV_Item_1 > 0) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV_Item_1))

  # this is not really made explicit but seems to follow the idea in the paper
  Item_Level <- Item_Level %>% dplyr::filter(Include == TRUE)

  # subset to labs which adiminstered the test in english for most direct comparison
  Item_Level <- Item_Level %>% dplyr::filter(Language == "English")

  # delete exclusion/inclusion columns
  Item_Level$Include <- NULL
  Item_Level$Language <- NULL

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


