#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Structure promotes goal pursuit (Kay, Laurin, Fitzsimons, & Landau, 2014)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918810225
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

# DV transformation that follows the idea in https://raw.githubusercontent.com/ManyLabsOpenScience/manylabRs/master/R/manylabRs_SOURCE.R  (description of varfun.Kay.1)
# Note: in the original code "$residuals" is used as a way of extraction from the lm object. That creates a vector which omits NA and thus is too short for var.Order/var.DisOrder
# Solution: I added "na.action=na.exclude" and extracted the residuals with the function "residuals()". The result is a df with N, means and SDs that match what's reported in the publication.

convert_raw_data_Kay_fun <- function(data, variable_info = NULL, output_folder){

  # # print variables for Kay
  # Kay_vars <- variable_info[variable_info$study.analysis == "Kay.1",]
  # Kay_vars <- eval(parse(text=Kay_vars$study.vars[1]))
  # print(Kay_vars)

  ## dv_item_1_order
  # select dependent variable (/items) of interest
  # multivariate selection
  # see the note above for more details on the transformation
  dv_item_1_order <- data$kay1.4 #  dplyr::select(data, 'kay1.4')
  dv_item_2_order <- data$kay1.5 # dplyr::select(data, 'kay1.5')
  dv_item_3_order <- data$kay1.6 # dplyr::select(data, 'kay1.6')
  dv_item_1_disorder <- data$kay2.4 # dplyr::select(data, 'kay2.4')
  dv_item_2_disorder <- data$kay2.5 # dplyr::select(data, 'kay2.5')
  dv_item_3_disorder <- data$kay2.6 # dplyr::select(data, 'kay2.6')

  var.Order <- rowMeans(cbind(dv_item_2_order, dv_item_3_order))
  dv_treatment <- residuals(lm(scale(dv_item_1_order,scale=F)~var.Order, na.action=na.exclude)) + var.Order
  var.DisOrder <- rowMeans(cbind(dv_item_2_disorder, dv_item_3_disorder))
  dv_control <- residuals(lm(scale(dv_item_1_disorder,scale=F)~var.DisOrder, na.action=na.exclude)) + var.DisOrder

  # merge the two vectors
  dv_treatment[is.na(dv_treatment)] = dv_control[is.na(dv_treatment)]
  # store the merged vector as dv
  dv <- dv_treatment

  ## Group
  # select independent variable (/items) of interest
  # selecting all treatment group variables
  treatment_group <- cbind(dv_item_1_order, dv_item_2_order, dv_item_3_order)
  # take row means (any NA of a participant will result in exclusion of that row of data)
  treatment_group <- rowMeans(treatment_group)
  treatment_group <- replace(treatment_group, treatment_group > 0, 1)
  # selecting all control group variables
  control_group <- cbind(dv_item_1_disorder, dv_item_2_disorder, dv_item_3_disorder)
  # take row means (any NA of a participant will result in exclusion of that row of data)
  control_group <- rowMeans(control_group)
  # change all non NA values to 2
  control_group <- replace(control_group, control_group > 0, 2)
  # merge the two groups by NA
  treatment_group[is.na(treatment_group)] = control_group[is.na(treatment_group)]
  group <- treatment_group # treatment = 1, control = 2
  group <- replace(group, group ==2, 0) # treatment = 1, control = 0

  ## store raw DV values in a single vector per item for data cleaning
  dv_item_1_order[is.na(dv_item_1_order)] = dv_item_1_disorder[is.na(dv_item_1_order)]
  dv_raw_1 <- dv_item_1_order
  dv_item_2_order[is.na(dv_item_2_order)] = dv_item_2_disorder[is.na(dv_item_2_order)]
  dv_raw_2 <- dv_item_2_order
  dv_item_3_order[is.na(dv_item_3_order)] = dv_item_3_disorder[is.na(dv_item_3_order)]
  dv_raw_3 <- dv_item_3_order

  ## Data_Collection_Site
  Data_Collection_Site <- data$Source.Global

  ## create item level data
  Item_Level_before_cleaning <- data.frame(MultiLab = rep("ML2", length(dv)),
                                           MASC = rep("Kay", length(dv)),
                                           Data_Collection_Site = Data_Collection_Site,
                                           DV_raw_1 = dv_raw_1,
                                           DV_raw_2 = dv_raw_2,
                                           DV_raw_3 = dv_raw_3,
                                           DV = dv,
                                           Group = group
  )
  # apply data cleaning
  # including cases according to ML2_masteRkey
  #ML2_masteRkey[ML2_masteRkey$study.analysis == "Kai.1",]$study.cases.include

  ## create item level data (apply data cleaning)
  Item_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV_raw_1 > 0,
                  DV_raw_2 > 0,
                  DV_raw_3 > 0) %>%
    dplyr::filter(!is.na(Group),
                  !is.na(DV)) %>%
    dplyr::select(!DV)

  ## create IPD level data (apply data cleaning)
  IPD_Level <- Item_Level_before_cleaning %>%
    dplyr::filter(DV_raw_1 > 0,
                  DV_raw_2 > 0,
                  DV_raw_3 > 0) %>%
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

