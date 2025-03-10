#### Convert Raw to Trial Level
### MultiLab: Psychological Science Accelerator - CR 003 (Nguyen et al., 2022)
### MASC: Self Determination (Nguyen et al., 2022)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://www.pnas.org/doi/full/10.1073/pnas.2111091119
## Repository: https://osf.io/fc9y7/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the readr package.

### There are three conditions in the PSACR_003 data-set:
## "Controlling", "Neutral" and "Autonomy support"
## This script exports a data-set with the groups: "Controlling" and "Autonomy support" (for the largest possible effect)

convert_raw_data_Self_Determination_fun <- function(data, variable_info = NULL, output_folder){

  ## full data set
  d <- data

  ## apply filters as specified in the original code
  ## largely copied from https://osf.io/6dxv7

  # remove those with reported age less than 18
  data_less_than_18 <- dplyr::filter(d, age < 18) # identify data under 18 years old
  d <- dplyr::filter(d, is.na(age) | age > 17) # this leaves n = 27,158

  # count number of items completed before and after manipulation
  pre_items <- dplyr::select(d, BL_adhere_1, BL_avoid_1, BL_avoid_2) # 3 items
  post_items <- dplyr::select(d, manip_1, r_manip_2, r_manip_3, motiv_2, motiv_4, motiv_7, motiv_8, motiv_1, motiv_3, motiv_5, motiv_6, defy_1, defy_2, defy_3, defy_4, intention_1w, avoid_1w_1, avoid_1w_2, avoid_1w_3, intention_6m, avoid_6m_1, avoid_6m_2, avoid_6m_3, avoid_6m_4, avoid_6m_5, avoid_6m_6, avoid_6m_7) # 27 items

  d$missing_pre <- rowSums(is.na(pre_items)) # count number of pre-manipulation items completed
  d$missing_post <- rowSums(is.na(post_items)) # count number of post-manipulation items completed

  # only include participants who have filled out at least 1/4 of the total post-manipulation items (7 items)
  data_no_post_items <- dplyr::filter(d, missing_post > 20) # identify data with 75% missing data for post-manipulation items
  dplyr::count(group_by(data_no_post_items, cond)) # count per condition
  d <- dplyr::filter(d, missing_post < 21) #this leaves n = 25,718

  # for 3-item manipulation check, alpha yield < .70,
  # and only 2 items yield > .30 --> this result to new alpha = .82
  # recode r_manip_2 and r_manip_3 so higher score means more perceived control
  d$manip_2 <- 8 - d$r_manip_2
  d$manip_3 <- 8 - d$r_manip_3
  manip <- dplyr::select(d, manip_2, manip_3)
  d$manip <- rowMeans(manip, na.rm = TRUE)


  ## create DV
  DV <- rowMeans(cbind(d$defy_1,
                       d$defy_2,
                       d$defy_3,
                       d$defy_4),
                 na.rm = TRUE)

  ## create subset for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("PSACR_003", nrow(d)),
                  MASC = rep("Self_Determination", nrow(d)),
                  Data_Collection_Site = d$COUNID,
                  DV = DV,
                  DV_Item_1 = d$defy_1,
                  DV_Item_2 = d$defy_2,
                  DV_Item_3 = d$defy_3,
                  DV_Item_4 = d$defy_4,
                  Group = d$cond
  )

  ## treatment (1) and control group (3) as 1 and 0 (remove third group)
  ## "The group (variable named 'cond') is coded as follow: 1 = Controlling, 2 = Neutral, 3 = Autonomy support" (cited from https://osf.io/6dxv7)
  d <- d %>% dplyr::filter(Group != 2)
  d$Group[d$Group == 1] <- 1
  d$Group[d$Group == 3] <- 0
  d$Group <- as.numeric(d$Group)

  ## remove NA according to pre-aggregated variable
  d <- d[complete.cases(d$DV),]

  ## create item level data
  Item_Level <- d %>% dplyr::select(-c(DV))

  ## create IPD level data
  IPD_Level <- d %>% dplyr::select(MultiLab,
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

