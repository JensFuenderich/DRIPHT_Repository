#### Convert Raw to Trial Level
### MultiLab: Dang et al. 2021
### MASC: The Ego Could Be Depleted, Providing Initial Exertion Is Depleting (Dang, Liu, Liu, and Mao, 2017)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://doi.org/10.1177/1948550619887702
## Repository:https://osf.io/3txav/

## Type of effect: Experimental
## DV: error rate in 120 trials of an antisaccade task

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Dang2017_fun <- function(data, variable_info = NULL, output_folder){

  data_df <- do.call(rbind.data.frame, data)

  d <- data.frame(MultiLab = rep("Dang_2021", nrow(data_df)),
                  MASC = rep("Dang2017", nrow(data_df)),
                  Data_Collection_Site = rep(names(data), times = unlist(lapply(data, nrow))),
                  DV = data_df$error_rate,
                  Group = data_df$Condition,
                  Age = as.numeric(as.character(data_df$age)))

  # change group indication
  d$Group <- ifelse(d$Group == "control", 0, 1)

  d <- d %>% dplyr::filter(Age >= 18) %>% dplyr::filter(Age <= 30) %>% dplyr::select(-Age)

  ## remove NA
  d <- na.omit(d)

  ## create item level data & ipd
  Item_Level <- IPD_Level <- d

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






