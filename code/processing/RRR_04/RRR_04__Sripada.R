#### Convert Raw to Trial Level
### MultiLab: Registered Data_Collection_Site Report (Hagger et al.)
### MASC: Sripada, C., Kessler, D., & Jonides, J. (2014). Methylphenidate blocks effort-induced depletion of regulatory control in healthy volunteers.

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/1745691616664694
## Repository: https://osf.io/s3hfr/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Sripada_fun <- function(data, variable_info = NULL, output_folder){

  ## create final data set
  d <- data.frame(MultiLab = rep("RRR_04", length(data$Site)),
                  MASC = rep("Sripada", length(data$Site)),
                  Data_Collection_Site = data$Site,
                  DV = data$ExGauss.I.RTVar.MSIT,
                  Group =  as.character(data$Task),
                  Exclusion = data$Exclusions
  )

  ## apply exclusions and drop the column
  d <- subset(d, d$Exclusion == 1)
  d$Exclusion <- NULL

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "E"] <- 0
  d$Group[d$Group == "H"] <- 1
  d$Group <- as.numeric(d$Group)

  ## remove NA
  d <- stats::na.omit(d)

  # remove redundant parts from source names
  d$Data_Collection_Site <- stringr::str_remove(d$Data_Collection_Site,"BDEC_Results_Full_")
  d$Data_Collection_Site <- stringr::str_remove(d$Data_Collection_Site,".csv")

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

