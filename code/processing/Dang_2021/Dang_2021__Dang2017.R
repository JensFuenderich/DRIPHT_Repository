#### Convert Raw to Trial Level
### MultiLab: Dang 2021
### MASC:

## Type of effect: Experimental
## DV: ??? Item Likert Scale

### What you need to source this script/ run this function
### ...

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






