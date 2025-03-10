#### Convert Raw to Trial Level
### MultiLab: Registered Data_Collection_Site Report (Bouwmeester et al.)
### MASC: Rand, D. G., Greene, J. D., & Nowak, M. A. (2012). Spontaneous giving and calculated greed.

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/1745691617693624
## Repository: https://osf.io/scu2f/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Rand_fun <- function(data, variable_info = NULL, output_folder){

  # extract raw contribution data
  DV_treatment <- unlist(lapply(1:length(data), function(x){as.numeric(unlist(data[[x]][,18]))}))
  DV_control <- unlist(lapply(1:length(data), function(x){as.numeric(unlist(data[[x]][,24]))}))
  DV <- DV_treatment
  DV[is.na(DV)] = DV_control[is.na(DV)]
  # create % contribution
  total <- unlist(lapply(names(data), function(x){
    rep(max(as.numeric(c(unlist(data[[x]][,18]), unlist(data[[x]][,24]))), na.rm = TRUE), times = nrow(data[[x]]))
  }))
  DV <- (DV/total)*100

  # group
  group <- DV_treatment * 0 + 1
  control <- DV_control * 0
  group[is.na(group)] = control[is.na(group)]

  # Data_Collection_Site
  Data_Collection_Site <- unlist(lapply(names(data), function(x){rep(x, times = nrow(data[[x]]))}))

  # Exclusion
  age <- unlist(lapply(1:length(data), FUN = function(x){
    2015 - (1919 + as.numeric(unlist(data[[x]][,85])))
  }))

  ## final data frame
  d <- data.frame(MultiLab = rep("RRR_07", length(Data_Collection_Site)),
                  MASC = rep("Rand", length(Data_Collection_Site)),
                  Data_Collection_Site = Data_Collection_Site,
                  DV = DV,
                  Group = group,
                  Age = age
  )

  ## exclude according to age
  d <- d %>% dplyr::filter(Age >= 18 & Age <=34) %>% dplyr::select(-Age)

  ## remove NA
  d <- stats::na.omit(d)

  ## create Item level data
  Item_Level <- d %>% dplyr::rename(DV_Item_1 = DV)

  ## create IPD level data
  IPD_Level <- d

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






