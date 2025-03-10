#### Convert Raw to Trial Level
### MultiLab: Registered Data_Collection_Site Report (O'Donnell et al.)
### MASC: Dijksterhuis, A., & van Knippenberg, A. (1998). The relation between perception and behavior, or how to win a game of trivial pursuit.

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/1745691618755704
## Repository: https://osf.io/k27hm/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Dijksterhuis_fun <- function(data, variable_info = NULL, output_folder){

  ## select Data_Collection_Sites according to "enough males"
  Data_Collection_Site_names <- names(data)[
    unlist(lapply(1:length(data),
                  function(x){unique(data[[x]]$enough_males)})) == 1
    ]
  data <- data[Data_Collection_Site_names]

  ## select DV
  DV <- unlist(lapply(Data_Collection_Site_names, function(x){as.numeric(unlist(data[[x]]$correct_pct))}))

  ## select group
  group <- unlist(lapply(Data_Collection_Site_names, function(x){as.numeric(unlist(data[[x]]$prime_code))}))

  ## Data_Collection_Site names
  Data_Collection_Site <- rep(Data_Collection_Site_names, times = as.numeric(sapply(data, nrow)))

  ## create final data set
  d <- data.frame(MultiLab = rep("RRR_08", length(DV)),
                  MASC = rep("Dijksterhuis", length(DV)),
                  Data_Collection_Site = Data_Collection_Site,
                  DV = DV,
                  Group =  group
  )

  ## remove NA
  d <- stats::na.omit(d)

  ## create item level % ipd  data
  IPD_Level <- Item_Level <- d

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






