#### Convert Raw to Trial Level
### MultiLab: Registered Data_Collection_Site Report (Eerland et al., 2016)
### MASC: Hart & Albarracín (2011)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/1745691615605826
## Repository: https://osf.io/d3mw4/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Hart1_fun <- function(data, variable_info = NULL, output_folder){

  # create vector with Data_Collection_Site names
  Data_Collection_Site <- rep(names(data), times = sapply(data, nrow))

  # collect DV information from the list objects
  DV <- as.vector(unlist(lapply(data, function(data){data %>% dplyr::select(Intentionality)})))

  # collect group information from the list objects
  group <- as.vector(unlist(lapply(data, function(data){data %>% dplyr::select(Condition)})))

  ## create subset for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("RRR_03", length(DV)),
                  MASC = rep("Hart1", length(DV)),
                  Data_Collection_Site = Data_Collection_Site,
                  DV = DV,
                  Group = group
  )

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "perfective"] <- 1
  d$Group[d$Group == "imperfective"] <- 0

  ## remove NA
  d <- stats::na.omit(d)

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






