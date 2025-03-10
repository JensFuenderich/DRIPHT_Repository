#### Convert Raw to Trial Level
### MultiLab: Registered Data_Collection_Site Report (McCarthy et al.)
### MASC: Srull T. K., Wyer R. S. (1979). The role of category accessibility in the interpretation of information about persons: Some determinants and implications.

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918777487
## Repository: https://osf.io/mcvt7/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Srull2_fun <- function(data, variable_info = NULL, output_folder){


  # parts of the code were adopted from scripts of the RRR9 project: https://osf.io/ck625
  # the lines before the reverse coding are taken from the analysis file in the zip folder (which is downloaded through the 1_process_all_MultiLabs.Rmd)
  # OSF link to the zip folder: https://osf.io/qegfd

  # "these came from same lab but given two different names"
  RRR_09_data[RRR_09_data$lab.name == "Nahari", ]$lab.name <- "klein Selle \\& Rozmann"
  RRR_09_data[RRR_09_data$lab.name == "klein Selle", ]$lab.name <- "klein Selle \\& Rozmann"
  # "removes extra back slash before ampersand"
  RRR_09_data$lab.name <- gsub("\\\\", "", RRR_09_data$lab.name)
  # "was listed as both researchers"
  RRR_09_data[RRR_09_data$lab.name == "LoschelderMechtel", ]$lab.name <- "Loschelder"
  # "lab.name variable was listed as second author"
  RRR_09_data[RRR_09_data$lab.name == "Voracek", ]$lab.name <- "Tran"

  # first, reverse code items

  data$ron.kindR        <- 10 - data$ron.kind
  data$ron.considerateR <- 10 - data$ron.considerate
  data$ron.thoughtfulR  <- 10 - data$ron.thoughtful

  # second, ensure there are no missing values for the behavior ratings

  data$behavMissing <- rowSums(cbind(is.na(data$behavior2),
                                     is.na(data$behavior5),
                                     is.na(data$behavior8),
                                     is.na(data$behavior10),
                                     is.na(data$behavior13)))

  # third, average the individual behavior ratings together
  # for those who were not missing any behavior ratings

  data$amb.behaviors <- ifelse(data$behavMissing > 0,
                               NA,
                               rowMeans(cbind(data$behavior2, data$behavior5,
                                              data$behavior8, data$behavior10,
                                              data$behavior13),
                                        na.rm = TRUE))

  ## excluding individual participants from complete dataset

  data <- subset(data,
                     data$sw.prime.complete == "complete" &
                       data$behavMissing == 0 &
                       data$age >= 18 &
                       data$age <= 25 &
                       !is.na(data$gender) &
                       (data$inclusion == "inclusion Srull only" |
                          data$inclusion == "inclusion both RRR"))

  ## create final data set
  d <- data.frame(MultiLab = rep("RRR_09", nrow(data)),
                  MASC = rep("Srull2", nrow(data)),
                  Data_Collection_Site = data$lab.name,
                  DV = data$amb.behaviors,
                  DV_Item_1 = data$behavior2,
                  DV_Item_2 = data$behavior5,
                  DV_Item_3 = data$behavior8,
                  DV_Item_4 = data$behavior10,
                  DV_Item_5 = data$behavior13,
                  Group =  data$sw.prime.cond
  )


  ## treatment and control group as 1 and 0
  d$Group[d$Group == "neutral"] <- 0
  d$Group[d$Group == "hostile"] <- 1
  d$Group <- as.numeric(d$Group)

  ## remove NA
  d <- d %>% dplyr::filter(is.na(DV) == FALSE)

  ## reducing the data to primary analysis data (excluding Data_Collection_Sites with treatment/control group n < 100)
  # create TRUE/FALSE vector to to exclude Data_Collection_Sites with smaller group sizes
  include <- unlist(lapply(unique(d$Data_Collection_Site),
                           function(name){
                             nrow(subset(d, d$Data_Collection_Site == name & d$Group == 1)) >= 100 &
                               nrow(subset(d, d$Data_Collection_Site == name & d$Group == 0)) >= 100
                           }))
  # filter to reduce data set
  d <- d %>% dplyr::filter(Data_Collection_Site %in% unique(Data_Collection_Site)[include])

  ## change the source names, so just the first letter is capitalized
  d$Data_Collection_Site <- stringr::str_to_title(d$Data_Collection_Site)

  ## create item level data
  Item_Level <- d %>% dplyr::select(-DV)

  ## create ipd
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






