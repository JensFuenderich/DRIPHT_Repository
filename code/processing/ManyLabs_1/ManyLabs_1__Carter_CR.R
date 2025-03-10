#### Convert Raw to Trial Level
### MultiLab: Many Labs 1 (Klein et al., 2014)
### MASC: Flag Priming (Carter, Ferguson, & Hassin, 2011; Study 2)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://econtent.hogrefe.com/doi/10.1027/1864-9335/a000178
## Repository: https://osf.io/wx7ck/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

## This version uses a subset that is reduced to replications from the USA to achieve a close(r) replication.

convert_raw_data_Carter_CR_fun <- function(data, variable_info = NULL, output_folder){

  ## full data set
  d <- data
  ## create subset for this replication
  d <- data.frame(MultiLab = rep("ML1", nrow(d)),
                  MASC = rep("Carter_CR", nrow(d)),
                  Data_Collection_Site = d$sample,
                  DV = d$flagdv,
                  DV_Item_1 = d$flagdv1,
                  DV_Item_2 = d$flagdv2,
                  DV_Item_3 = d$flagdv3,
                  DV_Item_4 = d$flagdv4,
                  DV_Item_5 = d$flagdv5,
                  DV_Item_6 = d$flagdv6,
                  DV_Item_7 = d$flagdv7,
                  DV_Item_8 = d$flagdv8,
                  Group = as.character(d$flagGroup),
                  US_Samples = as.character(d$us_or_international)
  )


  # remove unintelligble group identifier
  d <- d[d$Group != 2,]

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "no prime"] <- 0
  d$Group[d$Group == "flag prime"] <- 1
  d$Group <- as.numeric(d$Group)

  ## possible exclusions
  d <- subset(d, d$US_Samples == "US")
  d$US_Samples <- NULL # drop US_Samples column

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

