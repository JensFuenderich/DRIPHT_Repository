#### Convert Raw to Trial Level
### MultiLab: Many Labs 1 (Klein et al., 2014)
### MASC: Currency priming (Caruso, Vohs, Baxter, & Waytz, 2013)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://econtent.hogrefe.com/doi/10.1027/1864-9335/a000178
## Repository: https://osf.io/wx7ck/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Caruso_fun <- function(data, variable_info = NULL, output_folder){

  ## full data set
  d <- data
  ## create subset for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("ML1", nrow(d)),
                  MASC = rep("Caruso", nrow(d)),
                  Data_Collection_Site = d$sample,
                  DV = d$Sysjust,
                  DV_Item_1 = d$sysjust1,
                  DV_Item_2 = d$sysjust2,
                  DV_Item_3 = d$sysjust3,
                  DV_Item_4 = d$sysjust4,
                  DV_Item_5 = d$sysjust5,
                  DV_Item_6 = d$sysjust6,
                  DV_Item_7 = d$sysjust7,
                  DV_Item_8 = d$sysjust8,
                  Group = as.character(d$MoneyGroup)
  )

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "Control group"] <- 0
  d$Group[d$Group == "Money priming group"] <- 1
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

  # ### raw data version (does not result in the published subset)
  #
  # ## select relevant data
  # raw <- data %>% dplyr::select(referrer,
  #                               moneyagea,
  #                               sysjust1,
  #                               sysjust2,
  #                               sysjust3,
  #                               sysjust4,
  #                               sysjust5,
  #                               sysjust6,
  #                               sysjust7,
  #                               sysjust8)
  #
  # # exclude data (less than 6 answers)
  # raw <- raw[which(rowSums(raw %>% dplyr::select(-c(referrer,
  #                                                   moneyagea)) %>% dplyr::mutate(. * 0 + 1) ) > 1),]
  #
  # raw[complete.cases(raw)  ,]
  #
  # raw %>% dplyr::drop_na
  #
  # # create Group indication
  # raw$Group <- ifelse(raw$moneyagea == ".", 1, 0 )
  #
  # ## create item level data
  # Item_Level <- data.frame(MultiLab = rep("ML1", nrow(raw)),
  #                          MASC = rep("Caruso", nrow(raw)),
  #                          Data_Collection_Site = raw$referrer,
  #                          DV_Item_1 = raw$sysjust1,
  #                          DV_Item_2 = raw$sysjust2,
  #                          DV_Item_3 = raw$sysjust3,
  #                          DV_Item_4 = raw$sysjust4,
  #                          DV_Item_5 = raw$sysjust5,
  #                          DV_Item_6 = raw$sysjust6,
  #                          DV_Item_7 = raw$sysjust7,
  #                          DV_Item_8 = raw$sysjust8,
  #                          Group = raw$Group
  # )
  #
  # ## create IPD level data
  # IPD_Level <- Item_Level
  # IPD_Level$DV_Item_1 <- IPD_Level %>% dplyr::select(-c(MultiLab, MASC, Data_Collection_Site, Group)) %>%
  #   rowMeans(.#, na.rm = TRUE
  #            )
  #
  # IPD_Level <- IPD_Level %>% dplyr::rename(DV = DV_Item_1) %>% dplyr::select(MultiLab,
  #                                                                            MASC,
  #                                                                            Data_Collection_Site,
  #                                                                            DV,
  #                                                                            Group)
  #
  # IPD_Level <- na.omit(IPD_Level)

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






