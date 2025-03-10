#### Convert Raw to Trial Level
### MultiLab: Many Labs 2 (Klein et al., 2018)
### MASC: Moral violations and desire for cleansing (Zhong & Liljenquist, 2006)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918810225
## Repository: https://osf.io/8cd4r/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

## This version applies the subset used in the meta-analysis of the ML2 publication

convert_raw_data_Zhong_fun <- function(data, variable_info = NULL, output_folder){


  Zhong_items <- names(ML2_S2)[grep("zhon\\.dv\\.1\\_\\d{1,2}", names(ML2_S2))]

  items_cleaning <- Zhong_items[grep("2|3|7|8|10", Zhong_items)]
  items_other <- Zhong_items[grep("1|4|5|6|9", Zhong_items)]



  Zhong_dat <- data.frame(ML2_S2[,Zhong_items]
                          , source = ML2_S2$Source.Primary
                          , factor1 = ifelse(!is.na(ML2_S2$nCopied.zhon1), 1, ML2_S2$nCopied.zhon2)
                          , factor2 = ifelse(!is.na(ML2_S2$nCopied.zhon2), 0, ML2_S2$nCopied.zhon1)) %>%
    mutate(factor = ifelse(factor1 == 1, factor1, factor2)) %>% # constructing a group-variable
    filter(!is.na(rowSums(ML2_S2[,items_cleaning]))
           , !is.na(factor)
           , !is.na(rowSums(ML2_S2[,items_other]))
    )   # participants with missing data on the clean, other or factor variable are excluded


  Zhong_dat <- Zhong_dat[,-grep("^factor\\d", names(Zhong_dat))] # columns of factors are not needed


  Zhong_agg_dat <- data.frame(desirability_cleaning = rowMeans(Zhong_dat[,items_cleaning])
                              , desirability_other = rowMeans(Zhong_dat[,items_other])
                              , factor = Zhong_dat$factor
                              , source = Zhong_dat$source
  )


  ## create item level data
  Item_Level <- data.frame(MultiLab = rep("ML2", nrow(Zhong_agg_dat)),
                                           MASC = rep("Zhong", nrow(Zhong_agg_dat)),
                                           Data_Collection_Site = Zhong_agg_dat$source,
                                           DV_Item_1 = Zhong_agg_dat$desirability_cleaning,
                                           Group = Zhong_agg_dat$factor
  )

  ## create IPD level data
  IPD_Level <- Item_Level %>% dplyr::rename(DV = DV_Item_1)

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


