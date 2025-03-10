#### Convert Raw to Trial Level
### MultiLab: Many Labs 1 (Klein et al., 2014)
### MASC: Retrospective gambler’s fallacy (Oppenheimer & Monin, 2009)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://econtent.hogrefe.com/doi/10.1027/1864-9335/a000178
## Repository: https://osf.io/wx7ck/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Oppenheimer_Monin_fun <- function(data, variable_info = NULL, output_folder){

  ## full data set
  d <- data
  ## create subset for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("ML1", nrow(d)),
                  MASC = rep("Oppenheimer_Monin", nrow(d)),
                  Data_Collection_Site = d$sample,
                  DV = d$gambfalDV,
                  Group = as.character(d$gambfalgroup)
  )

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "three6"] <- 1
  d$Group[d$Group == "two6"] <- 0
  d$Group <- as.numeric(d$Group)

  ## remove NA according to pre-aggregated variable
  d <- d[complete.cases(d$DV),]

  ## create item level data
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






