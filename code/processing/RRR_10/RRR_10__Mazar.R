#### Convert Raw to Trial Level
### MultiLab: Registered Data_Collection_Site Report (Verschuere et al.)
### MASC: Mazar N., Amir O., Ariely D. (2008). The dishonesty of honest people: A theory of self-concept maintenance.

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/2515245918781032
## Repository: https://osf.io/mcvt7/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Mazar_fun <- function(data, variable_info = NULL, output_folder){

  ## collect relevant data
  data <- data %>%
    dplyr::filter(inclusion == "inclusion Mazar only" | inclusion == "inclusion both RRR") %>%
    dplyr::filter(maz.cheat.cond == "cheat")

  ## use the selection of labs from Fig.2 in the publicatiomn
  labs_in_paper <- c("Laine", "klein Selle & Rozmann", "Aczel", "Ferreira-Santos", "Meijer", "Loschelder", "Wick", "Suchotzki", "Sutan", "Vanpaemel", "Verschuere", "Wiggins", "González-Iraizoz", "Koppel", "Birt", "McCarthy", "Evans", "Holzmeister", "Özdoğru")

  ## apply selection
  data <- data[data$lab.name %in% labs_in_paper,]

  ## create df for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("RRR_10", nrow(data)),
                  MASC = rep("Mazar", nrow(data)),
                  Data_Collection_Site = data$lab.name,
                  DV = data$num.boxes,
                  Group = data$maz.prime.cond
  )

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "books"] <- 0
  d$Group[d$Group == "commandments"] <- 1
  d$Group <- as.numeric(d$Group)

  Item_Level <- IPD_Level <- d
#
#
#   ## remove NA
#   d <- d %>% dplyr::filter(is.na(DV) == FALSE)
#
#
#   ## create item level data
#   Item_Level <- d %>% dplyr::select(-DV)
#
#   ## create ipd
#   IPD_Level <- d %>% dplyr::select(MultiLab,
#                                    MASC,
#                                    Data_Collection_Site,
#                                    DV,
#                                    Group)

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






