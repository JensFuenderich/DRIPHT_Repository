#### Convert Raw to Trial Level
### MultiLab: Many Labs 3 (Ebersole et al., 2016)
### MASC: Moral credentials and the expression of prejudice (Monin & Miller, 2001, study 1)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://www.sciencedirect.com/science/article/abs/pii/S0022103115300123
## Repository: https://osf.io/ct89g/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the readr package.

convert_raw_data_Monin_fun <- function(data, variable_info = NULL, output_folder){

  ## this is a "manipulation check" (which works -> produces an effect)
  # DV_treatment <- data$MostEndorse
  # DV_control <- data$SomeEndorse
  # DV <- DV_treatment
  # DV[is.na(DV)] = DV_control[is.na(DV)]

  ## this is the actual DV
  DV <- data$mcdv1

  ## create df for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("ML3", length(DV)),
                  MASC = rep("Monin", length(DV)),
                  Data_Collection_Site = data$Site,
                  DV = DV,
                  Group = data$CredCond,
                  Genderfactor = data$Genderfactor # we need to include this to get to the correct n after na.omit
  )

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "NoCredentials"] <- 0
  d$Group[d$Group == "Credentials"] <- 1
  d$Group <- as.numeric(d$Group)

  d <- stats::na.omit(d)

  # delete data selection column
  d$Genderfactor <- NULL

  ## create item level & ipd level data sets
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






