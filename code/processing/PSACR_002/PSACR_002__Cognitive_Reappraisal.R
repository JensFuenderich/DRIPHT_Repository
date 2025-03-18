#### Convert Raw to Trial Level
### MultiLab: Psychological Science Accelerator - CR 002 (Wang et al., 2021)
### MASC: Cognitive_Reappraisal (Wang et al., 2021)

## More information on the effect may be taken from the:
## Main Publication: https://www.nature.com/articles/s41562-021-01173-x
## Repository: https://osf.io/jeu73/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the readr package.

### The original conditions were merged:
## control group: "Active Control" and "Passive Control"
## treatment group: "Repurposing" and "Reconstrual"

convert_raw_data_Cognitive_Reappraisal_fun <- function(data, variable_info = NULL, output_folder){

  ## A few chunks in the following code were copied from "Frequentist analysis.Rmd" https://osf.io/b5ch6

  ## full data set
  d <- data

  # data_wide is the data after preregistered exclusion
  # data_wide<-read.csv("data_wide_exclusion_12.23.2020.csv") # I replaced this
  d <- d %>%
    filter( is.na(condition)== FALSE & unique_id!="6166368928a08ae218a7252105dc862a5e1b106b__gIHcPPwiIDeq2v4KEfnvFPS0WnCrLkNjFHerNYGyT6hpV0EkvLS1OVw30c-ICJ8Z")
  # This ID appeared more than once in the demographic section of the survey. Eight other IDs appeared more than once in other sections of the survey and were removed during the data merging process. See details in the data pre-processing script.

  contrast <- c("contrast_1","contrast_2","contrast_control")
  #"contrast_control" contrasts the two control conditions
  d <- d %>%
    mutate_at(contrast,
              function(x) as.numeric(x))
  table(as.character(sapply(d, class)))

  d$condition<- as.factor(d$condition)
  d$condition <- relevel(d$condition, ref = "Passive Control")

  ## create DV
  # Sample after preregistered exclusion (Table 2)
  d <- d %>%
    rowwise() %>%
    mutate(
      negativeemotion_photo_mean = mean(c(C2_negativeemotion_photo_1,
                                          C2_negativeemotion_photo_2,
                                          C2_negativeemotion_photo_3,
                                          C2_negativeemotion_photo_4,
                                          C2_negativeemotion_photo_5,
                                          C2_negativeemotion_photo_6,
                                          C2_negativeemotion_photo_7,
                                          C2_negativeemotion_photo_8,
                                          C2_negativeemotion_photo_9,
                                          C2_negativeemotion_photo_10))
    )

  ## create numerical condition
  ## control group: "Active Control" and "Passive Control"
  ## treatment group: "Repurposing" and "Reconstrual"

  ## The following line would code 0: control and 1: treatment
  # d$condition_new <- ifelse(d$condition == "Repurposing", 1, 0) + ifelse(d$condition == "Reconstrual", 1, 0)
  ## To make sure that positive effects represent can be interpreted as being consistent with the hypothesis, the coding is:
  ## 0: treatment and 1: control
  d$condition_new <- ifelse(d$condition == "Active Control", 1, 0) + ifelse(d$condition == "Passive Control", 1, 0)

  ## create subset for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("PSACR_002", nrow(d)),
                  MASC = rep("Cognitive_Reappraisal", nrow(d)),
                  Data_Collection_Site = d$country,
                  DV = d$negativeemotion_photo_mean,
                  DV_Item_01 = d$C2_negativeemotion_photo_1,
                  DV_Item_02 = d$C2_negativeemotion_photo_2,
                  DV_Item_03 = d$C2_negativeemotion_photo_3,
                  DV_Item_04 = d$C2_negativeemotion_photo_4,
                  DV_Item_05 = d$C2_negativeemotion_photo_5,
                  DV_Item_06 = d$C2_negativeemotion_photo_6,
                  DV_Item_07 = d$C2_negativeemotion_photo_7,
                  DV_Item_08 = d$C2_negativeemotion_photo_8,
                  DV_Item_09 = d$C2_negativeemotion_photo_9,
                  DV_Item_10 = d$C2_negativeemotion_photo_10,
                  Group = d$condition_new
  )

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




