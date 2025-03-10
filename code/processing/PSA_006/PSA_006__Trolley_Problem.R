#### Convert Raw to Trial Level
### MultiLab: Psychological Science Accelerator - 006 (Bago et al., 2022)
### MASC: Trolley_Problem (Bago et al., 2022)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://www.nature.com/articles/s41562-022-01319-5
## Repository: https://github.com/marton-balazs-kovacs/trolleyMultilabReplication

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the readr package.

convert_raw_data_Trolley_Problem_fun <- function(data, variable_info = NULL, output_folder){

  ## the following chunk is copied from the PSA code to apply the according data cleaning
  # https://github.com/marton-balazs-kovacs/trolleyMultilabReplication/blob/master/R/add_ind_col_scale.R

  add_ind_col_scale <- function(df) {
    df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        hor_ind = mean(c(individualism_scale_1, individualism_scale_2, individualism_scale_3, individualism_scale_4)),
        ver_ind = mean(c(individualism_scale_5, individualism_scale_6, individualism_scale_7, individualism_scale_8)),
        hor_col = mean(c(individualism_scale_9, individualism_scale_10, individualism_scale_11, individualism_scale_12)),
        ver_col = mean(c(individualism_scale_13, individualism_scale_14, individualism_scale_15, individualism_scale_16))) %>%
      dplyr::ungroup()
  }

  ## the following chunks of code are mostly copied from the PSA code to apply the according data cleaning
  # https://github.com/marton-balazs-kovacs/trolleyMultilabReplication/blob/master/vignettes/manuscript.Rmd

  # Save data (filtering for familiarity)
  study1a <- dplyr::filter(data, include_study1a)

  # Add individualism and collectivism scales
  data <- add_ind_col_scale(data)

  ## Exclusion

  all_responses <-
    data %>%
    dplyr::count(Region, name = "all", .drop = TRUE)

  overall <-
    data %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(Reason = "N without exclusion",
                     n = n(),
                     .groups = "drop")

  exclude_careless <-
    data %>%
    dplyr::filter(!include_nocareless) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(Reason = "Careless responding",
                     n = n(),
                     .groups = "drop")

  exclude_confusion <-
    data %>%
    dplyr::filter(!include_noconfusion) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(Reason = "Confusion",
                     n = n(),
                     .groups = "drop")

  exclude_familiar <-
    data %>%
    dplyr::filter(!include_nofamiliarity) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(Reason = "Familiarity with moral dilemmas",
                     n = n(),
                     .groups = "drop")

  exclude_techproblem <-
    data %>%
    dplyr::filter(!include_notechproblem) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(Reason = "Technical problem",
                     n = n(),
                     .groups = "drop")

  exclude_nonnative <-
    data %>%
    dplyr::filter(!include_nonativelang) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(Reason = "Non-native speaker",
                     n = n(),
                     .groups = "drop")

  exclude_study1a <-
    data %>%
    dplyr::filter(!include_study1a_attention) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(Reason = "Failed attention check (Study1a)",
                     n = n(),
                     .groups = "drop")

  # Create a table with all included studies in
  all_wide <-
    dplyr::bind_rows(tibble(study = "study1a", dplyr::count(study1a, Region))) %>%
    tidyr::pivot_wider(names_from = "Region",
                       values_from = "n",
                       names_prefix = "n_") %>%
    dplyr::mutate(info = "Final sample",
                  n_All = n_Eastern + n_Southern + n_Western,
                  Reason = stringr::str_glue("{str_to_sentence(study)}")) %>%
    dplyr::select(-study)

  all_wide_wofamiliarity <-
    dplyr::bind_rows(tibble(study = "study1a", dplyr::count(study1a, Region))) %>%
    tidyr::pivot_wider(names_from = "Region",
                       values_from = "n",
                       names_prefix = "n_") %>%
    dplyr::mutate(info = "Final sample",
                  n_All = n_Eastern + n_Southern + n_Western,
                  Reason = stringr::str_glue("{str_to_sentence(study)}")) %>%
    dplyr::select(-study)

  exclude_all <-
    dplyr::bind_rows(overall,
                     exclude_careless,
                     exclude_confusion,
                     exclude_familiar,
                     exclude_techproblem,
                     exclude_nonnative,
                     exclude_study1a) %>%
    dplyr::left_join(all_responses, by = "Region") %>%
    dplyr::mutate(perc = n / all) %>%
    dplyr::select(-all) %>%
    tidyr::pivot_wider(names_from = "Region",
                       values_from = c(n, perc)) %>%
    dplyr::mutate(n_All = n_Eastern + n_Southern + n_Western,
                  perc_All = n_All/nrow(data),
                  info = "Reason to exclude") %>%
    dplyr::mutate_at(dplyr::vars(perc_All, perc_Eastern, perc_Southern, perc_Western),
                     list(~ dplyr::case_when(Reason == "N without exclusion" ~ NA_real_,
                                             TRUE ~ .)))


  #### https://github.com/marton-balazs-kovacs/trolleyMultilabReplication/blob/master/R/calculate_study1_stat.R

  d <- study1a %>%
    dplyr::select(c("trolley_1_rate", "trolley_2_rate")) %>%
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "Group",
                        values_to = "DV",
                        values_drop_na = TRUE)

  d$Data_Collection_Site <- study1a$lab

  ## full data set
  d <- data.frame(MultiLab = rep("PSA_006", nrow(d)),
                  MASC = rep("Trolley_Problem", nrow(d)),
                  Data_Collection_Site = d$Data_Collection_Site,
                  DV = d$DV,
                  DV_Item_1 = d$DV,
                  Group = d$Group
  )

  ## treatment and control group as 1 and 0
  # these are defined so that a positive effect is an effect in the same direction as the original result
  d$Group[d$Group == "trolley_1_rate"] <- 0
  d$Group[d$Group == "trolley_2_rate"] <- 1
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


















