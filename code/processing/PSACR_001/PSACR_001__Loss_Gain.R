#### Convert Raw to Trial Level
### MultiLab: Psychological Science Accelerator - CR 001 (Dorison et al., 2022)
### MASC: Loss Gain (Dorison et al., 2022)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://link.springer.com/article/10.1007/s42761-022-00128-3
## Repository: https://osf.io/m6q8f/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the readr package.

convert_raw_data_Loss_Gain_fun <- function(data, variable_info = NULL, output_folder){

  # study1_data.csv
  DF.s <- data$PSACR_001_study1_data
  # study1_country.csv
  DF.c <- data$PSACR_001_study1_country
  # general_data.csv
  DF.g <- data$PSACR_001_general_data

  # parts of the code were adopted from scripts of the PSACR_001 project (https://osf.io/es37w)

  ### preprocess study1_data.csv

  DF.s <- DF.s %>%
    # remove observations that should be excluded
    # Note: there was a technical issue that caused there to be duplicate observations for the excluded id below. Thus, we have excluded it.
    dplyr::filter(excluded_technical == "No",
           unique_id != "",
           unique_id != "__gloriousDragonflyXXX3KJkOduV3LZq4kRYxG8SRwIb9uHJteJZGnE8G-WSnG71") %>%

    # select relevant variables
    dplyr::select(unique_id, item_name, answer,
           where, language, assigned_design) %>%

    # truncate characters of item_name values
    dplyr::mutate(item_name = substr(x = item_name,
                              start = 4,
                              stop = 50)
    ) %>%

    # pivot wider (one row per participant)
    tidyr::pivot_wider(names_from = item_name,
                values_from = answer) %>%

    # split group variable
    tidyr::separate(col = group,
             into = c("frame", "version")) %>%

    # remove blank infoseeking observations
    dplyr::filter(infoseeking != "") %>%

    # begin rowwise operations
    dplyr::rowwise() %>%

    # code source data
    dplyr::mutate(where = dplyr::if_else(condition =
                             where == "pool" |
                             where == "social",
                           true = "nonpanel",
                           false = "panel")) %>%

    # relabel infoseeking data
    dplyr::mutate(infoseeking = dplyr::if_else(condition = infoseeking == "2",
                                 true = "No",
                                 false = "Yes")) %>%

    # convert variables to correct format
    dplyr::mutate_at(dplyr::vars(unique_id : version,
                   infoseeking, manipcheck),
              factor) %>%

    dplyr::mutate_at(dplyr::vars(likelihood_care : negative_fearful),
              as.numeric) %>%

    # remove incomplete observations
    na.omit() %>%

    # fix scaling of behavioral intention and policy support measures
    dplyr::mutate_at(.vars = dplyr::vars(likelihood_care : policy_decision),
              .funs = function(x){8 - x}) %>%

    # fix scaling of anxiety measure
    dplyr::mutate_at(.vars = dplyr::vars(negative_afraid : negative_fearful),
              .funs = function(x){6 - x}) %>%

    # process behavioral intention items
    dplyr::rename(behint_it1 = likelihood_care,
           behint_it2 = likelihood_isolate,
           behint_it3 = likelihood_home,
           behint_it4 = likelihood_mask) %>%

    dplyr::mutate(behint_avg = mean(c(behint_it1, behint_it2,
                               behint_it3, behint_it4))) %>%

    # process policy support items
    dplyr::rename(policy_it1 = policy_individuals,
           policy_it2 = policy_decision,
           policy_it3 = policy_limits,
           policy_it4 = policy_fine,
           policy_it5 = policy_officials) %>%

    dplyr::mutate(policy_sc1 = mean(c(policy_it1,
                               policy_it2)),
           policy_sc2 = mean(c(policy_it3,
                               policy_it4,
                               policy_it5))) %>%

    # process anxiety items
    dplyr::rename(anxiety_it1 = negative_afraid,
           anxiety_it2 = negative_anxious,
           anxiety_it3 = negative_fearful) %>%

    dplyr::mutate(anxiety_avg = mean(c(anxiety_it1,
                                anxiety_it2,
                                anxiety_it3))) %>%

    # reorder frame factor levels
    dplyr::mutate(frame = factor(frame,
                          levels = c("loss", "gain"))) %>%

    dplyr::ungroup()

  # Using two different operationalizations, identify participants who failed the manipulation check
  DF.s$manipcheck_pass1 <- 0
  DF.s[DF.s$frame == "gain" & DF.s$manipcheck == "gain", ]$manipcheck_pass1 <- 1
  DF.s[DF.s$frame == "loss" & DF.s$manipcheck == "loss", ]$manipcheck_pass1 <- 1

  DF.s$manipcheck_pass2 <- 0
  DF.s[DF.s$frame == "gain" & DF.s$manipcheck == "gain" |
         DF.s$manipcheck == "gain, neither", ]$manipcheck_pass2 <- 1
  DF.s[DF.s$frame == "loss" & DF.s$manipcheck == "loss" |
         DF.s$manipcheck == "loss, neither", ]$manipcheck_pass2 <- 1

  ### merge the data files from study1_data.csv and study1_country.csv
  DF.sc <- merge(x = DF.s,
                 y = DF.c,
                 by = "unique_id")

  rm(DF.c, DF.s)

  ### process general_data.csv

  DF.g <- DF.g %>%
    # remove ids not in survey dataset
    filter(unique_id %in% DF.sc$unique_id) %>%

    # select relevant variables
    select(unique_id, item_name, answer) %>%

    # truncate characters of item_name values
    mutate(item_name = substr(x = item_name,
                              start = 4,
                              stop = 50)
    ) %>%

    # pivot wider (one row per participant)
    tidyr::pivot_wider(names_from = item_name,
                values_from = answer) %>%

    # remove irrelevant variables
    dplyr::select(-c(country, nationality, state,
              consent_form))  %>%

    # pull information from outside reasons variable
    dplyr::mutate(outside_r_work =
                    dplyr::if_else(str_detect(outside_reasons, "1"),
                     true = 1,
                     false = 0),
           outside_r_health =
             dplyr::if_else(str_detect(outside_reasons, "2"),
                     true = 1,
                     false = 0),
           outside_r_food =
             dplyr::if_else(str_detect(outside_reasons, "3"),
                     true = 1,
                     false = 0),
           outside_r_buy =
             dplyr::if_else(str_detect(outside_reasons, "4"),
                     true = 1,
                     false = 0),
           outside_r_visit =
             dplyr::if_else(str_detect(outside_reasons, "5"),
                     true = 1,
                     false = 0),
           outside_r_phy =
             dplyr::if_else(str_detect(outside_reasons, "6"),
                     true = 1,
                     false = 0),
           outside_r_animal =
             dplyr::if_else(str_detect(outside_reasons, "7"),
                     true = 1,
                     false = 0),
           outside_r_other =
             dplyr::if_else(str_detect(outside_reasons, "8"),
                     true = 1,
                     false = 0),

           # note: you cannot use string detect here because some people said
           # they never went outside and then indicated going out
           # in the other response options.
           outside_r_none =
             dplyr::if_else(outside_reasons == "9",
                     true = 1,
                     false = 0)
    ) %>%
    dplyr::select(-outside_reasons) %>%

    # convert numeric variables to correct format
    dplyr::mutate_at(dplyr::vars(age, outside_number,
                   outside_crowded, outside_maskfreq,
                   prevent_handwash,
                   confidence_spread, confidence_contract,
                   worry_physical, worry_emotional,
                   household_number, household_health,
                   restrictions_manage, trust_central,
                   trust_local, govt_satisfaction,
                   outside_r_work : outside_r_work,
                   ses_subjective),
              as.numeric) %>%

    # convert factor variables to correct format
    ## convert blank observations to NA's
    dplyr::mutate_at(dplyr::vars(gender, outside_masktype,
                   prevent_mask, tested,
                   isolating, exposure,
                   recruit_method, employment,
                   employment_essential, education,
                   community, restrictions),
              ~na_if(., "")
    ) %>%

    ## set labels
    dplyr::mutate(gender = factor(gender,
                           labels = c("Female",
                                      "Male",
                                      "Other",
                                      "Decline")),
           outside_masktype = factor(outside_masktype,
                                     labels = c("Cloth mask",
                                                "Surgical mask",
                                                "N95/FFP1/P100/other respirator",
                                                "Homemade/makeshift mask",
                                                "Unsure",
                                                "None",
                                                "Not applicable")),
           prevent_mask = factor(prevent_mask,
                                 labels = c("Air",
                                            "Palms",
                                            "Tissue/handkerchief",
                                            "Elbow",
                                            "Mask",
                                            "Not applicable")),
           tested = factor(tested,
                           labels = c("Yes, tested positive",
                                      "Yes, tested negative, but diagnosed positive",
                                      "Yes, tested negative, not diagnosed positive",
                                      "No, diagnosed positive",
                                      "No")),
           isolating = factor(isolating,
                              labels = c("Yes", "No")),
           exposure = factor(exposure,
                             labels = c("Yes", "No")),
           recruit_method = factor(recruit_method,
                                   labels = c("Research agency",
                                              "University pool",
                                              "Friends or family",
                                              "Social media",
                                              "Other")),
           employment = factor(employment,
                               labels = c("Employed with current income",
                                          "Employed without current income",
                                          "Not employed with current income",
                                          "Not employed without current income")),
           employment_essential = factor(employment_essential,
                                         labels = c("Yes", "No", "Not employed")),
           education = factor(education,
                              labels = c("Less than high school",
                                         "High school",
                                         "Some college",
                                         "Two year degree",
                                         "Four year degree",
                                         "Professional degree",
                                         "Doctorate",
                                         "Unknown")), # Note: there isn't supposed to be an 8th option,
           # so it's not clear what this is.
           community = factor(community,
                              labels = c("Urban",
                                         "Suburban",
                                         "Rural")),
           restrictions = factor(restrictions,
                                 labels = c("Total lockdown",
                                            "Partial lockdown",
                                            "No lockdown"))
    ) %>%

    # fix scaling
    ## 10-item scale
    dplyr::mutate_at(.vars = dplyr::vars(ses_subjective),
              .funs = function(x){11 - x}) %>%

    ## 7-item scales
    dplyr::mutate_at(.vars = dplyr::vars(trust_central, trust_local, govt_satisfaction),
              .funs = function(x){8 - x}) %>%

    ## 6-item scales
    dplyr::mutate_at(.vars = dplyr::vars(outside_crowded, outside_maskfreq),
              .funs = function(x){7 - x}) %>%

    ## 5-item scales
    dplyr::mutate_at(.vars = dplyr::vars(confidence_spread, confidence_contract,
                           worry_physical, worry_emotional,
                           restrictions_manage),
              .funs = function(x){6 - x})

  ### merge previous data files with data file from general_data.csv

  DF.scg <- merge(x = DF.sc,
                  y = DF.g,
                  by = "unique_id",
                  all.x = T)

  rm(DF.g, DF.sc)

  ## full data set
  d <- DF.scg

  ## create subset for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("PSACR_001", nrow(d)),
                  MASC = rep("Loss_Gain", nrow(d)),
                  Data_Collection_Site = d$country,
                  DV = d$anxiety_avg,
                  DV_Item_1 = d$anxiety_it1,
                  DV_Item_2 = d$anxiety_it2,
                  DV_Item_3 = d$anxiety_it3,
                  Group = as.character(d$frame)
  )

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "loss"] <- 1
  d$Group[d$Group == "gain"] <- 0
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

