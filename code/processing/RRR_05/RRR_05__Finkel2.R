#### Convert Raw to Trial Level
### MultiLab: Registered Data_Collection_Site Report (Cheung et al.)
### MASC: Finkel, E. J., Rusbult, C. E., Kumashiro, M., & Hannon, P. A. (2002). Dealing with betrayal in close relationships: Does commitment promote forgiveness?

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/10.1177/1745691616664694
## Repository: https://osf.io/s3hfr/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Finkel2_fun <- function(data, variable_info = NULL, output_folder){

  # parts of the code were adopted from scripts of the RRR5 project (https://osf.io/3nz7j/)

  all_Data_Collection_Sites_fun <- function(name){

    Data_Collection_Site <- data[[name]]

    # function that converts a column of Qualtrix output to useable form in R
    convert <- function(x){as.numeric(as.character(x))}

    include <- 0 # first run with only 0
    #include <- c(0,2) # second run with 0 and 2

    # exclusion criteria
    Exclude <- convert(Data_Collection_Site$Exclude)
    # include specified observations
    Data_Collection_Site <- Data_Collection_Site[which(Exclude %in% include),]

    # Condition #
    Condition <- with(Data_Collection_Site,convert(Q7.1)) # convert condition item
    Condition[Condition==1] <- 'H' # recode 1 (high) --> H
    Condition[Condition==2] <- 'L' # recode 2 (low) --> L
    Condition <- factor(Condition,levels=c('H','L')) # designate Condition as factor object
    stats::contrasts(Condition) <- stats::contr.sum(2) # set factor-effects contrasts (i.e., dummy code 1 for high and -1 for low)

    # Neglect Forgiveness #
    Neglect.raw <- with(Data_Collection_Site,data.frame(Q2.2_4,Q2.3_2,Q2.4_2,Q2.5_4,Q2.6_1,Q2.7_2,Q2.8_4,Q2.9_2,Q2.10_4,Q2.11_1,Q2.12_1,Q2.13_1)) # read Neglect items
    Neglect.data <- apply(Neglect.raw,2,convert) # convert Neglect items
    Neglect <- apply(Neglect.data,1,mean) # average Neglect across all items

    return(
      cbind(
        data.frame(
          Data_Collection_Site = rep(name, length(Neglect)),
          DV = Neglect,
          group = Condition
          ),
        Neglect.data)
      )
  }

  # apply data selection function
  all_Data_Collection_Sites <- lapply(names(data), all_Data_Collection_Sites_fun)

  # rename list objects
  names(all_Data_Collection_Sites) <- names(data)

  # create df from list
  all_Data_Collection_Sites_df <- do.call(rbind.data.frame, all_Data_Collection_Sites)

  # rename DV items
  names(all_Data_Collection_Sites_df) <- c("Data_Collection_Site",
                                  "DV",
                                  "group",
                                  stringr::str_c(rep("DV_Item_",
                                                     times = length(names(all_Data_Collection_Sites_df[,-c(1:3)]))),
                                                 1:length(names(all_Data_Collection_Sites_df[,-c(1:3)])))
                                  )

  ## create final data set
  d <- data.frame(MultiLab = rep("RRR_05",
                                 length(nrow(all_Data_Collection_Sites_df))),
                  MASC = rep("Finkel2",
                                           length(nrow(all_Data_Collection_Sites_df))),
                  Data_Collection_Site = all_Data_Collection_Sites_df$Data_Collection_Site,
                  DV = all_Data_Collection_Sites_df$DV,
                  Group =  as.character(all_Data_Collection_Sites_df$group)
                  )

  d <- cbind(d, all_Data_Collection_Sites_df[,-c(1:3)])

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "L"] <- 0
  d$Group[d$Group == "H"] <- 1
  d$Group <- as.numeric(d$Group)

  ## remove NA
  d <- stats::na.omit(d)

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






