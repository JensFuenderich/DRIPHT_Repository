#### Convert Raw to Trial Level
### MultiLab: Registered Data_Collection_Site Report (Wagenmakers et al.)
### MASC: Strack, F., Martin, L. L., & Stepper, S. (1988). Inhibiting and facilitating conditions of the human smile: A nonobtrusive test of the facial feedback hypothesis.

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://journals.sagepub.com/doi/full/10.1177/1745691616674458
## Repository: https://osf.io/pkd65/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the packages readr and dplyr.

convert_raw_data_Strack_fun <- function(data, variable_info = NULL, output_folder){

  # parts of the code were adopted from scripts of the RRR6 project (https://osf.io/tbq26)

  all_Data_Collection_Sites_fun <- function(name){

    Data_Collection_Site <- data[[name]]

    # remove rows without cartoon ratings
    cartoonRatingSubset1 <- Data_Collection_Site[ , c("ratingCartoon1", "ratingCartoon2", "ratingCartoon3", "ratingCartoon4")]
    noObsIndex <- apply(cartoonRatingSubset1, 1, function(row) all(is.na(row)) )
    Data_Collection_Site <-  Data_Collection_Site[ ! noObsIndex, ]

    ##@@ CHECK EXCLUSION CRITERIA: 1 @@##

    # check whether participants performed 3 or 4 cartoon tasks correctly
    performedCorrectlyIndex <- ! is.na(Data_Collection_Site$performedCorrectlyTotal) & Data_Collection_Site$performedCorrectlyTotal >= 3
    Data_Collection_Site <- Data_Collection_Site[performedCorrectlyIndex, ]

    # check whether participants understood the cartoons
    comprehensionCartoonsIndex <- ! is.na(Data_Collection_Site$comprehensionCartoons) &  Data_Collection_Site$comprehensionCartoons == 1
    Data_Collection_Site <- Data_Collection_Site[comprehensionCartoonsIndex, ]

    # check whether participants were aware of goal
    notAwareOfGoalIndex <- ! is.na(Data_Collection_Site$awareOfGoal) & Data_Collection_Site$awareOfGoal == 0
    Data_Collection_Site <- Data_Collection_Site[notAwareOfGoalIndex, ]

    performedCorrectlyIndices <- Data_Collection_Site[ , c("performedCorrectlyCartoon1", "performedCorrectlyCartoon2",
                                                "performedCorrectlyCartoon3", "performedCorrectlyCartoon4")]

    # calculate mean rating for each participant
    cartoonRatingSubset2 <- Data_Collection_Site[ , c("ratingCartoon1", "ratingCartoon2", "ratingCartoon3", "ratingCartoon4")]

    meanCartoonRating <- numeric( nrow(cartoonRatingSubset2) )

    for (i in seq_len( nrow(cartoonRatingSubset2) ))
      meanCartoonRating[i] <- mean( as.numeric(cartoonRatingSubset2[i, which(performedCorrectlyIndices[i, ] == 1)]), na.rm = TRUE )

    Data_Collection_Site$meanCartoonRating <- meanCartoonRating

    # groups
    ratingsSmile <- unlist(Data_Collection_Site[Data_Collection_Site$condition == 1, "meanCartoonRating"])
    ratingsPout <- unlist(Data_Collection_Site[Data_Collection_Site$condition == 0, "meanCartoonRating"])

    ##@@ CHECK EXCLUSION CRITERIA: 2 @@##

    # exclude participants that are more than 2.5 standard deviations away from condition mean

    outliersSmile <- (Data_Collection_Site$condition == 1) & ((Data_Collection_Site$meanCartoonRating > (mean(ratingsSmile) + 2.5 * sd(ratingsSmile)))
                                                   | (Data_Collection_Site$meanCartoonRating < (mean(ratingsSmile) - 2.5 * sd(ratingsSmile))))

    outliersPout <- (Data_Collection_Site$condition == 0) & ((Data_Collection_Site$meanCartoonRating > (mean(ratingsPout) + 2.5 * sd(ratingsPout)))
                                                  | (Data_Collection_Site$meanCartoonRating < (mean(ratingsPout) - 2.5 * sd(ratingsPout))))

    Data_Collection_Site <- Data_Collection_Site[ ! outliersSmile & ! outliersPout, ]



    return(
        data.frame(
          Data_Collection_Site = rep(name, nrow(Data_Collection_Site)),
          DV = Data_Collection_Site$meanCartoonRating,
          DV_Item_1 = as.numeric(Data_Collection_Site$ratingCartoon1),
          DV_Item_2 = as.numeric(Data_Collection_Site$ratingCartoon2),
          DV_Item_3 = as.numeric(Data_Collection_Site$ratingCartoon3),
          DV_Item_4 = as.numeric(Data_Collection_Site$ratingCartoon4),
          group = Data_Collection_Site$condition
        )
    )
  }

  # apply data selection function
  all_Data_Collection_Sites <- lapply(names(data), all_Data_Collection_Sites_fun)

  # rename list objects
  names(all_Data_Collection_Sites) <- names(data)

  # create df from list
  all_Data_Collection_Sites_df <- do.call(rbind.data.frame, all_Data_Collection_Sites)

  ## create item level data set
  d <- data.frame(
    MultiLab = rep("RRR_06", nrow(all_Data_Collection_Sites_df)),
    MASC = rep("Strack", nrow(all_Data_Collection_Sites_df)),
    Data_Collection_Site = all_Data_Collection_Sites_df$Data_Collection_Site,
    DV = all_Data_Collection_Sites_df$DV,
    DV_Item_1 = all_Data_Collection_Sites_df$DV_Item_1,
    DV_Item_2 = all_Data_Collection_Sites_df$DV_Item_2,
    DV_Item_3 = all_Data_Collection_Sites_df$DV_Item_3,
    DV_Item_4 = all_Data_Collection_Sites_df$DV_Item_4,
    Group =  all_Data_Collection_Sites_df$group
  )

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






