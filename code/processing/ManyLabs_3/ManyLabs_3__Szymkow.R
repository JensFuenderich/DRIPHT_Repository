#### Convert Raw to Trial Level
### MultiLab: Many Labs 3 (Ebersole et al., 2016)
### MASC: Warmer hearts, warmer rooms (Szymkow, Chandler, IJzerman, Parzuchowski, & Wojciszke, 2013, study 1)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://www.sciencedirect.com/science/article/abs/pii/S0022103115300123
## Repository: https://osf.io/ct89g/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the readr package.

convert_raw_data_Szymkow_fun <- function(data, variable_info = NULL, output_folder){

  ## using the code from "Warmer Hearts Rooms Script.R" to get to the necessary variables
  # some of the original lines did not manipulate the data and were thus excluded from this script

  #Reformatting temperature estimate
  str(data$tempest1)
  list(data$tempest1)
  regexp<-"[[:digit:]]+"
  data$tempestclean<-stringr::str_extract(data$tempest1,regexp)
  list(data$tempestclean)
  str(data$tempestclean)
  data$tempestclean<-as.numeric(data$tempestclean)

  #Checking other variables
  str(data$TempCond)
  data$TempCond<-as.factor(data$TempCond)
  list(levels(data$TempCond))
  str(data$TargetGender)
  data$TargetGender<-as.factor(data$TargetGender)
  str(data$Genderfactor)
  data$Genderfactor<-as.factor(data$Genderfactor)
  str(data$Temperatureinlab)
  data$Temperatureinlab<-as.numeric(data$Temperatureinlab)

  ###Eliminating estimates higher than 95, lower than 50
  tempclean1<-subset(data,data$tempestclean<95)
  TempClean<-subset(tempclean1,tempclean1$tempestclean>50)
  head(TempClean)
  range(TempClean$tempestclean)
  str(TempClean$tempestclean)
  length(TempClean$tempestclean)
  list(TempClean$tempestclean)


  ## final data frameTempClean
  d <- data.frame(MultiLab = rep("ML3", length(TempClean$tempestclean)),
                  MASC = rep("Szymkow", length(TempClean$tempestclean)),
                  Data_Collection_Site = TempClean$Site,
                  DV = TempClean$tempestclean,
                  Group = as.character(TempClean$TempCond)
  )

  ## treatment and control group as 1 and 0
  d$Group[d$Group == "Agentic"] <- 0
  d$Group[d$Group == "Communal"] <- 1
  d$Group <- as.numeric(d$Group)

  d <- stats::na.omit(d)

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






