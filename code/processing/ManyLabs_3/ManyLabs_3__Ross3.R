#### Convert Raw to Trial Level
### MultiLab: Many Labs 3 (Ebersole et al., 2016)
### MASC: It feels like yesterday: self-esteem, valence of personal past experiences, and judgments of subjective distance (Ross & Wilson, 2002, study 2)

## More information on the effect may be taken from the "metadata_sheet.csv" (in the root folder of this repository) or the:
## Main Publication: https://www.sciencedirect.com/science/article/abs/pii/S0022103115300123
## Repository: https://osf.io/ct89g/

### What you need to source this script/ run this function
## If you want to run it as intended for this repository, go to the "1_process_all_MultiLabs.Rmd" and run the chunks for this MultiLab.
## You can use this function in your own script as well. Make sure to load the readr package.

convert_raw_data_Ross3_fun <- function(data, variable_info = NULL, output_folder){

  ## using the code from "Power Perspective Script.R" to get to the necessary variables
  # some of the original lines did not manipulate the data and were thus excluded from this script

  data$SubDistCond[data$bestgrade2!="NA"]<-"BestGrade"
  data$SubDistCond[data$worstgrade2!="NA"]<-"WorstGrade"
  #list(data$SubDistCond)

  ###worst/bestgrade3 is DV
  BestGrade<-subset(data,data$SubDistCond=="BestGrade")
  WorstGrade<-subset(data,data$SubDistCond=="WorstGrade")
  #head(BestGrade)
  #head(WorstGrade)
  #tail(WorstGrade)

  BestGrade$SubDist<-BestGrade$bestgrade3
  WorstGrade$SubDist<-WorstGrade$worstgrade3
  BestGrade$TimeSince<-BestGrade$bestgrade1
  WorstGrade$TimeSince<-WorstGrade$worstgrade1
  BestGrade$SESDOrder<-BestGrade$bestgrade_order
  WorstGrade$SESDOrder<-WorstGrade$worstgrade_order
  SESDdata<-rbind(BestGrade,WorstGrade)
  #head(SESDdata)
  #tail(SESDdata)
  #list(SESDdata$SubDistCond)

  ###Adding Months Since data
  ###Make sure to pull these data from https://osf.io/64z7s/
  Months <- variable_info

  Months$YearMonths <- (2014-Months$Year)*12
  Months$TermMonths[Months$TermsBack==0]<-0
  Months$TermMonths[Months$TermsBack==1]<-1
  Months$TermMonths[Months$TermsBack==2]<-4
  Months$TermMonths[Months$TermsBack==3]<-8
  Months$MonthsSince<-Months$YearMonths+Months$TermMonths

  #merging data
  SESDdata<-merge(SESDdata,Months,by="session_id",all.x=TRUE)

  #Centering continuous predictors (months since class and self-esteem)
  SESDdata$MonthsSince<-SESDdata$MonthsSince-mean(SESDdata$MonthsSince,na.rm=TRUE)

  SESDdata$SelfEsteem<-SESDdata$SelfEsteem-mean(SESDdata$SelfEsteem,na.rm=TRUE)

  SESDdata<-subset(SESDdata,SESDdata$SubDistCond!="NA")
  SESDdata<-subset(SESDdata,SESDdata$SelfEsteem!="NA")
  SESDdata<-subset(SESDdata,SESDdata$MonthsSince!="NA")
  SESDdata$SubDistCond<-as.factor(SESDdata$SubDistCond)

  ## create df for this Data_Collection_Site
  d <- data.frame(MultiLab = rep("ML3", length(SESDdata$SubDist)),
                  MASC = rep("Ross3", length(SESDdata$SubDist)),
                  Data_Collection_Site = SESDdata$Site,
                  DV = SESDdata$SubDist,
                  Group = as.character(SESDdata$SubDistCond)
  )


  ## treatment and control group as 1 and 0
  # these are defined so that a positive effect is an effect in the same direction as the original result
  d$Group[d$Group == "WorstGrade"] <- 1
  d$Group[d$Group == "BestGrade"] <- 0
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






