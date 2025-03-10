#### Analyze & Document Meta-Analyses for MD & SMD
### All data in "data/processed" is checked by the script

### What you need to source this script/ run this function
### ...


## Dependencies (only necessary when this script is not sourced in "analyze_all_MultiLabs.Rmd")
# # Library Loading
# packages <- c("dplyr",
#               "readr",
#               "renv")
#
# # check, whether library already installed or not - install and load as needed:
# apply(as.matrix(packages), MARGIN = 1, FUN = function(x) {
#
#   pkg_avail <- nzchar(system.file(package = x))   # check if library is installed on system
#
#   if(pkg_avail){
#     require(x, character.only = TRUE)             # load the library, if already installed
#
#   }else{
#     install.packages(x)                           # install the library, if missing
#     require(x, character.only = TRUE)             # load after installation
#   }
# })
#
# # GitHub packages
# apply(as.matrix("MetaPipeX"), MARGIN = 1, FUN = function(x) {
#
#   pkg_avail <- nzchar(system.file(package = x))   # check if library is installed on system
#
#   if(pkg_avail){
#     require(x, character.only = TRUE)             # load MetaPipeX, if already installed
#
#   }else{
#     renv::install("JensFuenderich/MetaPipeX/R-Package") # install MetaPipeX, if missing
#     require(x, character.only = TRUE)             # load after installation
#   }
# })

# identify all processed data files
file_paths_processed_data <- list.files(path = "data/processed/",
                                        pattern = ".csv",
                                        full.names = TRUE,
                                        recursive = TRUE)

# remove codebooks
file_paths_processed_data <- file_paths_processed_data[- grep("codebook", file_paths_processed_data)]

# only IPD
file_paths_processed_data <- file_paths_processed_data[grep("IPD", file_paths_processed_data)]

# import files
all_IPD_data <- lapply(file_paths_processed_data, readr::read_csv)

# select relevant subset (two-group comparisons)
MD_SMD_selector <- data.frame(ID = seq(from = 1,
                                       to = length(all_IPD_data),
                                       by = 1),
                              Select = unlist(
                                lapply(1:length(all_IPD_data),
                                       function(x){
                                         ifelse("Group" %in%
                                                  names(all_IPD_data[[x]]) &
                                                  sum(unique(all_IPD_data[[x]]$Group), na.rm = T) == 1,
                                                1,
                                                0)})))

# select only data fit for meta-analyses of MD & SMD
MD_SMD_IPD_data <- all_IPD_data[unlist(MD_SMD_selector %>% dplyr::filter(Select == 1) %>% dplyr::select(ID))]

# # create folder for results
# dir.create(file.path("data/processed/meta_analyses"))

# run analyes & create documentation
MetaPipeX::full_pipeline(data = MD_SMD_IPD_data,
                         output_path = file.path("data/processed/"),
                         folder_name = "MetaPipeX_Output",
                         suppress_list_output = TRUE)

# keep environment tidy
rm(all_IPD_data,
   MD_SMD_IPD_data,
   MD_SMD_selector,
   file_paths_processed_data)
