source("data-raw/BuildData.R")


# > devtools::use_data_raw()
# Creating data-raw/
#   Next:
#   * Add data creation scripts in data-raw
#   * Use devtools::use_data() to add data to package

# Read the raw data and convert it into data.frame that will be saved
raw_data <- fread('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt',sep="|",header=TRUE)
RAW_DATA<-buildData(raw_data)
# save as R/sysdata.rda The objects in R/sysdata.rda are only availalbe inside
# the package. training_data will be avaialble as a global inside the package.
devtools::use_data(RAW_DATA, internal = TRUE, overwrite = TRUE)
