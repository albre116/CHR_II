###This is the script to run to prep the data


###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}


path <- c('/srv/shiny_data/2015_04_06_New.txt')  ###set this to file path location for raw data
sample_pct <- 1 ###set between [0,1]
RAW <- DataPull::loadData(path,sample_pct)  ### see help file for documentation
RAW <- DataPull::geocodeData(RAW)   ###Geocoding the Data
RAW <- DataPull::tallyDailyVolume(RAW) ###only perform if volume compute desired... this step is slow
RAW <- DataPull::characterConversion(RAW)
save(RAW,file="RAW_100_New.RData")###save it so we don't always have to run this
