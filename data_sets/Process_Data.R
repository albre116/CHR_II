###This is the script to run to prep the data


###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}

fileName <- "2015_04_27.txt"
save_name <- gsub(".txt",".RData",fileName)
Minimal_Data=TRUE
if(Sys.info()["sysname"]=="Windows"){path <- paste0("C:/Users/albre116/Documents/CHR_II/data_sets/",fileName)}else{
  path <- paste0("/srv/shiny_data/",fileName)}
 ###set this to file path location for raw data
sample_pct <- 1 ###set between [0,1]
RAW <- DataPull::loadData(path,sample_pct)  ### see help file for documentation
RAW <- DataPull::geocodeData(RAW)   ###Geocoding the Data
if(Minimal_Data==FALSE){RAW <- DataPull::tallyDailyVolume(RAW)} ###only perform if volume compute desired... this step is slow
RAW <- DataPull::characterConversion(RAW)
if(Minimal_Data==TRUE){
  keep <- c(  "CustomerCarrier","EntryDate","CustomerCCode","SumOfStops",
              "CarrierTCode","OrigLongitude","OrigLatitude","LoadMiles",
              "DestLongitude","DestLatitude","loadnum","NumericDate","Day365",
              "OrigCity","DestCity","Dest3DigZip","Orig3DigZip","CPM_AllInCarrier")
  RAW <- RAW[,keep]
  save_name <- paste0("Min_",save_name)
}


if(Sys.info()["sysname"]=="Windows"){savepath <- paste0("C:/Users/albre116/Documents/CHR_II/data_sets/",save_name)}else{
  savepath <- paste0("/srv/shiny_data/",save_name)}

save(RAW,file=savepath)###save it so we don't always have to run this

###to check file dates
RAW_CUT <- RAW[RAW[["EntryDate"]]>as.Date("3/20/2015",format="%m/%d/%Y"),]

