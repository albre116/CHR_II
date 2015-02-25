if(!require("devtools"))
  install.packages("devtools")
if(!require("shiny"))
  install.packages("shiny")
if(!require("shinyBS"))
  install.packages("shinyBS")
if(!require("rCharts"))
  install.packages("rCharts")
if(!require("dplyr"))
  install.packages("dplyr")
if(!require("class"))
  install.packages("class")
if(!require("data.table"))
  install.packages("data.table")
if(!require("lubridate"))
  install.packages("lubridate")
if(!require("quantreg"))
  install.packages("quantreg")
if(!require("splines"))
  install.packages("splines")
if(!require("ggplot2"))
  install.packages("ggplot2")
if(!require("rCharts"))
  install_github('rCharts', 'ramnathv')
if(!require("mgcv"))
  install.packages("mgcv")
if(!require("xts"))
  install.packages("xts")
if(!require("gbm"))
  install.packages("gbm")
if(!require("e1071"))
  install.packages("e1071")
if(!require("dtw"))
  install.packages("dtw")
if(!require("wavelets"))
  install.packages("wavelets")
if(!require("zipcode"))
  install.packages("zipcode")
if(!require("cluster"))
  install.packages("cluster")
if(!require("fields"))
  install.packages("fields")
if(!require("rgdal"))
  install.packages("rgdal")
if(!require("ggmap"))
  install.packages("ggmap")
if(!require("rgeos"))
  install.packages("rgeos")
if(!require("reshape2"))
  install.packages("reshape2")
if(!require("RColorBrewer"))
  install.packages("RColorBrewer")
if(!require("BayesTree"))
  install.packages("BayesTree")
if(!require("mboost"))
  install.packages("mboost")
if(!require("maps"))
  install.packages("maps")
if(!require("forecast"))
  install.packages("forecast")
if(!require("maptools"))
  install.packages("maptools")
if(!require("ggplot2"))
  install.packages("ggplot2")
if(!require("shinydashboard"))
  devtools::install_github("rstudio/shinydashboard")
if(!require("bubbles"))
  devtools::install_github("jcheng5/bubbles")
if(!require("shinySignals"))
  devtools::install_github("hadley/shinySignals")
if(!require("htmlwidgets"))
  devtools::install_github("ramnathv/htmlwidgets")
if(!require("dygraphs"))
  devtools::install_github("rstudio/dygraphs")



###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}


####Generate the data if it doesn't exist

if(!file.exists("RAW.RData")){
  path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')  ###set this to file path location for raw data
  sample_pct <- 0.1 ###set between [0,1]
  RAW <- DataPull::loadData(path,sample_pct)  ### see help file for documentation
  RAW <- DataPull::geocodeData(RAW)   ###Geocoding the Data
  RAW <- DataPull::tallyDailyVolume(RAW)
  save(RAW,file="RAW.RData")###save it so we don't always have to run this
}else{load("RAW.RData")}###identify the data path from the datapull package


states.model <- c("Washington", "Montana", "Maine", "North Dakota", "South Dakota", 
                  "Wyoming", "Wisconsin", "Idaho", "Vermont", "Minnesota", "Oregon", 
                  "New Hampshire", "Iowa", "Massachusetts", "Nebraska", "New York", 
                  "Pennsylvania", "Connecticut", "Rhode Island", "New Jersey", 
                  "Indiana", "Nevada", "Utah", "California", "Ohio", "Illinois", 
                  "District of Columbia", "Delaware", "West Virginia", "Maryland", 
                  "Colorado", "Kentucky", "Kansas", "Virginia", "Missouri", "Arizona", 
                  "Oklahoma", "North Carolina", "Tennessee", "Texas", "New Mexico", 
                  "Alabama", "Mississippi", "Georgia", "South Carolina", "Arkansas", 
                  "Louisiana", "Florida", "Michigan")

states <- map("state",regions = states.model,plot=F,fill=TRUE)
data(state.fips)
states_labels <- data.frame(labels=states$names) %>% 
  left_join(state.fips,by=c("labels"="polyname")) %>%
  select(abb)
state_labs <- states
state_labs$names <- as.character(states_labels$abb)

###########################################################################
###########################################################################
########## Modeling Kernel Functions
########## Will be moved to its own package
########## With Documentation
###########################################################################
###########################################################################


modelCPDS <- function(f,       #pass in the model formula
                      data,          #data to be fit
                      kernel="GAM",  #type of modeling kernel
                      gamma=1.4,
                      ...            #additional parameters to the model
                      ){
  ###put all of the model functions here in the switch statement
  fit <- switch(kernel,
        "Generalized Additive Model"=bam(f,data=data,gamma=gamma)
         )
  
  ###return the model image
  return(fit)
}





