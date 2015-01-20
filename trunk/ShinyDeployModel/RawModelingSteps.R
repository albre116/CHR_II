if(!require("devtools"))
  (install.packages("devtools"))
if(!require("shiny"))
  (install.packages("shiny"))
if(!require("shinyBS"))
  (install.packages("shinyBS"))
if(!require("rCharts"))
  (install.packages("rCharts"))
if(!require("dplyr"))
  (install.packages("dplyr"))
if(!require("data.table"))
  (install.packages("data.table"))
if(!require("lubridate"))
  (install.packages("lubridate"))
if(!require("ggplot2"))
  (install.packages("ggplot2"))
if(!require("rCharts"))
  (install_github('rCharts', 'ramnathv'))


###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}


####Load in Data####
path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')  ###set this to file path location for raw data
sample_pct <- 0.1 ###set between [0,1]
CHR<-DataPull::loadData(path,sample_pct)  ### see help file for documentation



####lets look at some basic associations between the different profit measures




