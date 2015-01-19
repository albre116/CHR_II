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
  print('HaplotypeServiceClient not installed. Please install it from github using devtools.')
  print("devtools::install_svn('http://svn.nmdp.org/repos/dev/bioinformatics/projects/search-prognosis-tool', 'DataPull')")
}


####Load in Data####
path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')  ###set this to file path location for raw data
sample_pct <- 0.1 ###set between [0,1]
CHR<-DataPull::loadData(path,sample_pct)  ### see help file for documentation



####lets look at some basic associations between the different profit measures


