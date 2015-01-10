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

if(!requireNamespace('DataFetch')) {
  print('Data Loading Service not installed. Please install it from github using devtools.')
  print("devtools::install_svn('http://svn.nmdp.org/repos/dev/bioinformatics/projects/search-prognosis-tool', 'HaplotypeServiceClient')")
} else {
  library(DataFetch)
}


####source all java scripts and functions related to shiny display (data processing functions should be in packages highlighted in the export space)
source("matrixCustom.R")
#source("helpers.R")

####Build up the test scripts here and we will move them over
####to the server file as they begin to work

load("../DataFetch/R/sysdata.rda")###this will be replaced by loading the DataFetch Package

