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

# example of how to load custom package and instructions
# if(!requireNamespace('DataFetch')) {
#   print('Data Loading Service not installed. Please install it from github using devtools.')
#   print("devtools::install_svn('http://svn.nmdp.org/repos/dev/bioinformatics/projects/search-prognosis-tool', 'HaplotypeServiceClient')")
# } else {
#   library(DataFetch)
# }



####Load in Data####
data_path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')
raw_data <- fread(data_path,sep="|",header=TRUE)

