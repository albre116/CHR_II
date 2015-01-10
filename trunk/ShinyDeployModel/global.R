library(shiny)
library(shinyBS)
library(rCharts)

if(!requireNamespace('DataFetch')) {
  print('Data Loading Service not installed. Please install it from github using devtools.')
  print("devtools::install_svn('http://svn.nmdp.org/repos/dev/bioinformatics/projects/search-prognosis-tool', 'HaplotypeServiceClient')")
} else {
  library(DataFetch)
}


source("matrixCustom.R")
#source("helpers.R")

SERVER_HOST_ADDRESS <- 'http://p1haplostats-s1:28080'


POPULATIONS <- c("CAU", "AFA","HIS", "API", "UNK","NAM")
POPULATIONS_TEXT<- c("White", "African American", "Hispanic", "Asian Pacific Islander", "Unknown","Native American")
host<- 'http://p1haplostats-s1:28080'

PROGNOSIS <- c("Good","Fair","Poor")
VALID_RIDS <- c(2082108,2087222,2097353,2065723,2065509,2065558,2065756,2066150)


# display_mug <- data.frame(locus = c('A', 'B', 'C', 'DRB1', 'DQB1', 'DRB3', 'DRB4', 'DRB5'), 
#                           type1 = '', 
#                           type2 = '',
#                           stringsAsFactors = FALSE)
display_mug <- data.frame(locus = c('A', 'B', 'C', 'DRB1', 'DQB1'), 
                          type1 = '', 
                          type2 = '',
                          stringsAsFactors = FALSE)
