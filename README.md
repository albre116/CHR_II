# CHR_II
2nd development phase of CPDS timeseries forecasting algorithms

To initialize, install the following software packages: 1. The latest version of R from http://cran.us.r-project.org/ 2. The latest version of Rstudio for desktop from https://www.rstudio.com/ide/download/ Then start Rstudio and enter the following commands into the R-console window to install the required packages: install.packages("devtools", dependencies = TRUE) install.packages("shiny", dependencies = TRUE)

TO RUN AFTER INSTALLATION:

To run enter the following 2 commands into the R-console window: 
Command 1:
devtools::install_github("albre116/CHR_II/branches",auth_token="9b170fa4551681199539cf0a38e0b3d7892c4bb5") 

Command 2:
shiny::runApp(system.file(package='shinydashboardlayout'))