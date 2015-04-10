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
if(!require("zoo"))
  install.packages("zoo")
if(!require("MatrixModels"))
  install.packages("MatrixModels")
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
if(!require("Matrix"))
  install.packages("Matrix")
if(!require("shape"))
  install.packages("shape")
if (!require("DT"))
  devtools::install_github("rstudio/DT")

###custom and local packages
if(!require('dyPencilgraphs')) {
  devtools::install_github(c("albre116/DygraphsPencil"))
}


####Load Data File
load("RAW_10.RData")


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


city_lookup <- data.frame(city=RAW$OrigCity,x=RAW$OrigLongitude,y=RAW$OrigLatitude)
city_lookup <- rbind(city_lookup,data.frame(city=RAW$DestCity,x=RAW$DestLongitude,y=RAW$DestLatitude))
city_lookup <- unique(city_lookup)




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
                "Generalized Additive Model"=mgcv::gam(f,data=data,gamma=gamma)
  )

  ###return the model image
  return(fit)
}



####simple fourier transformation for seasonal decomposition
fourier <- function(t,terms,period)
{
  n <- length(t)
  X <- matrix(,nrow=n,ncol=2*terms)
  for(i in 1:terms)
  {
    X[,2*i-1] <- sin(2*pi*i*t/period)
    X[,2*i] <- cos(2*pi*i*t/period)
  }
  colnames(X) <- paste(c("S","C"),rep(1:terms,rep(2,terms)),sep="")
  return(X)
}


######circle plotting function
radius_xyunits<-function(miles=50,earth_radius = 3960.0, radians_to_degrees = 180.0/pi){
  #"Given a distance north, return the change in latitude."
  degrees<-(miles/earth_radius)*radians_to_degrees
  return(degrees)
}







