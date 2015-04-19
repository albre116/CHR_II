rm(list=ls(all=TRUE))
gc()

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
#if(!require("shinydashboard"))
#  devtools::install_github("rstudio/shinydashboard")
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
#if (!require("shinyFiles"))
#  devtools::install_github("thomasp85/shinyFiles")

###custom and local packages
if(!require('dyPencilgraphs')) {
  devtools::install_github(c("albre116/DygraphsPencil"))
}


####Load Data File
load("RAW_100_Min.RData")


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
########## Functions
########## Will be moved to its own package
########## With Documentation
###########################################################################
###########################################################################

####fit the main model

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


####function for plotting states
plot_states <- function(selectStates,reduced,cities,mapLayer,color){
  map(states)
  if(!is.null(cities) | !is.null(selectStates)){
    selected <- c(selectStates,
                  (as.character(state.fips$polyname[
                    state.fips$abb %in% unlist(lapply(cities,function(x){strsplit(x,",")[[1]][2]}))])))
    selected <- unlist(lapply(selected,function(x){strsplit(x,":")[[1]][1]}))
    selected <- unique(selected)
    selected <- selected[!is.null(selected)]
    mapOrig <- map("state",regions = selected,plot=F,fill=T,col="yellow")} else{mapOrig <- NULL}
  if(!is.null(mapOrig)){map(mapOrig,fill=T,add=T,col="yellow")}
  if("State Names" %in% mapLayer){map.text(state_labs,add=T)}
  if("Data" %in% mapLayer){
    points(x=reduced$x,y=reduced$y,cex=0.1,col=color,pch=19)
  }
}


####function for plotting counties
plot_counties <- function(counties,reduced,selectCounties,City,radius,Circles,layers,color){
  
  if(is.null(counties)){return(NULL)}
  if(length(selectCounties)>0){
    mapOrig <- map("county",regions = selectCounties,plot=F,fill=T,col="yellow")} else{mapOrig <- NULL}
  map(counties)
  if(!is.null(mapOrig)){map(mapOrig,fill=T,add=T,col="yellow")}
  
  if(!is.null(City)){
    pickadd <- city_lookup[city_lookup$city %in% City,,drop=F]
    pickadd <- pickadd[1,,drop=F]
    if(!is.null(pickadd) & !is.na(pickadd)){pickadd <- paste(pickadd$x,pickadd$y,radius,sep=":")}else{pickadd <- NULL}
    lapply(pickadd,function(b){
      b <- strsplit(b,":")
      b <- unlist(b)
      plotcircle(r=radius_xyunits(miles=as.numeric(b[3])),mid=c(as.numeric(b[1]),as.numeric(b[2])),col="yellow",type="n")
    })
  }
  
  if(!is.null(Circles)){
    tmp <- Circles
    lapply(tmp,function(b){
      b <- strsplit(b,":")
      b <- unlist(b)
      plotcircle(r=radius_xyunits(miles=as.numeric(b[3])),mid=c(as.numeric(b[1]),as.numeric(b[2])),col="yellow",type="n")
    })
  }
  if("Data" %in% layers){
    points(x=reduced$x,y=reduced$y,cex=0.1,col=color,pch=19)
  }

}



######circle plotting function
radius_xyunits<-function(miles=50,earth_radius = 3960.0, radians_to_degrees = 180.0/pi){
  #"Given a distance north, return the change in latitude."
  degrees<-(miles/earth_radius)*radians_to_degrees
  return(degrees)
}


wellHeader <- function(title="title",id=NULL,value=TRUE){
  if(is.null(id)){
    h3(title,
       style = "font-family: 'Source Sans Pro'; font-weight: 300;
       color: #fff; text-align: left;
       background-image: url(texturebg.png);
       padding: 0px")
  }else{
  fluidRow(
    column(width=11,
  h3(title,
     style = "font-family: 'Source Sans Pro'; font-weight: 300;
     color: #fff; text-align: left;
     background-image: url(texturebg.png);
     padding: 0px")),
  column(width=1,
         checkboxInput(id,label=NULL,value=value)
         )
  )
  }
}







