rm(list=ls())
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
if(!require("mgcv"))
  (install.packages("mgcv"))
if(!require("gbm"))
  (install.packages("gbm"))
if(!require("e1071"))
  (install.packages("e1071"))
if(!require("dtw"))
  (install.packages("dtw"))
if(!require("wavelets"))
  (install.packages("wavelets"))
if(!require("zipcode"))
  (install.packages("zipcode"))
if(!require("cluster"))
  (install.packages("cluster"))
if(!require("fields"))
  (install.packages("fields"))
if(!require("rgdal"))
  (install.packages("rgdal"))
if(!require("ggmap"))
  (install.packages("ggmap"))
if(!require("rgeos"))
  (install.packages("rgeos"))
if(!require("reshape2"))
  (install.packages("reshape2"))
if(!require("RColorBrewer"))
  (install.packages("RColorBrewer"))
if(!require("BayesTree"))
  (install.packages("BayesTree"))
if(!require("mboost"))
  (install.packages("mboost"))


###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}

source("HelperFunctions.R")
####Load in Data####
path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')  ###set this to file path location for raw data
#path <- c('C:/Users/malbrech/Desktop/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')
sample_pct <- 1 ###set between [0,1]
RAW <- DataPull::loadData(path,sample_pct)  ### see help file for documentation
RAW <- GeoCodeData(RAW)   ###Geocoding the Data
RAW <- TallyDailyVolume(RAW)
save(RAW,file="RAW.RData")###save it so we don't always have to run this



load("RAW.RData")###we can start here most times

#####do some kriging

###look at volume variation

KrigPlot(RAW,#intput data
         type="Orig",#select either Orig or Dest
         states.model=c("California","Nevada","Arizona","Oregon"),#model area
         states.plot=c("California","Nevada","Arizona","Oregon"),#plot area
         npoints=5000,###krig points,
         dist=50,#the miles aroound each krig point to summarize data over
         DatePlotStart=as.Date("2011-6-1",format="%Y-%m-%d"),#start date for the plot data
         DatePlotEnd=as.Date("2011-6-10",format="%Y-%m-%d"),#end date for the plot data
         summary_fun="n()",##the dplyr function to run : mean(RPM_NormalizedCustomer)
         summary_name="Volume",##the name of the summary measure
         summary_variable="1",###the name of the variable or a number
         low_quantile=0,#the quantile to cut the points at
         high_quantile=100,#the quantile to cut the points at
         theta=20# the kriging exponential decay parameter in miles
)





###look at the RPM variation

KrigPlot(RAW,#intput data
 type="Orig",#select either Orig or Dest
 states.model=c("California","Nevada","Arizona","Oregon"),#model area
 states.plot=c("California"),#plot area
 npoints=5000,###krig points,
 dist=50,#the miles aroound each krig point to summarize data over
 DatePlotStart=as.Date("2011-6-1",format="%Y-%m-%d"),#start date for the plot data
 DatePlotEnd=as.Date("2011-6-10",format="%Y-%m-%d"),#end date for the plot data
 summary_fun="mean(RPM_NormalizedCustomer)",##the dplyr function to run : mean(RPM_NormalizedCustomer)
 summary_name="RPM_NormalizedCustomer",##the name of the summary measure
 summary_variable="DATA$RPM_NormalizedCustomer",###the name of the variable or a number
 low_quantile=5,#the quantile to cut the points at
 high_quantile=95,#the quantile to cut the points at
 theta=20# the kriging exponential decay parameter in miles
)








################################
####stopped here ####
################################



#DWT reference
###http://www.jstatsoft.org/v31/i07/paper
#Wavlet clustering reference
###http://www.rdatamining.com/examples/time-series-clustering-classification


###now filter for display
#grab the Low-High-th percentiles
Low=5
High=95
DATA<-dplyr::filter(RAW,!is.infinite(RPM_NormalizedCustomer) &
                      !is.na(RPM_NormalizedCustomer))
DATA<-dplyr::filter(DATA,ntile(RPM_NormalizedCustomer,100)>Low &
                      ntile(RPM_NormalizedCustomer,100)<=High)



####lets do a simple model here
fit<-mgcv::gam(RPM_NormalizedCustomer~EntryDate+s(DayEntryDate,bs="cc")+s(SumOfStops),data=DATA)
summary(fit)
plot(fit,pages=1,all.terms=TRUE)


####make an even spaced set of days to predict over
min_time<-min((DATA$EntryDate))
max_time<-max((DATA$EntryDate))
difftime(max_time,min_time)
min_time+24*60*60#add 1 day of time


min(as.numeric(format(DATA$EntryDate[1:100],format="%z")))
max(format(DATA$EntryDate,format="%z"))


####lets look at the data fit
plot<-ggplot(DATA)+geom_point(aes(x=EntryDate,y=RPM_NormalizedCustomer),alpha=0.1)

print(plot)









###i like the idea of





