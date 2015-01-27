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


###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}


####Load in Data####
path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')  ###set this to file path location for raw data
path <- c('C:/Users/malbrech/Desktop/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')
sample_pct <- 1 ###set between [0,1]
RAW <- DataPull::loadData(path,sample_pct)  ### see help file for documentation

###do some data cleanup that should be moved to the DataPull package
shipped <- unique(RAW$LoadCondition)[grep("F",unique(RAW$LoadCondition))]
#Cutout Non-shipped and Shippers Agent Loads (check about the SA thing)
RAW <- dplyr::filter(RAW,LoadCondition == shipped & SAFlag == "False")
RAW$DayEntryDate<-as.numeric(format(RAW$EntryDate,format="%j"))

###now filter for display
#grab the Low-High-th percentiles
Low=25
High=75
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





