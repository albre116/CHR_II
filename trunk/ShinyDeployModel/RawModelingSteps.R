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


###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}


####Load in Data####
path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')  ###set this to file path location for raw data
#path <- c('C:/Users/malbrech/Desktop/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')
sample_pct <- 0.1 ###set between [0,1]
RAW <- DataPull::loadData(path,sample_pct)  ### see help file for documentation



###do some data cleanup that should be moved to the DataPull package
shipped <- unique(RAW$LoadCondition)[grep("F",unique(RAW$LoadCondition))]
#Cutout Non-shipped and Shippers Agent Loads (check about the SA thing)
RAW <- dplyr::filter(RAW,LoadCondition == shipped & SAFlag == "False")
RAW$Day365<-as.numeric(format(RAW$EntryDate,format="%j"))
RAW$Day<-as.numeric(format(RAW$EntryDate,format="%d"))
RAW$Month<-as.numeric(format(RAW$EntryDate,format="%m"))
RAW$Year<-as.numeric(format(RAW$EntryDate,format="%Y"))


Get5DigZip <- function(x){
  tmp <- strsplit(x,",")[[1]][3]
  tmp <- gsub(" ","",tmp)
  tmp <- substr(tmp,1,5)
  return(tmp)
}

RAW$Orig5DigZip <- unlist(lapply(RAW$Origin,Get5DigZip))
RAW$Dest5DigZip <- unlist(lapply(RAW$Destination,Get5DigZip))

####Geocode the 5-digit zips to lat/long pairs####
data(zipcode)###pull in reference data mapping zip to lat/long

###code the origin
orig_zip_geocode <- zipcode %>% 
  select(zip,OrigLatitude=latitude,OrigLongitude=longitude)
RAW <- RAW %>% left_join(orig_zip_geocode,c("Orig5DigZip" = "zip"))
rm(orig_zip_geocode)

###code the destination
dest_zip_geocode <- zipcode %>% 
  select(zip,DestLatitude=latitude,DestLongitude=longitude)
RAW <- RAW %>% left_join(dest_zip_geocode,c("Dest5DigZip" = "zip"))
rm(dest_zip_geocode)


####Now create the 3-digit zip table centroids
###simpliest approach is to just use the medoid of the 5-digit zip data for each 3-dig roll-up
###i.e. we are going to use the most central point

pull_mediod_x<-function(lat,long){
  tmp <- data.frame(lat,long)
  tmp<-tmp[complete.cases(tmp),]
  if(nrow(tmp)>1){
  return(pam(tmp,1,diss=FALSE)$medoids[1])}
  if(nrow(tmp)==1){
    return(tmp[,1])
  }else{return(NA)}
}

pull_mediod_y<-function(lat,long){
  tmp <- data.frame(lat,long)
  tmp<-tmp[complete.cases(tmp),]
  if(nrow(tmp)>1){
    return(pam(tmp,1,diss=FALSE)$medoids[2])}
  if(nrow(tmp)==1){
    return(tmp[,2])
  }else{return(NA)}
}


Zip3DigMedioids <- zipcode %>%
  select(zip,latitude,longitude) %>%
  mutate(Zip3dig=substr(zip,1,3)) %>%
  select(-zip) %>%
  group_by(Zip3dig) %>%
  summarise(N=n(),
            Lat3Dig=pull_mediod_x(latitude,longitude),
            Long3Dig=pull_mediod_y(latitude,longitude))

RAW<-RAW %>% mutate(Orig3DigZip=substr(Orig5DigZip,1,3),
                    Dest3DigZip=substr(Dest5DigZip,1,3))

###code the origin
orig_zip_geocode <- Zip3DigMedioids %>% 
  select(Zip3dig,OrigLatitude3Dig=Lat3Dig,OrigLongitude3Dig=Long3Dig)
RAW <- RAW %>% left_join(orig_zip_geocode,c("Orig3DigZip" = "Zip3dig"))
rm(orig_zip_geocode)

###code the destination
dest_zip_geocode <- Zip3DigMedioids %>% 
  select(Zip3dig,DestLatitude3Dig=Lat3Dig,DestLongitude3Dig=Long3Dig)
RAW <- RAW %>% left_join(dest_zip_geocode,c("Dest3DigZip" = "Zip3dig"))
rm(dest_zip_geocode)

####Now pull out the City from the address field
GetCity <- function(x){
  tmp <- strsplit(x,",")[[1]][1]
  tmp <- gsub(" ","",tmp)
  return(tmp)
}

RAW <- RAW %>% mutate(OrigCity=unlist(lapply(Origin,GetCity)),
                      DestCity=unlist(lapply(Destination,GetCity)))
####geocode complete

##########################################################################################
#####All of the above steps should be pushed to the datapull package and documented within
##########################################################################################

##########################################################################################
#####Now we are going to construct the transactional volume model
#####For both the destination and origin
##########################################################################################

OrigVolumeTally<-RAW %>% 
  group_by(Year,Month,Day,Orig3DigZip) %>%
  summarise(OrigVolume=n())

DestVolumeTally<-RAW %>% 
  group_by(Year,Month,Day,Dest3DigZip) %>%
  summarise(DestVolume=n())

####Now construct the time series of Origin and Destination 3-dig zips for each day
min_time<-as.Date(format(min((RAW$EntryDate)),format="%Y-%m-%d"))
max_time<-as.Date(format(max((RAW$EntryDate)),format="%Y-%m-%d"))
span<-difftime(max_time,min_time)
time_sequence<-min_time+0:span  ##this is the date set over which to compute the volume


OrigZip3Categories <- RAW %>%
  select(Orig3DigZip) %>%
  arrange(Orig3DigZip) %>%
  distinct() %>%
  filter(Orig3DigZip %in% Zip3DigMedioids$Zip3dig)

DestZip3Categories <- RAW %>%
  select(Dest3DigZip) %>%
  arrange(Dest3DigZip) %>%
  distinct() %>%
  filter(Dest3DigZip %in% Zip3DigMedioids$Zip3dig)


OrigVolumeSeries <- rbind_all(lapply(time_sequence,function(x){
  data.frame(Date=x,OrigZip3Categories)
}))

DestVolumeSeries <- rbind_all(lapply(time_sequence,function(x){
  data.frame(Date=x,DestZip3Categories)
}))


OrigVolumeSeries <- OrigVolumeSeries %>%
  mutate(Year=as.numeric(format(Date,format="%Y")),
             Month=as.numeric(format(Date,format="%m")),
             Day=as.numeric(format(Date,format="%d"))) %>%
  left_join(OrigVolumeTally,c("Year"="Year","Month"="Month","Day"="Day","Orig3DigZip"="Orig3DigZip"))

OrigVolumeSeries$OrigVolume[is.na(OrigVolumeSeries$OrigVolume)]=0 ##set NA's to 0
  


DestVolumeSeries <- DestVolumeSeries %>%
  mutate(Year=as.numeric(format(Date,format="%Y")),
         Month=as.numeric(format(Date,format="%m")),
         Day=as.numeric(format(Date,format="%d"))) %>%
  left_join(DestVolumeTally,c("Year"="Year","Month"="Month","Day"="Day","Dest3DigZip"="Dest3DigZip"))

DestVolumeSeries$DestVolume[is.na(DestVolumeSeries$DestVolume)]=0 ##set NA's to 0


####stopped here ####

DATA<-dplyr::filter(RAW,!is.infinite(RPM_NormalizedCustomer) &
                      !is.na(RPM_NormalizedCustomer))

Low=0
High=100
DATA<-dplyr::filter(DATA,ntile(RPM_NormalizedCustomer,100)>Low &
                      ntile(RPM_NormalizedCustomer,100)<=High)













sc <- read.table("synthetic_control.data", header=F, sep="")


# randomly sampled n cases from each class, to make it easy for plotting

n <- 10

s <- sample(1:100, n)

idx <- c(s, 100+s, 200+s, 300+s, 400+s, 500+s)

sample2 <- sc[idx,]

observedLabels <- c(rep(1,n), rep(2,n), rep(3,n), rep(4,n), rep(5,n), rep(6,n))

# compute DTW distances

library(dtw)

distMatrix <- dist(sample2, method="DTW")

# hierarchical clustering

hc <- hclust(distMatrix, method="average")

plot(hc, labels=observedLabels, main="")


library(wavelets)
wtData <- NULL

for (i in 1:nrow(sc)) {
  
a <- t(sc[i,])
  
wt <- dwt(a, filter="haar", boundary="periodic")
  
wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
  
}

wtData <- as.data.frame(wtData)

classId <- c(rep("1",100), rep("2",100), rep("3",100),
               
               rep("4",100), rep("5",100), rep("6",100))

wtSc <- data.frame(cbind(classId, wtData))


fit <- gbm(classId~.,data=wtSc,n.trees=1000)
best.iter <- gbm.perf(fit,method="OOB")
print(best.iter)
predict(fit,n.trees=best.iter,type="link")



#DWT reference
###http://www.jstatsoft.org/v31/i07/paper
#Wavlet clustering reference
###http://www.rdatamining.com/examples/time-series-clustering-classification


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





