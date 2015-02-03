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


###custom and local packages
if(!require('DataPull')) {
  devtools::install_github(c("albre116/CHR_II/trunk/DataPull"),auth_token="ffcaf9fb4036981ec6022f13d2a1d05df97a5ff3")
}


####Load in Data####
path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')  ###set this to file path location for raw data
#path <- c('C:/Users/malbrech/Desktop/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')
sample_pct <- 1 ###set between [0,1]
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

#######################################################
####In this part of the code we are going to create
####A grid of shipper's access to volume points
####and then krig the volume access over this grid
#######################################################
usa.states <- readOGR(dsn = "Shapefiles", layer = "states")
states.in <- c("Washington", "Montana", "Maine", "North Dakota", "South Dakota", 
               "Wyoming", "Wisconsin", "Idaho", "Vermont", "Minnesota", "Oregon", 
               "New Hampshire", "Iowa", "Massachusetts", "Nebraska", "New York", 
               "Pennsylvania", "Connecticut", "Rhode Island", "New Jersey", 
               "Indiana", "Nevada", "Utah", "California", "Ohio", "Illinois", 
               "District of Columbia", "Delaware", "West Virginia", "Maryland", 
               "Colorado", "Kentucky", "Kansas", "Virginia", "Missouri", "Arizona", 
               "Oklahoma", "North Carolina", "Tennessee", "Texas", "New Mexico", 
               "Alabama", "Mississippi", "Georgia", "South Carolina", "Arkansas", 
               "Louisiana", "Florida", "Michigan")
usa.selected <- usa.states[(usa.states@data$STATE_NAME %in% states.in),]
usa.selected@data$id=rownames(usa.selected@data)
usa.selected.points=fortify(usa.selected,region="id")
usa.selected.df=left_join(usa.selected.points,usa.selected@data,by="id")

b <- bbox(usa.selected)
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])
# scale longitude and latitude (increase bb by 5% for plot) replace 1.05
# with 1.xx for an xx% increase in the plot size

#baselayer <- ggmap(get_map(location = b))


###create grid from bounding box for kriging
npoints<-2500
lon <- seq(b[1,1],b[1,2],length.out = ceiling(sqrt(npoints)))
lat <- seq(b[2,1],b[2,2],length.out = ceiling(sqrt(npoints)))
kgrid<-expand.grid(x = lon, y = lat)

kgrid<-SpatialPoints(kgrid)

int<-gIntersects(kgrid,usa.selected,byid=T)

clipped <- apply(int == F, MARGIN = 2, all)
plot(kgrid[which(clipped), ])  # shows all stations we DO NOT want
kgrid.cl <- kgrid[which(!clipped), ]  # use ! to select the invers
points(kgrid.cl, col = "green")  # check that it's worked

kgrid <- kgrid.cl
rm(kgrid.cl)  # tidy up: we're only interested in clipped ones

Map <- ggplot(usa.selected.df)+
  aes(long,lat,group=group)+
  geom_polygon(color="black",alpha=0)+
  labs(x = "Long", y = "Lat") + 
  theme_bw()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

points<-as.data.frame(kgrid@coords)
Map+geom_point(data=points,aes(x=x,y=y,group=1))+ggtitle("Kriging Support Points")


###now for the kriging grid, we need to compute a volume measure within a specified distance
# Distances are measured in miles.
# Longitudes and latitudes are measured in degrees.
# Earth is assumed to be perfectly spherical.
dist=50#miles for load to be in cell
earth_radius = 3960.0
degrees_to_radians = pi/180.0
radians_to_degrees = 180.0/pi

change_in_latitude<-function(miles){
  #"Given a distance north, return the change in latitude."
  degrees_lat<-(miles/earth_radius)*radians_to_degrees
  return(degrees_lat)
}


change_in_longitude <- function(latitude, miles){
  #"Given a latitude and a distance west, return the change in longitude."
  # Find the radius of a circle around the earth at given latitude.
  r = earth_radius*cos(latitude*degrees_to_radians)
  degrees_lon <- (miles/r)*radians_to_degrees
  return(degrees_lon)
}


bounds <- apply(kgrid@coords,1,function(x){
  delta_lat <- change_in_latitude(dist/2)
  delta_long <- change_in_longitude(x[2],dist/2)
  lower_long <- x[1] - delta_long
  upper_long <- x[1] + delta_long
  lower_lat <- x[2] - delta_lat
  upper_lat <- x[2] + delta_lat
  return(data.frame(lower_long,upper_long,lower_lat,upper_lat))
})

bounds<-matrix(unlist(bounds),ncol=4,byrow=T)
colnames(bounds) <- c("long_lower","long_upper","lat_lower","lat_upper")
kgrid <- SpatialPointsDataFrame(kgrid,data=as.data.frame(bounds))

####the kriging grid and boundaries are now complete###
####Now construct the time series of Origin and Destination 3-dig zips for each day
min_time<-as.Date(format(min((RAW$EntryDate)),format="%Y-%m-%d"))
max_time<-as.Date(format(max((RAW$EntryDate)),format="%Y-%m-%d"))
span<-difftime(max_time,min_time)
time_sequence<-min_time+0:span  ##this is the date set over which to compute the summary measures

DatePlotStart <- as.Date(max_time,format="%Y-%m-%d")-3
DatePlotEnd <- as.Date(max_time,format="%Y-%m-%d")
DATA <- RAW %>% filter(Year>=as.numeric(format(DatePlotStart,format="%Y")) & Year<=as.numeric(format(DatePlotEnd,format="%Y")),
                       Month>=as.numeric(format(DatePlotStart,format="%m")) & Month<=as.numeric(format(DatePlotEnd,format="%m")),
                       Day>=as.numeric(format(DatePlotStart,format="%d")) & Day<=as.numeric(format(DatePlotEnd,format="%d")))

####summarise observations landing within krig-grid cells
x <- DATA$OrigLongitude
y <- DATA$OrigLatitude
Grid_Summary<- apply(kgrid@data,1,function(b){
  tmp<-filter(DATA,(OrigLongitude>=b[1] & OrigLongitude<=b[2]) & 
                (OrigLatitude>=b[3] & OrigLatitude<=b[4]))
  summarise(tmp,Volume=n(),RPM_NormalizedCustomer=mean(RPM_NormalizedCustomer))
})
Colnames <- colnames(Grid_Summary[[1]])
Ncol <- ncol(Grid_Summary[[1]])
Grid_Summary <- as.data.frame(matrix(unlist(Grid_Summary),ncol=Ncol,byrow=T))
colnames(Grid_Summary) <- Colnames

KrigData<- SpatialPointsDataFrame(coords=kgrid@coords,data=Grid_Summary)


Map <- ggplot(usa.selected.df)+
  aes(long,lat,group=group)+
  geom_polygon(color="black",alpha=0)+
  labs(x = "Long", y = "Lat") + 
  theme_bw()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

points<-as.data.frame(x=x,y=y)
Map+geom_point(data=points,aes(x=x,y=y,group=1))+
  ggtitle(paste("Observed Destination Transactions",DatePlotStart,"to",DatePlotEnd))

xx <- KrigData@coords
yy <- KrigData@data$Volume
# fit<- Tps(xx,yy)
fit <- Krig(xx, yy, theta=20) 
out<- predictSurface( fit)

r<-nrow(out$z)
c<-ncol(out$z)
df<-matrix(ncol=3,nrow=r*c)
colnames(df)<-c("x","y","value")
i=2
for(i in 1:ncol(out$z)){
  df[((i-1)*r+1):(i*r),1]=(out$x)
  df[((i-1)*r+1):(i*r),2]=(out$y[i])
  df[((i-1)*r+1):(i*r),3]=(out$z[,i])
}
df<-as.data.frame(df)

Map+geom_tile(data=df,aes(x=x,y=y,fill=value,group=1),alpha=0.75)+
  stat_contour(data=df,aes(x=x,y=y,z=value,group=1))

surface( out, type="C") # option "C" our favorite
US(add=TRUE) # add US map

#quilt.plot(x,y)
#US(add=TRUE) # add US map

# fit<- Tps(xx,yy)
# # fits a GCV thin plate smoothing spline surface to ozone measurements.
# # Hey, it does not get any easier than this!
# 
# summary(fit) #diagnostic summary of the fit 
# 
# set.panel(2,2)
# plot(fit) # four diagnostic plots of  fit and residuals.
# 
# set.panel()
# surface(fit) # contour/image plot of the fitted surface
# US( add=TRUE, col="magenta", lwd=2) # US map overlaid
# title(paste("Origin Volume Activity for ",DatePlotStart,"to",DatePlotEnd))

# 
# fit <- Krig(xx, yy, theta=20) 
# 
# summary( fit) # summary of fit 
# set.panel( 2,2) 
# plot(fit) # four diagnostic plots of fit  
# set.panel()
# surface( fit, type="C") # look at the surface 
# 
# # predict at data
# predict( fit)
# 
# # predict using 7.5 effective degrees of freedom:
# predict( fit, df=7.5)
# 
# 
# # predict on a grid ( grid chosen here by defaults)
# out<- predictSurface( fit)
# surface( out, type="C") # option "C" our favorite
# US(add=TRUE) # add US map
# 
# # predict at arbitrary points (10,-10) and (20, 15)
# xnew<- rbind( c( 10, -10), c( 20, 15))
# predict( fit, xnew)
# 
# # standard errors of prediction based on covariance model.  
# predictSE( fit, xnew)
# 
# # surface of standard errors on a default grid
# predictSurfaceSE( fit)-> out.p # this takes some time!
# surface( out.p, type="C")
# points( fit$x)






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





