



KrigPlot <- function(RAW,#intput data
                     type="Orig",#select either Orig or Dest
                     states.model=c("California","Nevada","Arizona","Oregon"),#model area
                     states.plot=c("California"),#plot area
                     npoints=2500,###krig points,
                     dist=50,#the miles aroound each krig point to summarize data over
                     DatePlotStart=as.Date("2011-1-1",format="%Y-%m-%d"),#start date for the plot data
                     DatePlotEnd=as.Date("2011-1-10",format="%Y-%m-%d"),#end date for the plot data
                     summary_fun="n()",##the dplyr function to run
                     summary_name="Volume",##the name of the summary measure
                     summary_variable="1",###the name of the variable or a number
                     low_quantile=0,#the quantile to cut the points at
                     high_quantile=100,#the quantile to cut the points at
                     theta=20# the kriging exponential decay parameter
                     ){
  
  usa.states <- readOGR(dsn = "Shapefiles", layer = "states")
#   states.model <- c("Washington", "Montana", "Maine", "North Dakota", "South Dakota", 
#                     "Wyoming", "Wisconsin", "Idaho", "Vermont", "Minnesota", "Oregon", 
#                     "New Hampshire", "Iowa", "Massachusetts", "Nebraska", "New York", 
#                     "Pennsylvania", "Connecticut", "Rhode Island", "New Jersey", 
#                     "Indiana", "Nevada", "Utah", "California", "Ohio", "Illinois", 
#                     "District of Columbia", "Delaware", "West Virginia", "Maryland", 
#                     "Colorado", "Kentucky", "Kansas", "Virginia", "Missouri", "Arizona", 
#                     "Oklahoma", "North Carolina", "Tennessee", "Texas", "New Mexico", 
#                     "Alabama", "Mississippi", "Georgia", "South Carolina", "Arkansas", 
#                     "Louisiana", "Florida", "Michigan")
  usa.selected <- usa.states[(usa.states@data$STATE_NAME %in% states.plot),]
  usa.selected@data$id=rownames(usa.selected@data)
  usa.selected.points=fortify(usa.selected,region="id")
  usa.selected.df=left_join(usa.selected.points,usa.selected@data,by="id")
  
  usa.model <- usa.states[(usa.states@data$STATE_NAME %in% states.model),]
  usa.model@data$id=rownames(usa.model@data)
  usa.model.points=fortify(usa.model,region="id")
  usa.model.df=left_join(usa.model.points,usa.model@data,by="id")
  
  
  b <- bbox(usa.model)
  b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
  b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])
  # scale longitude and latitude (increase bb by 5% for plot) replace 1.05
  # with 1.xx for an xx% increase in the plot size
  
  #baselayer <- ggmap(get_map(location = b))
  
  
  ###create grid from bounding box for kriging
  #npoints<-2500
  lon <- seq(b[1,1],b[1,2],length.out = ceiling(sqrt(npoints)))
  lat <- seq(b[2,1],b[2,2],length.out = ceiling(sqrt(npoints)))
  kgrid<-expand.grid(x = lon, y = lat)
  
  kgrid<-SpatialPoints(kgrid)
  
  int<-gIntersects(kgrid,usa.model,byid=T)
  
  clipped <- apply(int == F, MARGIN = 2, all)
  plot(kgrid[which(clipped), ])  # shows all stations we DO NOT want
  kgrid.cl <- kgrid[which(!clipped), ]  # use ! to select the invers
  points(kgrid.cl, col = "green")  # check that it's worked
  
  kgrid <- kgrid.cl
  rm(kgrid.cl)  # tidy up: we're only interested in clipped ones
  Map <- ggplot(usa.model.df)+
    aes(long,lat,group=group)+
    geom_polygon(color="black",alpha=0)+
    labs(x = "Long", y = "Lat") + coord_fixed()+
    theme_bw()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  points<-as.data.frame(kgrid@coords)
  plt <- Map+geom_point(data=points,aes(x=x,y=y,group=1))+ggtitle("Kriging Support Points")
  print(plt)
  
  ###now for the kriging grid, we need to compute a volume measure within a specified distance
  # Distances are measured in miles.
  # Longitudes and latitudes are measured in degrees.
  # Earth is assumed to be perfectly spherical.
  #dist=50#miles for load to be in cell
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
  #min_time<-min(RAW$EntryDate)
  #max_time<-max(RAW$EntryDate)
  #span<-difftime(max_time,min_time)
  #time_sequence<-min_time+0:span  ##this is the date set over which to compute the summary measures
  
  #DatePlotStart <- as.Date("2011-1-1",format="%Y-%m-%d")
  #DatePlotEnd <- as.Date("2011-1-10",format="%Y-%m-%d")
  DATA <- RAW %>% filter(EntryDate>=DatePlotStart & EntryDate<=DatePlotEnd,
                         !is.na(OrigLongitude),!is.na(OrigLatitude))
  check_data<-SpatialPoints(DATA[c("OrigLongitude","OrigLatitude")])
  int<-gIntersects(check_data,usa.model,byid=T)
  clipped <- apply(int == F, MARGIN = 2, all)
  DATA <- DATA[!clipped,]
  if(summary_variable!="1"){
    e1<-parse(text=paste0("DATA <- filter(DATA,!is.na(",summary_variable,"),ntile(",summary_variable,",100)>=low_quantile & ntile(",summary_variable,",100)<=high_quantile)"))
    eval(e1)
  }
  
  ####summarise observations landing within krig-grid cells
  Grid_Summary<- apply(kgrid@data,1,function(b){
    tmp<-filter(DATA,(OrigLongitude>=b[1] & OrigLongitude<=b[2]) & 
                  (OrigLatitude>=b[3] & OrigLatitude<=b[4]))
    expr1<-parse(text=paste0("ans<-summarise(tmp,Quantity=",summary_fun,")"))
    eval(expr1) 
    return(data.frame(ans))
  })

  Colnames <- colnames(Grid_Summary[[1]])
  Ncol <- ncol(Grid_Summary[[1]])
  tmp <- unlist(Grid_Summary)
  tmp[is.infinite(tmp)] <- NA
  Grid_Summary <- as.data.frame(matrix(tmp,ncol=Ncol,byrow=T))
  colnames(Grid_Summary) <- Colnames
  
  ###we have the kriging data for the model area
  KrigData<- SpatialPointsDataFrame(coords=kgrid@coords,data=Grid_Summary)
  
  ###we now trim the other data outside the plotting area
  check_data<-SpatialPoints(DATA[c("OrigLongitude","OrigLatitude")])
  int<-gIntersects(check_data,usa.selected,byid=T)
  clipped <- apply(int == F, MARGIN = 2, all)
  DATA <- DATA[!clipped,]
  
  
  Map <- ggplot(usa.selected.df)+
    aes(long,lat,group=group)+
    geom_polygon(color="black",alpha=0,lwd=1)+
    labs(x = "Long", y = "Lat") + coord_fixed()+
    theme_bw()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  e1<-parse(text=paste0("points<-data.frame(x=DATA$OrigLongitude,y=DATA$OrigLatitude,fill_var=",summary_variable,")"))
  eval(e1)
  if(length(unique(points$fill_var))<=1){
    cols <- rev(brewer.pal(11, 'RdYlBu'))
    plt <- Map+geom_point(data=points,aes(x=x,y=y,group=1),size=3)+
      ggtitle(paste("Observed Transactions",DatePlotStart,"to",DatePlotEnd))
    
  }else{
  cols <- rev(brewer.pal(11, 'RdYlBu'))
  plt <- Map+geom_point(data=points,aes(x=x,y=y,group=1,colour=fill_var),size=3)+scale_color_gradientn(colours = cols)+
    ggtitle(paste("Observed Transactions for",summary_variable,DatePlotStart,"to",DatePlotEnd))
  }
  print(plt)
  
  xx <- KrigData@coords
  yy <- KrigData@data$Quantity
  # fit<- Tps(xx,yy)
  fit <- Krig(xx, yy, theta=theta) 
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
  
  check_data<-SpatialPoints(df[c("x","y")])
  int<-gIntersects(check_data,usa.selected,byid=T)
  clipped <- apply(int == F, MARGIN = 2, all)
  df <- df[!clipped,]
  


  ###Plot the raw raw tiled variables
  raw_plt <- data.frame(xx,yy)
  colnames(raw_plt) <- c("x","y","value")
  check_data<-SpatialPoints(raw_plt[c("x","y")])
  int<-gIntersects(check_data,usa.selected,byid=T)
  clipped <- apply(int == F, MARGIN = 2, all)
  raw_plt <- raw_plt[!clipped,]
  cols <- rev(brewer.pal(11, 'RdYlBu'))
  plt <- Map+geom_tile(data=raw_plt,aes(x=x,y=y,fill=value,group=1),alpha=0.75)+
    stat_contour(data=raw_plt,aes(x=x,y=y,z=value,group=1))+ scale_fill_gradientn(colours = cols)+
    ggtitle(paste("Raw Kriging Points Tiled For ",summary_name,DatePlotStart,"to",DatePlotEnd))
  print(plt)


  ####plot the kriging surface
  cols <- rev(brewer.pal(11, 'RdYlBu'))
  Map+geom_tile(data=df,aes(x=x,y=y,fill=value,group=1),alpha=0.75)+
    stat_contour(data=df,aes(x=x,y=y,z=value,group=1))+ scale_fill_gradientn(colours = cols)+
    ggtitle(paste("Kriging Estimate for ",summary_name,DatePlotStart,"to",DatePlotEnd))

}





