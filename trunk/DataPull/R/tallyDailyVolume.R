#' @title tallyDailyVolume
#' @description tallies the volume within a X by X mile box
#' @details Computes an X mile bounding box around each coordinate and then tallies the number of transactions (both origin and destination)
#' occuring with that proximity window for that day and the prior t-volume_lag days
#'
#'  @param RAW data frame of the CHR data from the geocodeData function
#'  @param dist the size of the X by X mile box
#'  @param volume_lag the number of prior day's transactions that should be included in the volume computation
#'  @return data frame of Volume tallied data appended to the RAW file
#'
#'
#' @examples
#' \dontrun{
#' RAW <- tallyDailyVolume(RAW,dist=50,volume_lag=2)
#' }
#'
#' @export
tallyDailyVolume <- function(RAW,dist=50,volume_lag=2){
  #dist=50
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


  ###perfect way of doing it
  GetBounds<-function(LongLat){
    delta_lat <- change_in_latitude(dist/2)##only need to compute this 1 time
    bounds <- apply(LongLat,1,function(x){
      delta_long <- change_in_longitude(x[2],dist/2)
      lower_long <- x[1] - delta_long
      upper_long <- x[1] + delta_long
      lower_lat <- x[2] - delta_lat
      upper_lat <- x[2] + delta_lat
      return(data.frame(lower_long,upper_long,lower_lat,upper_lat))
    })
    bounds<-matrix(unlist(bounds),ncol=4,byrow=T)
    colnames(bounds) <- c("long_lower","long_upper","lat_lower","lat_upper")
    return(bounds)
  }
  OrigBounds <- GetBounds(RAW[c("OrigLongitude","OrigLatitude")])
  colnames(OrigBounds)<-paste0("orig_",colnames(OrigBounds))
  RAW <- cbind(RAW,OrigBounds)
  DestBounds <- GetBounds(RAW[c("DestLongitude","DestLatitude")])
  colnames(DestBounds)<-paste0("dest_",colnames(DestBounds))
  RAW <- cbind(RAW,DestBounds)

  #   ###approximate method only used if true method takes too long
  #   ###of doing the bounding box
  #   delta_lat <- change_in_latitude(dist/2)
  #   delta_long <- change_in_longitude(median(RAW$OrigLatitude,na.rm=T),dist/2)
  #
  #   RAW <- RAW %>% mutate(orig_long_lower=OrigLongitude-delta_long,
  #                         orig_long_upper=OrigLongitude+delta_long,
  #                         orig_lat_lower=OrigLatitude-delta_lat,
  #                         orig_lat_upper=OrigLatitude+delta_lat)
  #
  #   RAW <- RAW %>% mutate(dest_long_lower=DestLongitude-delta_long,
  #                         dest_long_upper=DestLongitude+delta_long,
  #                         dest_lat_lower=DestLatitude-delta_lat,
  #                         dest_lat_upper=DestLatitude+delta_lat)

  RAW_FINAL <- list()
  min_time<-min(RAW$EntryDate)
  max_time<-max(RAW$EntryDate)
  span<-difftime(max_time,min_time)
  time_sequence<-min_time+0:span  ##this is the date set over which to compute the summary measures
  ii=1

  ###this section became unstable using dplyr commands
  ###rewritten with primitive r commands
  for (i in 1:length(time_sequence)){
    t=time_sequence[i]
    print(t)
    t_min<-t-volume_lag
    #DATA <- RAW %>% filter(EntryDate==t)
    DATA <- RAW[RAW$EntryDate==t,]
    #DATA_WINDOW <-RAW %>% filter(EntryDate<=t & EntryDate>=t_min)
    DATA_WINDOW <-RAW[(RAW$EntryDate<=t & RAW$EntryDate>=t_min),]

    if(nrow(DATA)>0){
      ####Code the Origin volume
      OrigGrid_Summary<- apply(DATA[c("orig_long_lower","orig_long_upper","orig_lat_lower","orig_lat_upper")],1,function(b){
        #tmp<-filter(DATA_WINDOW,(OrigLongitude>=b[1] & OrigLongitude<=b[2]) &
        #              (OrigLatitude>=b[3] & OrigLatitude<=b[4]))
        tmp<-DATA_WINDOW[((DATA_WINDOW$OrigLongitude>=b[1] & DATA_WINDOW$OrigLongitude<=b[2]) &
                      (DATA_WINDOW$OrigLatitude>=b[3] & DATA_WINDOW$OrigLatitude<=b[4])),]
        #summarise(tmp,OrigVolume=n())
        return(nrow(tmp))
      })
      #Colnames <- colnames(OrigGrid_Summary[[1]])
      #Ncol <- ncol(OrigGrid_Summary[[1]])
      Ncol=1
      OrigGrid_Summary <- as.data.frame(matrix(unlist(OrigGrid_Summary),ncol=Ncol,byrow=T))
      #colnames(OrigGrid_Summary) <- Colnames
      colnames(OrigGrid_Summary) <- "OrigVolume"

      ####Code the Destination volume
      DestGrid_Summary<- apply(DATA[c("dest_long_lower","dest_long_upper","dest_lat_lower","dest_lat_upper")],1,function(b){
        #tmp<-filter(DATA_WINDOW,(DestLongitude>=b[1] & DestLongitude<=b[2]) &
        #              (DestLatitude>=b[3] & DestLatitude<=b[4]))
        tmp<-DATA_WINDOW[((DATA_WINDOW$DestLongitude>=b[1] & DATA_WINDOW$DestLongitude<=b[2]) &
                            (DATA_WINDOW$DestLatitude>=b[3] & DATA_WINDOW$DestLatitude<=b[4])),]
        #summarise(tmp,DestVolume=n())
        return(nrow(tmp))
      })
      #Colnames <- colnames(DestGrid_Summary[[1]])
      #Ncol <- ncol(DestGrid_Summary[[1]])
      Ncol=1
      DestGrid_Summary <- as.data.frame(matrix(unlist(DestGrid_Summary),ncol=Ncol,byrow=T))
      #colnames(DestGrid_Summary) <- Colnames
      colnames(DestGrid_Summary) <- "DestVolume"
      
      #ADD <- bind_cols(DATA,OrigGrid_Summary,DestGrid_Summary)
      ADD <- cbind(DATA,OrigGrid_Summary,DestGrid_Summary)
      RAW_FINAL[[ii]] <-ADD
      ii=ii+1
    }
  }

  RAW_FINAL <- rbind_all(RAW_FINAL) ###leave this dplyr function to collapse the array (I don't think this is an error point)
  RAW_FINAL <- RAW_FINAL %>% select(-(orig_long_lower:dest_lat_upper))
  RAW_FINAL <- as.data.frame(RAW_FINAL)
  return(RAW_FINAL)
}
