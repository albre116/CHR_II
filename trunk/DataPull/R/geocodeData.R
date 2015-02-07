#' @title geocodeData
#' @description Geocodes the data to the 5-digit and 3-digit zip level
#' @details Uses the zipcode package reference data to geocode all of the data at the
#'
#'  @param RAW data frame of the CHR data from the loadData function

#'  @return data frame of geocoded data
#'
#'
#' @examples
#' \dontrun{
#' RAW <- geocodeData(RAW)
#' }
#'
#' @export
geocodeData<-function(RAW){

  ###pull the 5-digit zip codes form the dirty addresses
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
  return(RAW)
}
