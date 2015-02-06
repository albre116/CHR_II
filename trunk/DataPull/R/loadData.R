#' @title loadData
#' @description Loads and formats source data into data needed in application
#' @details Fuller description to be added when SQL language is integrated in here (for now we just parse a random data file)
#'
#'  @param path System path to raw data file
#'  @param sample_pct Random sample to take of data [0,1]

#'  @return data frame of formatted data
#'
#'
#' @examples
#' \dontrun{
#' path <- c('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt')
#' sample_pct <- 0.1
#' loadData(path,sample_pct)
#' }
#'
#' @export
loadData <- function(path,sample_pct){
  ###get the column classes by reading in a small portion of the data
  Classes = sapply(read.table(path,sep="|",nrows=1000,header=TRUE,stringsAsFactors = F),class)

  ###read in full data
  raw_data <- fread(path,sep="|",nrow=-1,header=TRUE)

  ###take sample of data
  raw_data<-raw_data[sample(1:nrow(raw_data),floor(nrow(raw_data)*sample_pct)),]

  ###set the data classes for the data frame
  raw_data <- as.data.frame(raw_data)
  for(i in 1:length(Classes)){
    class(raw_data[,i])<-Classes[i]
  }

  ####identify date terms####
  date_fields<-c("Date","DepartureTime","ArrivalTime")
  date_fields<-unlist(lapply(date_fields,grep,colnames(raw_data)))
  for(i in date_fields){
    raw_data[,i]<-ymd_hms(raw_data[,i])
  }

  ####Construct Model Data####
  ###use dput() trick to get test cases together
  include<-c("NormalizedCustomerLineHaul", "NormalizedCarrierLineHaul",
             "MarketRateFuel", "DurationOfContractedRate", "NormalizedDistanceByCity",
             "SumOfStops", "OriginDwellTime", "DestinationDwellTime", "loadnum",
             "OwningBranch", "ControllingBranch", "CoveringBranch", "LoadMiles",
             "SumOfStopDistance", "TotalCustomerCharges", "TotalCarrierCharges",
             "CustomerLineHaul", "CarrierLineHaul", "CustomerFuelSurcharge",
             "CarrierFuelSurcharge", "CustomerStopAccessorial", "CarrierStopAccessorial",
             "SumCustomerAccessorials", "SumCarrierAccessorials", "NetRevenue",
             "LoadCondition", "HazardousFlag", "HighValue", "EDIShipmentFlag",
             "TeamService", "CargoInsuranceValue", "CustomerCCode", "CustomerCount",
             "CarrierTCode", "CarrierCount", "Mode", "SAFlag", "ServiceOfferring",
             "MinTemp", "MaxTemp", "EntryDate", "ShipDate", "CarrierBookDate",
             "OriginArrivalTime", "OriginDepartureTime", "DestinationArrivalTime",
             "DestinationDepartureTime", "QuoteEffectiveDate", "QuoteExpirationDate",
             "QuoteID", "CarrierEmptyLocation", "Deadhead", "Origin", "OriginClassification",
             "Destination", "DestinationClassification", "CustomerName", "EAR2Name",
             "AnnualSales", "SIC", "SegmentationDescription", "ActivityYear",
             "OriginState", "DestinationState")

  if(!all(include %in% colnames(raw_data))){
    print("Data seems to be missing some expected fields")
    print(include[!(include %in% colnames(raw_data))])
    print("Are missing")
  }


  CHR<-select(raw_data, one_of(include))
  CHR<-CHR[complete.cases(CHR[c("TotalCustomerCharges","TotalCarrierCharges")]),]###minimum quantities needed to make any inference
  CHR$RPM_NormalizedCustomer <- CHR$NormalizedCustomerLineHaul/CHR$NormalizedDistanceByCity
  CHR$RPM_AllInCustomer <- CHR$TotalCustomerCharges/CHR$LoadMiles ###should I use normalized miles
  CHR$CPM_NormalizedCarrier <- CHR$NormalizedCarrierLineHaul/CHR$NormalizedDistanceByCity
  CHR$CPM_AllInCarrier <- CHR$TotalCarrierCharges/CHR$LoadMiles###should I use normalized miles
  CHR$RPMProfit_Normalized <- CHR$RPM_NormalizedCustomer-CHR$CPM_NormalizedCarrier
  CHR$RPMProfit_AllIn <- CHR$RPM_AllInCustomer-CHR$CPM_AllInCarrier
  CHR$Profit_Normalized <- CHR$NormalizedCustomerLineHaul-CHR$NormalizedCarrierLineHaul
  CHR$Profit_AllIn <- CHR$TotalCustomerCharges-CHR$TotalCarrierCharges
  ###do some data cleanup that should be moved to the DataPull package
  shipped <- unique(CHR$LoadCondition)[grep("F",unique(CHR$LoadCondition))]
  #Cutout Non-shipped and Shippers Agent Loads (check about the SA thing)
  CHR <- dplyr::filter(CHR,LoadCondition == shipped & SAFlag == "False")
  CHR$Day365<-as.numeric(format(CHR$EntryDate,format="%j"))
  #CHR$Day<-as.numeric(format(CHR$EntryDate,format="%d"))
  #CHR$Month<-as.numeric(format(CHR$EntryDate,format="%m"))
  #CHR$Year<-as.numeric(format(CHR$EntryDate,format="%Y"))
  CHR$EntryDate <- as.Date(format(CHR$EntryDate,format="%Y-%m-%d"))
  return(CHR)
}
