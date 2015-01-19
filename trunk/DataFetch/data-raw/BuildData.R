
buildData <- function(raw_data) {
  ######Put data conversion setps here to change field attributes
  ###empty for now, just a skeleton
  RAW_DATA<-raw_data
  return(RAW_DATA)
}


###this is a quick check of the build data function that should be run each time one
###updates the source data (which isn't all of the time)

library(testthat)
test_that("build data", {

  #pull in data
  raw_data <- fread('~/CHR_Reference_Data/FullDataSet_AllYearsCombined.txt',sep="|",nrows=100,header=TRUE)
  RAW_DATA<-buildData(raw_data)

  #create quick test to confirm that the column dimensions are the same
  expect_that(ncol(RAW_DATA), equals(c(64)))

})


