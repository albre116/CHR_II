
buildData <- function(raw_data) {
  ######Put data conversion setps here to change field attributes
  ###empty for now, just a skeleton
  RAW_DATA<-raw_data
  return(RAW_DATA)
}


library(testthat)
test_that("build data", {

  #pull in data
  raw_data <- read.table('data-raw/FullDataSet_AllYearsCombined.txt',sep="|",nrows=1000,header=TRUE)
  RAW_DATA<-buildData(raw_data)

  #create quick test to confirm that the column dimensions are the same
  expect_that(ncol(RAW_DATA), equals(c(64)))

})


