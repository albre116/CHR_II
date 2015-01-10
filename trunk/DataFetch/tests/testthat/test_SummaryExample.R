
test_that("Summary Example", {
  # Setup
  out<-summaryExample(RAW_DATA)


  # validate
  expect_that(class(out), equals("table"))
})
