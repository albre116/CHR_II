
test_that("Summary Example", {
  # Setup
  out<-summaryExample(RAW_DATA[1:100,])
  # validate
  expect_that(class(out), equals("table"))
})
