#' @title summaryExample
#' @description Example function that retruns summary
#' @details Simple example to demonstrate function documentation/construction
#'
#'  @param raw_data is the data.frame of CHR lane data
#'
#'  @return prints summary statistics and returns a data frame with these values
#'
#'
#' @examples
#' \dontrun{
#'  summary_example <- summaryExample(RAW_DATA)
#' }
#'
#' @export
summaryExample <- function(raw_data){
  out<-summary(raw_data)
  print(out)
  return(out)
}
