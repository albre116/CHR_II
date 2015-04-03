#' @title characterConversion
#' @description removes illegal characters from encoding formats when transferring between windows and unix
#' @details Converts "English_United States.1252" to ASCII format comaptible with unix en_US.UTF-8 encoding for character vectors
#'
#'  @param RAW data frame of Data to be encoded
#'
#'
#' @examples
#' \dontrun{
#' RAW <- characterConversion(RAW)
#' }
#'
#' @export
characterConversion <- function(RAW){
  #sessionInfo()
  #localeToCharset("English_United States.1252")
  ###encoding type for unix en_US.UTF-8
  classes <- sapply(RAW,class)
  idx <- classes=="character"
  for(i in 1:length(classes)){
    if(idx[i]){
      RAW[,i]<- iconv(RAW[,i],from="ISO8859-1",to="ASCII")
      RAW[,i]<- toupper(RAW[,i])
      }
  }
  return(RAW)
}