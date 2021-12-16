#' Convert CMHC month and date field to Date
#'
#' @param X1 month and date field as returned by CMHC
#' @return a vector of dates
#' @export
date_from_cmhc_year_month <- function(X1){
  as.Date(paste0("01 ",X1),format="%d %b %Y")
}
