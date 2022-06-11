#' Convert CMHC month and date field to Date
#'
#' @param X1 month and date field as returned by CMHC
#' @return a vector of dates
#' @export
date_from_cmhc_year_month <- function(X1){
  as.Date(paste0("01 ",X1),format="%d %b %Y")
}


#' CMHC quality indicators
#'
#' @export
cmhc_quality_labels <- c(a = "Excellent", b = "Very good", c = "Good", d = "Fair (Use with Caution)")

#' parse numeric values
#' @param x character vector to convert to mumeric
parse_numeric <- function(x){
  xx<-x %>%
    sub(",", "", ., fixed = TRUE) %>%
    sub(" %","",.,fixed = TRUE)
  xx[xx=="-"]="0"
  #xx[xx=="++"]=NA_character_
  xx %>%
    as.numeric
}

#' A dataset with geo identifiers for CMHC and Census at the CMA level
#' @name cmhc_cma_translation_data
#' @docType data
#' @author derived from CMHC geographic data
#' @keywords data
NULL

#' A dataset with geo identifiers for CMHC and Census at the CSD level
#' @name cmhc_csd_translation_data
#' @docType data
#' @author derived from CMHC geographic data
#' @keywords data
NULL

#' A dataset with geo identifiers for CMHC and Census at the CT level
#' @name cmhc_ct_translation_data
#' @docType data
#' @author derived from CMHC geographic data
#' @keywords data
NULL


