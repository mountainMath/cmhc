#' Convert CMHC month and date field to Date
#'
#' @param X1 month and date field as returned by CMHC
#' @noRd
#' @return a vector of dates
date_from_cmhc_year_month <- function(X1){
  as.Date(paste0("01 ",X1),format="%d %b %Y")
}


#' A list of CMHC quality indicators
#'
#' @description Data obtained via this package will automatically translate internal CMHC quality labels
#' using this translation vector, this named vector is useful when working with CMHC data obtained from other
#' sources like sporadic excel sheets or data scraped from PDF reports.
#'
#' @format A named vector to translate internal CMHC quality indicators to plain text.
#' @export
cmhc_quality_labels <- c(a = "Excellent", b = "Very good", c = "Good", d = "Fair (Use with Caution)")


#' parse numeric values
#' @param x character vector to convert to mumeric
#' @noRd
#' @return a vector of numeric values
parse_numeric <- function(x){
  xx<-x %>%
    sub(",", "", ., fixed = TRUE) %>%
    sub(" %","",.,fixed = TRUE)
  xx[xx=="-"]="0"
  xx[xx=="++"]=NA_character_
  xx[xx=="n/a"]=NA_character_
  xx[xx=="**"]=NA_character_
  as.numeric(xx)
}

#' A dataset with geographic identifiers for CMHC and Census at the CMA level
#' @name cmhc_cma_translation_data
#' @docType data
#' @author derived from CMHC geographic data
#' @source Custom data extract from CMHC
#' @format A tibble with Census and CMHC geographic identifiers
#' @keywords data
NULL

#' A dataset with geographic identifiers for CMHC and Census at the CSD level
#' @name cmhc_csd_translation_data
#' @docType data
#' @author derived from CMHC geographic data
#' @source Custom data extract from CMHC
#' @format A tibble with Census and CMHC geographic identifiers
#' @keywords data
NULL

#' A dataset with geographic identifiers for CMHC and Census at the CT level
#' @name cmhc_ct_translation_data
#' @docType data
#' @author derived from CMHC geographic data
#' @source Custom data extract from CMHC
#' @format A tibble with Census and CMHC geographic identifiers
#' @keywords data
NULL


