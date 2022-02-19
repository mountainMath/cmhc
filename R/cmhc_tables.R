cmhc_survey <- list(
  "Scss" = 1,
  "Rms" = 2,
  "Srms" = 4
)

cmhc_scss_geo_level <- list(
  "Survey Zone" = 8,
  "Census Subdivision" = 9,
  "Neighbourhood" = 10,
  "Census Tract" = 11
)

#' table lookup
#' @export
cmhc_table_list=list(
  "Scss Absorbtion Rate Time Series" = "1.2.6",
  "Scss Starts Base" = "1.1.1",
  "Scss Completions" = "1.1.2.9",
  "Scss Completions Time Series" = "1.2.2",
  "Scss Completions CT" = "1.1.2.11",
  "Scss Under Construction CT" = "1.1.3.11",
  "Scss Under Construction CSD" = '1.1.3.9',
  "Scss Under Construction Time Series" = '1.2.3',
  "Scss Starts Time Series" = '1.2.1',
  "Scss Starts SZ" = '1.1.1.8',
  "Scss Starts CSD" = '1.1.1.9',
  "Scss Starts NBHD" = '1.1.1.10',
  "Scss Starts CT" = '1.1.1.11',
  "Scss Unabsorbed Inventory Time Series" ="1.2.4",
  "Scss Length of Construction Time Series" = "1.2.7",
  "Scss Unabsorbed Inventory Base" = "1.1.4", # 9 for CT
  "Srms Condo Number" = "4.2.3",
  "Srms Condo Average Rent" = "4.4.2",
  "Srms Other Number" = "4.6.1",
  "Srms Other Average Rent" = "4.6.2",
  "Srms Vacancy Rate Time Series" = "4.2.1",
  "Srms Condo Rental Number" = "4.2.4",
  "Rms Vacancy Rate Time Series" = "2.2.1",
  "Rms Vacancy Rate CSD" = "2.1.1.4",
  "Rms Vacancy Rate CT" = "2.1.1.6",
  "Rms Vacancy Rate Base" = "2.1.1",
  "Rms Rental Universe Time Series" = "2.2.26",
  "Rms Rental Universe Age Time Series" = "2.2.27",
  "Rms Rental Universe Structure Size Time Series" = "2.2.28",
  "Rms Rental Universe Bedrooms Base"= "2.1.26",
  "Rms Rental Universe Age Base"= "2.1.27",
  "Rms Rental Universe Structure Size Base"= "2.1.28",
  "Rms Rental Universe CT"="2.1.26.6",
  "Rms Rent Change Time Series" = "2.2.12",
  "Rms Rent Change CT" = "2.1.12.6",
  "Rms Rent Change CSD" = "2.1.12.4",
  "Rms Average Rent" = "2.1.11.3",
  "Rms Average Rent Time Series" = "2.2.11",
  "Rms Average Rent Bedroom Base" = "2.1.11",
  "Rms Average Rent Age Base" = "2.1.13",
  "Rms Average Rent Age CT"="2.1.13.6",
  "Rms Average Rent Bedroom Type CT"="2.1.11.6",
  "Rms Average Rent Bedroom Type Nbhd"="2.1.11.5",
  "Rms Median Rent Bedroom Type CT"="2.1.21.6",
  "Rms Median Rent Bedroom Type Time Series"="2.2.21",
  "Scss Absorbed Unit Price CSD"="1.9.1.7",
  "Scss Absorbed Unit Price Time Series"="1.10.1"
)

#' Start to organize data better
#' CMCH identifier rules:
#' first number for survey
#' second number 1=fixed time, 2=time series
#' thrid number series
#' forth number (only for fixed time) geographic breakdown
#' @export
cmhc_data_table <- list(
  Scss="Starts and Completions Survey",
  Rms="Rental Market Survey",
  Srms="Secondary Rental Market Survey",
  Srhs="Seniors Rental Housing Survey"
)


cmhc_series_codes <- list(
  Scss=1,
  Rms=2,
  Srms=4
)

cmhc_mode_codes <- list(
  snapshot=1,
  timeline=2
)

cmhc_duration_codes <- list(
  monthly=1,
  annual=8
)

cmhc_timeseries_params2 <- function(census_region,survey,series,level){

}

scss_series_codes <- list(
  "Starts by Dwelling Type"=1,
  "Completions by Dwelling Type"=2,
  "Under Construction"=3,
  "Starts by Intended Market"=4,
  "Absorbed Units"=5,
  "Unabsorbed Inventory"=6,
  "Length of Construction"=7,
  "Completions by Intended Market"=8,
  "Absorbed Prices"=9
)

rms_series_codes <- list(
  "Vacancy Rate"=1,
  "Average Rent"=2,
  "Availability Rate by Bedrooms"=6,
  "Availability Rate by Age"=7,
  "Availability Rate by Size"=8,
  "Average Rent by Bedrooms"=11,
  "Average Rent by Age"=13,
  "Average Rent by Size"=15,
  "Average Rent by Change by Bedrooms"=12,
  "Median Rent by Bedrooms"=21,
  "Median Rent by Age"=22,
  "Median Rent by Size"=23,
  "Rental Universe by Bedrooms"=26,
  "Rental Universe by Age"=27,
  "Rental Universe by Size"=28
)

list_cmhc_tables <- function(){
  tibble::tribble(
    ~SurveyName,~SureveyCode,~SeriesName,~SeriesCode,~Filters,
    "Scss",1,"Starts by Dwelling Type","1.1",c(),
    "Scss",1,"Completions","1.2",c(),
    "Scss",1,"Under Construction","1.3",c(),
    "Scss",1,"","1.4",c(),
    "Scss",1,"Absorbed Units","1.5",c("Condo","Homeowner","All"),
    "Scss",1,"Unabsorbed Inventory","1.6",c(),
    "Scss",1,"Length of Construction","1.7",c(),
    "Scss",1,"Absorbed Prices","9.1",c(),
    "Rms",2,"Vacancy Rate","1.1",c(),
    "Rms",2,"Vacancy Rate","1.2",c(),
    "Rms",2,"Average Rent","1.3",c(),
    "Rms",2,"Average Rent by Bedrooms","1.11",c(),
    "Rms",2,"Average Rent Change by Bedrooms","1.12",c(),
    "Rms",2,"Average Rent by Age","1.13",c(),
    "Rms",2,"Median Rent Bedroom","1.21",c(),
    "Rms",2,"Rental Universe by Bedrooms","1.26",c(),
    "Rms",2,"Rental Universe by Age","1.27",c(),
    "Rms",2,"Rental Universe by Structure","1.28",c()
  )
}

list_cmhc_geography_types=list(
  CMA=3,
  CSD=5
)


cmhc_table_suffix_for_breakdown_geography_CMA=list(
  #ZONE=6,
  CSD=7,
  ZONE=8,
  CT=9
)

cmhc_table_suffix_for_breakdown_geography_CSD=list(
  #ZONE=7,
  CSD=7,
  ZONE=8,
  CT=9
)

list_cmhc_breakdown_geography_codes=list(
  CMA=3,
  CSD=4,
  ZONE=5,
  NEIGHBOUHOOD=6,
  CT=7
)


#' table region code lookup
#' @export
cmhc_table_region_code=list(
  CMA=3,
  ZONES=3,
  CSD=4,
  NBHD=9,
  CT=11
)

