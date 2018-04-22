# cmhc
#
# An R package to access CMHC data.

#' Hack into CMHC data portal to pull out csv tables
#' @param query_params Query parameters to be sent to CMHC.
#'
#' returns dataframe with CMHC data, tile and subtitle are set as attributes
#' @keywords canada cmhc data api hack
#' @export
#'
#' @examples
#' dat=get_cmhc(cmhc_rent_change_history_params())
get_cmhc <- function(query_params) {
  url="https://www03.cmhc-schl.gc.ca/hmip-pimh/en/TableMapChart/ExportTable"
  cookie='ORDERDESKSID=jFINZMyDxkcEQBY3IJL4p2tWB0kFbPOXLJC7Fv4uVCdYBCNcqIUgi7N53swo1Qty; WT_FPC=id=66.183.109.243-320627712.30508028:lv=1466113349996:ss=1466113349996; BIGipServerpool-HMIP-PROD=rd22o00000000000000000000ffff0a009815o80; _ga=GA1.3.64898709.1458624685; DoNotShowIntro=true; _ga=GA1.4.64898709.1458624685; ORDERDESKSID=cCfzb1jZknrSTdfE1Db8rxWifrIuRL9BGT4ae8kd5xDATcXjkfkVDDDuTn6Fxhgl; LUI=; AUTOLOGINTOKEN='
  share_token="L2htaXAtcGltaC9lbi9UYWJsZU1hcENoYXJ0L1RhYmxlP1RhYmxlSWQ9MS4xLjIuOSZHZW9ncmFwaHlJZD0yNDEwJkdlb2dyYXBoeVR5cGVJZD0zJkJyZWFrZG93bkdlb2dyYXBoeVR5cGVJZD00JkRpc3BsYXlBcz1UYWJsZSZHZW9ncmFnaHlOYW1lPVZhbmNvdXZlciZZdGQ9RmFsc2UmRGVmYXVsdERhdGFGaWVsZD1tZWFzdXJlLTExJlN1cnZleT1TY3NzJkZvclRpbWVQZXJpb2QuWWVhcj0yMDE2JkZvclRpbWVQZXJpb2QuUXVhcnRlcj0zJkZvclRpbWVQZXJpb2QuTW9udGg9OA%253D%253D"
  dir.create('data_cache', showWarnings = FALSE) # make sure cache directory exists
  data_file="data_cache/test.csv"

  response <- httr::POST(url,
                  body=query_params,
                  encode = "form",
                  httr::set_cookies(cookie),
                  #httr::progress(), # too noisy
                  httr::write_disk(data_file, overwrite = TRUE)
                  )
  dat=readLines(data_file, encoding="latin1") # yes, CMHC does not use UTF-8...
  last_row=match("",dat)
  range=grep("^,.+$",dat)
  if (length(range)==1) {
    range[2]=last_row
  }
  result=read.table(text=dat[range[1]:(range[2]-1)],
                    sep = ",",
                    header=TRUE,
                    na.strings = c("**"),
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  region_title=lapply(strsplit(dat[1],"\u0097"),trimws)[[1]]
  attr(result,"region")=region_title[1]
  attr(result,"title")=region_title[2]
  attr(result,"subtitle")=dat[2]
  names(result)[names(result)==""] <- paste0("X",sequence(length(names(result)[names(result)==""])))
  #file.remove(data_file) #keep file for now for debugging purposes

  #parse integers
  parse_integer <- function(x){return(as.numeric(sub(",", "", x, fixed = TRUE)))}
  as_integer=c("Bachelor", "1 Bedroom", "2 Bedroom", "3 Bedroom +", "Total",
               "Single","Semi-Detached","Row","Apartment","All",
               "Semi / Row / Duplex","Other- Primarily Accessory Suites",
               "Before 1960", "1960 - 1979", "1980 - 1999", "2000 or Later",
               "3-5 Units", "6-19 Units", "20-49 Units", "50-199 Units", "200+ Units")
  result <- result %>% mutate_at(intersect(names(result), as_integer), funs(parse_integer))

  return(result)
}

#' Parameters for completion data
#' @export
cmhc_completion_params=  function(){
  year=2017
  month=7
  table_id="1.1.2.9"
  cmhc_filter="All"
  geography_id=2410
  geography_type=3
  breakdown_geography_type=4

  query_params=list(
    TableId=table_id,
    GeographyId=geography_id,
    GeographyTypeId=geography_type,
    BreakdownGeographyTypeId=breakdown_geography_type,
    DisplayAs="Table",
    GeograghyName="Vancouver",
    Ytd="false",
    Frequency="",
    RowSortKey="",
    DefaultDataField="measure-11",
    Survey="Scss",
    DataSource="1",
    exportType="csv",
    "ForTimePeriod.Year"=year,
    "ForTimePeriod.Quarter"="",
    ForTimePeriod.Month=month,
    ForTimePeriod.Season="",
    ToTimePeriod.Year=2016,
    ToTimePeriod.Quarter="",
    ToTimePeriod.Month=8,
    ToTimePeriod.Season="",
    "AppliedFilters%5B0%5D.Key"="dimension-18",
    "AppliedFilters%5B0%5D.Value"=cmhc_filter
  )
  return(query_params)
}

#' Parameters for under construction data
#' @param geography_id Geography for which to get the data
#' @param year Year for which to get the data
#' @param month Month for which to get the data
#' @export
cmhc_completion_params=  function(geography_id=2410, year=2017, month=7){
  table_id="1.1.3.11"
  cmhc_filter="All"
  cmhc_dimension="dimension-18"
  geography_type=3
  breakdown_geography_type=7 # CT level

  query_params=list(
    TableId=table_id,
    GeographyId=geography_id,
    GeographyTypeId=geography_type,
    BreakdownGeographyTypeId=breakdown_geography_type,
    GeographyBreakdownFieldKey="dimension-code-29",
    DisplayAs="Table",
    GeograghyName=names(cmhc_geography_list[cmhc_geography_list==geography_id]),
    Ytd="false",
    Frequency="",
    RowSortKey="",
    DefaultDataField="measure-11",
    Survey="Scss",
    DataSource="1",
    exportType="csv",
    ForTimePeriod.Year=year,
    ForTimePeriod.Quarter="",
    ForTimePeriod.Month=month,
    ForTimePeriod.Season="",
    ToTimePeriod.Year="",
    ToTimePeriod.Quarter="",
    ToTimePeriod.Month="",
    ToTimePeriod.Season="",
    "AppliedFilters%5B0%5D.Key"=cmhc_dimension,
    "AppliedFilters%5B0%5D.Value"=cmhc_filter
  )
  return(query_params)
}

#' Build ct_uid out of cmhc data
#' @param cma_header Census CMA code without province part
#' @param GeoUID ct geo_uid
#' @export
cmhc_geo_uid_for_ct <- function(cma_header,GeoUID) {
  return(paste0(cma_header,sprintf("%07.2f", GeoUID)))
}

#' Parameters for primary market vacancy data by survey zones
#' @param geography_id Geography for which to get the data
#' @export
cmhc_vacancy_params=  function(geography_id=2410){
  year=2017
  month=10
  table_id="2.1.12.3"
  cmhc_filter="Row / Apartment"
  cmhc_key="dwelling_type_desc_en"
  geography_type=3
  breakdown_geography_type=5

  query_params=list(
    TableId=table_id,
    GeographyBreakdownFieldKey="historic_survey_zone_geographic_layer_id",
    GeographyId=geography_id,
    GeographyTypeId=geography_type,
    BreakdownGeographyTypeId=breakdown_geography_type,
    DisplayAs="Table",
    GeograghyName=names(cmhc_geography_list[cmhc_geography_list==geography_id]),
    Ytd="false",
    Frequency="",
    RowSortKey="",
    DefaultDataField="measure-11",
    Survey="Rms",
    DataSource="1",
    exportType="csv",
    "ForTimePeriod.Year"=year,
    "ForTimePeriod.Quarter"="",
    ForTimePeriod.Month=month,
    ForTimePeriod.Season="",
    ToTimePeriod.Year=year,
    ToTimePeriod.Quarter="",
    ToTimePeriod.Month=month,
    ToTimePeriod.Season="",
    "AppliedFilters%5B0%5D.Key"=cmhc_key,
    "AppliedFilters%5B0%5D.Value"=cmhc_filter
  )
  return(query_params)
}

#' Parameters for primary market vacancy data timeline
#' @param geography_id Geography for which to get the data
#' @export
cmhc_vacancy_history_params=  function(geography_id=2410){
  year=2017
  month=""
  table_id="2.2.1"
  cmhc_filter="Row / Apartment"
  cmhc_key="dwelling_type_desc_en"
  geography_type=3
  breakdown_geography_type=0
  default_data_field="vacancy_rate_pct"

  query_params=list(
    TableId=table_id,
    GeographyBreakdownFieldKey="",
    GeographyId=geography_id,
    GeographyTypeId=geography_type,
    BreakdownGeographyTypeId=breakdown_geography_type,
    DisplayAs="Table",
    GeograghyName=names(cmhc_geography_list[cmhc_geography_list==geography_id]),
    Ytd="false",
    Frequency="",
    RowSortKey="",
    DefaultDataField=default_data_field,
    Survey="Rms",
    DataSource="1",
    exportType="csv",
    ForTimePeriod.Year=1990,
    ForTimePeriod.Quarter="",
    ForTimePeriod.Month="",
    ForTimePeriod.Season="",
    ToTimePeriod.Year=year,
    ToTimePeriod.Quarter="",
    ToTimePeriod.Month=month,
    ToTimePeriod.Season="",
    "AppliedFilters%5B0%5D.Key"=cmhc_key,
    "AppliedFilters%5B0%5D.Value"=cmhc_filter
  )
  return(query_params)
}


#' Parameters for primary market rent change fixed sample data timeline
#' @param geography_id Geography for which to get the data
#' @export
cmhc_rent_change_history_params=  function(geography_id=2410){
  return(cmhc_primary_rental_params(geography_id, default_data_field="fixed_sample_rent_change_pct"))
}

#' Parameters for primary market rent change fixed sample data timeline
#' @param geography_id Geography for which to get the data
#' @param default_data_field data field
#' @param data_source not sure what this is
#' @param table_id cmhc table id
#' @export
cmhc_primary_rental_params=  function(geography_id=2410, table_id = "2.2.12",default_data_field = "fixed_sample_rent_change_pct", data_source="1"){
  year=2017
  month=""
  cmhc_filter="Row / Apartment"
  cmhc_key="dwelling_type_desc_en"
  geography_type=3
  breakdown_geography_type=0

  query_params=list(
    TableId=table_id,
    GeographyBreakdownFieldKey="",
    GeographyId=geography_id,
    GeographyTypeId=geography_type,
    BreakdownGeographyTypeId=breakdown_geography_type,
    DisplayAs="Table",
    GeograghyName=names(cmhc_geography_list[cmhc_geography_list==geography_id]),
    Ytd="false",
    Frequency="",
    RowSortKey="",
    DefaultDataField=default_data_field,
    Survey="Rms",
    DataSource=data_source,
    exportType="csv",
    ForTimePeriod.Year=1990,
    ForTimePeriod.Quarter="",
    ForTimePeriod.Month="",
    ForTimePeriod.Season="",
    ToTimePeriod.Year=year,
    ToTimePeriod.Quarter="",
    ToTimePeriod.Month=month,
    ToTimePeriod.Season="",
    "AppliedFilters%5B0%5D.Key"=cmhc_key,
    "AppliedFilters%5B0%5D.Value"=cmhc_filter
  )
  return(query_params)
}



#' Parameters for time series
#' @param geography_id Geography for which to get the data
#' @param geography_type type corrsponding to geography
#' @param table_id CMHC table id
#' @export
cmhc_snapshot_params=  function(table_id = "2.2.12",
                                geography_id=2410, geography_type=3, breakdown_geography_type="CSD",
                                filter=list(), region=NA,
                                year=2017, month=7,frequency=NA){

  if (length(region)>1 || !is.na(region)) {
    geography_id=region["geography_id"]
    geography_type=region["geography_type_id"]
  }
  query_params=list(
    TableId=as.character(table_id),
    GeographyId=as.character(geography_id),
    GeographyTypeId=as.character(geography_type),
    BreakdownGeographyTypeId=as.integer(as.character(cmhc_geography_type_list[breakdown_geography_type])),
    ForTimePeriod.Year=year,
    exportType="csv"
  )
  if (!is.na(month)) {
    query_params["ForTimePeriod.Month"]=month
  }
  if (!is.na(frequency)) {
    query_params["Frequency"]=frequency
  }
  if (length(filter)> 0) for (i in 1:length(filter)) {
    x=filter[i]
    query_params[paste0("AppliedFilters[",i-1,"].Key")]=names(x)
    query_params[paste0("AppliedFilters[",i-1,"].Value")]=as.character(x)
  }


  return(query_params)
}


#' Parameters for time series
#' @param geography_id Geography for which to get the data
#' @param geography_type type corrsponding to geography
#' @param table_id CMHC table id
#' @export
cmhc_timeseries_params=  function(table_id = "1.1.2.9", geography_id=2410, geography_type=3, region=NA){
  breakdown_geography_type=0
  if (length(region)>1 || !is.na(region)) {
    geography_id=region["geography_id"]
    geography_type=region["geography_type_id"]
  }
  query_params=list(
    TableId=as.character(table_id),
    GeographyId=as.character(geography_id),
    GeographyTypeId=as.character(geography_type),
    BreakdownGeographyTypeId=as.character(breakdown_geography_type),
    exportType="csv"
  )
  return(query_params)
}


#' table region code lookup
#' @export
cmhc_table_region_code=list(
  CMA=3,
  ZONES=3,
  CSD=4,
  NBHD=9,
  CT=11
)

cmhc_survey=list(
  "Scss"=1,
  "Rms"=2,
  "Srms"=4
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
  "Scss Unabsorbed Inventory Time Series" ="1.2.4",
  "Scss Length of Construction Time Series" = "1.2.7",
  "Scss Unabsorbed Inventory Base" = "1.1.4", # 9 for CT
  "Srms Condo Number" = "4.2.4",
  "Srms Condo Average Rent" = "4.4.2",
  "Srms Other Number" = "4.6.1",
  "Srms Other Average Rent" = "4.6.2",
  "Srms Vacancy Rate Time Series" = "4.2.1",
  "Rms Vacancy Rate Time Series" = "2.2.1",
  "Rms Vacancy Rate CSD" = "2.1.1.4",
  "Rms Vacancy Rate CT" = "2.1.1.6",
  "Rms Average Rent Time Series" = "2.2.11",
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
  "Rms Median Rent Bedroom Type CT"="2.1.21.6"
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


#' cmhc geography lookup for cma
#' @export
cmhc_geography_list=list(Vancouver="2410", Toronto="2270", Calgary="0140", Victoria="2440", Winnipeg="2680",
                         Montreal="1060", Ottawa="1265", Halifax="0580", Edmonton="0340")

#' cmhc geography type lookup
#' @export
cmhc_geography_type_list=list(CMA=3, CSD=4, CT=7)

#' cmhc rms geography type lookup
#' @export
cmhc_rms_geography_list=list(CMA=3, CSD=4, NBHD=5, CT=6)

#' census geography lookup
#' @export
census_geography_list=list(Vancouver="59933", Toronto="35535", Calgary="48825", Victoria="59935")

#' census geography cities
#' @export
cmhc_geography_csd_list=list(Vancouver="5915022",
                             Burnaby="5915025",
                             "New Westminster"="5915029",
                             Victoria="5917034",
                             Calgary="4806016",
                             Toronto="3520005",
                             Burnaby="5915025",
                             Surrey="5915004",
                             Richmond="5915015",
                             "North Vancouver (CY)"="5915051",
                             "North Vancouver (DM)"="5915046",
                             Edmonton="4811061",
                             Winnipeg="4611040",
                             Montreal="2466023",
                             Ottawa="3506008")



#' census geography params
#' @export
cmhc_region_params <- function(geography,type='CMA'){
  list <- list("geography_type_id"=as.character(cmhc_geography_type_list[type]))
  list["geography_id"] = ifelse(type=="CMA",
                                as.character(cmhc_geography_list[geography]),
                                as.character(cmhc_geography_csd_list[geography]))
  return(list)
}

#' census geography lookup
#' @export
census_to_cmhc_translation=list("59933"="2410", "35535"="2270", "48825"="0140", "59935"="2440")

#' intended market rental
#' @export
intended_market_rental = list("dimension-18"="Rental")

