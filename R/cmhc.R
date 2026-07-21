# cmhc
#
# An R package to access CMHC data.

#' Access CMHC data via the HMIP.
#'
#' @description The data access needs to specify the survey, series, dimension (if any), and breakdown to specify the
#' CMHC table to pull the data from. The `list_cmhc_tables()` function can be used to list all the tables available
#' via this package. Snapshot data needs to specify the year, or if it is monthly data the month. Time series data, i.e.
#' when  `breakdown="Historical Time Series` is specified, does not need year or month parameters, but may have the
#' frequency parameter set. Filters provide additional ways to filter the tables by sub-categories.
#'
#' @param survey The CMHC survey, one of "Scss", "Rms", "Srms", and "Seniors",  consult `list_cmhc_surveys()` for possible values.
#'(Other surveys and more data series may be supported in future versions.)
#' @param series The CMHC data series of the survey, consult `list_cmhc_series()` for possible values.
#' @param dimension The dimension to show in the results, consult `list_cmhc_dimensions()` for possible values.
#' @param breakdown The geographic breakdown, one of "Survey Zones", "Census Subdivision", "Neighbourhoods", "Census Tracts",
#' if querying data for a snapshot in time or "Historical Time Periods" if querying time series data. Not all geographic breakdowns are available for all series.
#' returns data frame with CMHC data, tile and subtitle are set as attributes.  Consult `list_cmhc_breakdowns()` for possible values.
#' @param geoFilter optional geographic type, only relevaent for provincial and Canada-wide tables. Options are "Default" (the default)
#' which considers accesses the default tables which should be used for data at the metro area or finer geographies. The other designaters
#' are only useful when used in conjunction with `geo_uid`s for provinces or all of Canada. Specifying "All"  will
#' give data for all sub-regions, "Metro", which only considers data in metro areas, "50k" and "10k", which only considers data from metro areas and agglomerations with at least
#' 50k or 10k people, respectively.
#' @param geo_uid Census geographic identifier for which to query the data. Can be a census tract, census subdivision,
#' or census metropolitan area.
#' @param year optional, only needed when querying data for a snapshot in time.
#' @param quarter optional, only needed when querying data for a snapshot in time querying quarterly data.
#' @param month optional, only needed when querying data for a snapshot in time.
#' @param frequency optional, only needed when querying time series data, one of "Monthly", "Quarterly" or "Annual".
#' @param filters optional list of filters, consult `list_cmhc_filters()` for possible values.
#' @param refresh optional, re-download the data from CMHC instead of using the copy cached in
#' the R session's temporary directory. Defaults to `FALSE`.
#'
#' @return A tibble with the data in long form.
#'
#' @keywords Canada CMHC data API
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_cmhc("Rms","Vacancy Rate","Bedroom Type","Historical Time Periods","5915022")
#' }
get_cmhc <- function(survey,series, dimension, breakdown,geoFilter="Default",
                     geo_uid,
                     year=NULL, quarter=NULL, month=NULL, frequency = NULL,
                     filters=list(),
                     refresh=FALSE){
  table_list <- list_cmhc_tables(short=FALSE)
  if (!is.null(frequency)) {
    frequency <- stringr::str_to_title(frequency)
    allowed_frequencies <- c("Monthly","Annual","Quarterly")
    if (!(frequency %in% allowed_frequencies)) {
      stop(paste0("Frequency has to be one of ",paste0(allowed_frequencies,collapse=", ")))
    }
  }
  valid_geoFilters <- c("Default","All","Metro","50k","10k")
  if (!(geoFilter %in% valid_geoFilters)) stop(paste0("Parameter geoFilter needs to be one of ",
                                                        paste0(valid_geoFilters,collapse = ", ")))
  selectedTable<-table_list |>
    filter(.data$Survey==survey,
           .data$Series==series,
           .data$Breakdown==breakdown,
           .data$GeoFilter == geoFilter)
  if (!is.na(dimension) && !is.null(dimension)) {
    selectedTable <- selectedTable |>
      filter(.data$Dimension==dimension)
  }

  if (nrow(selectedTable)!=1) {
    if (nrow(selectedTable)>1) stop("Unexpected error, please file an issue on GitHub with your function call.")
    if (nrow(selectedTable)==0) {
      selectedSurvey<-table_list |>
        filter(.data$Survey==survey)
      if (nrow(selectedSurvey)==0) {
        stop(paste0("Survey ",survey," does not exist or is not supported. Valid surveys are ",
                    paste0(unique(list_cmhc_surveys()$Survey),collapse = ", "),"."))
      }
      selectedSeries<-table_list %>%
        filter(.data$Survey==survey, .data$Series==series)
      if (nrow(selectedSeries)==0) {
        stop(paste0("Series ",series," for survey ",survey,
                    " does not exist or is not supported. Valid series are ",
                    paste0(unique(selectedSurvey$Series),collapse = ", "),"."))
      }
      selectedDimension <- table_list %>%
        filter(.data$Survey==survey, .data$Series==series, is.null(dimension)||is.na(dimension)||.data$Dimension==dimension)
      if (nrow(selectedDimension)==0 && !(is.null(dimension)||is.na(dimension))) {
        stop(paste0("Dimension ",dimension," for ",series," and survey ",survey,
                    " does not exist or is not supported. Valid dimensions are ",
                    paste0(unique(selectedSeries$Dimension),collapse = ", "),"."))
      }
      stop(paste0("Breakdown ",breakdown," for ",series,",survey ",survey," and dimension ",dimension,
                  " does not exist or is not supported. Valid dimensions are ",
                  paste0(unique(selectedDimension$Breakdown),collapse = ", "),"."))
    }
  } # end validation

  if (length(filters)>0) {
    availableFilters <- selectedTable$Filters
    if (length(availableFilters)==1 && (is.null(names(availableFilters)) || names(availableFilters)=="")) {
      availableFilters <- unlist(availableFilters,recursive = FALSE)
    }
    unmatchedFilters <- names(filters)[!(names(filters) %in% names(availableFilters))]
    if (length(unmatchedFilters)>0) {
      stop("Filter ",paste0(unmatchedFilters,collapse = ", ")," not available for ",series,",survey ",survey," and dimension ",dimension,".")
    }
  }

  geo_names <- names(geo_uid)
  if (is.null(geo_names)) {
    region_params <- cmhc_region_params_from_census(geo_uid)
  } else {
    if (c("Neighbourhood") %in% geo_names|c("Hood") %in% geo_names) {
      nid <- geo_uid["Neighbourhood"] |> as.character()
      if (length(nid)==0) nid <- geo_uid["Hood"] |> as.character()
      region_params <- cmhc_region_params_from_census(as.character(geo_uid["CMA"]))
      hood <- cmhc::cmhc_ct_translation_data |> select(.data$NBHDCODE,.data$METNBHD,.data$NBHDNAME_EN,.data$METCODE) |>
        unique() |>
        filter(.data$METCODE==region_params$geography_id) |>
        filter(.data$NBHDCODE==nid|.data$NBHDNAME_EN==nid)
      if (nrow(hood)!=1) {
        stop("Could not find neighbourhood ",nid," in metro area ",geo_uid["CMA"],".")
      }
      region_params$geography_type_id="6"
      region_params$geography_id=hood$METNBHD
    } else {
      stop(paste0("Unknown region specification, geo_uid names must include ",
                  "\"Neighbourhood\" (or \"Hood\") together with \"CMA\", got: ",
                  paste0(geo_names,collapse=", "),"."))
    }
  }

  if (region_params$geography_type_id=="1") {
    if (selectedTable$TableCode=="5.11.2") selectedTable$TableCode="5.11.1"
    if (selectedTable$TableCode=="5.7.2") selectedTable$TableCode="5.7.1"
  }

  if (selectedTable$Series=="Starts (SAAR)") {
    codes <- selectedTable$TableCode |> strsplit("\\.") |>
      unlist()
    if (!(region_params$geography_type_id %in% c("1","2","3"))) {
      stop("SAAR tables are only available for Canada, Provinces and CMAs.")
    }
    if (region_params$geography_type_id %in% c("1","2") && selectedTable$GeoFilter == "Default") {
      warning("SAAR tables for Canada and the Provinces are only available for All or 10k geographies, changing to 10k.")
      codes[2]="1"
    }
    if (region_params$geography_type_id %in% c("3") && selectedTable$GeoFilter != "Default") {
      warning("SAAR tables for Metro Areas are only available for Default geographies, changing to Default")
      codes[2]="3"
    }
    codes[3]=region_params$geography_type_id
    selectedTable$TableCode=paste0(codes,collapse = ".")
  }

  query_params <- list(
    TableId=selectedTable$TableCode,
    GeographyId=region_params$geography_id,
    GeographyTypeId=region_params$geography_type_id,
    #BreakdownGeographyTypeId=as.integer(as.character(cmhc_geography_type_list[breakdown_geography_type])),
    #ForTimePeriod.Year=year,
    exportType="csv"
  )
  if (!is.null(region_params$MetId)) {
    query_params$MetId=region_params$MetId
  }
  if (!is.null(year)) {
    query_params$ForTimePeriod.Year=year
    if (is.null(frequency)) {
      query_params$Frequency="Annual"
    }
  }
  if (!is.null(month)) {
    query_params$ForTimePeriod.Month=month
    if (is.null(frequency)) {
      query_params$Frequency="Monthly"
    }
  }
  if (!is.null(quarter)) {
    query_params$ForTimePeriod.Quarter=quarter
    if (is.null(frequency)) {
      query_params$Frequency="Quarterly"
    }
  }

  if (!is.null(frequency)) {
    query_params["Frequency"]=frequency
  }
  if (length(filters)> 0) for (i in seq(1,length(filters))) {
    x=filters[i]
    query_params[paste0("AppliedFilters[",i-1,"].Key")]=names(x)
    query_params[paste0("AppliedFilters[",i-1,"].Value")]=as.character(x)
  }



  filehash <- digest::digest(query_params)
  data_file=file.path(tempdir(),paste0("cmhc_",filehash,".csv"))
  if (refresh||!file.exists(data_file)) {
    url="https://www03.cmhc-schl.gc.ca/hmip-pimh/en/TableMapChart/ExportTable"

    response <- httr::POST(url,
                           body=query_params,
                           encode = "form",
                           #httr::progress(), # too noisy
                           httr::timeout(300),
                           httr::write_disk(data_file, overwrite = TRUE)
    )
    if (response$status_code != 200) {
      if (file.exists(data_file)) file.remove(data_file)
      warning(paste0("Invalid response, status ",response$status_code,".","\n",
                     "This can happen when CMHC HMIP does not have data for the given geography ",geo_uid,"."))
      return(NULL)
    }
  }
  dat=readLines(data_file, encoding="latin1") # yes, CMHC does not use UTF-8...
  last_row=match("",dat)
  range=grep("^,.+$",dat)
  have_saar_table = FALSE
  if (length(range)==0) {
    range=grep("^ \u2014 Starts \\(SAAR\\)",dat)
    if (length(range)==1) {
      range[1]=range[1] + 1
      have_saar_table=TRUE
    }
  }
  if (length(range)==0) {
    warning("Problem reading response.")
    warning(paste0(dat,collapse = "\n"))
    return(NULL)
  } else if (length(range)==1) {
    while (dat[last_row]=="") last_row <- last_row - 1
    range[2]=last_row
  }

  header <-
    tibble(raw=dat[range[1]] %>% stringr::str_replace_all("(?<=\\$\\d),(?=\\d{3})", ".") %>%
             stringr::str_remove_all('\"') %>% strsplit(",") %>%
             lapply(\(x) stringr::str_replace_all(x, "(?<=\\$\\d)\\.(?=\\d{3})", ",")) %>%
             lapply(trimws) %>% unlist()) |>
    mutate(clean=ifelse(raw==""&lag(raw)!="",paste0(lag(raw)," - ","Quality"),raw)) |>
    mutate(clean=na_if(.data$clean,""))
  if (is.na(header$clean[1])) header$clean[1]="XX"
  if (nrow(header)==1&&have_saar_table) {
    header <- tibble::tibble(clean=c("XX","Starts (SAAR)"))
  }

  result=readr::read_csv(data_file,skip = range[1],n_max=range[2]-range[1],
                                          locale = readr::locale(encoding = "latin1"),
                                          col_types = readr::cols(.default = "c"),
                                          col_names=header$clean)

  last_column <- names(result) %>% last
  lcv <- pull(result,last_column) %>% unique
  if (grepl("^X\\d+$",last_column) && length(lcv)==1 && (is.na(lcv) || lcv=="")) {
    result <- result %>% select(-all_of(last_column))
  }

  regular_vars <- result |>
    select(-.data$XX) |>
    select(!matches("- Quality$")) |>
    names()

  quality_vars <- result |>
    select(-.data$XX) |>
    select(matches("- Quality$")) |>
    names()


  table <- result %>%
    select(!matches("- Quality$")) %>%
    tidyr::pivot_longer(-.data$XX,names_to="Metric",values_to="Value") %>%
    mutate(Value=parse_numeric(.data$Value))

  if (length(quality_vars)>0) {
    quality <- result %>%
      select(.data$XX,matches("- Quality$")) %>%
      tidyr::pivot_longer(-.data$XX,names_to="Metric",values_to="Quality") %>%
      mutate(Metric=gsub(" - Quality$","",.data$Metric)) %>%
      mutate(Quality=recode(.data$Quality,!!!cmhc_quality_labels)) %>%
      mutate(Quality=factor(.data$Quality,levels=as.character(cmhc_quality_labels)))

    table <- table %>%
      left_join(quality,by=c(names(table)[1],"Metric"))
  }

  table <- table |>
    mutate(Metric=factor(.data$Metric, levels= regular_vars))


  if (!(is.null(dimension) || is.na(dimension))) table <- table |> rename(!!dimension:=.data$Metric)

  if (have_saar_table) {
    table <- table |> mutate(`Dwelling Type`="Total")
    #table <- table |> select(-"Dwelling Type")
  }


  if (breakdown=="Historical Time Periods") {
    if (length(names(geo_uid))>0) {
      geo_identifier <- region_params$geography_id
    } else {
      geo_identifier <- geo_uid
    }
    table <- table |>
      rename(DateString=.data$XX) |>
      mutate(Date=.data$DateString) |>
      mutate(GeoUID=geo_identifier) |>
      relocate(.data$GeoUID,.data$Date,.data$DateString)
    date <- table$Date %>% na.omit() %>% first
    if (grepl("^\\d{4} .+$",date)) {
      table<- table |> mutate(Date=as.Date(paste0(.data$Date," 01"),format="%Y %B %d"))
    } else if (grepl("^.{3} \\d{4}$",date)) {
      table<- table |> mutate(Date=as.Date(paste0(.data$Date," 01"),format="%b %Y %d"))
    } else if (grepl("^\\d{4}$",date)) {
      table<- table |> mutate(Date=as.Date(paste0(.data$Date,"-07-01"),format="%Y-%m-%d"))
    } else if (grepl("^\\d{4}/Q\\d{1}$",date)) {
      table <- table |>
        mutate(Date=gsub("/Q1","-02-01",.data$Date) %>%
               gsub("/Q2","-05-01",.) %>%
               gsub("/Q3","-08-01",.) %>%
               gsub("/Q4","-11-01",.) %>%
               as.Date(.))
    } else {
      warning("Could not convert date string to date object.")
      table <- table |> select(-.data$Date)
    }
  } else {
    if (breakdown=="Census Tracts") {
      table <- table |> rename(GeoUID=.data$XX)
      table <- table |>
        mutate(GeoUID=cmhc_to_census_geocode(.data$GeoUID,geo_uid))
    } else {
      table <- table |> rename(!!as.name(breakdown):=.data$XX)
    }
  }


  region_title=lapply(strsplit(dat[1],"\u0097"),trimws)[[1]]
  attr(table,"region")=region_title[1]
  attr(table,"title")=region_title[2]
  attr(table,"subtitle")=dat[2]


  census_year_line <- grep("based on \\d{4} Census Geography Definitions",dat)
  if (length(census_year_line)==1) {
    census_year <- stringr::str_extract(dat[census_year_line],"\\d{4} Census") %>% gsub(" Census","",.)
    table <- table %>% mutate(`Census geography`=census_year)
  }

  if (!is.null(year)) {
    if (!is.null(month)) {
      date <- as.Date(paste0(year,"-",month,"-01"))
    } else if (!is.null(quarter)) {
      date <- as.Date(paste0(year,"-",stringr::str_pad((quarter-1)*3+1,width=2,pad="0"),"-01"))
    } else {
      date <- as.Date(paste0(year,"-07-01"))
    }
    table <- table |>
      mutate(Date=date,Year=year)
  }

  if (!is.null(month)) {
    table <- table |>
      mutate(Month=month)
  }
  if (!is.null(quarter)) {
    table <- table |>
      mutate(Quarter=quarter)
  }

  table <- table |>
    mutate(Survey=survey,Series=series)


  if (length(filters)>0) {
    ff <- filters |>
      unlist() |>
      as.character() |>
      paste0(collapse = " - ")
    table <- table |>
      mutate(Filters=ff)
  }

  if (geoFilter!="Default") {
    table <- table |>
      mutate(GeoFilter=geoFilter)
  }

  return(table)
}



# Suppress warnings for missing bindings for '.' in R CMD check.
if (getRversion() >= "4.1") utils::globalVariables(c("."))

#' @import dplyr
#' @importFrom rlang .data `:=`
#' @importFrom stats setNames na.omit

NULL

