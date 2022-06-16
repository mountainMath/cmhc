cmhc_dwelling_types <- c("Single","Semi-detached","Row","Apartment","All")
cmhc_intended_markets <- c("Homeowner","Rental","Condo","Co-op","All")
cmhc_type_codes1 <- c("Provinces"=2,"Centres"=3,"Survey Zones"=8,"Census Subdivision"=9,"Neighbourhoods"=10,"Census Tracts"=11)
cmhc_type_codes2 <- c("Survey Zones"=6,"Census Subdivision"=7,"Neighbourhoods"=8,"Census Tracts"=9)
cmhc_type_codes3 <- c("Survey Zones"=3,"Census Subdivision"=4,"Neighbourhoods"=5,"Census Tracts"=6)
cmhc_type_codes4 <- c("Survey Zones"=3,"Census Subdivision"=4)
cmhc_series_dimension_codes1 <- c("Dwelling Type"=1,"Intended Market"=4)
cmhc_bedroom_types <- c("Bachelor","1 Bedroom","2 Bedroom","3 Bedroom +","Total")


#' List available CMHC tables
#'
#' @param short Logical, determines how much detail is returned. Default is `TRUE`.
#' @return A tibble listing all available CMHC data tables
#'
#' @examples
#' list_cmhc_tables()
#'
#' @export
list_cmhc_tables <- function(short=TRUE){
  scss_snapshot1 <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~GeoCodes,
    "Scss","1","Starts","1","1",
    "Scss","1","Completions","2","1",
    "Scss","1","Under Construction","3","1",
    "Scss","1","Length of Construction","7","2",
    "Scss","1","Absorbed Units","5","2") |>
  mutate(a="1") |>
    left_join(tibble(a="1",
                     Dimension=c("Dwelling Type","Intended Market"),
                     DimensionCode=c("1","4"),
                     Filters=list("dimension-18"=cmhc_intended_markets,
                                    "dimension-1"=cmhc_dwelling_types)),
              by="a")


  scss_snapshot2 <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~GeoCodes,
    "Scss","1","Share absorbed at completion","6","2",
    "Scss","1","Unabsorbed Inventory","4","2") |>
    mutate(a="1") |>
    left_join(tibble(a="1",
                     Dimension=c("Dwelling Type","Intended Market"),
                     DimensionCode=c("1","4"),
                     Filters=list("dimension-18"=c("Condo","Homeowner","All"),
                                  "dimension-1"=cmhc_dwelling_types)),
              by="a")

  scss_snapshot <- bind_rows(scss_snapshot1,scss_snapshot2) |>
    left_join(tibble(GeoCodes=c(rep("1",length(cmhc_type_codes1)),rep("2",length(cmhc_type_codes1))),
                     Breakdown=c(names(cmhc_type_codes1),names(cmhc_type_codes2)),
                     BreakdownCode=as.character(c(cmhc_type_codes1,cmhc_type_codes2))),
              by="GeoCodes") |>
    select(-.data$a,-.data$GeoCodes) |>
    mutate(TableCode=paste0(.data$SureveyCode,".",.data$DimensionCode,".",
                            .data$SeriesCode,".",.data$BreakdownCode))


  scss_timeseries <- scss_snapshot |>
    select(-.data$TableCode,-.data$Breakdown,-.data$BreakdownCode) |>
    unique() %>%
    mutate(DimensionCode=recode(.data$DimensionCode,"1"="2","4"="9")) |>
    mutate(TableCode=paste0(.data$SureveyCode,".",.data$DimensionCode,".",.data$SeriesCode)) |>
    mutate(Breakdown="Historical Time Periods")

  scss_snapshot3 <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Filters,
    "Scss","1","Absorbed Prices","1","Dwelling Type","9",list("dwelling_type_desc_en"=c("Single / Semi-detached","Single", "Semi-detached")),
    "Scss","1","Unabsorbed Prices","2","Dwelling Type","9",list("dwelling_type_desc_en"=c("Single / Semi-detached","Single", "Semi-detached")))


  rms_snapshot <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~GeoCodes,~Filters,
    "Rms","2","Vacancy Rate","1", "Bedroom Type","1","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Vacancy Rate","1", "Year of Construction","2","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment"),
                                                                      bedroom_count_type_desc_en=cmhc_bedroom_types),
    "Rms","2","Vacancy Rate","1", "Structure Size","3","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Vacancy Rate","1", "Rent Ranges","4","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment"),
                                                             bedroom_count_type_desc_en=cmhc_bedroom_types),
    "Rms","2","Vacancy Rate","1", "Rent Quartiles","33","4",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment"),
                                                                 bedroom_count_type_desc_en=cmhc_bedroom_types),
    "Rms","2","Availability Rate","1", "Bedroom Type","6","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Availability Rate","1", "Year of Construction","7","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment"),
                                                                           bedroom_count_type_desc_en=cmhc_bedroom_types),
    "Rms","2","Availability Rate","1", "Structure Size","8","3",list(bedroom_count_type_desc_en=cmhc_bedroom_types),
    "Rms","2","Average Rent","1", "Bedroom Type","11","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Average Rent","1", "Year of Construction","13","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment"),
                                                                       bedroom_count_type_desc_en=cmhc_bedroom_types),
    "Rms","2","Average Rent","1", "Structure Size","15","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Average Rent Change","1", "Bedroom Type","12","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Median Rent","1", "Bedroom Type","21","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Median Rent","1", "Year of Construction","22","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment"),
                                                                      bedroom_count_type_desc_en=cmhc_bedroom_types),
    "Rms","2","Median Rent","1", "Structure Size","23","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Rental Universe","1", "Bedroom Type","26","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Rental Universe","1", "Year of Construction","27","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment"),
                                                                          bedroom_count_type_desc_en=cmhc_bedroom_types),
    "Rms","2","Rental Universe","1", "Structure Size","28","3",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment")),
    "Rms","2","Summary Statistics","1", NA, "31","4",list(dwelling_type_desc_en=c("Row / Apartment","Row","Apartment"),
                                                          bedroom_count_type_desc_en=cmhc_bedroom_types)) |>
    left_join(tibble(GeoCodes=c(rep("3",length(cmhc_type_codes3)),rep("4",length(cmhc_type_codes4))),
                     Breakdown=c(names(cmhc_type_codes3),names(cmhc_type_codes4)),
                     BreakdownCode=as.character(c(cmhc_type_codes3,cmhc_type_codes4))),
              by="GeoCodes") |>
    select(-.data$GeoCodes) |>
    mutate(TableCode=paste0(.data$SureveyCode,".",.data$SeriesCode,".",
                            .data$DimensionCode,".",.data$BreakdownCode))

  rms_timeseries <- rms_snapshot |>
    select(-.data$TableCode,-.data$Breakdown,-.data$BreakdownCode) |>
    unique() %>%
    mutate(SeriesCode="2") |>
    mutate(TableCode=paste0(.data$SureveyCode,".",.data$SeriesCode,".",.data$DimensionCode)) |>
    mutate(Breakdown="Historical Time Periods") |>
    mutate(Filters=lapply(.data$Filters,function(f)append(f,list(season=c("October","April")))))

  seniors <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~Filters,
    "Seniors","3","Rental Housing Vacancy Rates","2","Unit Type",NA, "Snapshot","1",list(),
    "Seniors","3","Rental Housing Vacancy Rates","8","Unit Type",NA, "Historical Time Periods","1",list(),
    "Seniors","3","Spaces","3","Unit Type",NA, "Snapshot","1",list(),
    "Seniors","3","Spaces","3","Unit Type",NA, "Historical Time Periods","2",list(),
    "Seniors","3","Universe and Number of Residents","1","Spaces and Residents",NA, "Snapshot","2",list(),
    "Seniors","3","Universe and Number of Residents","8","Spaces and Residents",NA, "Historical Time Periods","6",list(),
    "Seniors","3","Heavy Care Spaces","4","Vacancy Rate and Average Rent",NA, "Snapshot","1",list(),
    "Seniors","3","Heavy Care Spaces","8","Vacancy Rate and Average Rent",NA, "Historical Time Periods","3",list(),
    "Seniors","3","Proportion of Standard Spaces","6","Rent Range",NA, "Snapshot","1",list(),
    "Seniors","3","Proportion of Standard Spaces","6","Rent Range",NA, "Historical Time Periods","2",list(),
    "Seniors","3","Bachelor Units and Private Rooms where Meals are included in Rent","7","Rent and Vacancy Rate and Units",NA, "Snapshot","1",list(),
    "Seniors","3","Bachelor Units and Private Rooms where Meals are included in Rent","7","Rent and Vacancy Rate and Units",NA, "Historical Time Periods","2",list(),
    "Seniors","3","Bachelor Units and Private Rooms where Meals are included in Rent","8","Rent and Vacancy Rate",NA, "Snapshot","7",list(),
    "Seniors","3","Bachelor Units and Private Rooms where Meals are included in Rent","8","Rent and Vacancy Rate",NA, "Historical Time Periods","4",list()
  ) |>
    mutate(TableCode=paste0(.data$SureveyCode,".",.data$SeriesCode,".",.data$BreakdownCode))

  srms <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~Filters,
    "Srms","4","Other Rental","6","Dwelling Type",NA, "Estimated Number of Households","1",list(),
    "Srms","4","Other Rental","6","Dwelling Type",NA, "Average Rent","2",list(),
    "Srms","4","Condominium Apartment","2","Structure Size",NA, "Vacancy Rate","1",list(),
    "Srms","4","Condominium Apartment","2","Structure Size",NA, "Average Rent","2",list(),
    "Srms","4","Condominium Apartment","2","Structure Size",NA, "Estimated Number of Units","3",list(),
    "Srms","4","Condominium Apartment","2","Structure Size",NA, "Estimated Number of Units Used for Rental","4",list(),
    "Srms","4","Condominium Apartment","2","Structure Size",NA, "Estimated Share of Rental Units","5",list(),
  ) |>
    mutate(TableCode=paste0(.data$SureveyCode,".",.data$SeriesCode,".",.data$BreakdownCode))

  canada_tables <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~Filters,
    "Scss","5","Starts","7","All areas",NA, "Historical Time Periods","1",list(),
    "Scss","5","Starts","7","All areas",NA, "Historical Time Periods","1",list(),
    #"Scss","1","Starts","1","Census Metropolitan Areas, Census Agglomerations, and other, selected municipalities with at least 10,000 people",NA, "Historical Time Periods","1",list(),
    )


  table_list <- bind_rows(
    scss_snapshot,scss_timeseries,rms_snapshot,rms_timeseries,seniors
  )

  if (short) {
    table_list <- table_list |>
      select(.data$Survey,.data$Series,.data$Dimension,.data$Breakdown,.data$Filters)
  }

  table_list
}

#' List available CMHC surveys
#'
#' @return A data frame with available survey names.
#'
#' @examples
#' list_cmhc_surveys()
#'
#' @export
list_cmhc_surveys <- function(){
  list_cmhc_tables() |>
    select(.data$Survey) |>
    unique()
}

#' List available CMHC series
#'
#' @param survey Optional survey to filter by
#' @return A data frame with survey names, and available series names.
#'
#' @examples
#' list_cmhc_series("Rms")
#'
#' @export
list_cmhc_series <- function(survey=NULL){
  l <- list_cmhc_tables() |>
    select(.data$Survey,.data$Series) |>
    unique()

  if (!is.null(survey)) {
    l <- l |>
      filter(.data$Survey==survey) |>
      unique()
  }

  l
}

#' List available CMHC dimensions
#'
#' @param survey Optional survey to filter by
#' @param series Optional series to filter by
#' @return A data frame with survey names, series names, and available dimension names.
#'
#' @examples
#' list_cmhc_dimensions("Rms","Vacancy Rate")
#'
#' @export
list_cmhc_dimensions <- function(survey=NULL,series=NULL){
  l <- list_cmhc_tables() |>
    select(.data$Survey,.data$Series,.data$Dimension) |>
    unique()

  if (!is.null(survey)) {
    l <- l |>
      filter(.data$Survey==survey) |>
      unique()
  }

  if (!is.null(series)) {
    l <- l |>
      filter(.data$Series==series) |>
      unique()
  }

  l

}


#' List available CMHC breakdowns
#'
#' @param survey Optional survey to filter by
#' @param series Optional series to filter by
#' @param dimension Optional dimension to filter by
#' @return A data frame with survey names, series names, dimension names and available series breakdowns.
#'
#' @examples
#' list_cmhc_breakdowns("Rms","Vacancy Rate","Bedroom Type")
#'
#' @export
list_cmhc_breakdowns <- function(survey=NULL,series=NULL,dimension=NULL){
  l <- list_cmhc_tables() |>
    select(.data$Survey,.data$Series,.data$Dimension,.data$Breakdown) |>
    unique()

  if (!is.null(survey)) {
    l <- l |>
      filter(.data$Survey==survey) |>
      unique()
  }

  if (!is.null(series)) {
    l <- l |>
      filter(.data$Series==series) |>
      unique()
  }
  if (!is.null(dimension)) {
    l <- l |>
      filter(.data$Dimension==dimension) |>
      unique()
  }

  l

}

#' List available CMHC filters
#'
#' @param survey Optional survey to filter by
#' @param series Optional series to filter by
#' @param dimension Optional dimension to filter by
#' @param breakdown Optional breakdown to filter by
#' @return A data frame with available filters
#'
#' @examples
#' list_cmhc_filters("Rms","Vacancy Rate","Bedroom Type","Historical Time Periods")
#'
#' @export
list_cmhc_filters <- function(survey=NULL,series=NULL,dimension=NULL, breakdown=NULL){
  l <- list_cmhc_tables() |>
    select(.data$Survey,.data$Series,.data$Dimension,.data$Breakdown,.data$Filters) |>
    unique()

  if (!is.null(survey)) {
    l <- l |>
      filter(.data$Survey==survey) |>
      unique()
  }

  if (!is.null(series)) {
    l <- l |>
      filter(.data$Series==series) |>
      unique()
  }
  if (!is.null(dimension)) {
    l <- l |>
      filter(.data$Dimension==dimension) |>
      unique()
  }
  if (!is.null(breakdown)) {
    l <- l |>
      filter(.data$Breakdown==breakdown) |>
      unique()
  }

  l

}


