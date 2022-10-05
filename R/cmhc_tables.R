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
              by="a") |>
    mutate(h="16")


  # scss_snapshot2 <- tibble::tribble(
  #   ~Survey,~SureveyCode,~Series,~SeriesCode,~GeoCodes,
  #   "Scss","1","Share absorbed at completion","6","2",
  #   "Scss","1","Unabsorbed Inventory","4","2") |>
  #   mutate(a="1") |>
  #   left_join(tibble(a="1",
  #                    Dimension=c("Dwelling Type","Intended Market"),
  #                    DimensionCode=c("1","4"),
  #                    Filters=list("dimension-18"=c("Condo","Homeowner","All"),
  #                                 "dimension-1"=cmhc_dwelling_types)),
  #             by="a")
  scss_snapshot2 <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~GeoCodes,~Dimension,~DimensionCode,~Filters,
    "Scss","1","Share absorbed at completion","6","2",c("Dwelling Type"),"1",list("dimension-1"=cmhc_dwelling_types,"dimension-18"=c("Condo","Homeowner","All")),
    "Scss","1","Unabsorbed Inventory","4","2",c("Dwelling Type"),"1",list("dimension-1"=cmhc_dwelling_types,"dimension-18"=c("Condo","Homeowner","All"))) |>
    mutate(h="9")

  scss_snapshot <- bind_rows(scss_snapshot1,scss_snapshot2) |>
    left_join(tibble(GeoCodes=c(rep("1",length(cmhc_type_codes1)),rep("2",length(cmhc_type_codes2))),
                     Breakdown=c(names(cmhc_type_codes1),names(cmhc_type_codes2)),
                     BreakdownCode=as.character(c(cmhc_type_codes1,cmhc_type_codes2))),
              by="GeoCodes") |>
    select(-.data$a,-.data$GeoCodes) |>
    mutate(TableCode=paste0(.data$SureveyCode,".",.data$DimensionCode,".",
                            .data$SeriesCode,".",.data$BreakdownCode))


  scss_timeseries <- scss_snapshot |>
    select(-.data$TableCode,-.data$Breakdown,-.data$BreakdownCode) |>
    unique() %>%
    mutate(DimensionCode=.data$h) |>
    mutate(TableCode=paste0(.data$SureveyCode,".",.data$DimensionCode,".",.data$SeriesCode)) |>
    mutate(Breakdown="Historical Time Periods") |>
    select(-.data$h)

  scss_snapshot <- scss_snapshot |> select(-.data$h)

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

  scss_filter <- list("dimension-18"=cmhc_intended_markets,"dimension-1"=cmhc_dwelling_types)
  canada_tables <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~GeoFilter,~Filters,~TableCode,
    "Scss","5","Starts","7","Dwelling Type",NA, "Historical Time Periods","2","All",scss_filter,"5.7.2",
    "Scss","5","Starts","6","Dwelling Type",NA, "Historical Time Periods","2","10k",scss_filter,"5.6.2",
    "Scss","1","Starts","1","Intended Market","6", "Historical Time Periods","16","10k",scss_filter,"1.16.1.6",
    "Scss","1","Starts","1","Dwelling Type","5", "Historical Time Periods","2","50k",scss_filter,"1.2.1.5",
    "Scss","1","Starts","1","Intended Market","5", "Historical Time Periods","16","50k",scss_filter,"1.16.1.5",
    "Scss","1","Starts","1","Dwelling Type","4", "Historical Time Periods","2","Metro",scss_filter,"1.2.1.4",
    "Scss","1","Starts","1","Intended Market","4", "Historical Time Periods","16","Metro",scss_filter,"1.16.1.4",
    "Scss","1","Starts","1","Dwelling Type","5", "Centres","1","Metro",scss_filter,"1.1.1.5",
    "Scss","1","Starts","1","Dwelling Type","1", "Centres","6","50k",scss_filter,"1.1.1.6",
    "Scss","1","Starts","1","Intended Market","5", "Centres","6","Metro",scss_filter,"1.4.1.5",
    "Scss","1","Starts","1","Intended Market","6", "Centres","6","50k",scss_filter,"1.4.1.6",
    "Scss","1","Starts","1","Intended Market","7", "Centres","4","10k",scss_filter,"1.4.1.7",

    "Scss","5","Completions","11","Dwelling Type",NA, "Historical Time Periods","2","All",scss_filter,"5.11.2",
    "Scss","5","Completions","10","Dwelling Type",NA, "Historical Time Periods","2","10k",scss_filter,"5.10.2",
    "Scss","1","Completions","2","Intended Market","6", "Historical Time Periods","16","10k",scss_filter,"1.16.2.6",
    "Scss","1","Completions","2","Dwelling Type","5", "Historical Time Periods","2","50k",scss_filter,"1.2.2.5",
    "Scss","1","Completions","2","Intended Market","5", "Historical Time Periods","16","50k",scss_filter,"1.16.2.5",
    "Scss","1","Completions","2","Dwelling Type","4", "Historical Time Periods","2","Metro",scss_filter,"1.2.2.4",
    "Scss","1","Completions","2","Intended Market","4", "Historical Time Periods","16","Metro",scss_filter,"1.16.2.4",
    "Scss","1","Completions","2","Dwelling Type","5", "Centres","1","Metro",scss_filter,"1.1.2.5",
    "Scss","1","Completions","2","Dwelling Type","1", "Centres","6","50k",scss_filter,"1.1.2.6",
    "Scss","1","Completions","2","Intended Market","5", "Centres","6","Metro",scss_filter,"1.4.2.5",
    "Scss","1","Completions","2","Intended Market","6", "Centres","6","50k",scss_filter,"1.4.2.6",
    "Scss","1","Completions","2","Intended Market","7", "Centres","4","10k",scss_filter,"1.4.2.7",

    "Scss","5","Under Construction","13","Dwelling Type",NA, "Historical Time Periods","2","All",scss_filter,"5.13.2",
    "Scss","1","Under Construction","3","Dwelling Type","6", "Historical Time Periods","2","10k",scss_filter,"1.2.3.6",
    "Scss","1","Under Construction","3","Intended Market","6", "Historical Time Periods","9","10k",scss_filter,"1.9.3.6",
    "Scss","1","Under Construction","3","Dwelling Type","5", "Historical Time Periods","2","50k",scss_filter,"1.2.3.5",
    "Scss","1","Under Construction","3","Intended Market","5", "Historical Time Periods","16","50k",scss_filter,"1.16.3.5",
    "Scss","1","Under Construction","3","Dwelling Type","4", "Historical Time Periods","2","Metro",scss_filter,"1.2.3.4",
    "Scss","1","Under Construction","3","Intended Market","4", "Historical Time Periods","16","Metro",scss_filter,"1.16.3.4",
    "Scss","1","Under Construction","3","Dwelling Type","5", "Centres","1","Metro",scss_filter,"1.1.3.5",
    "Scss","1","Under Construction","3","Dwelling Type","1", "Centres","6","50k",scss_filter,"1.1.3.6",
    "Scss","1","Under Construction","3","Intended Market","5", "Centres","6","Metro",scss_filter,"1.4.3.5",
    "Scss","1","Under Construction","3","Intended Market","6", "Centres","6","50k",scss_filter,"1.4.3.6",
    "Scss","1","Under Construction","3","Intended Market","7", "Centres","4","10k",scss_filter,"1.4.3.7"
    #"Scss","5","Starts","7","All areas",NA, "Historical Time Periods","1",list(),
    #"Scss","1","Starts","1","Census Metropolitan Areas, Census Agglomerations, and other, selected municipalities with at least 10,000 people",NA, "Historical Time Periods","1",list(),
    )

  tenure_filter <-  list("Tenure"=c("Total","Renters","Owners"))

  income_tables <-  tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~GeoFilter,~Filters,~TableCode,
    "Census",NA,"Income","63","Average and Median","7", "Historical Time Periods","2","Default",tenure_filter,"7.63",
    "Census",NA,"Income","62","Average and Median","7", "Survey Zones","3","Default",tenure_filter,"7.62.3",
    "Census",NA,"Income","62","Average and Median","7", "Neighbourhoods","4","Default",tenure_filter,"7.62.4",
    "Census",NA,"Income","62","Average and Median","7", "Census Tracts","5","Default",tenure_filter,"7.62.5",
    "Census",NA,"Income","59","Ranges","6", "Historical Time Periods","2","Default",tenure_filter,"6.60",
    "Census",NA,"Income","59","Ranges","6", "Survey Zones","3","Default",tenure_filter,"6.59.3",
    "Census",NA,"Income","59","Ranges","6", "Neighbourhoods","4","Default",tenure_filter,"6.59.4",
    "Census",NA,"Income","59","Ranges","6", "Census Tracts","5","Default",tenure_filter,"6.59.5")

  dwelling_value_tables <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~GeoFilter,~Filters,~TableCode,
    "Census",NA,"Dwelling value","6","Average","3", "Historical Time Periods","2","Default",list(),"6.4",
    "Census",NA,"Dwelling value","6","Average","3", "Survey Zones","3","Default",list(),"6.3.3",
    "Census",NA,"Dwelling value","6","Average","3", "Neighbourhoods","4","Default",list(),"6.3.4",
    "Census",NA,"Dwelling value","6","Average","3", "Census Tracts","5","Default",list(),"6.3.5",
    "Census",NA,"Dwelling value","6","Median","5", "Historical Time Periods","2","Default",list(),"6.6",
    "Census",NA,"Dwelling value","6","Median","5", "Survey Zones","3","Default",list(),"6.5.3",
    "Census",NA,"Dwelling value","6","Median","5", "Neighbourhoods","4","Default",list(),"6.5.4",
    "Census",NA,"Dwelling value","6","Median","5", "Census Tracts","5","Default",list(),"6.5.5"
  )

  age_tables <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~GeoFilter,~Filters,~TableCode,
    "Census",NA,"All Households","6","Age of Population","12", "Historical Time Periods","2","Default",list(),"6.8",
    "Census",NA,"All Households","6","Age of Population","11", "Survey Zones","3","Default",list(),"6.7.3",
    "Census",NA,"All Households","6","Age of Population","11", "Neighbourhoods","4","Default",list(),"6.7.4",
    "Census",NA,"All Households","6","Age of Population","11", "Census Tracts","5","Default",list(),"6.7.5",
    "Census",NA,"65 and over","7","Age of Population","3", "Historical Time Periods","2","Default",list(),"6.98",
    "Census",NA,"65 and over","7","Age of Population","2", "Survey Zones","3","Default",list(),"6.97.3",
    "Census",NA,"65 and over","7","Age of Population","2", "Neighbourhoods","4","Default",list(),"6.97.4",
    "Census",NA,"65 and over","7","Age of Population","2", "Census Tracts","5","Default",list(),"6.97.5"
  )
  hm_tables <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~GeoFilter,~Filters,~TableCode,
    "Census",NA,"All Households","6","Age of Primary Household Maintainer","12", "Historical Time Periods","2","Default",tenure_filter,"6.12",
    "Census",NA,"All Households","6","Age of Primary Household Maintainer","11", "Survey Zones","3","Default",tenure_filter,"6.11.3",
    "Census",NA,"All Households","6","Age of Primary Household Maintainer","11", "Neighbourhoods","4","Default",tenure_filter,"6.11.4",
    "Census",NA,"All Households","6","Age of Primary Household Maintainer","11", "Census Tracts","5","Default",tenure_filter,"6.11.5",
    "Census",NA,"65 and over","7","Age of Primary Household Maintainer","3", "Historical Time Periods","2","Default",tenure_filter,"7.3",
    "Census",NA,"65 and over","7","Age of Primary Household Maintainer","2", "Survey Zones","3","Default",tenure_filter,"7.2.3",
    "Census",NA,"65 and over","7","Age of Primary Household Maintainer","2", "Neighbourhoods","4","Default",tenure_filter,"7.2.4",
    "Census",NA,"65 and over","7","Age of Primary Household Maintainer","2", "Census Tracts","5","Default",tenure_filter,"7.2.5"
  )
  hmm_tables <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~GeoFilter,~Filters,~TableCode,
    "Census",NA,"Mobility 5 of All Households","6","Age of Primary Household Maintainer","16", "Historical Time Periods","2","Default",tenure_filter,"6.16",
    "Census",NA,"Mobility 5 of All Households","6","Age of Primary Household Maintainer","15", "Survey Zones","3","Default",tenure_filter,"6.15.3",
    "Census",NA,"Mobility 5 of All Households","6","Age of Primary Household Maintainer","15", "Neighbourhoods","4","Default",tenure_filter,"6.15.4",
    "Census",NA,"Mobility 5 of All Households","6","Age of Primary Household Maintainer","15", "Census Tracts","5","Default",tenure_filter,"6.15.5",
    "Census",NA,"Mobility 5 of 65 and over","7","Age of Primary Household Maintainer","7", "Historical Time Periods","2","Default",tenure_filter,"7.7",
    "Census",NA,"Mobility 5 of 65 and over","7","Age of Primary Household Maintainer","6", "Survey Zones","3","Default",tenure_filter,"7.6.3",
    "Census",NA,"Mobility 5 of 65 and over","7","Age of Primary Household Maintainer","6", "Neighbourhoods","4","Default",tenure_filter,"7.6.4",
    "Census",NA,"Mobility 5 of 65 and over","7","Age of Primary Household Maintainer","6", "Census Tracts","5","Default",tenure_filter,"7.6.5",
    "Census",NA,"Mobility 1 of All Households","6","Age of Primary Household Maintainer","20", "Historical Time Periods","2","Default",tenure_filter,"6.20",
    "Census",NA,"Mobility 1 of All Households","6","Age of Primary Household Maintainer","19", "Survey Zones","3","Default",tenure_filter,"6.19.3",
    "Census",NA,"Mobility 1 of All Households","6","Age of Primary Household Maintainer","19", "Neighbourhoods","4","Default",tenure_filter,"6.19.4",
    "Census",NA,"Mobility 1 of All Households","6","Age of Primary Household Maintainer","19", "Census Tracts","5","Default",tenure_filter,"6.19.5",
    "Census",NA,"Mobility 1 of 65 and over","7","Age of Primary Household Maintainer","10", "Historical Time Periods","2","Default",tenure_filter,"7.11",
    "Census",NA,"Mobility 1 of 65 and over","7","Age of Primary Household Maintainer","10", "Survey Zones","3","Default",tenure_filter,"7.10.3",
    "Census",NA,"Mobility 1 of 65 and over","7","Age of Primary Household Maintainer","10", "Neighbourhoods","4","Default",tenure_filter,"7.10.4",
    "Census",NA,"Mobility 1 of 65 and over","7","Age of Primary Household Maintainer","10", "Census Tracts","5","Default",tenure_filter,"7.10.5"
  )

  core_housing_tables <- tibble::tribble(
    ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~GeoFilter,~Filters,~TableCode,
    "Core Housing Need",NA,"Housing Standards","7","% of Households in Core Housing Need","17", "Historical Time Periods",NA,"Default",tenure_filter,"7.17",
    "Core Housing Need",NA,"Housing Standards","7","Households in Core Housing Need","17", "Historical Time Periods",NA,"Default",tenure_filter,"7.15",
    "Core Housing Need",NA,"Housing Standards","6","Households Tested For Core Housing Need","69", "Historical Time Periods","3","Default",tenure_filter,"6.96",
  )

  # hmmp_tables <- tibble::tribble(
  #   ~Survey,~SureveyCode,~Series,~SeriesCode,~Dimension,~DimensionCode,~Breakdown,~BreakdownCode,~GeoFilter,~Filters,~TableCode,
  #   "Census",NA,"All Households","6","Mobility 5 of Primary Household Maintainer %","18", "Historical Time Periods","2","Default",tenure_filter,"6.18",
  #   "Census",NA,"All Households","6","Mobility 5 of Primary Household Maintainer %","17", "Survey Zones","3","Default",tenure_filter,"6.17.3",
  #   "Census",NA,"All Households","6","Mobility 5 of Primary Household Maintainer %","17", "Neighbourhoods","4","Default",tenure_filter,"6.17.4",
  #   "Census",NA,"All Households","6","Mobility 5 of Primary Household Maintainer %","17", "Census Tracts","5","Default",tenure_filter,"6.17.5",
  #   "Census",NA,"65 and over","7","Mobility 5 of Primary Household Maintainer %","9", "Historical Time Periods","2","Default",tenure_filter,"7.9",
  #   "Census",NA,"65 and over","7","Mobility 5 of Primary Household Maintainer %","8", "Survey Zones","3","Default",tenure_filter,"7.8.3",
  #   "Census",NA,"65 and over","7","Mobility 5 of Primary Household Maintainer %","8", "Neighbourhoods","4","Default",tenure_filter,"7.8.4",
  #   "Census",NA,"65 and over","7","Mobility 5 of Primary Household Maintainer %","8", "Census Tracts","5","Default",tenure_filter,"7.8.5",
  #   "Census",NA,"All Households","6","Mobility 1 of Primary Household Maintainer %","20", "Historical Time Periods","2","Default",tenure_filter,"6.22",
  #   "Census",NA,"All Households","6","Mobility 1 of Primary Household Maintainer %","21", "Survey Zones","3","Default",tenure_filter,"6.21.3",
  #   "Census",NA,"All Households","6","Mobility 1 of Primary Household Maintainer %","21", "Neighbourhoods","4","Default",tenure_filter,"6.21.4",
  #   "Census",NA,"All Households","6","Mobility 1 of Primary Household Maintainer %","21", "Census Tracts","5","Default",tenure_filter,"6.21.5",
  #   "Census",NA,"65 and over","7","Mobility 1 of Primary Household Maintainer %","13", "Historical Time Periods","2","Default",tenure_filter,"7.13",
  #   "Census",NA,"65 and over","7","Mobility 1 of Primary Household Maintainer %","12", "Survey Zones","3","Default",tenure_filter,"7.12.3",
  #   "Census",NA,"65 and over","7","Mobility 1 of Primary Household Maintainer %","12", "Neighbourhoods","4","Default",tenure_filter,"7.12.4",
  #   "Census",NA,"65 and over","7","Mobility 1 of Primary Household Maintainer %","12", "Census Tracts","5","Default",tenure_filter,"7.12.5"
  # )



  table_list <- bind_rows(
    scss_snapshot,scss_timeseries,rms_snapshot,rms_timeseries,seniors
  ) |>
    mutate(GeoFilter="Default") |>
    bind_rows(canada_tables) |>
    bind_rows(income_tables) |>
    bind_rows(dwelling_value_tables) |>
    bind_rows(age_tables) |>
    bind_rows(hm_tables,hmm_tables) |>
    bind_rows(core_housing_tables)

  if (short) {
    table_list <- table_list |>
      select(.data$Survey,.data$Series,.data$Dimension,.data$Breakdown,.data$GeoFilter,.data$Filters)
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

#` @internal`
get_input_for <- function(tables,column,allow_empty=FALSE) {
  selection <- tables |>
    select(!!as.name(column)) |>
    unique() |>
    mutate(n=row_number()) |>
    mutate(selection=paste0(!!as.name(column),": ",n))
  if (nrow(selection)==1) {
    filtered_selection=selection
  } else {
    user_prompt <- paste0("Select ",column,": \n",paste0(selection$selection,collapse="\n"),"\n")
    if (allow_empty) user_prompt <- paste0(user_prompt,"Press enter to skip\n")
    user_imput <- readline(prompt=user_prompt)
    filtered_selection <- selection|>filter(n==user_imput|!!as.name(column)==user_imput)
    if (nrow(filtered_selection)!=1 & !(nrow(filtered_selection)==0& allow_empty) ) stop("Invalid selection")
  }
  if (nrow(filtered_selection)==1) cat(paste0(column," ",pull(filtered_selection,column)," selected\n"))
  filtered_selection
}



#' Interactive table selector
#'
#' @return A string containing the function call to access the selected table
#'
#' @examples
#' \dontrun{
#' select_cmhc_table()
#' }
#' @export
select_cmhc_table <- function(){
  tables <- list_cmhc_tables()
  selection <- get_input_for(tables,"Survey")
  tables <- tables |>
    filter(.data$Survey==selection$Survey)

  selection <- get_input_for(tables,"Series")
  tables <- tables |>
    filter(.data$Series==selection$Series)

  selection <- get_input_for(tables,"Dimension")
  tables <- tables |>
    filter(.data$Dimension==selection$Dimension)

  selection <- get_input_for(tables,"Breakdown")
  tables <- tables |>
    filter(.data$Breakdown==selection$Breakdown)

  use_geofilters <- FALSE
  if (nrow(tables)>1) {
    use_geofilters=TRUE
    selection <- get_input_for(tables,"GeoFilter")
    tables <- tables |>
      filter(.data$GeoFilter==selection$GeoFilter)
  }

  if (nrow(tables)!=1) {
    stop("Selecting table unsuccessful")
  }

  vars <- c("Survey","Series","Dimension","Breakdown")
  if (use_geofilters) vars <- c(vars,"GeoFilter")

  arguments <- lapply(vars,function(v){
    paste0(tolower(v),' = "',pull(tables,v),'"')
  }) |>
    unlist() |>
    paste0(collapse = ", ")

  function_call <- paste0("get_cmhc(",arguments,", ","geo_uid = <Census UID>",")")

  cat("To access the CMHC data for the selected table use\n",
      function_call,"\n")


  result <- function_call
}


