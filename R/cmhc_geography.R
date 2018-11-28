
#' cmhc geography lookup for cma
#' @export
cmhc_geography_list=list(Vancouver="2410", Toronto="2270", Calgary="0140", Victoria="2440", Winnipeg="2680",
                         Montreal="1060", Ottawa="1265", Halifax="0580", Edmonton="0340")

#' cmhc geography type lookup
#' @export
cmhc_geography_type_list=list(PR=2,CMA=3, CSD=4, CT=7)

#' cmhc rms geography type lookup
#' @export
cmhc_rms_geography_list=list(CMA=3, CSD=4, NBHD=5, CT=6)

#' census geography lookup
#' @export
census_geography_list=list(Vancouver="59933", Toronto="35535", Calgary="48825", Victoria="59935",Montreal="24462")

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
                             "Langley (CY)"= "5915002",
                             "Langley (DM)"= "5915001",
                             Edmonton="4811061",
                             Winnipeg="4611040",
                             Montreal="2466023",
                             Ottawa="3506008")



#' cmhc geography params
#' @export
cmhc_region_params <- function(geography,type='CMA'){
  list <- list("geography_type_id"=as.character(cmhc_geography_type_list[type]))
  list["geography_id"] = ifelse(type=="CMA",
                                as.character(cmhc_geography_list[geography]),
                                as.character(cmhc_geography_csd_list[geography]))
  return(list)
}

#' census geography params
#' @export
cmhc_region_params_from_census <- function(GeoUID){
  list <- list("geography_type_id"=as.character(cmhc_geography_type_list[cmhc_geo_level_for_census(GeoUID)]),
               "geography_id"=census_to_cmhc_geocode(GeoUID))
  return(list)
}

#' Get cmhc geo level for census geo_uid
#' @export
cmhc_geo_level_for_census <- function(GeoUID){
  switch(as.character(nchar(GeoUID)),
         "5" = "CMA",
         "7" = "CSD",
         "2" = "PR",
         "3" = "CMA")
}

#' Get cmhc geo level for cmhc geo_uid
#' @export
cmhc_geo_level_for_cmhc <- function(GeoUID){
  switch(as.character(nchar(GeoUID)),
         "7" = ifelse(grepl("\\.",GeoUID),"CT","CSD"),
         "4" = "CMA")
}

#' translate census to CMHC geocodes
#' @param GeoUID Census GeoUID for CMA or CSD
#' @export
census_to_cmhc_geocode <- function(GeoUID){
  cmhc_name <- list(
    "CT"="CMHCCT_UID",
    "CSD"="CMHC_CSDUID",
    "CMA"="METCODE"
  )
  census_name <- list(
    "CT"="CTUID",
    "CSD"="CSDUID",
    "CMA"="CMAUID"
  )
  geo_level = cmhc_geo_level_for_census(GeoUID)

  if (is.null(geo_level)) stop("Could not recognize GeoUID.")

  result <- switch(
    geo_level,
    "CMA" = cmhc::cmhc_cma_translation_data %>% mutate(CMA_UID=ifelse(nchar(GeoUID)==3 & nchar(CMA_UID)==5,substr(CMA_UID,3,5),CMA_UID)) %>% filter(CMA_UID==GeoUID) %>% pull(METCODE),
    "CSD" = cmhc::cmhc_csd_translation_data %>% filter(CSDUID==GeoUID) %>% pull(CMHC_CSDUID),
    "PR" = GeoUID
  )

  result
}

#' translate CMHC to census geocodes
#' @param GeoUID CMHC GeoUID for CMA or CSD or CT
#' @export
cmhc_to_census_geocode <- function(GeoUID,CMA_GEOUID=NULL){
  cmhc_name <- list(
    "CT"="CMHCCT_UID",
    "CSD"="CMHC_CSDUID",
    "CMA"="METCODE"
  )
  census_name <- list(
    "CT"="CTUID",
    "CSD"="CSDUID",
    "CMA"="CMAUID"
  )
  geo_level = cmhc_geo_level_for_cmhc(GeoUID[1])

  if (is.null(geo_level)) stop("Could not recognize GeoUID.")

  if (!is.null(CMA_GEOUID) & nchar(CMA_GEOUID)==5) CMA_GEOUID=substr(CMA_GEOUID,3,5)

  result <- switch(geo_level,
    "CMA" = purrr::map(GeoUID,function(g)cmhc::cmhc_cma_translation_data %>% filter(METCODE==g) %>% pull(CMA_UID)) %>% unlist,
    "CSD" = purrr::map(GeoUID,function(g)cmhc::cmhc_csd_translation_data %>% filter(CMHC_CSDUID==g) %>% pull(CSDUID)) %>% unlist,
    "CT" = paste0(CMA_GEOUID,GeoUID)
  )

  result
}



#' census geography lookup
#' @export
census_to_cmhc_translation=list("59933"="2410", "35535"="2270", "48825"="0140", "59935"="2440")
