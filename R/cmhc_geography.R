#' Translate census geography to CMHC region parameters suitable for API call
#' @param GeoUID census region id
#' @keywords internal
#' @noRd
#' @return a list with geographies suitable for API calls
cmhc_region_params_from_census <- function(GeoUID){
  cmhc_geography_type_list=list(C=1,PR=2,CMA=3, CSD=4, CT=7, ZONE=5,NEIGHBOUHOOD=6)
  list <- list("geography_type_id"=as.character(cmhc_geography_type_list[cmhc_geo_level_for_census(GeoUID)]),
               "geography_id"=census_to_cmhc_geocode(GeoUID))
  metid <- cmhc_met_id_for_census(GeoUID)
  if (length(metid)>0&&!is.na(metid)) list$MetId <- metid
  return(list)
}

#' Get cmhc geographic level for census geographic identifier
#' @param GeoUID census region id
#' @keywords internal
#' @noRd
#' @return a character string with the CMHC geographic identifier
cmhc_geo_level_for_census <- function(GeoUID){
  if (GeoUID=="01") GeoUID="1"
  switch(as.character(nchar(GeoUID)),
         "5" = "CMA",
         "7" = "CSD",
         "10"= "CT",
         "2" = "PR",
         "3" = "CMA",
         "1" = "C")
}

#' Get cmhc geographic level for cmhc geographic identifier
#' @param GeoUID census geographic identifier
#' @keywords internal
#' @noRd
#' @return a character string with the CMHC geographic level
cmhc_geo_level_for_cmhc <- function(GeoUID){
  switch(as.character(nchar(GeoUID)),
         "7" = ifelse(grepl("\\.",GeoUID),"CT","CSD"),
         "4" = "CMA")
}

#' translate census to CMHC geographic identifier
#' @param GeoUID Census geographic identifier for CMA or CSD
#' @keywords internal
#' @noRd
#' @return a character string with the CMHC geographic identifier
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
    "CMA" = cmhc::cmhc_cma_translation_data %>% mutate(CMA_UID=ifelse(nchar(GeoUID)==3 & nchar(.data$CMA_UID)==5,substr(.data$CMA_UID,3,5),.data$CMA_UID)) %>% filter(.data$CMA_UID==GeoUID) %>% pull(.data$METCODE),
    "CT" = cmhc::cmhc_ct_translation_data %>% filter(.data$CTUID==GeoUID) %>% mutate(id=paste0(.data$METCODE,.data$NBHDCODE,.data$CMHC_CT)) %>% pull(.data$id),
    "CSD" = GeoUID, #cmhc::cmhc_csd_translation_data %>% filter(.data$CSDUID==GeoUID) %>% pull(.data$CMHC_CSDUID),
    "PR" = ifelse(GeoUID=="01","1",GeoUID),
    "C" = "1"
  )

  result
}

#' translate census to CMHC met id, if applicable
#' @param GeoUID Census geographic identifier for CMA or CSD
#' @keywords internal
#' @noRd
#' @return a character string with the CMHC MetId
cmhc_met_id_for_census <- function(GeoUID) {
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

  if (geo_level=="CSD") {
    met_lookup <- cmhc::cmhc_csd_translation_data_2023

    cmhc_cma_id <- met_lookup |>
      filter(.data$GeoUID==!!GeoUID) |>
      pull(.data$METCODE)
  }

  result <- switch(
    geo_level,
    "CMA" = cmhc::cmhc_cma_translation_data %>% mutate(CMA_UID=ifelse(nchar(GeoUID)==3 & nchar(.data$CMA_UID)==5,substr(.data$CMA_UID,3,5),.data$CMA_UID)) %>% filter(.data$CMA_UID==GeoUID) %>% pull(.data$METCODE),
    "CT" = cmhc::cmhc_ct_translation_data %>% filter(.data$CTUID==GeoUID) %>% pull(.data$METCODE) %>% unique(),
    "CSD" = cmhc_cma_id,
    "PR" = GeoUID,
    "C" = 1
  )

  result
}

#' translate CMHC to census geographic identifier
#' @param GeoUID CMHC geographic identifier for CMA or CSD or CT
#' @param parent_region census geographic identifier to identify CMA if needed
#' @keywords internal
#' @noRd
#' @return a character string with the census geographic identifier
cmhc_to_census_geocode <- function(GeoUID,parent_region=NULL){
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


  result <- switch(geo_level,
    "CMA" = lapply(GeoUID,function(g) cmhc::cmhc_cma_translation_data %>% filter(.data$METCODE==g) %>% pull(.data$CMA_UID)) %>% unlist,
    "CSD" = GeoUID, #lapply(GeoUID,function(g) cmhc::cmhc_csd_translation_data %>% filter(.data$CMHC_CSDUID==g) %>% pull(.data$CSDUID)) %>% unlist,
    "CT" = {
      if (!is.null(parent_region)) {
        if (length(names(parent_region))>1) {
          parent_region <- parent_region[!names(parent_region) %in% c("Neighbourhood","Zone","Survey Zone","Hood")]
        }
        parent_geo_level <- cmhc_geo_level_for_census(parent_region)
        if (parent_geo_level=="CMA") {
          CMA_GEOUID <- parent_region
        } else if (parent_geo_level=="CSD"){
          link <- cmhc::cmhc_ct_translation_data %>%
            filter(.data$CSDUID==parent_region) %>%
            select(.data$CTUID,.data$CMHC_CT) %>%
            mutate(CMA_UID=substr(.data$CTUID,1,3))
          CMA_GEOUID <- unique(link$CMA_UID)
          if (length(CMA_GEOUID)!=1) {
            warning("Unable to tranfrom CT UIDs.")
            CMA_GEOUID=""
          }
        }
        if (!is.null(CMA_GEOUID) & nchar(CMA_GEOUID)==5) CMA_GEOUID=substr(CMA_GEOUID,3,5)
      } else {
        CMA_GEOUID <- ""
        warning("Need parent geo level when converting CMHC census tract names to GeoUID")
      }

      paste0(CMA_GEOUID,GeoUID)
    }
  )

  result
}




#' download local copy of cmhc geography data
#' @param base_directory local directory to hold cmhc geography data
#' @noRd
#' @keywords internal
#' @return NULL
download_geographies <- function(base_directory=Sys.getenv("CMHC_CACHE_PATH")){
  if (is.null(base_directory)||base_directory=="") stop(paste0("Not a valid base directory ",base_directory))
  aws_bucket="cmhc"
  #aws_path=NULL
  if (!file.exists(base_directory)) dir.create(base_directory)
  message("Downloading geographies, this may take a minute...")
  for (d in paste0("RMS2017_",seq(1,3),".gdb")) {
    if (dir.exists(file.path(base_directory,d))) unlink(file.path(base_directory,d),recursive=TRUE)
    dir.create(file.path(base_directory,d))
    for (f in aws.s3::get_bucket(bucket=aws_bucket,
                                 prefix = d,#file.path(aws_path,d),
                                 region = 'us-west-2')) {
      key=f$Key
      #local_path=gsub(paste0("cmhc/",d,"/"),"",key)
      #local_file <- file.path(base_directory,d,local_path)
      local_file <- file.path(base_directory,key)
      if (!file.exists(local_file))
        aws.s3::save_object(object=key,
                            bucket=aws_bucket,
                            region='us-west-2',
                            file=local_file)
    }
  }
  NULL
}


#' Get CMHC geographies for CMHC Survey Zones and Neighbourhoods
#'
#' @description The data can be queried for Census Tracts, Survey Zones, Neighbourhoods, Census Subdivisions and Metropolitan Areas,
#' but it's most useful for Survey Zones, Neighbourhoods which are particular to CMHC and not available from other sources.
#' The geographic data corresponds to an extract from 2017, and won't necessarily match regions from other years. The Survey Zones
#' and Neighbourhoods have been quite stable, but census geographies change over time and can be matched with geographic
#' data obtained by using the `cancensus` package.
#'
#' The geographic data is quite large and a local cache directory needs to be provided. By default the
#' "CMHC_CACHE_PATH" environment variable is used to determine the cache directory, it can be set via the
#' `set_cache_path` function. The geographic data will take up about 55Mb of disk space.
#'
#' @param level aggregation level for geographic data, one of "CT","ZONE","NBHD","CSD","MET"
#' @param base_directory local directory to hold CMHC geography data, by default this is inferred from the
#' CMHC_CACHE_PATH environment variable. To use this function a local data directory has to be set.
#' @return A spatial data frame with the geographies for the specified geographic level.
#'
#' @examples
#' \dontrun{
#' get_cmhc_geography("ZONE")
#' }
#'
#' @export
get_cmhc_geography <- function(level=c("CT","ZONE","NBHD","CSD","MET"),base_directory=Sys.getenv("CMHC_CACHE_PATH")){
  if (is.null(base_directory)||base_directory==""||!dir.exists(base_directory)) stop(paste0("Not a valid base directory: ",base_directory,"."))
  paths <- dir(base_directory)
  if (!dir.exists(base_directory)) dir.create(base_directory)
  if (!dir.exists(base_directory)) stop ("Could not create base directory.")
  paths <- dir(base_directory)
  if (length(setdiff(c("RMS2017_1.gdb", "RMS2017_2.gdb", "RMS2017_3.gdb"),paths))>0) {
    download_geographies(base_directory = base_directory)
  }
  paths <- dir(base_directory)
  if (length(setdiff(c("RMS2017_1.gdb", "RMS2017_2.gdb", "RMS2017_3.gdb"),paths))>0) {
    stop("Problem finding local geographies, please file an issue report on GitHub.")
  }
  if(!(length(level)==1 & (level %in% c("CT","ZONE","NBHD","CSD","MET")))) stop("level needs to be one of CT, ZONE, NBHD, CSD, MET")
  if (!dir.exists(base_directory)) stop(paste0("Local directory ",base_directory," does not exists.\nMake sure geographit data was downloaded using the `download_geographies()` function."))
  result=NULL
  if (level=="CT") {
    path=file.path(base_directory,"RMS2017_2.gdb")
    if (!dir.exists(path)) stop(paste0("Local directory ",path," does not exists.\nDownload files first using the `download_geographies()` function."))
    result=sf::read_sf(path)
  } else if (level == "CSD") {
    path=file.path(base_directory,"RMS2017_1.gdb")
    if (!dir.exists(path)) stop(paste0("Local directory ",path," does not exists.\nDownload files first using the `download_geographies()` function."))
    result=sf::read_sf(path)
  } else {
    path=file.path(base_directory,"RMS2017_3.gdb")
    if (!dir.exists(path)) stop(paste0("Local directory ",path," does not exists.\nDownload files first using the `download_geographies()` function."))
    result=sf::read_sf(path,layer=paste0(level,"_2017"))
  }
  result
}

