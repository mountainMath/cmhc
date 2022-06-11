cmhc_geography_type_list=list(PR=2,CMA=3, CSD=4, CT=7, ZONE=5,NEIGHBOUHOOD=6)

#' census geography params
#' @param GeoUID census region id
#' @export
cmhc_region_params_from_census <- function(GeoUID){
  list <- list("geography_type_id"=as.character(cmhc_geography_type_list[cmhc_geo_level_for_census(GeoUID)]),
               "geography_id"=census_to_cmhc_geocode(GeoUID))
  return(list)
}

#' Get cmhc geo level for census geo_uid
#' @param GeoUID census region id
#' @export
cmhc_geo_level_for_census <- function(GeoUID){
  switch(as.character(nchar(GeoUID)),
         "5" = "CMA",
         "7" = "CSD",
         "2" = "PR",
         "3" = "CMA")
}

#' Get cmhc geo level for cmhc geo_uid
#' @param GeoUID census geographic idenitifier
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
    "CMA" = cmhc::cmhc_cma_translation_data %>% mutate(CMA_UID=ifelse(nchar(GeoUID)==3 & nchar(.data$CMA_UID)==5,substr(.data$CMA_UID,3,5),.data$CMA_UID)) %>% filter(.data$CMA_UID==GeoUID) %>% pull(.data$METCODE),
    "CSD" = cmhc::cmhc_csd_translation_data %>% filter(.data$CSDUID==GeoUID) %>% pull(.data$CMHC_CSDUID),
    "PR" = GeoUID
  )

  result
}

#' translate CMHC to census geocodes
#' @param GeoUID CMHC GeoUID for CMA or CSD or CT
#' @param parent_region census geographic identifier to identify CMA if needed
#' @export
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
    "CSD" = lapply(GeoUID,function(g) cmhc::cmhc_csd_translation_data %>% filter(.data$CMHC_CSDUID==g) %>% pull(.data$CSDUID)) %>% unlist,
    "CT" = {
      if (!is.null(parent_region)) {
        parent_geo_level <- cmhc_geo_level_for_cmhc(parent_region)
        if (parent_geo_level=="CMA") {
          CMA_GEOUID <- parent_geo_level
        } else if (parent_geo_level=="CSD"){
          link <- cmhc::cmhc_ct_translation_data |>
            filter(.data$CSDUID==parent_region) |>
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
#' @export
download_geographies <- function(base_directory=getOption("cmhc.cache_path")){
  if (is.null(base_directory)) stop(paste0("Not a valid base directory ",base_directory))
  aws_bucket="mountaimath"
  aws_path="cmhc"
  dir.create(file.path(base_directory))
  message("Downloading geographies, this may take a minute...")
  for (d in paste0("RMS2017_",seq(1,3),".gdb")) {
    dir.create(file.path(base_directory,d))
    for (f in aws.s3::get_bucket(bucket="mountainmath",prefix = file.path(aws_path,d))) {
      key=f$Key
      local_path=gsub(paste0("cmhc/",d,"/"),"",key)
      local_file <- file.path(base_directory,d,local_path)
      if (!file.exists(local_file))
        aws.s3::save_object(object=key,bucket="mountainmath",file=local_file)
    }
  }
}


#' get cmhc geographies, requires data has been downloaded with `download_geographies()` first
#' @param level aggreagtion level for geographic data, one of "CT","ZONE","NBHD","CSD","MET"
#' @param base_directory local directory to hold cmhc geography data
#' @export
get_cmhc_geography <- function(level=c("CT","ZONE","NBHD","CSD","MET"),base_directory=getOption("cmhc.cache_path")){
  if (is.null(base_directory)) stop(paste0("Not a valid base directory ",base_directory))
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

