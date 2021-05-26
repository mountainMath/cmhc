
#' cmhc geography lookup for cma
#' @export
cmhc_geography_list=list(Vancouver="2410", Toronto="2270", Calgary="0140", Victoria="2440", Winnipeg="2680",
                         Montreal="1060", Ottawa="1265", Halifax="0580", Edmonton="0340")

#' cmhc geography type lookup
#' @export
cmhc_geography_type_list=list(PR=2,CMA=3, CSD=4, CT=7, ZONE=5,NEIGHBOUHOOD=6)

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
    "CMA" = lapply(GeoUID,function(g)cmhc::cmhc_cma_translation_data %>% filter(METCODE==g) %>% pull(CMA_UID)) %>% unlist,
    "CSD" = lapply(GeoUID,function(g)cmhc::cmhc_csd_translation_data %>% filter(CMHC_CSDUID==g) %>% pull(CSDUID)) %>% unlist,
    "CT" = paste0(CMA_GEOUID,GeoUID)
  )

  result
}



#' census geography lookup
#' @export
census_to_cmhc_translation=list("59933"="2410", "35535"="2270", "48825"="0140", "59935"="2440")




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

