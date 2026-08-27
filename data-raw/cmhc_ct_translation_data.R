# Rebuild cmhc_ct_translation_data from two sources:
# 1. CMHC geospatial API — CT-level geography with METCODE, neighbourhoods, zones
# 2. cancensus (CA21) — authoritative CT → CSD mapping

# --- 1. Fetch all CT records from CMHC's ArcGIS REST service ---
url <- paste0(
  "https://geospatial.cmhc-schl.gc.ca/server/rest/services/",
  "CMHC_APPS/HMIP_CURRENT_CAWD/MapServer/0/query"
)
res <- httr::GET(
  url,
  query = list(
    where = "1=1",
    outFields = "*",
    returnGeometry = "false",
    f = "json",
    resultRecordCount = 10000
  )
)
raw_json <- jsonlite::fromJSON(httr::content(res, "text"), flatten = TRUE)
raw <- tibble::as_tibble(raw_json$features)
names(raw) <- sub("^attributes\\.", "", names(raw))

# --- 2. Build translation table ---
rebuild <- dplyr::transmute(
  raw,
  CTUID = paste0(sgc_cma_ca_cde, sgc_census_tract_cde),
  CMHC_CT = sgc_census_tract_cde,
  CMHCCT_UID = paste0(metropolitan_major_area_cde, sgc_census_tract_cde),
  CSDUID = NA_character_,
  METCODE = metropolitan_major_area_cde,
  METNAME_EN = metropolitan_major_area_current_nm,
  METNBHD = paste0(
    metropolitan_major_area_cde,
    stringr::str_pad(neighbourhood_cde, width = 3, pad = "0")
  ),
  NBHDCODE = neighbourhood_cde,
  NBHDNAME_EN = neighbourhood_current_nm_en,
  NBHDNAME_LONG_EN = neighbourhood_current_nm_en,
  ZONECODE = survey_zone_cde,
  ZONENAME_EN = survey_zone_current_nm_en,
  ZONENAME_LONG_EN = survey_zone_current_nm_en,
  CATEGORY_EN = "MET",
  NAME_EN = sgc_census_tract_cde
)

# --- 3. Join CSDUID from cancensus (authoritative CT → CSD mapping) ---
ct_cancensus <- cancensus::get_census(
  "CA21",
  regions = list(C = "01"),
  level = "CT"
)
csd_lookup <- ct_cancensus[, c("GeoUID", "CSD_UID")]
rebuild <- dplyr::left_join(rebuild, csd_lookup, by = c("CTUID" = "GeoUID"))
rebuild$CSDUID <- rebuild$CSD_UID
rebuild$CSD_UID <- NULL

# --- 4. Filter out CMHC-only CTs ---
# The CMHC API returns ~300 CTs that are CMHC-specific subdivisions with no
# corresponding StatCan census tract (e.g., CMHC splits Ile d'Orleans near
# Quebec City into 6 CTs while StatCan defines just 1). They return no
# data on the CMHC portal or the ExportTable endpoint, and have no census
# GeoUID, so they cannot be used with cmhc_geo_uid_for_census(). We exclude
# them by keeping only rows that matched a cancensus CT.
rebuild <- rebuild[!is.na(rebuild$CSDUID), ]

# --- 5. Save ---
cmhc_ct_translation_data <- rebuild
usethis::use_data(cmhc_ct_translation_data, overwrite = TRUE)
