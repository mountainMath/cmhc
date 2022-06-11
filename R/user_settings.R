# Functions for managing user settings, cache locations


#' Set persistent cmhc cache location
#'
#' @description cmhc provides access to custom cmhc geographies, these are large files and should be stored in a
#' permanent location.
#'
#' @param cache_path a local directory to use for saving cached data
#' @param overwrite Option to overwrite any existing cache path already stored locally.
#' @param install Option to install permanently for use across sessions.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' set_cache_path("~/cmhc_cache")
#'
#' # This will set the cache path permanently until ovewritten again
#' set_cache_path("~/cmhc_cache", install = TRUE)
#' }
set_cache_path <- function(cache_path, overwrite = FALSE, install = FALSE){
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(!file.exists(renv)){
      file.create(renv)
    } else{
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=readLines(renv)
        newenv <- oldenv[-grep("CMHC_CACHE_PATH", oldenv)]
        writeLines(newenv, renv, sep = "\n")
      } else{
        tv <- readLines(renv)
        if(any(grepl("CMHC_CACHE_PATH",tv))){
          stop("A saved cache already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("CMHC_CACHE_PATH='", cache_path, "'")
    # Append cache path .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    Sys.setenv('CMHC_CACHE_PATH' = cache_path)
    message('Your cache path has been stored in your .Renviron and can be accessed by Sys.getenv("CMHC_CACHE_PATH").')
  } else {
    message("Cache set for duration of session. To permanently add your cache path for use across sessions, run this function with `install = TRUE`.")
    Sys.setenv('CMHC_CACHE_PATH' = cache_path)
  }
  cache_path
}

#' View saved cache directory path
#'
#' @description View saved cache path'
#'
#' @export
show_cache_path <- function() {
  Sys.getenv('CMHC_CACHE_PATH')
}


