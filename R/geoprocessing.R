# geoprocessing.R

#' Get UTM zone from coordinates
#'
#' @param lon longitude in [-180,180]
#' @return The number of the UTM zone containing the longitude
#' @export
#' @examples
#' gis_longitude_to_utm_zone(-45)
gis_longitude_to_utm_zone <- function(lon) {
  utm_zone <- (base::floor((lon + 180)/6) %% 60) + 1
  return(utm_zone)
}

#' Convert utm zone to proj4string
#'
#' @param utm_zone UTM zone as numeric integer, or string
#' @return The proj4string of the UTM Zone
#' @export
#' @examples
#' gis_utm_zone_to_proj4(32)
#' gis_utm_zone_to_proj4(gis_longitude_to_utm_zone(-45))
gis_utm_zone_to_proj4 <- function(utm_zone) {
  proj4_base <- "+proj=utm +zone=UTM_ZONE +datum=WGS84 +units=m +no_defs"
  return(gsub("UTM_ZONE",as.character(utm_zone),proj4_base))
}
