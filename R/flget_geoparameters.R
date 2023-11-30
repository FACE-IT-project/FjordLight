#' Extract geo-parameters from a NetCDF file downloaded by \code{FjordLight}.
#'
#' Given a NetCDF file downloaded via \code{\link{fl_DownloadFjord}},
#' this function extracts the central longitude and latitude for that site,
#' as well as the surface are in km^2 for the shallow zone (< 50 m deep) and coastal
#' zone (< 200 m deep).
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}.
#'
#' @return A named vector containing the following items:
#'   \item{site_average_longitude}{The central longitude of the fjord [degree decimals]}
#'   \item{site_average_latitude}{The central latitude of the fjord [degree decimals]}
#'   \item{AreaOfCoastalZone}{The surface area of the grid cells with a depth of < 200 m [km^2]}
#'   \item{AreaOfShallowZone}{The surface area of the grid cells with a depth of < 50 m [km^2]}
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' # Load data
#' fjord_code <- "test"
#' fjorddata <- fl_LoadFjord(fjord_code, dirdata = system.file("extdata", package = "FjordLight"))
#'
#' # Load geo-parameters
#' geo_params <- flget_geoparameters(fjorddata)
#'
#' # Convert to a data.frame if desired
#' geo_params_df <- t(as.data.frame(geo_params))
#'
flget_geoparameters <- function(fjord) {
  vs <- c("site_average_longitude", "site_average_latitude", "AreaOfCoastalZone", "AreaOfShallowZone")
  geovars <- NULL
  for(v in vs) {
    geovars <- append(geovars, fjord[[v]])
  }
  names(geovars) <- vs
  return(geovars)
}
