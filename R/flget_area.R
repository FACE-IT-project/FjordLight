#' Extract pixel surface area data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will conveniently extract the pixel surface area data stored within a
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}. The user may choose to
#' load the data in either raster or data.frame formats. It is useful to combine these
#' data with others, e.g. bathymetry data loaded via \code{\link{fl_DownloadFjord}}
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}.
#' @param mode Determines the format to be loaded into the R environment.
#' The default \code{"raster"} will load the data as a raster format. \code{"3col"}
#' will load the data as a data.frame with three columns.
#'
#' @return Depending on which arguments the user chooses, this function will return the
#' surface area data as a \code{RasterLayer} (\code{mode = "raster"}) or
#' data.frame (\code{mode = "df"}). The data.frame will contain the following columns:
#'   \item{longitude}{degree decimals}
#'   \item{latitude}{degree decimals}
#'   \item{PixelArea_km2}{the surface area of the grid cell [km^2]}
#'
#' @author Bernard Gentili & Robert Schlegel
#'
#' @export
#'
#' @examples
#' # Download+load data
#' fjord_code <- "test"
#' fl_DownloadFjord(fjord_code, dirdata = tempdir())
#' fjorddata <- fl_LoadFjord(fjord_code, dirdata = tempdir())
#'
#' # Load area data
#' area <- flget_area(fjorddata, mode = "df")
#'
flget_area <- function(fjord,
                       mode = "raster") {

  mat <- fjord[["area"]]

  if(mode == "raster") {
    r <- raster::raster(list(x = fjord$longitude, y = fjord$latitude, z = mat))
    names(r) <- "pixelarea"
    raster::crs(r) <- 4326
    return(r)
  }

  if(mode == "df") {
    n <- nrow(mat)
    m <- ncol(mat)
    row_indices <- rep(1:n, each = m)
    col_indices <- rep(1:m, times = n)
    dum <- data.frame(
      longitude = fjord$longitude[row_indices],
      latitude = fjord$latitude[col_indices],
      PixArea_km2 = base::as.vector(t(mat))
    )
    return(dum)
  }

}
