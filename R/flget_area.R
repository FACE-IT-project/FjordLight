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
#' data.frame (\code{mode = "3col"}). The data.frame will contain the following columns:
#'   \item{longitude}{degree decimals}
#'   \item{latitude}{degree decimals}
#'   \item{PixelArea_km2}{the surface area of the grid cell [km^2]}
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' # Download+load data
#' fjord_code <- "kong"
#' fl_DownloadFjord(fjord_code)
#' fjorddata <- fl_LoadFjord(fjord_code)
#'
#' # Load area data
#' area <- flget_area(fjorddata, mode = "3col")
#' # Then load bathymetry
#' sealand <- flget_bathymetry(fjorddata, what = "sl", mode = "3col", PLOT = FALSE)
#' # Then combine
#' sealand_area <- cbind(sealand, area[3])
#'
flget_area <- function(fjord, mode = "raster") {
	with(fjord, {
		proj.lonlat.def = 4326
		r <- fjord[["area"]]
		r <- raster::raster(list(x = longitude, y = latitude, z = r))
		names(r) <- "pixelarea"
		raster::crs(r) <- proj.lonlat.def
		if(mode == "raster") {
			return(r)
		}
		if(mode == "3col") {
			dum <- as.data.frame(cbind(raster::xyFromCell(r, 1:raster::ncell(r)), raster::values(r)))
			names(dum) <- c("longitude", "latitude", "PixArea_km2")
			return(dum)
		}
	})
}
