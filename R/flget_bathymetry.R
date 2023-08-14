#' Extract and/or plot bathymetry data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will conveniently extract the bathymetry data stored within a
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}. There are options
#' for how the user would like to subset the data, which data format the data
#' should be extracted to, and if the user would like to plot the data in the process.
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}.
#' @param what The default value \code{"s"} will load all "sea" data, meaning it will filter
#' out any land pixels. The other options are: \code{"c"} filters out only bathymetry data
#' at a depth of 200 m to 0 m, and \code{"sl"} loads the entire data layer (all sea and land pixels).
#' @param mode Determines the format of the bathymetry data loaded into the R environment.
#' The default \code{"raster"} will load the data as a raster format. The other option \code{"3col"}
#' will load the data as a data.frame with three columns.
#' @param PLOT Boolean argument (default = \code{FALSE}) that tells the function if the user
#' would like the loaded bathymetry data to be plotted or not.
#'
#' @return Depending on which arguments the user chooses, this function will return the
#' filtered bathymetry data as a \code{RasterLayer} (\code{mode = "raster"}) or
#' data.frame (\code{mode = "3col"}). The data.frame will contain the following columns:
#'   \item{longitude}{degree decimals}
#'   \item{latitude}{degree decimals}
#'   \item{depth}{metres}
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download+load data
#' fjord <- "kong"
#' fl_DownloadFjord(fjord, dirdata = "data/PAR")
#' fjorddata <- fl_LoadFjord(fjord, dirdata = "data/PAR")
#'
#' # all depths (what = "s" ; s for Sea), as raster
#' bathy <- flget_bathymetry(fjorddata, what = "s", mode = "raster", PLOT = TRUE)
#' # coastal zone [0-200m] (what = "c" ; c for Coastal), as raster
#' bathy <- flget_bathymetry(fjorddata, what = "c", mode = "raster", PLOT = TRUE)
#' # as 3 columns data frame (mode = "3col" : longitude, latitude, depth)
#' sea <- flget_bathymetry(fjorddata, what = "s", mode = "3col", PLOT = FALSE)
#' cz <- flget_bathymetry(fjorddata, what = "c", mode = "3col", PLOT = FALSE)
#' # you may add letter "l" if you want land elevation
#' sealand <- flget_bathymetry(fjorddata, what = "sl", mode = "3col", PLOT = FALSE)
#' }
#'
flget_bathymetry <- function(fjord, what = "s", mode = "raster", PLOT = FALSE) {
	w <- unlist(strsplit(what, ""))
	if("l" %in% w) LAND <- TRUE else LAND <- FALSE
	if("c" %in% w | "s" %in% w) BATHY <- TRUE else BATHY <- FALSE
	if("c" %in% w) CZ <- TRUE else CZ <- FALSE
	with(fjord, {

		proj.lonlat.def = 4326

		l <- fjord[["land"]]
		l <- raster::raster(list(x = longitude, y = latitude, z = l))
		b <- fjord[["bathymetry"]]
		b <- raster::raster(list(x = longitude, y = latitude, z = b))

		if(BATHY) {
			if(CZ) {vb <- raster::values(b); vb[vb < -200] <- NA; raster::values(b) <- vb}
			if(!CZ) names(b) <- "bathymetry" else names(b) <- "Coastal_Zone"
			if(LAND) {
				r <- b
				vr <- raster::values(r)
				vl <- raster::values(l)
				i <- is.na(vr)
				vr[i] <- vl[i]
				raster::values(r) <- vr
			} else {
				r <- b
			}
			if(PLOT) {
				flplot_bathymetry(b, l, name)
			}
		} else {
			r <- l
			names(r) <- "land"
			if(PLOT) {
				flplot_land(r, b, name)
			}
		}
		raster::crs(r) <- proj.lonlat.def
		if(mode == "raster") {
			return(r)
		}
		if(mode == "3col") {
			dum <- as.data.frame(cbind(raster::xyFromCell(r, 1:raster::ncell(r)), raster::values(r)))
			names(dum) <- c("longitude", "latitude", "depth")
			return(dum)
		}
	})
}
