#' Extract and/or plot bathymetry data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will conveniently extract the bathymetry data stored within a
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}. There are options
#' for how the user would like to subset the data, which data format the data
#' should be extracted to, and if the user would like to plot the data in the process.
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}.
#' @param what The default value \code{"o"} will load all "ocean" data, meaning it will filter
#' out any land pixels. The other options are: \code{"c"} filters out only coastal bathymetry
#' data (depth of 200 m to 0 m), \code{"s"} filters out only shallow bathymetry data
#' (depth of 50 m to 0 m), and \code{"l"} loads the land data. One may combine \code{"o"},
#' \code{"c"}, or \code{"s"}, with \code{"l"} (e.g. \code{"ol"}) to load both sea and land data.
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
#' # Download+load data
#' fjord_code <- "test"
#' fl_DownloadFjord(fjord_code, dirdata = "test_dir")
#' fjorddata <- fl_LoadFjord(fjord_code, dirdata = "test_dir")
#'
#' # For all examples, change 'PLOT = TRUE' to visualise the output
#'
#' # Shallow and land data (what = "s"; s for shallow), as raster
#' # bathy <- flget_bathymetry(fjorddata, what = "s", mode = "raster", PLOT = FALSE)
#'
#' # Remove test files
#' unlink("test_dir", recursive = TRUE)
#'
flget_bathymetry <- function(fjord, what = "o", mode = "raster", PLOT = FALSE) {
  availwhat <- c("o", "c", "s", "lo", "lc", "ls", "ol", "cl", "sl")
  if(! what %in% availwhat) {
    cat("argument \"what\" is a string of one or two letters\n")
    cat("one letter is mandatory and to be chosen between \"o\" (for ocean) \"c\" (for coastal 0-200m) \"s\" (for shallow 0-50m)\n")
    cat("the second letter (optional) is \"l\" if you want to plot land elevation\n")
  }
  w <- unlist(strsplit(what, ""))
  if("l" %in% w) LAND <- TRUE else LAND <- FALSE
  if("s" %in% w | "c" %in% w | "o" %in% w) BATHY <- TRUE else BATHY <- FALSE
  if("c" %in% w) CZ <- TRUE else CZ <- FALSE
  if("s" %in% w) SH <- TRUE else SH <- FALSE
	with(fjord, {

		proj.lonlat.def = 4326

		l <- fjord[["elevation"]]
		l <- raster::raster(list(x = longitude, y = latitude, z = l))
		b <- fjord[["depth"]]
		b <- raster::raster(list(x = longitude, y = latitude, z = b))

		if(BATHY) {
		  names(b) <- "Site_Depth"
		  if(CZ) {vb <- raster::values(b); vb[vb < -200] <- NA; raster::values(b) <- vb; names(b) <- "Coastal_Zone_Depth"}
		  if(SH) {vb <- raster::values(b); vb[vb < -50 ] <- NA; raster::values(b) <- vb; names(b) <- "Shallow_Zone_Depth"}
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
			names(r) <- "Land_Elevation"
			if(PLOT) {
			  flplot_land(r, name)
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
