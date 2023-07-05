#' Extract monthly bottom PAR data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will extract the monthly bottom PAR data stored within a
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}. Note that these data are
#' very large. If one would prefer to work with the smaller annual or monthly climatology
#' values, instead use \code{\link{flget_climatology}}. There are options for how the user
#' would like to subset the data, which data format the data should be extracted to,
#' and if the user would like to plot the data in the process.
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}. NB: when loading
#' the data one must set the argument \code{fl_LoadFjord(..., TS = TRUE)}. See examples below.
#' @param month The monthly values to extract. Accepts one or many integer values from 3 to 10.
#' If no values are provided, the default value of \code{NULL} will be passed to the function,
#' telling it to load all available months of data (i.e. 3:10).
#' This is used in combination with \code{year} to determine which monthly data to extract.
#' @param year The years of data to extract. Currently accepts one or many integer values from 2003 to 2022.
#' If no values are provided, the default value of \code{NULL} will be passed to the function,
#' telling it to load all available years of data (i.e. currently 2003:2022).
#' This is used in combination with \code{month} to determine which monthly data to extract.
#' @param mode Determines the format of the data loaded into the R environment.
#' The default \code{"raster"} will load the data as a raster format. The other option \code{"3col"}
#' will load the data as a data.frame with three columns.
#' @param PLOT Boolean argument (default = \code{FALSE}) that tells the function if the user
#' would like the loaded data to be plotted or not.
#'
#' @return Depending on which arguments the user chooses, this function will return the
#' chosen monthly bottom PAR data as a \code{RasterStack} (\code{mode = "raster"})
#' or data.frame (\code{mode = "3col"}). The data.frame will contain the following columns:
#'   \item{longitude}{degree decimals}
#'   \item{latitude}{degree decimals}
#'   \item{PARbottom}{mol photons m-2 d-1}
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
#' fjorddata <- fl_LoadFjord(fjord, dirdata = "data/PAR", TS = TRUE) # NB: TS = TRUE
#'
#' # all months - years 2011 2012
#' mts <- flget_PARbottomMonthlyTS(fjorddata, year = 2011:2012, PLOT = TRUE)
#' print(mts)
#' # all years - months July August
#' mts <- flget_PARbottomMonthlyTS(fjorddata, month = 7:8, PLOT = TRUE)
#' print(mts)
#' # years 2003 to 2012 - months July August
#' mts <- flget_PARbottomMonthlyTS(fjorddata, month = 7:8, year = 2003:2012, PLOT = TRUE)
#' print(mts)
#' # all months - all years - as a data.frame
#' mts_full <- flget_PARbottomMonthlyTS(fjorddata, mode = "3col")
#' str(mts_full, max.level = 0)
#' print(names(mts_full))
#' }
#'
flget_PARbottomMonthlyTS <- function(fjord, month = NULL, year = NULL, mode = "raster", PLOT = FALSE) {
	if(is.null(fjord$PARbottom)) {
		cat("PARbottom monthly time series not loaded\n")
		return (invisible(NULL))
	}
	available.mode <- c("raster", "3col")
	if(! mode %in% available.mode) {
		cat("wrong mode, choose among :", available.mode, "\n")
		return (invisible(NULL))
	}
	with(fjord, {
		if(!is.null(month)){
			if(!all(month %in% Months)) return(paste("bad month(s)", ": available months", paste(months, collapse = " ")))
		} else {
			month <- Months
		}

		if(!is.null(year)){
			if(!all(year %in% Years)) return(paste("bad year(s)", ": available years", paste(years, collapse = " ")))
		} else {
			year <- Years
		}
		varname <- "PARbottom"

		proj.lonlat.def = "+init=epsg:4326"

		g <- fjord[[varname]]
		s <- stack()
		layernames <- NULL
		for(y in year) {
			for(m in month) {
				h <- g[, , Months == m, Years == y]
				r <- raster(list(x = longitude, y = latitude, z= h))
				layername <- paste(varname, formatC(y, format = "d", width = 4, flag = "0"), formatC(m, format = "d", width = 2, flag = "0"), sep = ".")
				layernames <- append(layernames, layername)
				names(r) <- layername
				s <- stack(s, r)
			}
		}
		if(PLOT) {
			flplot_PARbottomMonthlyTS(s)
		}
		if(mode == "raster") {
			return(s)
		}
		if(mode == "3col") {
			dum <- as.data.frame(cbind(xyFromCell(s, 1:ncell(s)), as.matrix(s)))
			names(dum) <- c("longitude", "latitude", layernames)
			return(dum)
		}
	})
}
