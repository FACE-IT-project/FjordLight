#' Load fjord PAR data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will load into the R environment the data within one NetCDF
#' file that has been downloaded via \code{\link{fl_DownloadFjord}}.
#'
#' @param fjord Expects a character vector for one of the 8 available fjords.
#' See \code{\link{fl_ListFjords}} for the list of possible choices.
#' @param dirdata The directory where the user would like to load the data from.
#' @param TS The default, \code{FALSE}, will prevent this function from loading the
#' monthly bottom PAR values. Instead it will load all global, annual, and monthly
#' climatology data. Set \code{TS = TRUE} to load all data, but note that these
#' may be extremely large.
#' @param verbose \code{TRUE} or \code{FALSE} (default) to provide a more verbose output
#' of the data loading process.
#'
#' @return Data are loaded in as a complex list format. Containing most of the data fields
#' described in the documentation for \code{\link{fl_DownloadFjord}}.
#'
#' @author Bernard Gentili & Robert Schlegel
#'
#' @export
#'
#' @examples
#' # Choose + download fjord
#' fjord_code <- "test"
#' fl_DownloadFjord(fjord_code, dirdata = tempdir())
#'
#' # Load global, annual, and monthly climatologies
#' fjorddata <- fl_LoadFjord(fjord_code, dirdata = tempdir())
#'
#' # Load ALL data
#' fjorddata_full <- fl_LoadFjord(fjord_code, dirdata = tempdir(), TS = TRUE)  # NB: TS = TRUE
#'
fl_LoadFjord <- function(fjord, dirdata = NULL, TS = FALSE, verbose = FALSE) {
  if(is.null(dirdata)) stop("Please provide the pathway from where you would like to load the data.")
  if(! file.exists(dirdata)) stop("Please ensure that the chosen directory exists.")
	ncfile <- paste(dirdata, paste(fjord, "nc", sep = "."), sep = "/")
	nc <- ncdf4::nc_open(ncfile, verbose = verbose)
	dims <- names(nc$dim)
	vars <- names(nc$var)
	if(!TS) vars <- vars[! vars %in% "MonthlyPARbottom"]
	vars_attributes <- list()
	for(d in dims) {
		assign(d, ncdf4::ncvar_get(nc, d))
		at <- list(ncdf4::ncatt_get(nc, d))
		names(at) <- d
		vars_attributes <- c(vars_attributes, at)
	}
	for(v in vars) {
		assign(v, ncdf4::ncvar_get(nc, v))
		at <- list(ncdf4::ncatt_get(nc, v))
		names(at) <- v
		vars_attributes <- c(vars_attributes, at)
	}
	glob_attributes <- ncdf4::ncatt_get(nc, 0)
	ncdf4::nc_close(nc)
	name <- fjord
	l <- as.list(mget(c("name", dims, vars, "vars_attributes", "glob_attributes")))
	return(l)
}
