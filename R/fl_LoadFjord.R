#' Load fjord PAR data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will load into the R environment the data within one NetCDF
#' file that has been downloaded via \code{\link{fl_DownloadFjord}}.
#'
#' @param fjord Expects a character vector for one of the 8 available fjords.
#' See \code{\link{fl_ListFjords}} for the list of possible choices.
#' @param layer The layer of data the user wants to load. The default "PAR_B"
#' will load monthly bottom PAR data. The other options, "K_PAR" will load monthly
#' values for the light extinction coefficient (i.e.  K_PAR) in the water column,
#' "ClimSD" will load the standard deviation values for the monthly climatologies,
#' and "YearlySD" will load the standard deviation values for the yearly climatologies.
#' Note that only the PAR_B files and do not contain all global values and metadata.
#' The other files are in supplement to the PAR_B files.
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
#' # Choose fjord
#' fjord_code <- "test"
#'
#' # Load global, annual, and monthly climatologies
#' fjorddata <- fl_LoadFjord(fjord_code, dirdata = system.file("extdata", package = "FjordLight"))
#'
#' # Load ALL data
#' fjorddata_full <- fl_LoadFjord(fjord_code,
#'                                dirdata = system.file("extdata", package = "FjordLight"), TS = TRUE)
#'
fl_LoadFjord <- function(fjord, layer = "PAR_B", dirdata = NULL, TS = FALSE, verbose = FALSE) {
  if(is.null(dirdata)) stop("Please provide the pathway from where you would like to load the data.")
  if(! file.exists(dirdata)) stop("Please ensure that the chosen directory exists.")
  if(layer == "PAR_B"){
    ncfile <- paste0(dirdata,"/",fjord,".nc")
  } else if(layer == "K_PAR"){
    ncfile <- paste0(dirdata,"/",fjord,"_MonthlyKpar.nc")
  } else if(layer == "ClimSD"){
    ncfile <- paste0(dirdata,"/",fjord,"_ClimSD.nc")
  } else if(layer == "YearlySD"){
    ncfile <- paste0(dirdata,"/",fjord,"_YearlySD.nc")
  } else{
    stop("Please ensure the 'layer' value is either 'PAR_B', 'K_PAR', 'ClimSD', or 'YearlySD'")
  }
	nc <- ncdf4::nc_open(ncfile, verbose = verbose)
	dims <- names(nc$dim)
	vars <- names(nc$var)
	if(!TS & layer == "PAR_B") vars <- vars[! vars %in% "MonthlyPARbottom"]
	if(!TS & layer == "K_PAR") vars <- vars[! vars %in% "MonthlyKpar"]
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
