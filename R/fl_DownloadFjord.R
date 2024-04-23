#' Download fjord PAR data as NetCDF files.
#'
#' This functions queries the FTP server where the NetCDF files are stored. It will
#' retrieve the one file that matches the name provided to it via the \code{fjord}
#' argument. Note that these files can be multiple gigabytes in size.
#'
#' @param fjord Expects a character vector for one of the 8 available fjords.
#' See \code{\link{fl_ListFjords}} for the list of possible choices.
#' @param layer The layer of data the user wants to download. The default "PAR_B"
#' will download monthly bottom PAR data, "K_PAR" will download monthly
#' values for the light extinction coefficient (i.e. K_PAR) in the water column,
#' "ClimSD" will download the standard deviations for the monthly climatologies, and
#' "YearlySD" will download the standard deviations for the yearly climatologies.
#' Note that if monthly K_PAR data are chosen, the file will be saved as e.g.
#' "kong_MonthlyKpar.nc". "ClimSD" and "YearlySD" data will also have this character
#' string appended to the file name. Whereas PAR_B data will be saved simply as e.g.
#' "kong.nc". NB: Only the "PAR_B" data contain the full list of metadata variables.
#' @param dirdata The directory where the user would like to download the data.
#'
#' @return The downloaded NetCDF file contains the following variables:
#'   \item{bathymetry}{depth [m]}
#'   \item{land}{elevation [m]}
#'   \item{area}{PixelArea_km2 [m]}
#'   \item{AreaOfCoastalZone}{Surface of Sea floor with a depth of between 0 and 200 meters [km2]}
#'   etc...
#'
#' @author Bernard Gentili and Robert Schlegel
#'
#' @export
#'
#' @examples
#' # Choose a fjord
#' fjord_code <- "kong"
#'
#' # Download it
#' # NB: One should provide a permanent directory when downloading a file.
#' \donttest{
#' fl_DownloadFjord(fjord_code, dirdata = tempdir())
#' }
#'
fl_DownloadFjord <- function(fjord,
                             layer = "PAR_B",
                             dirdata = NULL) {
  opt_orig <- options()
  on.exit(options(opt_orig))
  options(timeout = 0)
	urlpangaea <- "https://download.pangaea.de/dataset/962895/files"
	urlpangaea_addendum <- "https://download.pangaea.de/dataset/965460/files"
	dlnote <- "Please check your internet connection."
	if(curl::has_internet()){
	  fjords <- fl_ListFjords()
	  if(! fjord %in% fjords){
	    stop(paste(fjord, "not available"))
	  }
	  if(is.null(dirdata)) stop("Please provide the pathway to where you would like to download the data.")
	  if(! file.exists(dirdata)) stop("Please ensure that the chosen directory exists.")
	  ncurl <- urlpangaea_addendum
	  if(layer == "PAR_B"){
	    ncfile <- paste(fjord, "nc", sep = ".")
	    ncurl <- urlpangaea
	  } else if(layer == "K_PAR"){
	    ncfile <- paste0(fjord,"_MonthlyKpar.nc")
	  } else if(layer == "ClimSD"){
	    ncfile <- paste0(fjord,"_ClimSD.nc")
	  } else if(layer == "YearlySD"){
	    ncfile <- paste0(fjord,"_YearlySD.nc")
	  } else {
	    stop("Please ensure the 'layer' value is either 'PAR_B', 'K_PAR', 'ClimSD', or 'YearlySD'")
	  }
	  localf <- paste(dirdata, ncfile, sep = "/")
	  if(! file.exists(localf)) {
	    message("---> downloading fjord ", fjord)
	    utils::download.file(paste(ncurl, ncfile, sep = "/"), localf, method = "auto", mode = "wb")
	    dlnote <- paste0(ncfile, " downloaded in directory ", dirdata)
	  } else {
	    dlnote <- paste0(ncfile, " already downloaded in directory ", dirdata)
	  }
	}
	message(dlnote)
}
