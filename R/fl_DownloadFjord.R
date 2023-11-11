#' Download fjord PAR data as NetCDF files.
#'
#' This functions queries the FTP server where the NetCDF files are stored. It will
#' retrieve the one file that matches the name provided to it via the \code{fjord}
#' argument. Note that these files can be multiple gigabytes in size.
#'
#' @param fjord Expects a character vector for one of the 8 available fjords.
#' See \code{\link{fl_ListFjords}} for the list of possible choices.
#' @param dirdata The directory where the user would like to download the data.
#' Default is "FjordLight.d".
#'
#' @return The downloaded NetCDF file contains the following variables:
#'   \item{bathymetry}{depth [m]}
#'   \item{land}{elevation [m]}
#'   \item{area}{PixelArea_km2 [m]}
#'   \item{AreaOfCoastalZone}{Surface of Sea floor with a depth of between 0 and 200 meters [km2]}
#'   etc...
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' # Choose a fjord
#' fjord_code <- "test"
#'
#' # Download it
#' # NB: One should provide a permanent directory when downloading a file.
#' fl_DownloadFjord(fjord_code, dirdata = tempdir())
#'
#'
fl_DownloadFjord <- function(fjord,
                             dirdata = NULL) {
	urlobsvlfr <- "ftp://ftp.obs-vlfr.fr/pub/gentili/NC_c2_Fjords"
	fjords <- fl_ListFjords()
	if(! fjord %in% fjords){
	  if(fjord == "test"){
	    urlobsvlfr <- "ftp://ftp.obs-vlfr.fr/pub/gentili/tmpFjords"
	  } else {
	    stop(paste(fjord, "not available"))
	  }
	}
	if(is.null(dirdata)) stop("Please provide the pathway to where you would like to download the data.")
	if(! file.exists(dirdata)) stop("Please ensure that the chosen directory exists.")
	ncfile <- paste(fjord, "nc", sep = ".")
	localf <- paste(dirdata, ncfile, sep = "/")
	if(! file.exists(localf)) {
		message("---> downloading fjord ", fjord)
		utils::download.file(paste(urlobsvlfr, ncfile, sep = "/"), localf, method = "auto", mode = "wb")
		message(fjord, " downloaded in directory ", dirdata)
	} else {
	  message(fjord, " already downloaded in directory ", dirdata)
	}
}
