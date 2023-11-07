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
#' fjord_code <- "kong"
#' fl_DownloadFjord(fjord_code)
#'
fl_DownloadFjord <- function(fjord, dirdata = "FjordLight.d") {
	options(timeout = 0)
	urlobsvlfr <- "ftp://ftp.obs-vlfr.fr/pub/gentili/NC_c_Fjords"
	fjords <- fl_ListFjords()
	if(! fjord %in% fjords) stop(paste(fjord, "not available"))
	if(! file.exists(dirdata)) {
		dir.create(dirdata)
		cat("directory", dirdata, "created\n")
	}
	ncfile <- paste(fjord, "nc", sep = ".")
	localf <- paste(dirdata, ncfile, sep = "/")
	if(! file.exists(localf)) {
		cat("---> downloading fjord", fjord, "\n")
		utils::download.file(paste(urlobsvlfr, ncfile, sep = "/"), localf, method = "auto", mode = "wb")
		cat(fjord, "downloaded in directory", dirdata, "\n")
	} else {
		cat(fjord, "already downloaded in directory", dirdata, "\n")
	}
}
