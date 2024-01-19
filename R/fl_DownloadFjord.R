#' Download fjord PAR data as NetCDF files.
#'
#' This functions queries the FTP server where the NetCDF files are stored. It will
#' retrieve the one file that matches the name provided to it via the \code{fjord}
#' argument. Note that these files can be multiple gigabytes in size.
#'
#' @param fjord Expects a character vector for one of the 8 available fjords.
#' See \code{\link{fl_ListFjords}} for the list of possible choices.
#' @param monthly The layer of monthly data the user wants to download. The default "PAR_B"
#' will download monthly bottom PAR data. The other option "K_PAR" will download monthly
#' values for the light extinction coefficient (i.e.  K_PAR) in the water column.
#' Note that if monthly K_PAR data are chosen, the file will be saved as e.g.
#' "kong_MonthlyKpar.nc", whereas PAR_B data will be saved simply as e.g. "kong.nc"
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
                             monthly = "PAR_B",
                             dirdata = NULL) {
  opt_orig <- options()
  on.exit(options(opt_orig))
  options(timeout = 0)
	urlobsvlfr <- "ftp://ftp.obs-vlfr.fr/pub/gentili/NC_c2_Fjords_MonthlyKpar"
	urlpangaea <- "https://download.pangaea.de/dataset/962895/files"
	dlnote <- "Please check your internet connection."
	if(curl::has_internet()){
	  fjords <- fl_ListFjords()
	  if(! fjord %in% fjords){
	    stop(paste(fjord, "not available"))
	  }
	  if(is.null(dirdata)) stop("Please provide the pathway to where you would like to download the data.")
	  if(! file.exists(dirdata)) stop("Please ensure that the chosen directory exists.")
	  if(monthly == "PAR_B"){
	    ncfile <- paste(fjord, "nc", sep = ".")
	    ncurl <- urlpangaea
	  } else if(monthly == "K_PAR"){
	    ncfile <- paste0(fjord,"_MonthlyKpar.nc")
	    ncurl <- urlobsvlfr
	  } else {
	    stop("Please ensure the 'monthly' value is either 'PAR_B' or 'K_PAR'")
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
