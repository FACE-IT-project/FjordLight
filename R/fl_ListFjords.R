#' Simply lists the names of fjords with available data.
#'
#' @details
#' Run this to determine which character vectors to use when downloading data
#' via \code{\link{fl_DownloadFjord()}}, or a range of other uses within \code{FjordLight}.
#'
#' @return A list of currently 8 different character vectors representing a range of fjords
#' in the EU Arctic.
#'
#' @author Bernard Gentili, Robert W. Schlegel
#'
#' @export
#'
#' @examples
#' fl_ListFjords()
#'
fl_ListFjords <- function() {
	sub("\\.nc$", "", unlist(strsplit(getURL("ftp://ftp.obs-vlfr.fr/pub/gentili/NC_Fjords/", dirlistonly=TRUE), "\n")))
}

