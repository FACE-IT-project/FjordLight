#' Simply lists the names of fjords with available data.
#'
#' Run this to determine which character vectors to use when downloading data
#' via \code{\link{fl_DownloadFjord}}, or a range of other uses within \code{FjordLight}.
#'
#' @return A list of currently 8 different character vectors representing a range of fjords
#' in the EU Arctic.
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' fl_ListFjords()
#'
fl_ListFjords <- function() {
  fjord_ftp <- "ftp://ftp.obs-vlfr.fr/pub/gentili/NC_c2_Fjords/"
  h <- curl::new_handle(dirlistonly = TRUE)
  con <- curl::curl(fjord_ftp, "r", h)
  fjord_tbl <- utils::read.table(con)
  base::close(con)
  fjord_tbl$V1 <- sub("\\.nc$", "", fjord_tbl$V1)
  return(as.vector(fjord_tbl$V1))
}
