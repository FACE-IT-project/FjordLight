#' Simply lists the names of fjords with available data.
#'
#' Run this to determine which character vectors to use when downloading data
#' via \code{\link{fl_DownloadFjord}}, or a range of other uses within \code{FjordLight}.
#'
#' @return A list of currently 7 different character vectors representing a range of fjords
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
  fjord_list <- c("disko", "is", "kong", "nuup", "por", "stor", "young" )
  return(fjord_list)
}
