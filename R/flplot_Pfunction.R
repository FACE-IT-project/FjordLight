#' Plot p function data extracted from NetCDF files downloaded by \code{FjordLight}.
#'
#' Internal function used by \code{\link{flget_Pfunction}} that plots data
#' contained within the NetCDF files downloaded via \code{\link{fl_DownloadFjord}}.
#'
#' @keywords internal
#'
#' @param irrLev The chosen irradiance thresholds (x-axis).
#' @param g The percent of the fjord (y-axis) with a depth of 200 m or shallower receiving an
#' irradiance values (e.g. surface PAR) at or above the corresponding threshold (x-axis).
#' @param period The period being displayed. I.e. yearly or monthly.
#' @param month If monthly climatology data, which month(s) is being displayed.
#' @param year If yearly data, which year(s) is being displayed
#' @param Main The title of the plot. The default \code{NULL} will provide a title
#' 'Pfunction yyy'. Where yyy is the annual or monthly period chosen.
#' @param add Boolean value of \code{TRUE} or \code{FALSE} telling the function to add this plot to the
#' current plot in the R Plots panel.
#' @param ... Any other desired base R plotting functionality.
#'
#' @return A base R plot of the yearly or monthly p function data will be provided.
#'
#' @author Bernard Gentili
#'
flplot_Pfunction <- function(irrLev,
                             g,
                             period,
                             month,
                             year,
                             Main = NULL,
                             add = add,
                             ...) {

	if(is.null(Main)) {
		Main <- paste("Pfunction", period)
		if(!is.null(month)) Main = paste("Pfunction", month.name[month])
		if(!is.null(year)) Main = paste("Pfunction", year)
	}

	if(add) {
		graphics::lines(irrLev, g, ...)
	} else {
		plot(irrLev, g, xlim = rev(range(irrLev)),
		     xlab = expression(E~"mol photons"~m^-2~d^-1),
		     ylab = expression("% of the surface receiving more than E"),
		     main = Main, log = "x", type = "l", ...)
	}

}
