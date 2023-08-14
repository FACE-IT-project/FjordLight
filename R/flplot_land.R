#' Plot land data found within NetCDF files downloaded by \code{FjordLight}.
#'
#' Internal function used by \code{\link{flget_bathymetry}} that plots the land data
#' contained within the NetCDF files downloaded via \code{\link{fl_DownloadFjord}}.
#'
#' @keywords internal
#'
#' @param r The bathymetry data in a raster format.
#' @param l The land data in the same format.
#' @param name The name of the chosen fjord.
#'
#' @return A base R plot of the land data will be provided.
#'
#' @author Bernard Gentili
#'
flplot_land <- function(r, l, name) {
	vr <- raster::values(r)
	plot(r, col = grDevices::terrain.colors(255),
		colNA = "transparent", main = paste(name, names(r)),
		legend.width = 1.5, legend.shrink = 1, legend.mar = 10,
		legend.args = list(text = "",
			side = 4, cex = 1.5, line = 3.5)
	)
	plot(l, add = TRUE, col = grDevices::grey(0:100/100), legend = FALSE)
}
