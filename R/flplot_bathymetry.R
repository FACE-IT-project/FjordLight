#' Plot bathymetry data found within NetCDF files downloaded by \code{FjordLight}.
#'
#' Internal function used by \code{\link{flget_bathymetry}} that plots the bathymetry data
#' contained within the NetCDF files downloaded via \code{\link{fl_DownloadFjord}}.
#'
#' @keywords internal
#'
#' @param r The bathymetry data in a raster format.
#' @param l The land data in the same format.
#' @param name The name of the chosen fjord.
#'
#' @return A base R plot of the bathymetric data will be provided.
#'
#' @author Bernard Gentili
#'
flplot_bathymetry <- function(r,
                              l = NULL,
                              name) {

	vr <- raster::values(r)
	brco <- fl_topocolorscale(vr)

	raster::plot(r, breaks = brco$brks, col = brco$colors,
	             colNA = "transparent", main = paste(name, names(r)),
	             legend.width = 1.5, legend.shrink = 1, legend.mar = 10,
	             legend.args = list(text = "",
	                                side = 4, cex = 1.5, line = 3.5)
	)
	if(!is.null(l))	raster::plot(l, add = TRUE, col = grDevices::grey(0:100/100), legend = FALSE)
}
