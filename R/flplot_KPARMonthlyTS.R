#' Plot monthly K_PAR data extracted from NetCDF files downloaded by \code{FjordLight}.
#'
#' Internal function used by \code{\link{flget_KPARMonthlyTS}} that plots data
#' contained within the NetCDF files downloaded via \code{\link{fl_DownloadFjord}}.
#'
#' @keywords internal
#'
#' @param r The monthly K_PAR data in a raster stack format.
#'
#' @return A base R plot of the monthly K_PAR data will be provided.
#'
#' @author Bernard Gentili
#'
flplot_KPARMonthlyTS <- function(r) {
  nr <- names(r)
  vr <- raster::values(r)
  vr[vr <= 0] <- NA
  raster::values(r) <- vr
  names(r) <- nr
  vr <- raster::values(r)
  brks <- seq(0, 1.0, 0.1)
  cols <- cs_BuYlRd(length(brks))
  # A[2]~and~B[2]
  text <- expression(K[PAR]~m^-1)
  vr[vr > brks[11]] <- brks[11]; raster::values(r) <- vr
  names(r) <- sub("MonthlyKPAR", "Pb", names(r))
  raster::plot(r, zlim = range(brks), maxnl = raster::nlayers(r),
               breaks = brks, col = cols,
               xlab = "", ylab = "", colNA = "transparent",
               legend.width = 1.5, legend.shrink = 1, legend.mar = 10,
               legend.args = list(text = text, side = 4, cex = 0.75, line = 3.5
               )
  )
}
