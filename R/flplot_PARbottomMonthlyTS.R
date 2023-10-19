#' Plot monthly bottom PAR data extracted from NetCDF files downloaded by \code{FjordLight}.
#'
#' Internal function used by \code{\link{flget_PARbottomMonthlyTS}} that plots data
#' contained within the NetCDF files downloaded via \code{\link{fl_DownloadFjord}}.
#'
#' @keywords internal
#'
#' @param r The monthly bottom PAR data in a raster stack format.
#'
#' @return A base R plot of the monthly bottom PAR data will be provided.
#'
#' @author Bernard Gentili
#'
flplot_PARbottomMonthlyTS <- function(r) {
  nr <- names(r)
  vr <- raster::values(r)
  vr[vr <= 0] <- NA
  raster::values(r) <- vr
  r <- log10(r)
  names(r) <- nr
  vr <- raster::values(r)
  brks <- seq(-5, 2, 1)
  cols <- c("#2166AC", "#4393C3", "#92C5DE", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
  text = expression(mol.photons~m^2~day^-1)
  lab.breaks = c("", as.character(10^seq(-4, 1, 1)), "")
  vr[vr < brks[2]] <- brks[1]; vr[vr > brks[length(brks) - 1]] <- brks[length(brks)]; raster::values(r) <- vr
  names(r) <- sub("MonthlyPARbottom", "Pb", names(r))
  raster::plot(r, zlim = range(brks), maxnl = raster::nlayers(r),
               breaks = brks, col = cols,
               xlab = "", ylab = "", colNA = "transparent",
               legend.width = 1.5, legend.shrink = 1, legend.mar = 10,
               lab.breaks = lab.breaks,
               legend.args = list(text = text, side = 4, cex = 0.75, line = 3.5
               )
  )
}
