#' Plot annual/monthly climatology data extracted from NetCDF files downloaded by \code{FjordLight}.
#'
#' Internal function used by \code{\link{flget_climatology}} that plots data
#' contained within the NetCDF files downloaded via \code{\link{fl_DownloadFjord}}.
#'
#' @keywords internal
#'
#' @param r The yearly/monthly climatology data in a raster format.
#' @param l The land data in the same format.
#' @param name The name of the chosen fjord.
#' @param optics The name of the PAR variable being displayed.
#' @param period The period being displayed. I.e. yearly or monthly.
#' @param month If monthly climatology data, which month(s) is being displayed.
#' @param year If yearly data, which year(s) is being displayed
#'
#' @return A base R plot of the yearly or monthly climatology data will be provided.
#'
#' @author Bernard Gentili
#'
flplot_climatology <- function(r,
                               l,
                               name,
                               optics,
                               period,
                               month,
                               year) {

  Main <- paste(name, optics, period)
  if(!is.null(month)) Main = paste(name, optics, month.name[month])
  if(!is.null(year)) Main = paste(name, optics, year)
  if(optics == "PAR0m") {
    vr <- raster::values(r)
    brks <- seq(0, 45, 5)
    cols <- cs_BuYlRd(length(brks) - 1)
    text.leg <- expression(PAR*"("*0*"-)"~(mol.photons~m^2~day^-1))
    lab.breaks <- c("", brks[-c(1, length(brks))], "")
  }
  if(optics == "PARbottom"){
    nr <- names(r)
    vr <- raster::values(r)
    vr[vr <= 0] <- NA
    raster::values(r) <- vr
    r <- log10(r)
    names(r) <- nr
    vr <- raster::values(r)
    brks <- seq(-5, 2, 1)
    #	cols <- cs_blye(length(brks) - 1)
    cols <- c("#2166AC", "#4393C3", "#92C5DE", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
    text.leg<- expression(PAR[bottom]~(mol.photons~m^2~day^-1))
    lab.breaks <- c("", as.character(10^seq(-4, 1, 1)), "")
  }
  if(optics == "Kpar"){
    vr <- raster::values(r)
    brks <- seq(0.08, 0.5, by = 0.02)
    cols <- cs_BuYlRd(length(brks) - 1)
    text.leg <- expression(K[PAR]~(m^-1))
    lab.breaks <- c("", brks[-c(1, length(brks))], "")
  }
  vr[vr < brks[2]] <- brks[1]; vr[vr > brks[length(brks) - 1]] <- brks[length(brks)]; raster::values(r) <- vr
  raster::plot(r, zlim = range(brks),
               breaks = brks, col = cols,
               xlab = "", ylab = "", main = Main, colNA = "transparent",
               legend.width = 1.5, legend.shrink = 1, legend.mar = 10,
               lab.breaks = lab.breaks,
               legend.args = list(text = text.leg, side = 4, cex = 0.75, line = 3.5
               )
  )
  raster::plot(l, add = TRUE, col = grDevices::grey(0:100/100), legend = FALSE)
}
