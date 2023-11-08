#' Extract climatology data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will conveniently extract the climatology data stored within a
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}. To extract the monthly
#' bottom PAR data instead one must use \code{\link{flget_PARbottomMonthlyTS}}.
#' There are options for how the user would like to subset the data, which data format the
#' data should be extracted to, and if the user would like to plot the data in the process.
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}.
#' @param optics The PAR variable that the user would like to load. The option are:
#' \code{"PARbottom"} (default) to load the bottom PAR values, \code{"PAR0m"} surface PAR,
#' or \code{"kdpar"} for the extinction coefficient.
#' @param period Here the user determines which time period of data should be loaded. To load
#' the total average values (default) use \code{"Global"}. One may chose instead to load the
#' \code{"Yearly"} or \code{"Monthly"} values. Note that monthly values here represent the
#' climatological average for the month, not one month in a given year. If the user would
#' like one specific month of data (only available for bottom PAR), they should use
#' \code{\link{flget_PARbottomMonthlyTS}}.
#' @param month The monthly climatology to extract. Accepts an integer value from 3 to 10.
#' This argument is ignored if \code{period = "Yearly"}.
#' @param year The yearly average to extract. Currently accepts an integer value from 2003 to 2022.
#' This argument is ignored if \code{period = "Monthly"}.
#' @param mode Determines the format of the data loaded into the R environment.
#' The default \code{"raster"} will load the data as a raster format. The other option \code{"3col"}
#' will load the data as a data.frame with three columns.
#' @param PLOT Boolean argument (default = \code{FALSE}) that tells the function if the user
#' would like the loaded data to be plotted or not.
#'
#' @return Depending on which arguments the user chooses, this function will return the
#' chosen, global, annual, or monthly climatology data as a \code{RasterLayer}
#' (\code{mode = "raster"}) or data.frame (\code{mode = "3col"}).
#' The data.frame will contain the following columns:
#'   \item{longitude}{degree decimals}
#'   \item{latitude}{degree decimals}
#'   \item{optics_month|year}{The column name is determined by the arguments for
#'   \code{optics} and either \code{month} or \code{year}, depending on the users choice.}
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' # Download+load data
#' fjord_code <- "kong"
#' fl_DownloadFjord(fjord_code, dirdata = "test_dir")
#' fjorddata <- fl_LoadFjord(fjord_code, dirdata = "test_dir")
#'
#' # PAR0m and PARbottom for July
#' P07 <- flget_climatology(fjorddata, "PAR0m", "Clim", month = 7, PLOT = TRUE)
#' print(P07)
#' Pb7 <- flget_climatology(fjorddata, "PARbottom", "Clim", month = 7, PLOT = TRUE)
#' print(Pb7)
#'
#' # PARbottom Global
#' PbG <- flget_climatology(fjorddata, "PARbottom", "Global", PLOT = TRUE)
#' print(PbG)
#'
#' # PAR0m and kdpar for year 2012 as 3 columns data frame
#' P02012 <- flget_climatology(fjorddata, "PAR0m", "Yearly", year = 2012, mode = "3col")
#' k2012 <- flget_climatology(fjorddata, "Kpar", "Yearly", year = 2012, mode = "3col")
#'
#' # Remove test files
#' unlink("test_dir", recursive = TRUE)
#'
flget_climatology <- function(fjord, optics = "PARbottom", period = "Global", month = NA, year = NA, mode = "raster", PLOT = FALSE) {

  Months <- 3:10; Years <- 2003:2022

  if(!is.na(month) & !is.na(year)) return("You have to indicate month or year, not both")
  available.optics <- c("PARbottom", "PAR0m", "Kpar")
  available.period <- c("Clim", "Yearly", "Global")
  available.mode <- c("raster", "3col")
  if(! mode %in% available.mode) {
    cat("wrong mode, choose among :", available.mode, "\n")
    return (invisible(NULL))
  }
  if(! optics %in% available.optics) {
    cat("wrong optics, choose among :", available.optics, "\n")
    return (invisible(NULL))
  }
  if(! period %in% available.period) {
    cat("wrong period, choose among :", available.period, "\n")
    return (invisible(NULL))
  }

  with(fjord, {
    if(period == "Clim")
      if(!is.na(month)){
        if(!(month %in% Months)) return(paste("bad month", ": available months", paste(Months, collapse = " ")))
      } else {
        return("you have to indicate the month\n")
      }

    if(period == "Yearly")
      if(!is.na(year)){
        if(!(year %in% Years)) return(paste("bad year", ": available years", paste(Years, collapse = " ")))
      } else {
        return("you have to indicate the year\n")
      }
    varname <- paste(period, optics, sep = "")
    layername <- optics
    if(is.na(month) & is.na(year)) layername <- paste(layername, "Global", sep = "_")
    if(!is.na(month)) layername <- paste(layername, month.abb[month], sep = "_")
    if(!is.na(year)) layername <- paste(layername, year, sep = "_")

    proj.lonlat.def = 4326

    g <- fjord[[varname]]
    if(!is.na(month) & is.na(year)) g <- g[, , Months == month]
    if(is.na(month) & !is.na(year)) g <- g[, , Years == year]
    r <- raster::raster(list(x = longitude, y = latitude, z = g))
    names(r) <- layername
    raster::crs(r) <- proj.lonlat.def
    if(PLOT) {
      l <- fjord[["elevation"]]
      l <- raster::raster(list(x = longitude, y = latitude, z = l))
      flplot_climatology(r, l, name, optics, period, month, year)
    }
    if(mode == "raster") {
      return(r)
    }
    if(mode == "3col") {
      #			dum <- as.data.frame(rasterToPoints(r))
      #			names(dum)[1:2] <- c("longitude", "latitude")
      dum <- as.data.frame(cbind(raster::xyFromCell(r, 1:raster::ncell(r)), raster::values(r)))
      names(dum) <- c("longitude", "latitude", layername)
      return(dum)
    }
  })
}
