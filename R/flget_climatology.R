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
#' or \code{"Kpar"} for the extinction coefficient.
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
#' The default \code{"raster"} will load the data as a raster format. The other option \code{"df"}
#' will load the data as a data.frame with three columns.
#' @param PLOT Boolean argument (default = \code{FALSE}) that tells the function if the user
#' would like the loaded data to be plotted or not.
#'
#' @return Depending on which arguments the user chooses, this function will return the
#' chosen, global, annual, or monthly climatology data as a \code{RasterLayer}
#' (\code{mode = "raster"}) or data.frame (\code{mode = "df"}).
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
#' # Load data
#' fjord_code <- "test"
#' fjorddata <- fl_LoadFjord(fjord_code, dirdata = system.file("extdata", package = "FjordLight"))
#'
#' # PAR0m and PARbottom for July
#' \donttest{
#' P07 <- flget_climatology(fjorddata, "PAR0m", "Clim", month = 7, PLOT = TRUE)
#' Pb7 <- flget_climatology(fjorddata, "PARbottom", "Clim", month = 7, PLOT = TRUE)
#' }
#'
#' # PARbottom Global
#' \donttest{
#' PbG <- flget_climatology(fjorddata, "PARbottom", "Global", PLOT = TRUE)
#' }
#'
#' # PAR0m and kdpar for year 2012 as 3 columns data frame
#' P02012 <- flget_climatology(fjorddata, "PAR0m", "Yearly", year = 2012, mode = "df")
#' k2012 <- flget_climatology(fjorddata, "Kpar", "Yearly", year = 2012, mode = "df")
#'
#' # For more examples see: https://face-it-project.github.io/FjordLight/articles/fl_example.html
#'
flget_climatology <- function(fjord,
                              optics = "PARbottom",
                              period = "Global",
                              month = NULL,
                              year = NULL,
                              mode = "raster",
                              PLOT = FALSE) {

  time_base <- strsplit(fjord$glob_attributes$available_months_by_year, " / ")
  time_df <- as.data.frame(time_base, col.names = "col")

  Years <- as.numeric(substr(time_df$col, start = 1, stop = 4))
  Months <- strsplit(trimws(substring(time_base[[1]], 7), "both"), " ")
  Months <- as.numeric(Months[which.max(lengths(Months))][[1]])

  if(!is.null(month) & !is.null(year)) stop("You have to indicate month or year, not both")
  available.optics <- c("PARbottom", "PAR0m", "Kpar")
  available.period <- c("Clim", "Yearly", "Global")
  available.mode <- c("raster", "df")
  if(! mode %in% available.mode) stop("Wrong mode, choose among: 'raster', 'df'")
  if(! optics %in% available.optics) stop("Wrong optics, choose among: 'PARbottom', 'PAR0m', 'Kpar'")
  if(! period %in% available.period) stop("Wrong period, choose among: 'Clim', 'Yearly', 'Global'")

  if(period == "Clim") {
    if(is.null(month)) stop("Please indicate the month")
    if(length(month) > 1) stop("Please select a single month")
    if(!(month %in% Months)) {
      stop(paste("Bad month: available months", paste(Months, collapse = " ")))
    }
  }

  if(period == "Yearly") {
    if(is.null(year)) stop("Please indicate the year")
    if(length(year) > 1) stop("Please select a single year")
    if(!(year %in% Years)){
      stop(paste("Bad year: available years", paste(Years, collapse = " ")))
    }
  }

  varname <- paste(period, optics, sep = "")
  layername <- optics
  if(is.null(month) & is.null(year)) layername <- paste(layername, "Global", sep = "_")
  if(!is.null(month)) layername <- paste(layername, month.abb[month], sep = "_")
  if(!is.null(year)) layername <- paste(layername, year, sep = "_")

  g <- fjord[[varname]]
  if(!is.null(month) & is.null(year)) g <- g[, , Months == month]
  if(is.null(month) & !is.null(year)) g <- g[, , Years == year]

  r <- raster::raster(list(x = fjord$longitude, y = fjord$latitude, z = g))
  names(r) <- layername
  raster::crs(r) <- 4326

  if(PLOT) {
    l <- fjord[["elevation"]]
    fjord_name <- fjord[["name"]]
    l <- raster::raster(list(x = fjord$longitude, y = fjord$latitude, z = l))
    flplot_climatology(r, l, fjord_name, optics, period, month, year)
  }

  if(mode == "raster") {
    return(r)
  }

  if(mode == "df") {
    dum <- as.data.frame(cbind(raster::xyFromCell(r, 1:raster::ncell(r)), raster::values(r)))
    names(dum) <- c("longitude", "latitude", layername)
    return(dum)
  }

}
