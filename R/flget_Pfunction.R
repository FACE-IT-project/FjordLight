#' Extract p function data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will conveniently extract the p function data stored within a
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}.
#' There are options for how the user would like to subset the data, which data format the
#' data should be extracted to, and if the user would like to plot the data in the process.
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}.
#' @param type Whether the p function should show values for the \code{coastal} zone (< 200 m deep),
#' or the \code{shallow} zone (< 50 m deep).
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
#' @param mode Determines the basic process that this function performs. The default
#' \code{mode = "function"} will allow the user to create a function that they can then use
#' themselves to determine p curves using their own input (see examples). Or to access the
#' underlying p function data directly set \code{mode = "df"}.
#' @param PLOT Boolean argument (default = \code{FALSE}) that tells the function if the user
#' would like the loaded data to be plotted or not.
#' @param add Boolean (i.e. \code{TRUE/FALSE}) to tell the function to add the p function plot
#' to an existing plot. See examples below.
#' @param ... Additional arguments that may be passed to \code{\link{flplot_Pfunction}}, which
#' will implement them using base R plotting functionality.
#'
#' @return Depending on which arguments the user chooses for \code{mode}, a function will
#' be returned (see examples). Or a two column data.frame:
#'   \item{irradianceLevel}{A threshold value [mol photons m-2 d-1]}
#'   \item{optics_global|year|month}{The column name is determined by the arguments passed to \code{optics}
#'   and either \code{global}, \code{year}, or \code{month}, depending on which \code{period} was indicated.
#'   These values show the percent of the fjord (filtered for pixels with a depth of 200 m or shallower) that
#'   received at least the amount of irradiance indicated in the \code{irradianceLevel} column.}
#'
#' @author Bernard Gentili & Robert Schlegel
#'
#' @export
#'
#' @examples
#' # Download+load data
#' fjord_code <- "test"
#' fl_DownloadFjord(fjord_code, dirdata = tempdir())
#' fjorddata <- fl_LoadFjord(fjord_code, dirdata = tempdir())
#'
#' # Create a function
#' fG <- flget_Pfunction(fjorddata, "shallow", "Global")
#'
#' # Then use it with specific PAR thresholds
#' irradiance_levels <- c(0.1, 1, 10)
#' fG(irradiance_levels)
#'
#' # As a 2 column data.frame
#' f2012 <- flget_Pfunction(fjorddata, "shallow", "Yearly", year = 2012, mode = "df", PLOT = TRUE)
#' str(f2012)
#'
#' # Plot a P-function
#' fGlob <- flget_Pfunction(fjorddata, "coastal", "Global", PLOT = TRUE, lty = 1, col = 1, lwd = 2,
#'                          Main = paste(fjord_code, "P-functions"), ylim = c(0, 50))
#'
flget_Pfunction <- function(fjord,
                            type = "coastal",
                            period = "Global",
                            month = NULL,
                            year = NULL,
                            mode = "function",
                            PLOT = FALSE,
                            add = FALSE,
                            ...) {

  Months <- 3:10; Years <- 2003:2022

  if(!is.null(month) & !is.null(year)) stop("You have to indicate month or year, not both")
  available.type <- c("coastal", "shallow")
  available.period <- c("Clim", "Yearly", "Global")
	available.mode <- c("function", "df")
	if(! type %in% available.type) stop("Wrong type, choose among: 'coastal', 'shallow'")
	if(! period %in% available.period) stop("Wrong period, choose among: 'Clim', 'Yearly', 'Global'")
	if(! mode %in% available.mode) stop("Wrong mode, choose among: 'function', 'df'")

	if(period == "Clim") {
	  if(is.null(month)) stop("Please indicate the month")
	  if(length(month) > 1) stop("Please select a single month")
	  if(!(month %in% Months)) stop(paste("Bad month: available months", paste(Months, collapse = " ")))
	}

	if(period == "Yearly") {
	  if(is.null(year)) stop("Please indicate the year")
	  if(length(year) > 1) stop("Please select a single year")
	  if(!(year %in% Years)) stop(paste("Bad year: available years", paste(Years, collapse = " ")))
	}

	varname <- paste(period, "P", type, sep = "")
	g <- fjord[[varname]]
	if(period != "Global") {
	  if(!is.null(month) & is.null(year)) g <- g[, Months == month]
	  if(is.null(month) & !is.null(year)) g <- g[, Years == year]
	}

	if(PLOT) flplot_Pfunction(fjord$irradianceLevel, g, period, month, year, add = add, ...)

	if(mode == "df") {
	  layername <- paste("P", type, sep = "")
	  if(is.null(month) & is.null(year)) layername <- paste(layername, "Global", sep = "_")
	  if(!is.null(month)) layername <- paste(layername, month.abb[month], sep = "_")
	  if(!is.null(year)) layername <- paste(layername, year, sep = "_")
	  ret <- data.frame(fjord$irradianceLevel, g)
	  names(ret) <- c("irradianceLevel", layername)
	  return(ret)
	}

	if(mode == "function") {
	  f1 <- stats::approxfun(log10(fjord$irradianceLevel), g, rule = 1)
	  f2 <- function(level) f1(log10(level))
	  return(f2)
	}

}
