#' Extract p function data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will conveniently extract the p function data stored within a
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}.
#' There are options for how the user would like to subset the data, which data format the
#' data should be extracted to, and if the user would like to plot the data in the process.
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}.
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
#' underlying p function data directly set \code{mode = "2col"}.
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
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download+load data
#' fjord <- "kong"
#' fl_DownloadFjord(fjord, dirdata = "data/PAR")
#' fjorddata <- fl_LoadFjord(fjord, dirdata = "data/PAR")
#'
#' # Create a function
#' fG <- flget_Pfunction(fjorddata, "Global")
#' # Then use it with specific PAR thresholds
#' irradiance_levels <- c(0.1, 1, 10)
#' fG(irradiance_levels)
#'
#' # As a 2 column data.frame
#' f2012 <- flget_Pfunction(fjorddata, "Yearly", year = 2012, mode = "2col", PLOT = TRUE)
#' str(f2012)
#'
#' # plot of 3 P-functions on the same graph
#' fGlob <- flget_Pfunction(fjorddata, "Global", PLOT = TRUE, lty = 1, col = 1, lwd = 2,
#'                          Main = paste(fjord, "P-functions"), ylim = c(0, 50))
#' f2012 <- flget_Pfunction(fjorddata, "Yearly", year = 2012, PLOT = TRUE, add = TRUE,
#'                          lty = 1, col = 2, lwd = 2)
#' fJuly <- flget_Pfunction(fjorddata, "Monthly", month = 7, PLOT = TRUE, add = TRUE,
#'                          lty = 1, col = 3, lwd = 2)
#' legend("topleft", legend = c("Global", "2012", "July"), lty = 1, col = 1:3, lwd = 2)
#' }
#'
flget_Pfunction <- function(fjord, period = "Global", month = NA, year = NA, mode = "function", PLOT = FALSE, add = FALSE, ...) {
	if(!is.na(month) & !is.na(year)) return("You have to indicate month or year, not both")
	available.period <- c("Monthly", "Yearly", "Global")
	available.mode <- c("function", "2col")
	if(! mode %in% available.mode) {
		cat("wrong mode, choose among :", available.mode, "\n")
		return (invisible(NULL))
	}
	if(! period %in% available.period) {
		cat("wrong period, choose among :", available.period, "\n")
		return (invisible(NULL))
	}
	with(fjord, {
		if(period == "Monthly") {
			if(!is.na(month)){
				if(!(month %in% Months)) return(paste("bad month", ": available months", paste(Months, collapse = " ")))
			} else {
				return("you have to indicate the month\n")
			}
		}

		if(period == "Yearly") {
			if(!is.na(year)){
				if(!(year %in% Years)) return(paste("bad year", ": available years", paste(Years, collapse = " ")))
			} else {
				return("you have to indicate the year\n")
			}
		}
		varname <- paste(period, "Pfunction", sep = "")
		g <- fjord[[varname]]
		if(period != "Global") {
			if(!is.na(month) & is.na(year)) g <- g[, Months == month]
			if(is.na(month) & !is.na(year)) g <- g[, Years == year]
		}

		if(PLOT) flplot_Pfunction(irradianceLevel, g, period, month, year, add = add, ...)

		if(mode == "2col") {
			layername <- "Pfunction"
			if(is.na(month) & is.na(year)) layername <- paste(layername, "Global", sep = "_")
			if(!is.na(month)) layername <- paste(layername, month.abb[month], sep = "_")
			if(!is.na(year)) layername <- paste(layername, year, sep = "_")
			ret <- data.frame(irradianceLevel, g)
			names(ret) <- c("irradianceLevel", layername)
			return(ret)
		}
		if(mode == "function") {
			f1 <- approxfun(log10(irradianceLevel), g, rule = 1)
			f2 <- function(level) f1(log10(level))
			return(f2)
		}
	})
}
