#' Extract monthly K_PAR data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will extract the monthly K_PAR data stored within a K_PAR
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}. Note that these data are
#' very large. If one would prefer to work with the smaller annual or monthly climatology
#' values, instead use \code{\link{flget_climatology}}. There are options for how the user
#' would like to subset the data, which data format the data should be extracted to,
#' and if the user would like to plot the data in the process.
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}. NB: when loading
#' the data one must set the argument \code{fl_LoadFjord(..., TS = TRUE)}. See examples below.
#' @param month The monthly values to extract. Accepts one or many integer values from 3 to 10.
#' If no values are provided, the default value of \code{NULL} will be passed to the function,
#' telling it to load all available months of data (i.e. 3:10).
#' This is used in combination with \code{year} to determine which monthly data to extract.
#' @param year The years of data to extract. Currently accepts one or many integer values from 2003 to 2022.
#' If no values are provided, the default value of \code{NULL} will be passed to the function,
#' telling it to load all available years of data (i.e. currently 2003:2022).
#' This is used in combination with \code{month} to determine which monthly data to extract.
#' @param mode Determines the format of the data loaded into the R environment.
#' The default \code{"raster"} will load the data as a raster format. The other option \code{"df"}
#' will load the data as a data.frame with three columns.
#' @param PLOT Boolean argument (default = \code{FALSE}) that tells the function if the user
#' would like the loaded data to be plotted or not.
#'
#' @return Depending on which arguments the user chooses, this function will return the
#' chosen monthly K_PAR data as a \code{RasterStack} (\code{mode = "raster"})
#' or data.frame (\code{mode = "df"}). The data.frame will contain the following columns:
#'   \item{longitude}{degree decimals}
#'   \item{latitude}{degree decimals}
#'   \item{PARbottom}{mol photons m-2 d-1}
#'
#' @author Bernard Gentili & Robert Schlegel
#'
#' @export
#'
#' @examples
#' # Load ALL data
#' fjord_code <- "test"
#' fjorddata <- fl_LoadFjord(fjord_code,
#'                          dirdata = system.file("extdata", package = "FjordLight"),
#'                          TS = TRUE, layer = "K_PAR")
#'
#' # Load a small subset as a data.frame
#' mts_single <- flget_KPARMonthlyTS(fjorddata, month = 6, year = 2016, mode = "df", PLOT = FALSE)
#'
#' # Years 2003 to 2004 - months July to August
#' # NB: This may be too large for smaller laptops
#' \donttest{
#' mts_many <- flget_KPARMonthlyTS(fjorddata, month = 7:8, year = 2003:2004, PLOT = FALSE)
#'
#' # May also plot the data
#' mts_plot <- flget_KPARMonthlyTS(fjorddata, month = 6:9, year = 2010, PLOT = TRUE)
#' }
#'
#' # For more examples: https://face-it-project.github.io/FjordLight/articles/fl_example.html
#'
flget_KPARMonthlyTS <- function(fjord,
                                month = NULL,
                                year = NULL,
                                mode = "raster",
                                PLOT = FALSE) {

  Years <- fjord$Years
  Months <- fjord$Months

  if(is.null(fjord$MonthlyKpar)) stop("MonthlyKpar time series not loaded")
  available.mode <- c("raster", "df")
  if(! mode %in% available.mode) stop("Wrong mode, choose among: 'raster', 'df'")

  if(!is.null(month)){
    if(!all(month %in% Months)) stop(paste("Bad month(s)", ": available months", paste(Months, collapse = " ")))
  } else {
    month <- Months
  }

  if(!is.null(year)){
    if(!all(year %in% Years)) stop(paste("Bad year(s)", ": available years", paste(Years, collapse = " ")))
  } else {
    year <- Years
  }

  g <- fjord[["MonthlyKpar"]]
  s <- raster::stack()
  layernames <- NULL
  for(y in year) {
    for(m in month) {
      h <- g[, , Months == m, Years == y]
      # if(!is.matrix(h)) h <- h[,,1]
      r <- raster::raster(list(x = fjord$longitude, y = fjord$latitude, z = h))
      layername <- paste("MonthlyKpar", formatC(y, format = "d", width = 4, flag = "0"),
                         formatC(m, format = "d", width = 2, flag = "0"), sep = ".")
      layernames <- append(layernames, layername)
      names(r) <- layername
      s <- raster::stack(s, r)
    }
  }

  raster::crs(s) <- 4326

  if(PLOT) {
    flplot_KPARMonthlyTS(s)
  }

  if(mode == "raster") {
    return(s)
  }

  if(mode == "df") {
    dum <- as.data.frame(cbind(raster::xyFromCell(s, 1:raster::ncell(s)), raster::as.matrix(s)))
    names(dum) <- c("longitude", "latitude", layernames)
    return(dum)
  }

}
