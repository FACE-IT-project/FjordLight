#' Extract and/or plot bathymetry data from a NetCDF file downloaded by \code{FjordLight}.
#'
#' This functions will conveniently extract the bathymetry data stored within a
#' NetCDF file downloaded via \code{\link{fl_DownloadFjord}}. There are options
#' for how the user would like to subset the data, which data format the data
#' should be extracted to, and if the user would like to plot the data in the process.
#'
#' @param fjord Expects the object loaded via \code{\link{fl_LoadFjord}}.
#' @param what The default value \code{"o"} will load all "ocean" data, meaning it will filter
#' out any land pixels. The other options are: \code{"c"} filters out only coastal bathymetry
#' data (depth of 200 m to 0 m), \code{"s"} filters out only shallow bathymetry data
#' (depth of 50 m to 0 m), and \code{"l"} loads only the land data. One may combine \code{"o"},
#' \code{"c"}, or \code{"s"}, with \code{"l"} (e.g. \code{"ol"}) to load both sea and land data.
#' @param mode Determines the format of the bathymetry data loaded into the R environment.
#' The default \code{"raster"} will load the data as a raster format. The other option \code{"df"}
#' will load the data as a data.frame with three columns.
#' @param PLOT Boolean argument (default = \code{FALSE}) that tells the function if the user
#' would like the loaded bathymetry and/or elevation data to be plotted or not.
#' NB: this argument will only run if the user chooses \code{mode = "raster"}.
#'
#' @return Depending on which arguments the user chooses, this function will return the
#' filtered bathymetry data as a \code{RasterLayer} (\code{mode = "raster"}) or
#' data.frame (\code{mode = "df"}). The data.frame will contain the following columns:
#'   \item{longitude}{degree decimals}
#'   \item{latitude}{degree decimals}
#'   \item{depth}{metres}
#' Note that the depth column will contain both elevation (positive) and depth (negative) values.
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
#' # Shallow data (what = "s"; s for shallow) as a data.frame
#' shallow_df <- flget_bathymetry(fjorddata, what = "s", mode = "df")
#'
#' # Plot all ocean and land data
#' \donttest{
#' full_bathy <- flget_bathymetry(fjorddata, what = "ol", mode = "raster", PLOT = TRUE)
#' }
#'
#' # Plot only land data
#' \donttest{
#' land_only <- flget_bathymetry(fjorddata, what = "l", mode = "raster", PLOT =TRUE)
#' }
#'
#' # For more examples: https://face-it-project.github.io/FjordLight/articles/fl_example.html
#'
flget_bathymetry <- function(fjord,
                             what = "o",
                             mode = "raster",
                             PLOT = FALSE) {

  availwhat <- c("o", "c", "s", "ol", "cl", "sl", "l")
  if(! what %in% availwhat) {
    stop("Ensure that 'what' is one of the following options: 'o', 'ol', 'c', 'cl', 's', 'sl', 'l'")
  }

  w <- unlist(strsplit(what, ""))
  if("l" %in% w) LAND <- TRUE else LAND <- FALSE
  if("s" %in% w | "c" %in% w | "o" %in% w) BATHY <- TRUE else BATHY <- FALSE
  if("c" %in% w) CZ <- TRUE else CZ <- FALSE
  if("s" %in% w) SH <- TRUE else SH <- FALSE

  elev <- fjord[["elevation"]]
  dep <- fjord[["depth"]]
  fjord_name <- fjord[["name"]]

  if(mode == "raster") {
    elev <- raster::raster(list(x = fjord$longitude, y = fjord$latitude, z = elev))
    dep <- raster::raster(list(x = fjord$longitude, y = fjord$latitude, z = dep))
    if(BATHY) {
      names(dep) <- "Site_Depth"
      if(CZ) {
        vb <- raster::values(dep)
        vb[vb < -200] <- NA
        raster::values(dep) <- vb
        names(dep) <- "Coastal_Zone_Depth"
        }
      if(SH) {
        vb <- raster::values(dep)
        vb[vb < -50 ] <- NA
        raster::values(dep) <- vb
        names(dep) <- "Shallow_Zone_Depth"
        }
      if(LAND) {
        r <- dep
        vr <- raster::values(r)
        vl <- raster::values(elev)
        i <- is.na(vr)
        vr[i] <- vl[i]
        raster::values(r) <- vr
      } else {
        r <- dep
      }
      if(PLOT) {
        if(LAND){
          flplot_bathymetry(dep, elev, name = fjord_name)
        } else {
          flplot_bathymetry(dep, name = fjord_name)
        }
      }
    } else {
      r <- elev
      names(r) <- "Land_Elevation"
      if(PLOT) {
        flplot_land(r, fjord_name)
      }
    }
    raster::crs(r) <- 4326
    return(r)
  }

  if(mode == "df"){
    n <- nrow(dep); m <- ncol(dep)
    row_indices <- rep(1:n, each = m)
    col_indices <- rep(1:m, times = n)
    dum_dep <- data.frame(
      longitude = fjord$longitude[row_indices],
      latitude = fjord$latitude[col_indices],
      depth = base::as.vector(t(dep))
    )
    if(CZ) {
      dum_dep$depth[dum_dep$depth <= -200] <- NaN
    }
    if(SH) {
      dum_dep$depth[dum_dep$depth <= -50 ] <- NaN
    }
    if(LAND) {
      n <- nrow(elev); m <- ncol(elev)
      row_indices <- rep(1:n, each = m)
      col_indices <- rep(1:m, times = n)
      dum_elev <- data.frame(
        longitude = fjord$longitude[row_indices],
        latitude = fjord$latitude[col_indices],
        depth = base::as.vector(t(elev))
      )
      dum <- rbind(dum_dep, dum_elev)
      dum <- dum[stats::complete.cases(dum),]
    } else {
      dum <- dum_dep
    }
    return(dum)
  }

}
