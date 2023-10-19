require(FjordLight)

##### Caveat emptor
###
# With 64 GiB of RAM you should have no problem, even with the most extensive fjords.
# With 4 GiB you can only handle the smaller fjords: kong, young, nuup
# However if you do not need the monthly PARbottom time series, the package should work with all fjords.
# In this case, when calling the fl_LoadFjord() function, set the "TS" argument to FALSE (this is the default value).
###
# make sure that the directory used by the "raster" package for temporary files is large enough,
# otherwise you can use the "tmpdir" option; for example :
# rasterOptions(tmpdir="/A/Bigger/Place")

##### list of available fjords
fl_ListFjords()

##### download a fjord and load it as a list ###############################
# default directory is ./FjordLight.d
fjord <- "kong"

fl_DownloadFjord(fjord)
WANT_TIME_SERIES <- TRUE
fjorddata <- fl_LoadFjord(fjord, TS = WANT_TIME_SERIES)
str(fjorddata)

# if you want to download in an other directory use :
#   fl_DownloadFjord(fjord , dirdata = "MyDirectory")
#   fjorddata <- fl_LoadFjord(fjord, dirdata = "MyDirectory")

##### open graphics ########################################################
X11(width = 12, height = 8)
par(mfrow = c(2, 4), mgp = c(2, 1, 0))

##### get bathymetry #######################################################

# all depths (what = "o" ; o for Ocean), as raster
bathy <- flget_bathymetry(fjorddata, what = "o", mode = "raster", PLOT = TRUE)
# coastal zone [0-200m] (what = "c" ; c for Coastal), as raster
bathy <- flget_bathymetry(fjorddata, what = "c", mode = "raster", PLOT = TRUE)
# shallow zone [0-50m] (what = "sl" ; s for Shallow, l to add land), as raster
bathy <- flget_bathymetry(fjorddata, what = "sl", mode = "raster", PLOT = TRUE)
# as 3 columns data frame (mode = "3col" : longitude, latitude, depth)
sea <- flget_bathymetry(fjorddata, what = "s", mode = "3col", PLOT = FALSE)
str(sea)
head(sea)
cz <- flget_bathymetry(fjorddata, what = "c", mode = "3col", PLOT = FALSE)
# you may add letter "l" if you want land elevation
sealand <- flget_bathymetry(fjorddata, what = "sl", mode = "3col", PLOT = FALSE)

##### get climatologies of PAR0m Kpar PARbottom #####################################

# PAR0m and PARbottom for July
P07 <- flget_climatology(fjorddata, "PAR0m", "Clim", month = 7, PLOT = TRUE)
print(P07)
Pb7 <- flget_climatology(fjorddata, "PARbottom", "Clim", month = 7, PLOT = TRUE)
print(Pb7)

# PARbottom Global
PbG <- flget_climatology(fjorddata, "PARbottom", "Global", PLOT = TRUE)
print(PbG)

# PAR0m and Kpar for year 2012 as 3 columns data frame
P02012 <- flget_climatology(fjorddata, "PAR0m", "Yearly", year = 2012, mode = "3col")
k2012 <- flget_climatology(fjorddata, "Kpar", "Yearly", year = 2012, mode = "3col")
head(k2012)

##### if you want to group the "3col" data #################################
# first get pixels area
area <- flget_area(fjorddata, mode = "3col")
# then group 3col data
data <- cbind(sea, area[3], P02012[3], k2012[3])
head(data)

##### P-functions ##########################################################
# as a function
fG <- flget_Pfunction(fjorddata, period = "Global", plot = FALSE)
# then you can use it; for instance :
irradiance_levels <- c(0.1, 1, 10)
fG(irradiance_levels)

# as a 2 column data.frame
f2012 <- flget_Pfunction(fjorddata, period = "Yearly", year = 2012, mode = "2col")
str(f2012)

# plot of 3 coastal P-functions on the same graph
fGlob <- flget_Pfunction(fjorddata, type = "coastal", period = "Global", PLOT = TRUE, lty = 1, col = 1, lwd = 2, Main = paste(fjord, "coastal P-functions"), ylim = c(0, 50))
f2012 <- flget_Pfunction(fjorddata, type = "coastal", period = "Yearly", year = 2012, PLOT = TRUE, add = TRUE, lty = 1, col = 2, lwd = 2)
fJuly <- flget_Pfunction(fjorddata, type = "coastal", period = "Clim", month = 7, PLOT = TRUE, add = TRUE, lty = 1, col = 3, lwd = 2)
legend("topleft", legend = c("Global", "2012", "July"), lty = 1, col = 1:3, lwd = 2)

# plot of 3 shallow P-functions on the same graph
fGlob <- flget_Pfunction(fjorddata, type = "shallow", period = "Global", PLOT = TRUE, lty = 1, col = 1, lwd = 2, Main = paste(fjord, "shallow P-functions"), ylim = c(0, 70))
f2012 <- flget_Pfunction(fjorddata, type = "shallow", period = "Yearly", year = 2012, PLOT = TRUE, add = TRUE, lty = 1, col = 2, lwd = 2)
fJuly <- flget_Pfunction(fjorddata, type = "shallow", period = "Clim", month = 7, PLOT = TRUE, add = TRUE, lty = 1, col = 3, lwd = 2)
legend("topleft", legend = c("Global", "2012", "July"), lty = 1, col = 1:3, lwd = 2)

# get geographic parameters (position, surfaces)
flget_geoparameters(fjorddata)

if(WANT_TIME_SERIES) {
  ##### PARbottom Monthly time series ########################################
  X11(width = 12, height = 8)
  # all months - years 2011 2012
  mts <- flget_PARbottomMonthlyTS(fjorddata, year = 2011:2012, PLOT = TRUE)
  print(mts)
  X11(width = 12, height = 8)
  # all years - months July August
  mts <- flget_PARbottomMonthlyTS(fjorddata, month = 7:8, PLOT = TRUE)
  print(mts)
  X11(width = 12, height = 8)
  # years 2003 to 2012 - months July August
  mts <- flget_PARbottomMonthlyTS(fjorddata, month = 7:8, year = 2003:2012, PLOT = TRUE)
  print(mts)
  # all months - all years - as data.frame : #columns = #months (8, March to October) * #years (20, 2003 to 2022) + 2 (lon lat) = 162
  mts_full <- flget_PARbottomMonthlyTS(fjorddata, mode = "3col")
  str(mts_full, max.level = 0)
  print(names(mts_full))
}

locator()
