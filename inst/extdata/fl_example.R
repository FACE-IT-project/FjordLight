##### load the library ####################################################

require(FjordLight)


##### list of available fjords ############################################

fl_ListFjords()


##### download a fjord and load it as a list ###############################

# Choose a fjord
fjord <- "kong"

# default directory is ./FjordLight.d
fl_DownloadFjord(fjord)
fjorddata <- fl_LoadFjord(fjord)
str(fjorddata)

# if you want to download in an other directory use :
#   fl_DownloadFjord(fjord , dirdata = "MyDirectory")
#   fjorddata <- fl_LoadFjord(fjord, dirdata = "MyDirectory")


##### open graphics ########################################################

X11(width = 12, height = 8)
par(mfrow = c(2, 3), mgp = c(2, 1, 0))


##### get bathymetry #######################################################

# all depths (what = "s" ; s for Sea), as raster
bathy <- flget_bathymetry(fjorddata, what = "s", mode = "raster", PLOT = TRUE)
# coastal zone [0-200m] (what = "c" ; c for Coastal), as raster
bathy <- flget_bathymetry(fjorddata, what = "c", mode = "raster", PLOT = TRUE)
# as 3 columns data frame (mode = "3col" : longitude, latitude, depth)
sea <- flget_bathymetry(fjorddata, what = "s", mode = "3col", PLOT = FALSE)
str(sea)
head(sea)
cz <- flget_bathymetry(fjorddata, what = "c", mode = "3col", PLOT = FALSE)
# you may add letter "l" if you want land elevation
sealand <- flget_bathymetry(fjorddata, what = "sl", mode = "3col", PLOT = FALSE)


##### get optics PAR0m kdpar PARbottom #####################################

# PAR0m and PARbottom for July
P07 <- flget_optics(fjorddata, "PAR0m", "Monthly", month = 7, PLOT = TRUE)
print(P07)
Pb7 <- flget_optics(fjorddata, "PARbottom", "Monthly", month = 7, PLOT = TRUE)
print(Pb7)

# PARbottom Global
PbG <- flget_optics(fjorddata, "PARbottom", "Global", PLOT = TRUE)
print(PbG)

# PAR0m and kdpar for year 2012 as 3 columns data frame
P02012 <- flget_optics(fjorddata, "PAR0m", "Yearly", year = 2012, mode = "3col")
k2012 <- flget_optics(fjorddata, "kdpar", "Yearly", year = 2012, mode = "3col")
head(k2012)


##### if you want to group the "3col" data #################################

# first get pixels area
area <- flget_area(fjorddata, mode = "3col")
# then group 3col data
data <- cbind(sea, area[3], P02012[3], k2012[3])
head(data)


##### P-functions ##########################################################
# as a function
fG <- flget_Pfunction(fjorddata, "Global", plot = FALSE)
# then you can use it; for instance :
irradiance_levels <- c(0.1, 1, 10)
fG(irradiance_levels)

# as a 2 column data.frame
f2012 <- flget_Pfunction(fjorddata, "Yearly", year = 2012, mode = "2col")
str(f2012)

# plot of 3 P-functions on the same graph
fGlob <- flget_Pfunction(fjorddata, "Global", PLOT = TRUE, lty = 1, col = 1, lwd = 2, Main = paste(fjord, "P-functions"), ylim = c(0, 50))
f2012 <- flget_Pfunction(fjorddata, "Yearly", year = 2012, PLOT = TRUE, add = TRUE, lty = 1, col = 2, lwd = 2)
fJuly <- flget_Pfunction(fjorddata, "Monthly", month = 7, PLOT = TRUE, add = TRUE, lty = 1, col = 3, lwd = 2)
legend("topleft", legend = c("Global", "2012", "July"), lty = 1, col = 1:3, lwd = 2)

locator()

