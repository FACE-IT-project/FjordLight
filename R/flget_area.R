flget_area <- function(fjord, mode = "raster") {
	with(fjord, {
		proj.lonlat.def = "+init=epsg:4326"
		r <- fjord[["area"]]
		r <- raster(list(x = longitude, y = latitude, z = r))
		names(r) <- "pixelarea"
		crs(r) <- proj.lonlat.def
		if(mode == "raster") {
			return(r)
		}
		if(mode == "3col") {
			dum <- as.data.frame(cbind(xyFromCell(r, 1:ncell(r)), values(r)))
			names(dum) <- c("longitude", "latitude", "PixArea_km2")
			return(dum)
		}
	})
}
