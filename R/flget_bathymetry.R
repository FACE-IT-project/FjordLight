flget_bathymetry <- function(fjord, what = "s", mode = "raster", PLOT = FALSE) {
	w <- unlist(strsplit(what, ""))
	if("l" %in% w) LAND <- TRUE else LAND <- FALSE
	if("c" %in% w | "s" %in% w) BATHY <- TRUE else BATHY <- FALSE
	if("c" %in% w) CZ <- TRUE else CZ <- FALSE
	with(fjord, {
	
		proj.lonlat.def = "+init=epsg:4326"
	
		l <- fjord[["land"]]
		l <- raster(list(x = longitude, y = latitude, z = l))
		b <- fjord[["bathymetry"]]
		b <- raster(list(x = longitude, y = latitude, z = b))

		if(BATHY) {
			if(CZ) {vb <- values(b); vb[vb < -200] <- NA; values(b) <- vb}
			if(!CZ) names(b) <- "bathymetry" else names(b) <- "Coastal_Zone"
			if(LAND) {
				r <- b
				vr <- values(r)
				vl <- values(l)
				i <- is.na(vr)
				vr[i] <- vl[i]
				values(r) <- vr
			} else {
				r <- b
			}
			if(PLOT) {
				flplot_bathymetry(b, l, name)
			}
		} else {
			r <- l
			names(r) <- "land"
			if(PLOT) {
				flplot_land(r, b, name)
			}
		}
		crs(r) <- proj.lonlat.def
		if(mode == "raster") {
			return(r)
		}
		if(mode == "3col") {
			dum <- as.data.frame(cbind(xyFromCell(r, 1:ncell(r)), values(r)))
			names(dum) <- c("longitude", "latitude", "depth")
			return(dum)
		}
	})
}
