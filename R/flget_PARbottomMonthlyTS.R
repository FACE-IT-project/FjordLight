flget_PARbottomMonthlyTS <- function(fjord, month = NULL, year = NULL, mode = "raster", PLOT = FALSE) {
	if(is.null(fjord$PARbottom)) {
		cat("PARbottom monthly time series not loaded\n")
		return (invisible(NULL))
	}
	available.mode <- c("raster", "3col")
	if(! mode %in% available.mode) {
		cat("wrong mode, choose among :", available.mode, "\n")
		return (invisible(NULL))
	}
	with(fjord, {
		if(!is.null(month)){
			if(!all(month %in% Months)) return(paste("bad month(s)", ": available months", paste(months, collapse = " ")))
		} else {
			month <- Months
		}
	
		if(!is.null(year)){
			if(!all(year %in% Years)) return(paste("bad year(s)", ": available years", paste(years, collapse = " ")))
		} else {
			year <- Years
		}
		varname <- "PARbottom"
	
		proj.lonlat.def = "+init=epsg:4326"
	
		g <- fjord[[varname]]
		s <- stack()
		layernames <- NULL
		for(y in year) {
			for(m in month) {
				h <- g[, , Months == m, Years == y]
				r <- raster(list(x = longitude, y = latitude, z= h))
				layername <- paste(varname, formatC(y, format = "d", width = 4, flag = "0"), formatC(m, format = "d", width = 2, flag = "0"), sep = ".")
				layernames <- append(layernames, layername)
				names(r) <- layername
				s <- stack(s, r)
			}
		}
		if(PLOT) {
			flplot_PARbottomMonthlyTS(s)
		}
		if(mode == "raster") {
			return(s)
		}
		if(mode == "3col") {
			dum <- as.data.frame(cbind(xyFromCell(s, 1:ncell(s)), as.matrix(s)))
			names(dum) <- c("longitude", "latitude", layernames)
			return(dum)
		}
	})
}
