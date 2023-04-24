flget_optics <- function(fjord, optics = "PARbottom", period = "Global", month = NA, year = NA, mode = "raster", PLOT = FALSE) {
	if(!is.na(month) & !is.na(year)) return("You have to indicate month or year, not both")
	available.optics <- c("PARbottom", "PAR0m", "kdpar")
	available.period <- c("Monthly", "Yearly", "Global")
	available.mode <- c("raster", "3col")
	if(! mode %in% available.mode) {
		cat("wrong mode, choose among :", available.mode, "\n")
		return (invisible(NULL))
	}
	if(! optics %in% available.optics) {
		cat("wrong optics, choose among :", available.optics, "\n")
		return (invisible(NULL))
	}
	if(! period %in% available.period) {
		cat("wrong period, choose among :", available.period, "\n")
		return (invisible(NULL))
	}

	with(fjord, {
		if(period == "Monthly")
			if(!is.na(month)){
				if(!(month %in% Months)) return(paste("bad month", ": available months", paste(months, collapse = " ")))
			} else {
				return("you have to indicate the month\n")
			}
	
		if(period == "Yearly")
			if(!is.na(year)){
				if(!(year %in% Years)) return(paste("bad year", ": available years", paste(years, collapse = " ")))
			} else {
				return("you have to indicate the year\n")
			}
		varname <- paste(period, optics, sep = "")
		layername <- optics
		if(is.na(month) & is.na(year)) layername <- paste(layername, "Global", sep = "_")
		if(!is.na(month)) layername <- paste(layername, month.abb[month], sep = "_")
		if(!is.na(year)) layername <- paste(layername, year, sep = "_")
	
		proj.lonlat.def = "+init=epsg:4326"
	
		g <- fjord[[varname]]
		if(!is.na(month) & is.na(year)) g <- g[, , Months == month]
		if(is.na(month) & !is.na(year)) g <- g[, , Years == year]
		r <- raster(list(x = longitude, y = latitude, z= g))
		names(r) <- layername
		if(PLOT) {
			l <- fjord[["land"]]
			l <- raster(list(x = longitude, y = latitude, z = l))
			flplot_optics(r, l, name, optics, period, month, year)
		}
		if(mode == "raster") {
			return(r)
		}
		if(mode == "3col") {
#			dum <- as.data.frame(rasterToPoints(r))
#			names(dum)[1:2] <- c("longitude", "latitude")
			dum <- as.data.frame(cbind(xyFromCell(r, 1:ncell(r)), values(r)))
			names(dum) <- c("longitude", "latitude", layername)
			return(dum)
		}
	})
}
