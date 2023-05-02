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
