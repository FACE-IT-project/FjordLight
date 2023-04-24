flplot_Pfunction <- function(irrLev, g, period, month, year, Main = NULL, add = add, ...) {
	if(is.null(Main)) {
		Main <- paste("Pfunction", period)
		if(!is.na(month)) Main = paste("Pfunction", month.name[month])
		if(!is.na(year)) Main = paste("Pfunction", year)
	}
	xlab <- expression(E~"mol photons"~m^{-2}~d^{-1})
	ylab <- expression("% of the surface receiving more than E")
	if(add) {
		lines(irrLev, g, ...)
	} else {
		plot(irrLev, g, xlim = rev(range(irrLev)), xlab = xlab, ylab = ylab, main = Main, log = "x", type = "l", ...)
	}
}
