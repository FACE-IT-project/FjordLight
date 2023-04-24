flplot_bathymetry <- function(r, l, name) {
	vr <- values(r)
	brco <- fl_topocolorscale(vr)
	plot(r, breaks = brco$brks, col = brco$colors,
		colNA = "transparent", main = paste(name, names(r)),
		legend.width = 1.5, legend.shrink = 1, legend.mar = 10,
		legend.args = list(text = "",
			side = 4, cex = 1.5, line = 3.5)
	)
	plot(l, add = TRUE, col = grey(0:100/100), legend = FALSE)
}
