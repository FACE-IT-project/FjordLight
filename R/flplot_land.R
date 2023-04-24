flplot_land <- function(r, b, name) {
	vr <- values(r)
	plot(r, col = terrain.colors(255),
		colNA = "transparent", main = paste(name, names(r)),
		legend.width = 1.5, legend.shrink = 1, legend.mar = 10,
		legend.args = list(text = "",
			side = 4, cex = 1.5, line = 3.5)
	)
	plot(b, add = TRUE, col = grey(0:100/100), legend = FALSE)
}
