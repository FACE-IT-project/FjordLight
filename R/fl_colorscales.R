fl_topocolorscale <- function(v) {
	dvm <- unique(diff(pretty(v[v <= 0], n = 10)))[1]
	vm <- max(-v[v <= 0], na.rm = TRUE)
	brks <- -rev(seq(0, dvm * ((0.999999*vm) %/% dvm + 1), dvm))
	colors <- cs_blue(length(brks) - 1)
	list(brks = brks, colors = colors)
}
cs_blue <- function(n) {
	cols <- c("#213f77", "#2154a2", "#1b69d0", "#0080ff", "#0080ff", "#6093ff", "#89a8ff", "#abbdff", "#c8d2ff", "#e4e8ff")
	rgb.list <- col2rgb(cols)/255
	l <- length(cols)
	r <- approx(1:l, rgb.list[1, 1:l], xout = seq(1, l, length.out = n))$y
	g <- approx(1:l, rgb.list[2, 1:l], xout = seq(1, l, length.out = n))$y
	b <- approx(1:l, rgb.list[3, 1:l], xout = seq(1, l, length.out = n))$y
	res <- rgb(r, g, b)
	res
}
cs_BuYlRd <- function(n) {
	cols <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")

	rgb.list <- col2rgb(cols)/255
	l <- length(cols)
	r <- approx(1:l, rgb.list[1, 1:l], xout = seq(1, l, length.out = n))$y
	g <- approx(1:l, rgb.list[2, 1:l], xout = seq(1, l, length.out = n))$y
	b <- approx(1:l, rgb.list[3, 1:l], xout = seq(1, l, length.out = n))$y
	res <- rgb(r, g, b)
	res
}
cs_blye <- function(n) {
	cols <- c("#0080ff", "#7492e2", "#9da6c4", "#b9bba5", "#ced183", "#e0e85b", "#eeff00")
	rgb.list <- col2rgb(cols)/255
	l <- length(cols)
	r <- approx(1:l, rgb.list[1, 1:l], xout = seq(1, l, length.out = n))$y
	g <- approx(1:l, rgb.list[2, 1:l], xout = seq(1, l, length.out = n))$y
	b <- approx(1:l, rgb.list[3, 1:l], xout = seq(1, l, length.out = n))$y
	res <- rgb(r, g, b)
	res
}
