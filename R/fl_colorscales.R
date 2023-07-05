#' Create a colour palette for bathymetry plots.
#'
#' This function will create a lovely, evenly spaced colour palette
#' designed for use with bathymetry plots.
#'
#' @param v Expects a single negative numeric value.
#' This should be the maximum depth of the bathymetry.
#'
#' @return A list containing two vectors. First the numeric breaks (\code{brks}) that will
#' be shown as different isobath colours. Second the colours (\code{colors}) that will be shown. The
#' colours are given as hexadecimal values.
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' fl_topocolorscale(-200)
#'
fl_topocolorscale <- function(v) {
	dvm <- unique(diff(pretty(v[v <= 0], n = 10)))[1]
	vm <- max(-v[v <= 0], na.rm = TRUE)
	brks <- -rev(seq(0, dvm * ((0.999999*vm) %/% dvm + 1), dvm))
	colors <- cs_blue(length(brks) - 1)
	list(brks = brks, colors = colors)
}

#' Create a colour palette of blues.
#'
#' This function will create a lovely, evenly spaced colour palette.
#'
#' @param n The number of colours of blue to return.
#'
#' @return A character vector of hexadecimal values.
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' cs_blue(10)
#'
cs_blue <- function(n) {
	cols <- c("#213f77", "#2154a2", "#1b69d0", "#0080ff", "#0080ff", "#6093ff", "#89a8ff", "#abbdff", "#c8d2ff", "#e4e8ff")
	rgb.list <- grDevices::col2rgb(cols)/255
	l <- length(cols)
	r <- stats::approx(1:l, rgb.list[1, 1:l], xout = seq(1, l, length.out = n))$y
	g <- stats::approx(1:l, rgb.list[2, 1:l], xout = seq(1, l, length.out = n))$y
	b <- stats::approx(1:l, rgb.list[3, 1:l], xout = seq(1, l, length.out = n))$y
	res <- grDevices::rgb(r, g, b)
	res
}

#' Create a colour palette from blues to reds.
#'
#' This function will create a lovely, evenly spaced colour palette.
#'
#' @param n The number of colours of blue to red to return.
#'
#' @return A character vector of hexadecimal values.
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' cs_BuYlRd(10)
#'
cs_BuYlRd <- function(n) {
	cols <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
	rgb.list <- grDevices::col2rgb(cols)/255
	l <- length(cols)
	r <- stats::approx(1:l, rgb.list[1, 1:l], xout = seq(1, l, length.out = n))$y
	g <- stats::approx(1:l, rgb.list[2, 1:l], xout = seq(1, l, length.out = n))$y
	b <- stats::approx(1:l, rgb.list[3, 1:l], xout = seq(1, l, length.out = n))$y
	res <- grDevices::rgb(r, g, b)
	res
}

#' Create a colour palette from blues to yellows.
#'
#' This function will create a lovely, evenly spaced colour palette.
#'
#' @param n The number of colours of blue to yellow to return.
#'
#' @return A character vector of hexadecimal values.
#'
#' @author Bernard Gentili
#'
#' @export
#'
#' @examples
#' cs_blye(10)
#'
cs_blye <- function(n) {
	cols <- c("#0080ff", "#7492e2", "#9da6c4", "#b9bba5", "#ced183", "#e0e85b", "#eeff00")
	rgb.list <- grDevices::col2rgb(cols)/255
	l <- length(cols)
	r <- stats::approx(1:l, rgb.list[1, 1:l], xout = seq(1, l, length.out = n))$y
	g <- stats::approx(1:l, rgb.list[2, 1:l], xout = seq(1, l, length.out = n))$y
	b <- stats::approx(1:l, rgb.list[3, 1:l], xout = seq(1, l, length.out = n))$y
	res <- grDevices::rgb(r, g, b)
	res
}
