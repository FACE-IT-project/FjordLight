% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fl_colorscales.R
\name{fl_topocolorscale}
\alias{fl_topocolorscale}
\title{Create a colour palette for bathymetry plots.}
\usage{
fl_topocolorscale(v)
}
\arguments{
\item{v}{Expects a single negative numeric value.
This should be the maximum depth of the bathymetry.}
}
\value{
A list containing two vectors. First the numeric breaks (\code{brks}) that will
be shown as different isobath colours. Second the colours (\code{colors}) that will be shown. The
colours are given as hexadecimal values.
}
\description{
This function will create a lovely, evenly spaced colour palette
designed for use with bathymetry plots.
}
\examples{
fl_topocolorscale(-200)

}
\author{
Bernard Gentili
}
