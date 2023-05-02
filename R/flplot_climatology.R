flplot_climatology <- function(r, l, name, optics, period, month, year) {
	Main <- paste(name, optics, period)
	if(!is.na(month)) Main = paste(name, optics, month.name[month])
	if(!is.na(year)) Main = paste(name, optics, year)
	if(optics == "PAR0m") {
		vr <- values(r)
		brks <- seq(0, 45, 5)
		cols <- cs_BuYlRd(length(brks) - 1)
		text = expression(PAR*"("*0*"-)"~(mol.photons~m^2~day^-1))
		lab.breaks = c("", brks[-c(1, length(brks))], "")
	}
	if(optics == "PARbottom"){
		nr <- names(r)
		vr <- values(r)
		vr[vr <= 0] <- NA
		values(r) <- vr
		r <- log10(r)
		names(r) <- nr
		vr <- values(r)
	       	brks <- seq(-5, 2, 1)
#		cols <- cs_blye(length(brks) - 1)
		cols <- c("#2166AC", "#4393C3", "#92C5DE", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B")
		text = expression(PAR[bottom]~(mol.photons~m^2~day^-1))
		lab.breaks = c("", as.character(10^seq(-4, 1, 1)), "")
	}
	if(optics == "kdpar"){
		vr <- values(r)
		brks <- seq(0.08, 0.5, by = 0.02)
		cols <- cs_BuYlRd(length(brks) - 1)
		text = expression(Kd[PAR]~(m^{-1}))
		lab.breaks = c("", brks[-c(1, length(brks))], "")
	}
	vr[vr < brks[2]] <- brks[1]; vr[vr > brks[length(brks) - 1]] <- brks[length(brks)]; values(r) <- vr
	plot(r, zlim = range(brks),
		breaks = brks, col = cols,
		xlab = "", ylab = "", main = Main, colNA = "transparent",
		legend.width = 1.5, legend.shrink = 1, legend.mar = 10,
		lab.breaks = lab.breaks,
		legend.args = list(text = text, side = 4, cex = 0.75, line = 3.5
		)
	)
	plot(l, add = TRUE, col = grey(0:100/100), legend = FALSE)
}
