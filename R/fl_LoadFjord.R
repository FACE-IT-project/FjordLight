fl_LoadFjord <- function(fjord, dirdata = "FjordLight.d", TS = FALSE, verbose = FALSE) {
	### read data from ncfile
	ncfile <- paste(dirdata, paste(fjord, "nc", sep = "."), sep = "/")
	nc <- nc_open(ncfile, verbose = verbose)
	dims <- names(nc$dim)
	vars <- names(nc$var)
	if(!TS) vars <- vars[! vars %in% "PARbottom"]
	vars_attributes <- list()
	for(d in dims) {
		assign(d, ncvar_get(nc, d))
		at <- list(ncatt_get(nc, d))
		names(at) <- d
		vars_attributes <- c(vars_attributes, at)
	}
	for(v in vars) {
		assign(v, ncvar_get(nc, v))
		at <- list(ncatt_get(nc, v))
		names(at) <- v
		vars_attributes <- c(vars_attributes, at)
	}
	glob_attributes <- ncatt_get(nc, 0)
	nc_close(nc)
	name <- fjord
	l <- as.list(mget(c("name", dims, vars, "vars_attributes", "glob_attributes")))
	return(l)
}
