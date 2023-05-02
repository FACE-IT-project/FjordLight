fl_DownloadFjord <- function(fjord, dirdata = "FjordLight.d") {
	options(timeout = 0)
	urlobsvlfr <- "ftp://ftp.obs-vlfr.fr/pub/gentili/NC_Fjords/"
	fjords <- fl_ListFjords()
	if(! fjord %in% fjords) stop(paste(fjord, "not available"))
	if(! file.exists(dirdata)) {
		dir.create(dirdata)
		cat("directory", dirdata, "created\n")
	}
	ncfile <- paste(fjord, "nc", sep = ".")
	localf <- paste(dirdata, ncfile, sep = "/")
	if(! file.exists(localf)) {
		cat("---> downloading fjord", fjord, "\n")
		download.file(paste(urlobsvlfr, ncfile, sep = "/"), localf)
		cat(fjord, "downloaded in directory", dirdata, "\n")
	} else {
		cat(fjord, "already downloaded in directory", dirdata, "\n")
	}
}
