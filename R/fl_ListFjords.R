fl_ListFjords <- function() {
	sub("\\.nc$", "", unlist(strsplit(getURL("ftp://ftp.obs-vlfr.fr/pub/gentili/NC_Fjords/", dirlistonly=TRUE), "\n")))
}
	
