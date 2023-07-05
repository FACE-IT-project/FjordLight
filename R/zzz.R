.onAttach <- function(lib, pkg) {
	ex.script <- system.file("extdata", "fl_example.R", package = "FjordLight")
	message <- paste("\n###\nAn example script for how to use FjordLight may be found at: ", ex.script, "\n###")
	packageStartupMessage(message)
}
