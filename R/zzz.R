.onAttach <- function(lib, pkg) {
	ex.script <- system.file("extdata", "fl_example.R", package = "FjordLight")
	message <- paste("\n### to use FjordLight see (copy) example script", ex.script, "\n###")
	packageStartupMessage(message)
}
