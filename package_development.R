# package_development.R
# Monday, October 23rd, 2023
# The code used to develop the FjordLight package


# Getting started ---------------------------------------------------------

# These packages are centrally important
library(devtools)
library(usethis)
library(testthat)
library(checkhelper)
library(pkgdown)
library(qpdf) # Seems to be required for vignette
# For examples: https://thinkr-open.github.io/checkhelper/

# Disable package build note about not finding local time
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)

# edit_r_profile()
# create_package("FjordLight")

# For the hex sticker
library(openai) # For interfacing with DALL-E
library(magick) # For visualising and saving results
library(showtext) # For adding text
library(cropcircles) # For creating the hex sticker

# For creating the final hex
library(ggplot2)
library(ggpath)
library(ggtext)
library(glue)


# Next steps --------------------------------------------------------------

# After running the above line your new package skeleton will be created
# Up next one needs to edit the DESCRIPTION file

# Then run this to create license:
options(usethis.full_name = "Robert William Schlegel")
use_mit_license()

# Then run this to create a function skeleton:
use_r(name = "example_name")

# One must still then go about writing the function

# Once you've made a bit, run this to check the package:
load_all()

# To insert Roxygen skeleton for function go: code -> insert Roxygen skeleton

# After finishing the roxygen skeleton run:
document()
# This creates the documentation

# Then click "Install and Restart" in the build tab
# This will also load your new package

# Once built, set up your Git credentials:
use_git_config(user.name = "Robert William Schlegel", user.email = "robwschlegel@gmail.com")

# Then run this to create the other bits Git will need:
use_git()

# Finally, to push it to your Github as a new repo first start a new repo on
# your GitHub account with the exact same name.
# Do not add a README file or anything else. Leave it completely bare
# Then scroll down on the newly created landing page and click on the
# copy button for how to connect a remote repo.
# This will give you two lines of command line code
# Paste them into a terminal that is pointed at the home directory for the package
# This will then connect and auto push everything to your GitHub account

# Then run this to create the overall documentation for your package:
use_package_doc()
# And again run this to update it all:
document()

# Use the following line to add a package to the import:
use_package(package = "example_package")
# This makes sure your external dependencies are in order
# Or just add it manually to DESCRIPTION

# Add the following line to function documentation to ensure use of a needed function:
#' @importFrom magrittr "%>%"
# But only do this after you've already added the skeleton as detailed above


# Testing -----------------------------------------------------------------

# To begin testing first run:
use_testthat()

# Then to test a function:
use_test("flget_KPARMonthlyTS")

# After creating this documentation one will need to edit it
# Example:

# test_that("make_whole() returns a dataframe", {
#   expect_is(make_whole(), "data.frame")
# })

# Always write tests immediately as you almost certainly won't do it later

# Give it all a thorough look over with:
devtools::check()
# One must have zero errors, warnings, notes

# A better way to check as CRAN
checkhelper::check_as_cran()

# Check if any files are created during check
checkhelper::check_clean_userspace()


# Importing data ----------------------------------------------------------

# First run:
use_data_raw()

# Then go and throw the data in the "data-raw" folder that was created

# Then run this on the data in question after loading it:
# load("data-raw/sst_Med.RData")
# use_data(sst_Med)
# load("data-raw/sst_NW_Atl.RData")
# use_data(sst_NW_Atl)
# load("data-raw/sst_WA.RData")
# use_data(sst_WA)

# After doing so one must manually create an Rscript for these data
# There is currently no shortcut for this
use_r(name = "example_dataset")

# Once this has been done run:
# document()


# Additional development --------------------------------------------------

# First add a read me file with:
use_readme_md()

# Run this in console with the project open to set up GitHub actions
usethis::use_github_action(name = "check-standard")

# Pop in your build status from GitHub actions by copying the output from the above line
# [![R-CMD-check](https://github.com/face-it-project/FjordLight/workflows/R-CMD-check/badge.svg)](https://github.com/face-it-project/FjordLight/actions)

# Add a coverage status badge
use_coverage(type = "codecov")
# Looks like:
# [![Coverage status](https://codecov.io/gh/face-it-project/FjordLight/branch/master/graph/badge.svg)](https://codecov.io/github/face-it-project/FjordLight?branch=master)

# Then add a code of conduct:
use_code_of_conduct(contact = "robert.schlegel@imev-mer.fr")

# Once the package is on CRAN
# [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FjordLight)](https://cran.r-project.org/package=FjordLight)


# Vignettes ---------------------------------------------------------------

# Now to create some better documentation
use_vignette(name = "fl_example")


# pkgdown -----------------------------------------------------------------

# Start with this to build skeleton:
use_pkgdown()

# Then run:
pkgdown::build_site()

# After that, spruce things up a bit:
sink("_pkgdown.yml")
template_navbar()
template_reference()
template_articles()
sink()

# Don't forget that after you make any changes you must run this then push again:
build_site()
# build_news() # See following section
# build_article("complex_clims")


# News --------------------------------------------------------------------

# Add a news tracking file with
use_news_md()

# I did not find the following options to be helpful, decided not to use them
# Better off manually managing the NEWS and DESCRIPTION files
# When you want to assign a version number to the current state of your R package, call
# fledge::bump_version("patch")
# # OR
# fledge::bump_version("minor")
# # OR
# fledge::bump_version("major")
#
# fledge::finalize_version()
#
# fledge::tag_version(force = TRUE)


# Hex sticker -------------------------------------------------------------

## Get API key for DALL-E
# Follow instructions here:
# https://irudnyts.github.io/openai/

# Save key in a private folder and load for use
load("~/pCloudDrive/R/openAI_key.RData")

Sys.setenv(
  OPENAI_API_KEY = openAI_key
)

# If you run out of credits, try HotPot AI: https://hotpot.ai/art-generator?s=site-menu
# There is a limit of 10 free images per day

## Create image

# An idea...
x <- create_image('Light entering an Arctic fjord.')

# What do we have?
# NB: Images don't last forever. Save them locally if you want to keep them.
x1_local <- image_read(x$data$url)

# Save it
image_write(image = x1_local, path = "~/pCloudDrive/R/images/fjord_light.png")

## Prep text

# See all possible fonts from Google
font_families_google()

# choose a font from Google Fonts
font_add_google("Bangers", "bangers")
font_add_google("Roboto", "rob")
font_add_google("Baskervville", "bask")
font_add_google("Fuzzy Bubbles", "fuzz")
font_add_google("Raleway", "rale")
showtext_auto()
ft <- "bangers"
ft1 <- "rale"
txt <- "white"

# fontawesome fonts (optional - this adds the git logo - download from https://fontawesome.com/)
font_add("fa-brands", regular = "~/pCloudDrive/R/fonts/fontawesome-free-6.4.0-web/webfonts/fa-brands-400.ttf")

# package name and github repo
pkg_name <- "Fjord\nLight"
git_name <- "face-it-project/FjordLight"

## Crop image

# Load image as necessary
x_image <- image_read("~/pCloudDrive/R/images/fjord_light.png")

# Hex crop
img_cropped <- hex_crop(
  images = x_image,
  border_colour = "grey20",
  border_size = 24,
  just = "top"
)

## Plot and save
# NB: Limits are fixed between 0 and 1 to make it easy to position text and other elements

# Plot
ggplot() +

  # The image
  geom_from_path(aes(0.5, 0.5, path = img_cropped)) +

  # package name
  ## Shadow text
  # annotate("text", x = 0.12, y = 0.55, label = pkg_name, family = ft1, size = 24,
  #          fontface = "bold", colour = "black", angle = 0, hjust = 0, lineheight = 0.25) +
  ## Main text
  # annotate("text", x = 0.32, y = 0.2, label = pkg_name, family = ft1, size = 16,
  #          fontface = "bold", colour = "steelblue3", angle = 0, hjust = 0, lineheight = 0.25) +

  annotate("text", x = 0.255, y = 0.25, label = "Fjord", family = ft1, size = 14,
           fontface = "bold", colour = "grey20", angle = 0, hjust = 0, lineheight = 0.25) +
  annotate("text", x = 0.355, y = 0.16, label = "Light", family = ft1, size = 8,
           fontface = "bold", colour = "white", angle = 0, hjust = 0, lineheight = 0.25) +

  # add github directory + logo - remove if not wanted
  # annotate("richtext", x = 0.05, y = 0.28, family = ft1, size = 5.5, angle = -31, colour = txt, hjust = 0,
  #          label = glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;&nbsp;</span> {git_name}"),
  #          label.color = NA, fill = NA) +

  # Remove themes and set limits
  xlim(0, 1) + ylim(0, 1) + theme_void() + coord_fixed()

# Play with width/height to get text to fit accordingly
ggsave("logo.png", height = 2, width = 2)


# Package logs ------------------------------------------------------------

# An example of how to check package downloads
ggplot2_logs <- cranlogs::cran_downloads(packages = c("ggplot2", "dplyr"),
                                         when = "last-month")
ggplot2::ggplot(ggplot2_logs) +
  ggplot2::geom_line(aes(date, count, col = package)) +
  viridis::scale_color_viridis(discrete = TRUE)

# Many other options for package analytics


# Submitting to CRAN ------------------------------------------------------

# Some good additional advice may be found here:
# http://kbroman.org/pkg_primer/pages/cran.html

# Run check and make sure there are no ERROR, WARNING, or NOTE

# After that run the following command to check the package on a
# Windows or Mac OS if you are not currently running on that
devtools::check_win_release()
devtools::check_mac_release()

# Or check specific CRAN flavours via the rhub package
# https://blog.r-hub.io/2019/04/25/r-devel-linux-x86-64-debian-clang/
# platforms_df <- as.data.frame(rhub::platforms())

# Local check - Requires bash and Docker
# rhub::local_check_linux(image = "rhub/debian-gcc-devel")

# Online check
rhub::check(platform = "debian-gcc-devel")

# Then use the gear button in the top right pane to 'Build Source Package'

# The source package should be submitted via this web form:
# http://xmpalantir.wu.ac.at/cransubmit/


# Publishing version ------------------------------------------------------

# Once the new version is up on CRAN it is time to re-publish this in a couple of places

# Up first is the Journal of open-source software (JOSS)
# http://joss.theoj.org
# Actually, it looks like this doesn't need to be updated with new releases as it is
# pointed at the following source, which does need updating...

# Second is to publish the new version on GitHub
# Instructions for how to do so are here:
# https://help.github.com/articles/creating-releases/

# This then should automagically update the version listed on zenodo
# https://zenodo.org/
# If not then follow these instructions:
# http://help.zenodo.org/#versioning
# Or just go to the Zenodo website for heatwaveR and click the big green button 'New version'


# Speed tests -------------------------------------------------------------

library(FjordLight)
library(profvis)
profvis(fjorddata <- fl_LoadFjord("kong", TS = TRUE))

