# Test all functions in one script to avoid multiple downloads

if (Sys.getenv('NOT_CRAN') == "true") {

  # Downloading -------------------------------------------------------------

  test_that("fl_DownloadFjord error messages signal correctly", {
    expect_error(fl_DownloadFjord(fjord = "banana"), "banana not available")
    expect_error(fl_DownloadFjord(fjord = "test"),
                 "Please provide the pathway to where you would like to download the data.")
    expect_error(fl_DownloadFjord(fjord = "test", dirdata = "mango"),
                 "Please ensure that the chosen directory exists.")
  })

  test_that("fl_DownloadFjord gets the 'test.nc' file only once", {
    test_dl <- fl_DownloadFjord(fjord = "test", tempdir())
    test_dl <- fl_DownloadFjord(fjord = "test", tempdir())
    expect_type(test_dl, "NULL")
  })


  # Loading -----------------------------------------------------------------

  test_that("loading errors are correct", {
    expect_error(fl_LoadFjord("test"),
                 "Please provide the pathway from where you would like to load the data.")
    expect_error(fl_LoadFjord("test", dirdata = "guava"),
                 "Please ensure that the chosen directory exists.")
  })

  test_that("loading without TS works", {
    fl_DownloadFjord(fjord = "test", tempdir())
    test_dat <- fl_LoadFjord("test", dirdata = tempdir())
    expect_type(test_dat, "list")
    expect_equal(length(test_dat$longitude), 7)
    expect_equal(round(test_dat$AreaOfShallowZone), 106)
  })

  test_that("TS loading works", {
    fl_DownloadFjord(fjord = "test", tempdir())
    test_dat <- fl_LoadFjord("test", dirdata = tempdir(), TS = TRUE)
    expect_type(test_dat, "list")
    expect_type(test_dat$MonthlyPARbottom, "double")
  })


  # Area --------------------------------------------------------------------

  test_that("flget_area works", {
    fl_DownloadFjord(fjord = "test", tempdir())
    dat_no_TS <- fl_LoadFjord("test", dirdata = tempdir())
    res_rast <- flget_area(dat_no_TS, mode = "raster")
    res_df <- flget_area(dat_no_TS, mode = "df")
    expect_s4_class(res_rast, "RasterLayer")
    expect_s3_class(res_df, "data.frame")
  })


  # Geo-parameters ----------------------------------------------------------

  test_that("flget_geoparameters works", {
    fl_DownloadFjord(fjord = "test", tempdir())
    dat_no_TS <- fl_LoadFjord("test", dirdata = tempdir())
    res_geo <- flget_geoparameters(dat_no_TS)
    expect_type(res_geo, "double")
  })


  # Bathymetry --------------------------------------------------------------

  test_that("Bathymetry retrieval works correctly", {
    fl_DownloadFjord(fjord = "test", tempdir())
    dat_no_TS <- fl_LoadFjord("test", dirdata = tempdir())

    expect_error(flget_bathymetry(dat_no_TS, what = "banana"),
                 "Ensure that 'what' is one of the following options: 'o', 'ol', 'c', 'cl', 's', 'sl', 'l'")

    expect_s4_class(flget_bathymetry(dat_no_TS, what = "o"), "RasterLayer")
    expect_s4_class(flget_bathymetry(dat_no_TS, what = "c"), "RasterLayer")
    expect_s4_class(flget_bathymetry(dat_no_TS, what = "s", PLOT = TRUE), "RasterLayer")
    expect_s4_class(flget_bathymetry(dat_no_TS, what = "l", PLOT = TRUE), "RasterLayer")
    expect_s4_class(flget_bathymetry(dat_no_TS, what = "ol"), "RasterLayer")
    expect_s4_class(flget_bathymetry(dat_no_TS, what = "cl"), "RasterLayer")
    expect_s4_class(flget_bathymetry(dat_no_TS, what = "sl", PLOT = TRUE), "RasterLayer")

    expect_s3_class(flget_bathymetry(dat_no_TS, what = "l", mode = "df"), "data.frame")
    expect_s3_class(flget_bathymetry(dat_no_TS, what = "o", mode = "df"), "data.frame")
  })


  # PARclim -----------------------------------------------------------------

  test_that("flget_climatology functions correctly", {
    fl_DownloadFjord(fjord = "test", tempdir())
    dat_no_TS <- fl_LoadFjord("test", tempdir())

    expect_s4_class(flget_climatology(dat_no_TS), "RasterLayer")

    expect_error(flget_climatology(dat_no_TS, month = 5, year = 2020),
                 "You have to indicate month or year, not both")

    expect_error(flget_climatology(dat_no_TS, mode = "banana"),
                 "Wrong mode, choose among: 'raster', 'df'")
    expect_error(flget_climatology(dat_no_TS, optics = "papaya"),
                 "Wrong optics, choose among: 'PARbottom', 'PAR0m', 'Kpar'")
    expect_error(flget_climatology(dat_no_TS, period = "mango"),
                 "Wrong period, choose among: 'Clim', 'Yearly', 'Global'")

    # TODO: More testing required
    # expect_type(flget_climatology(dat_no_TS, year = 2012), "character")
    # expect_type(flget_climatology(dat_no_TS, period = "Clim"), "character")
    # expect_type(flget_climatology(dat_no_TS, period = "Clim", month = 1), "character")
    # expect_type(flget_climatology(dat_no_TS, period = "Yearly"), "character")
    # expect_type(flget_climatology(dat_no_TS, period = "Yearly", year = 2000), "character")

    expect_s3_class(flget_climatology(dat_no_TS, period = "Clim", month = 8, mode = "df"), "data.frame")

    # NB: This also tests flplot_PARbottomMonthlyTS
    expect_s4_class(flget_climatology(dat_no_TS, period = "Yearly", year = 2010, PLOT = TRUE), "RasterLayer")
    expect_s4_class(flget_climatology(dat_no_TS, period = "Clim", month = 4, PLOT = TRUE), "RasterLayer")
    expect_s4_class(flget_climatology(dat_no_TS, period = "Clim", optics = "PAR0m",
                                      month = 6, PLOT = TRUE), "RasterLayer")
    expect_s4_class(flget_climatology(dat_no_TS, period = "Clim", optics = "Kpar",
                                      month = 6, PLOT = TRUE), "RasterLayer")
  })


  # PARbottom ---------------------------------------------------------------

  test_that("flget_PARbottomMonthlyTS functions correctly", {
    fl_DownloadFjord(fjord = "test", tempdir())
    dat_no_TS <- fl_LoadFjord("test", dirdata = tempdir())
    dat_TS <- fl_LoadFjord("test", dirdata = tempdir(), TS = TRUE)
    res_rast <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, mode = "raster")
    res_df <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, mode = "df")
    res_df_years <- flget_PARbottomMonthlyTS(dat_TS, month = 4, mode = "df")

    # NB: This also  tests flplot_PARbottomMonthlyTS
    res_plot <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, PLOT = TRUE)

    # TODO: It would probably be better if the default behaviour was an error or warning, not NULL
    # This needs to be changed for CRAN
    expect_null(flget_PARbottomMonthlyTS(dat_no_TS))
    expect_type(flget_PARbottomMonthlyTS(dat_TS, month = 1), "character")
    expect_type(flget_PARbottomMonthlyTS(dat_TS, year = 2000), "character")
    expect_null(flget_PARbottomMonthlyTS(dat_TS, mode = "banana"))
    expect_s4_class(res_rast, "RasterStack")
    expect_s3_class(res_df, "data.frame")
    expect_s3_class(res_df_years, "data.frame")
    expect_s4_class(res_plot, "RasterStack")
  })


  # P-functions -------------------------------------------------------------

  test_that("P-functions error messages signal correctly", {
    fl_DownloadFjord(fjord = "test", tempdir())
    dat_no_TS <- fl_LoadFjord("test", tempdir())

    # TODO: These should rather be errors, not character vectors or NULL
    expect_type(flget_Pfunction(dat_no_TS, month = 4, year = 4), "character")
    expect_null(flget_Pfunction(dat_no_TS, type = "mango"))
    expect_null(flget_Pfunction(dat_no_TS, mode = "banana"))
    expect_null(flget_Pfunction(dat_no_TS, period = "papaya"))
    expect_type(flget_Pfunction(dat_no_TS, period = "Clim"), "character")
    expect_type(flget_Pfunction(dat_no_TS, period = "Clim", month = 1), "character")
    expect_type(flget_Pfunction(dat_no_TS, period = "Yearly"), "character")
    expect_type(flget_Pfunction(dat_no_TS, period = "Yearly", year = 2000), "character")

    res_year <- flget_Pfunction(dat_no_TS, period = "Yearly", year = 2010, mode = "df")
    res_month <- flget_Pfunction(dat_no_TS, period = "Clim", month = 4, mode = "df")
    res_func <- flget_Pfunction(dat_no_TS, period = "Clim", month = 4, mode = "function")

    # NB: This also  tests flplot_Pfunction
    res_plot <- flget_Pfunction(dat_no_TS, year = 2010, mode = "df", PLOT = TRUE)
    res_plot_add <- flget_Pfunction(dat_no_TS, year = 2010, mode = "df", PLOT = TRUE, add = TRUE)

    expect_s3_class(res_year, "data.frame")
    expect_s3_class(res_month, "data.frame")
    expect_type(res_func, "closure")
    expect_s3_class(res_plot, "data.frame")
  })

}
