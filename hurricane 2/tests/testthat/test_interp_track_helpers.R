
test_that("fill_values looks ok",{

    # Creating a test data frame
    storm_dat <- data.frame(
      max_wind = c(10, NA, NA, 20, NA, NA, NA, 30),
      min_pressure = c(1000, NA, NA, 950, NA, NA, NA, 900)
    )

    filled_vals <- fill_values("max_wind", storm_dat)

    #testing that the output has no missing values
    expect_false(any(is.na(filled_vals)))

    #testing that the output has the correct values
    expect_equal(filled_vals, c(10, 10, 10, 20, 20, 20, 20, 30))

    filled_vals <- fill_values("min_pressure", storm_dat)

    # testing that the output has no missing values
    expect_false(any(is.na(filled_vals)))

    # testing that the output has the correct values
    expect_equal(filled_vals, c(1000, 1000, 1000, 950, 950, 950, 950, 900))

  })



