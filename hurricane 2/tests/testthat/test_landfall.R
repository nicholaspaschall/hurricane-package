test_that("Landfall looks ok", {

  #testing that an error is produced if the storm year is incorrect
  expect_error(landfall("CAROL", 2018, hurdat) )

  #testing that an error is produced when the storm name is not found
  expect_error(landfall("INVALID_STORM", 2017, hurdat) )

  #testing that an error is produced when "UNNAMED" is given
  expect_error(landfall("UNNAMED", 1958, hurdat) )

  #testing that landfall returns equivalent results for storm name and id inputted
  florence.landfall1 <- landfall( "FLORENCE", 1988, hurdat, method="landfall.check")
  florence.landfall2 <- landfall( "AL071988", 1988, hurdat, method="landfall.check" )
  florence.landfall3 <- landfall( "FLORENCE", 1988, hurdat, method="landfall.data")
  florence.landfall4 <- landfall( "AL071988", 1988, hurdat, method="landfall.data" )
  florence.landfall5 <- landfall( "FLORENCE", 1988, hurdat, method="landfall.strongest")
  florence.landfall6 <- landfall( "AL071988", 1988, hurdat, method="landfall.strongest" )
  expect_equal(florence.landfall1, florence.landfall2)
  expect_equal(florence.landfall3, florence.landfall4)
  expect_equal(florence.landfall5, florence.landfall6)

  #testing that the function returns a boolean when the method inputted is "landfall.check"
  dog<- landfall("DOG", 1950, hurdat, method="landfall.check")
  expect_equal("logical", class(dog))

  #testing that the function returns a data frame when the method inputted is "landfall.data"
  kyle<- landfall("KYLE", 2008, hurdat, method="landfall.data")
  expect_equal("data.frame", class(kyle))

  #testing that the function returns a data frame with expected columns for when the method is "landfall.data"
  expect_identical(colnames(landfall("KATRINA", 2005, hurdat, method="landfall.data")),
                   c("id", "date.time", "latitude", "longitude", "max.wind", "inside"))

  #testing that the function returns a data frame with one row for when the method is "landfall.strongest"
  expect_equal(nrow(landfall("SANDY", 2012, hurdat, method="landfall.strongest")), 1)

  #testing if the function returns the row with the highest max wind speed when method is "landfall.strongest"'
  expect_equal(landfall("OLGA", 2019, hurdat, method="landfall.strongest")$max.wind,
                         max(landfall("OLGA", 2019, hurdat, method="landfall.strongest")$max.wind))


})
