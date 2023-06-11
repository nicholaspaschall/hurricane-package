
test_that("Storm Size looks ok", {

  #testing that an error is produced if the storm year is incorrect
  expect_error(storm_size(c("IRENE"), c(2015), hurdat, "2011-08-27 18:00:00") )

  #testing that an error is produced when the storm name is not found
  expect_error(storm_size(c("INVALID_STORM"), c(2017), hurdat, "2011-08-27 18:00:00") )

  #testing that an error is produced when "UNNAMED" is given
  expect_error(storm_size(c("UNNAMED"), c(1958), hurdat, "1851-06-25 00:00:00") )

  #testing that storm_size returns equivalent results for storm id and name
  florence.size1 <- storm_size( c("IVAN"), c(2004), hurdat,
                                "2004-09-04 06:00:00")
  florence.size2 <- storm_size( c("AL092004"), c(2004), hurdat,
                                "2004-09-04 06:00:00")
  expect_equal(florence.size1$data, florence.size2$data)

  #testing that storm_size returns equivalent results for lists of storm id and name
  storms.size1 <- storm_size( c("EMILY", "IVAN", "DANNY"),
                              c(2005, 2004, 2021), hurdat, c("2005-07-15 18:00:00", "2004-09-22 18:00:00", "2021-06-28 00:00:00"))
  storms.size2 <- storm_size( c("AL052005", "AL092004", "AL042021"),
                              c(2005, 2004, 2021), hurdat, c("2005-07-15 18:00:00", "2004-09-22 18:00:00", "2021-06-28 00:00:00"))
  expect_equal( storms.size1$data, storms.size2$data )

})
