
test_that("cyclone_energy looks ok", {

  #testing that an error is produced if the storm year is incorrect
  expect_error(cyclone_energy("CAROL", 2018, hurdat) )

  #testing that an error is produced when the storm name is not found
  expect_error(cyclone_energy("INVALID_STORM", 2017, hurdat) )

  #testing that an error is produced when "UNNAMED" is given
  expect_error(cyclone_energy("UNNAMED", 1958, hurdat) )

  #testing that cyclone_energy returns equivalent maps for storm id and name
  florence.cyclone.energy1 <- cyclone_energy( "FLORENCE", 1988, hurdat)
  florence.cyclone.energy2 <- cyclone_energy( "AL071988", 1988, hurdat)
  expect_equal(florence.cyclone.energy1, florence.cyclone.energy2)

  #testing that the function returns a numeric value
  katrina<- cyclone_energy("KATRINA", 2005, hurdat)
  expect_equal(class(katrina), "numeric")

  #testing that ACE is 0 when max winds are less than 35
  sample <- subset(hurdat, id=="AL051986" & max.wind < 35)
  testthat::expect_equal(cyclone_energy("AL051986", 1986, sample), 0)
})
