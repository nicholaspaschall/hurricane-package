

test_that("Track map looks ok", {

  #testing that an error is produced if the storm year is incorrect
  expect_error( track_map(c("CAROL"), c(2018), hurdat) )

  #testing that an error is produced when the storm name is not found
  expect_error( track_map(c("INVALID_STORM"), c(2017), hurdat) )

  #testing that an error is produced when "UNNAMED" is given
  expect_error( track_map(c("UNNAMED"), c(1958), hurdat) )

  #testing that track_map returns a ggplot object
  expect_true( ggplot2::is.ggplot(track_map(c("CLAUDETTE"), c(2021), hurdat)) )

  #testing that track_map returns equivalent maps for storm id and name
  florence.track1 <- track_map( c("FLORENCE"), c(1988), hurdat )
  florence.track2 <- track_map( c("AL071988"), c(1988), hurdat )
  expect_equal(florence.track1$data, florence.track2$data)

  #testing that track_map returns equivalent maps for lists of storm id and name
  storms.track1 <- track_map( c("EMILY", "IVAN", "DANNY"),
                              c(1993, 2004, 2009), hurdat)
  storms.track2 <- track_map( c("AL051993", "AL092004", "AL052009"),
                              c(1993, 2004, 2009), hurdat)
  expect_equal( storms.track1$data, storms.track2$data )

})
