
test_that("Interp_track looks ok", {

  #testing that storm name and storm id produce identical data frames
  expect_equal(interp_track( "KATRINA", 2005, hurdat ), interp_track( "AL122005", 2005, hurdat ))

  #testing that an error is produced if the storm year is incorrect
  expect_error( interp_track("CAROL", 2018, hurdat) )

  #testing that an error is produced when the storm name is not found
  expect_error( interp_track("INVALID_STORM", 2017, hurdat) )

  #testing that an error is produced when "UNNAMED" is given
  expect_error( interp_track("UNNAMED", 1958, hurdat) )

  #testing that the data frame produced has the correct number of rows
  baker.interp <- interp_track( "baker", 1950, hurdat )
  baker.interp.rows <- nrow( baker.interp )
  baker.original <- hurdat[ which(hurdat$id == "AL021950"), ]
  baker.times <- dplyr::filter(
    baker.original, grepl("0000|0600|1200|1800", UTC.time))
  baker.original.record <- dplyr::filter(
    baker.original, !is.na(record.id) &
      !grepl("0000|0600|1200|1800", UTC.time) )
  value <- (nrow(baker.times)-1)*11 +
    nrow(baker.original) - nrow(baker.original.record)
  expect_equal(baker.interp.rows, value)

})
