test_that("distance_coordinates2 looks ok",{

  #sample data
  storm.dat <- data.frame(
    latitude = c(25, 26, 27, 28),
    longitude = c(-80, -79, -78, -77),
    NE1 = c(100, 200, 300, 400),
    NE2 = c(150, 250, 350, 450),
    SE1 = c(200, 300, 400, 500),
    SE2 = c(250, 350, 450, 550),
    SW1 = c(300, 400, 500, 600),
    SW2 = c(350, 450, 550, 650),
    NW1 = c(400, 500, 600, 700),
    NW2 = c(450, 550, 650, 750)
  )

  # testing that distance_coordinates returns a matrix
  result <- distance_coordinates2(storm.dat, "2022-01-01 00:00:00")
  expect_true(is.matrix(result))

})


test_that("storm_size_interp_34 looks ok", {

  #sample data
  center.point <- c(-50, 30)
  NE.point <- c(-40, 40)
  SE.point <- c(-40, 20)
  SW.point <- c(-60, 20)
  NW.point <- c(-60, 40)

  #testing storm_size_interp_34 returns a data frame with two columns each with length 400
    result <- storm_size_interp_34(center.point, NE.point, SE.point, SW.point, NW.point)
    expect_s3_class(result, "data.frame")
    expect_equal(length(result$lon.theta), 400)
    expect_equal(length(result$lat.theta), 400)

})


test_that("storm_size_interp_50 looks ok", {

  #sample data
  center.point <- c(-50, 30)
  NE.point <- c(-40, 40)
  SE.point <- c(-40, 20)
  SW.point <- c(-60, 20)
  NW.point <- c(-60, 40)

  #testing storm_size_interp_50 returns a data frame with two columns each with length 400
  result <- storm_size_interp_50(center.point, NE.point, SE.point, SW.point, NW.point)
  expect_s3_class(result, "data.frame")
  expect_equal(length(result$lon.theta), 400)
  expect_equal(length(result$lat.theta), 400)

})


test_that("storm_size_interp_64 looks ok", {

  #sample data
  center.point <- c(-50, 30)
  NE.point <- c(-40, 40)
  SE.point <- c(-40, 20)
  SW.point <- c(-60, 20)
  NW.point <- c(-60, 40)

  #testing storm_size_interp_64 returns a data frame with two columns each with length 400
  result <- storm_size_interp_64(center.point, NE.point, SE.point, SW.point, NW.point)
  expect_s3_class(result, "data.frame")
  expect_equal(length(result$lon.theta), 400)
  expect_equal(length(result$lat.theta), 400)

})

