test_that("world_map_helper looks ok", {

  #testing that world_map_helper returns a ggplot
  result<- world_map_helper()
  expect_s3_class(result, "gg")

  #testing that world_map_helper takes longitude and latitude range as inputs
  lon.range<- c(-100, 100)
  lat.range<- c(-50, 50)
  result<- world_map_helper(lon.range, lat.range)
  expect_s3_class(result, "gg")

})
