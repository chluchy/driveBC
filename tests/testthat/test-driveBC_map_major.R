test_that("We are able to return a leaflet map ", {
  expect_error(driveBC_map_major(bob))
  expect_length(driveBC_map_major(15),8)
  expect_length(driveBC_map_major(10,2),8)
  expect_error(driveBC_map_major(10,2,"MINOR"))
})
