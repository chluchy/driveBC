test_that("We are able to return a leaflet map ", {
  expect_error(driveBC_map_route("New York"))
  expect_length(driveBC_map_route("Vancouver"),2)
  expect_length(driveBC_map_route("Blind Bay","Vernon"),2)
  expect_error(driveBC_map_route("Vancouver",2))
})
