test_that("We are able to return a leaflet map ", {
  expect_error(driveBC_map_route("New York"))
  expect_length(driveBC_map_major("Vancouver"),2)
  expect_length(driveBC_map_major("Blind Bay","Vernon"),2)
  expect_error(driveBC_map_major("Vancouver",2))
})
