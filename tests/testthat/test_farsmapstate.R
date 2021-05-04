# Test to check when invalid number id is provided

expect_error()

test_that("fars_map_state throws an error", {
  expect_error(fars_map_state(255,2015))
})
