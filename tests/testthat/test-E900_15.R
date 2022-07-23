test_that("calculate TTS from ASTM E900-15", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 5), 31.74387)
})

test_that("calculate TTS from ASTM E900-15", {
  test1 <- E900_15(c("P", "F", "W"), 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 5), c(31.74387, 28.87909, 34.92180))
})
