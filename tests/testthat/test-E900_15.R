test_that("1", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 5), 31.74387)
})
test_that("2", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "SD")
  expect_equal(round(test1, 2), 11.58)
})
test_that("3", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS1", temperature_unit = "F")
  expect_equal(round(test1, 2), 7.85)
})
test_that("4", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS2", temperature_unit = "F")
  expect_equal(round(test1, 2), 23.90)
})
test_that("5", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS", temperature_unit = "F")
  expect_equal(round(test1, 2), 31.74)
})
test_that("6", {
  test1 <- E900_15("W", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 2), 34.92)
})
