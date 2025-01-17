test_that("1", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13)
  expect_equal(round(test1, 2), c(31.81))
})

test_that("2", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "TTS1")
  expect_equal(round(test1, 2), c(9.39))
})

test_that("3", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "TTS2")
  expect_equal(round(test1, 2), c(22.42))
})

test_that("4", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "SD")
  expect_equal(round(test1, 2), c(12.66))
})

test_that("5", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13)
  expect_equal(round(test1, 2), c(31.81))
})

test_that("6", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "TTS1")
  expect_equal(round(test1, 2), c(9.39))
})

test_that("7", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "TTS2")
  expect_equal(round(test1, 2), c(22.42))
})

test_that("8", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "SD")
  expect_equal(round(test1, 2), c(12.66))
})

test_that("9", {
  test1 <- E900_flux("P", 0.2, 0.18, 1.36, 0.012, 1.8 * 290 + 32, 2.56894e18, 1e13, temperature_unit = "F")
  expect_equal(round(test1, 2), c(57.26))
})

test_that("10", {
  test1 <- E900_flux(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13)
  y <- c(29.18, 31.81, 32.22)
  expect_equal(round(test1, 2), y)
})

test_that("11", {
  test1 <- E900_flux(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, c(0, 1e18, 1e19), 1e13)
  y <- c(0, 22.94, 50.79)
  expect_equal(round(test1, 2), y)
})

test_that("12", {
  test1 <- E900_flux(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, c(0, 1e18, 1e19), 1e13,
    output = "SD"
  )
  y <- c(0, 11.85, 17.41)
  expect_equal(round(test1, 2), y)
})
