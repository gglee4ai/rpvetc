test_that("TTS (total) is correctly computed for plate (P) with given flux", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13)
  expect_equal(round(test1, 2), c(31.81))
})

test_that("TTS1 component is correctly computed with flux input", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "TTS1")
  expect_equal(round(test1, 2), c(9.39))
})

test_that("TTS2 component is correctly computed with flux input", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "TTS2")
  expect_equal(round(test1, 2), c(22.42))
})

test_that("Standard Deviation (SD) is correctly computed with flux input", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "SD")
  expect_equal(round(test1, 2), c(12.66))
})

test_that("Repeated total TTS computation returns consistent result", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13)
  expect_equal(round(test1, 2), c(31.81))
})

test_that("Repeated TTS1 computation returns consistent result", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "TTS1")
  expect_equal(round(test1, 2), c(9.39))
})

test_that("Repeated TTS2 computation returns consistent result", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "TTS2")
  expect_equal(round(test1, 2), c(22.42))
})

test_that("Repeated SD computation returns consistent result", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output = "SD")
  expect_equal(round(test1, 2), c(12.66))
})

test_that("Output in Fahrenheit is correctly converted from Celsius", {
  test1 <- E900_15_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13, output_unit = "F")
  expect_equal(round(test1, 2), c(57.26))
})

test_that("TTS is computed correctly across multiple product forms (F, P, W)", {
  test1 <- E900_15_flux(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13)
  y <- c(29.18, 31.81, 32.22)
  expect_equal(round(test1, 2), y)
})

test_that("TTS responds correctly to varying fluence values", {
  test1 <- E900_15_flux(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290,
                        c(0, 1e18, 1e19), 1e13)
  y <- c(0, 22.94, 50.79)
  expect_equal(round(test1, 2), y)
})

test_that("SD responds correctly to varying fluence values", {
  test1 <- E900_15_flux(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290,
                        c(0, 1e18, 1e19), 1e13, output = "SD")
  y <- c(0, 11.85, 17.41)
  expect_equal(round(test1, 2), y)
})
