test_that("TTS (total) is computed correctly for base metal P", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 2), c(31.74))
})

test_that("TTS1 is computed correctly for base metal P", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS1")
  expect_equal(round(test1, 2), c(7.85))
})

test_that("TTS2 is computed correctly for base metal P", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS2")
  expect_equal(round(test1, 2), c(23.90))
})

test_that("Standard Deviation (SD) is computed correctly for base metal P", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "SD")
  expect_equal(round(test1, 2), c(11.58))
})

test_that("Repeated test for total TTS (should return same value)", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 2), c(31.74))
})

test_that("Repeated test for TTS1 (should return same value)", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS1")
  expect_equal(round(test1, 2), c(7.85))
})

test_that("Repeated test for TTS2 (should return same value)", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS2")
  expect_equal(round(test1, 2), c(23.90))
})

test_that("Repeated test for SD (should return same value)", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "SD")
  expect_equal(round(test1, 2), c(11.58))
})

test_that("Output in Fahrenheit is converted correctly from Celsius", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18,
                   output_unit = "F", temperature_unit = "C")
  expect_equal(round(test1, 2), c(57.14))
})

test_that("TTS computed correctly for multiple product forms with same conditions", {
  test1 <- E900_15(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  y <- c(28.88, 31.74, 34.92)
  expect_equal(round(test1, 2), y)
})

test_that("TTS reflects fluence variation across multiple product forms", {
  test1 <- E900_15(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, c(0, 1e18, 1e19))
  y <- c(0, 22.91, 52.05)
  expect_equal(round(test1, 2), y)
})

test_that("SD reflects fluence variation across multiple product forms", {
  test1 <- E900_15(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, c(0, 1e18, 1e19),
                   output = "SD")
  y <- c(0, 10.98, 15.71)
  expect_equal(round(test1, 2), y)
})
