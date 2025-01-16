test_that("1", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 2), c(`TTS °C` = 31.74))
})

test_that("2", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS1")
  expect_equal(round(test1, 2), c(`TTS1 °C` = 7.85))
})

test_that("3", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS2")
  expect_equal(round(test1, 2), c(`TTS2 °C` = 23.90))
})

test_that("4", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "SD")
  expect_equal(round(test1, 2), c(`SD °C` = 11.58))
})

test_that("5", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 2), c(`TTS °C` = 31.74))
})

test_that("6", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS1")
  expect_equal(round(test1, 2), c(`TTS1 °C` = 7.85))
})

test_that("7", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS2")
  expect_equal(round(test1, 2), c(`TTS2 °C` = 23.90))
})

test_that("8", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "SD")
  expect_equal(round(test1, 2), c(`SD °C` = 11.58))
})

test_that("9", {
  test1 <- E900_15("P", 0.2, 0.18, 1.36, 0.012, 1.8 * 290 + 32, 2.56894e18, temperature_unit = "F")
  expect_equal(round(test1, 2), c(`TTS °F` = 57.14))
})

test_that("10", {
  test1 <- E900_15(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  y <- c(28.88, 31.74, 34.92)
  names(y) <- rep("TTS °C", 3)
  expect_equal(round(test1, 2), y)
})

test_that("11", {
  test1 <- E900_15(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, c(0, 1e18, 1e19))
  y <- c(0, 22.91, 52.05)
  names(y) <- rep("TTS °C", 3)
  expect_equal(round(test1, 2), y)
})

test_that("12", {
  test1 <- E900_15(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, c(0, 1e18, 1e19),
    output = "SD"
  )
  y <- c(0, 10.98, 15.71)
  names(y) <- rep("SD °C", 3)
  expect_equal(round(test1, 2), y)
})
