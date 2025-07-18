library(testthat)

# -------------------------------
# Helper Wrappers for Testing
# -------------------------------

rg199r2_p1 <- function(..., output_unit = "F") {
  RG199R2_P1(..., output_unit = output_unit)
}

rg199r2_p2 <- function(..., output_unit = "F") {
  RG199R2_P2(..., output_unit = output_unit, SV_tts_unit = output_unit)
}

# -------------------------------
# Input Data for Testing
# -------------------------------

flu1 <- c(0.54, 1.12, 1.23, 2.94, 3.89, 5.52) * 1e19
tts1 <- c(-9.27, -11.05, 3.12, 12.80, 24.03, 37.56)

flu2 <- c(1e18, 1e19, 1e20)
tts2 <- c(50, 142, 200)

flu3 <- c(0, 1e18, 1e19, 1e20)
tts3 <- c(0, 100, 200, 250)

# -------------------------------
# RG199R2_P1 Tests
# -------------------------------

test_that("P1: TTS for base and weld metals", {
  test1 <- rg199r2_p1(c("B", "W"), 0.2, 0.2, 1e19)
  expect_equal(round(test1, 2), c(102, 104))
})

test_that("P1: CF for base and weld metals", {
  test1 <- rg199r2_p1(c("B", "W"), 0.2, 0.2, output = "CF")
  expect_equal(round(test1, 2), c(102, 104))
})

test_that("P1: FF computation", {
  test1 <- rg199r2_p1(fluence = c(1e19, 2e19), output = "FF")
  expect_equal(round(test1, 2), c(1, 1.19))
})

test_that("P1: SD base vs weld", {
  test1 <- rg199r2_p1(c("B", "W"), output = "SD")
  expect_equal(round(test1, 2), c(17, 28))
})

test_that("P1: Margin for base and weld metals", {
  test1 <- rg199r2_p1(c("B", "W"), 0.2, 0.2, 1e19, output = "Margin")
  expect_equal(round(test1, 2), c(34, 56))
})

test_that("P1: TTS in Celsius", {
  test1 <- rg199r2_p1("B", 0.2, 0.2, 1e19, output_unit = "C")
  expect_equal(round(test1, 1), round(102 * 5 / 9, 1))
})

test_that("P1: Error on mismatched input length", {
  expect_error(
    rg199r2_p1(c("B", "W"), 0.2, 0.2, fluence = c(1e19, 1e20, 1e21)),
    regexp = "the same"
  )
})

test_that("P1: Error on invalid product_form", {
  expect_error(
    rg199r2_p1("X", 0.2, 0.2, 1e19),
    regexp = "Invalid"
  )
})

# -------------------------------
# RG199R2_P2 Tests
# -------------------------------

test_that("P2: TTS interpolation", {
  test1 <- rg199r2_p2("B", flu1, tts1, c(1e17, 1e18, 1e19, 1e20))
  expect_equal(round(test1, 2), c(1.13, 4.31, 10.34, 15.65))
})

test_that("P2: CF estimation", {
  test1 <- rg199r2_p2("B", flu1, tts1, output = "CF")
  expect_equal(round(test1, 2), 10.34)
})

test_that("P2: FF computation", {
  test1 <- rg199r2_p2(fluence = c(1e19, 2e19), output = "FF")
  expect_equal(round(test1, 2), c(1, 1.19))
})

test_that("P2: SD for base", {
  test1 <- rg199r2_p2("B", flu2, tts2, output = "SD")
  expect_equal(round(test1, 2), 8.5)
})

test_that("P2: SD for weld", {
  test1 <- rg199r2_p2("W", flu2, tts2, output = "SD")
  expect_equal(round(test1, 2), 14.0)
})

test_that("P2: Margin for weld", {
  test1 <- rg199r2_p2("W", flu2, tts2, c(1e17, 1e18, 1e19, 1e20), output = "Margin")
  expect_equal(round(test1, 2), c(14.73, 28, 28, 28))
})

test_that("P2: Only SV input, default fluence = SV flu", {
  test1 <- rg199r2_p2(SV_flu = flu3, SV_tts = tts3)
  expect_equal(round(test1, 2), c(0.00, 74.61, 178.97, 270.89))
})

test_that("P2: TTS from SV and fluence input", {
  test1 <- rg199r2_p2("B", flu3, tts3, c(0, 1e19, 1e20, 1e21))
  expect_equal(round(test1, 2), c(0.0, 178.97, 270.89, 258.69))
})

test_that("P2: CF returned with SV + fluence input", {
  test1 <- rg199r2_p2("B", flu3, tts3, c(1e19), output = "CF")
  expect_equal(round(test1, 2), 178.97)
})

test_that("P2: CF with only SV input", {
  test1 <- rg199r2_p2(SV_flu = flu3, SV_tts = tts3, output = "CF")
  expect_equal(round(test1, 2), 178.97)
})

test_that("P2: CF with fluence + SV input", {
  test1 <- rg199r2_p2(fluence = c(0, 1e19, 1e20, 1e21), SV_flu = flu3, SV_tts = tts3, output = "CF")
  expect_equal(round(test1, 2), 178.97)
})

test_that("P1: CF for fixed Cu/Ni and multiple product forms", {
  test1 <- rg199r2_p1(c("B", "W"), 0.1, 0.6, c(1e19), output = "CF")
  expect_equal(round(test1, 2), c(65.00, 122.00))
})

test_that("P1: SD constant over fluence range", {
  test1 <- rg199r2_p1("B", fluence = 1:5 * 1e19, output = "SD")
  expect_equal(round(test1, 2), rep(17, 5))
})

test_that("P2: SD for multiple weld inputs", {
  test1 <- rg199r2_p2(rep("W", 5), output = "SD", SV_flu = flu2, SV_tts = tts2)
  expect_equal(round(test1, 2), rep(14, 5))
})

test_that("P2: Margin for multiple weld inputs", {
  test1 <- rg199r2_p2(rep("W", 5), fluence = 10^(17:21), output = "Margin", SV_flu = flu2, SV_tts = tts2)
  expect_equal(round(test1, 2), c(14.73, 28.00, 28.00, 28.00, 28.00))
})


test_that("P2: CF in Celcius", {
  test1 <- rg199r2_p2(SV_flu = flu2, SV_tts = tts2, output = "CF")
  expect_equal(round(test1, 2), 134.37)
})

test_that("P2: CF in Fahrenheit", {
  test1 <- RG199R2_P2(SV_flu = flu2, SV_tts = tts2, output = "CF")
  expect_equal(round(test1, 2), 134.37)
})

test_that("P2: Margin for multiple weld inputs", {
  test1 <- rg199r2_p2(rep("W", 5), fluence = 10^(17:22), output = "TTS", SV_flu = flu2, SV_tts = tts2)
  expect_equal(round(test1, 2), c(14.73, 56.02, 134.37, 203.38, 194.23, 117.03))
})

