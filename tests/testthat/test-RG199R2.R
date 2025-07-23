# Input Data for Testing
flu1 <- c(0.54, 1.12, 1.23, 2.94, 3.89, 5.52) * 1e19
tts1 <- c(-9.27, -11.05, 3.12, 12.80, 24.03, 37.56)

flu2 <- c(1e18, 1e19, 1e20)
tts2 <- c(50, 142, 200)

flu3 <- c(0, 1e18, 1e19, 1e20)
tts3 <- c(0, 100, 200, 250)

test_that("RG199R2 default call returns value", {
  test1 <- RG199R2(20, 1e19)
  expect_equal(round(test1, 2), c(20))
})

test_that("RG199R2 converts Celsius input to Fahrenheit output", {
  test1 <- RG199R2(20, 1e19, output_unit = "F", input_unit = "C")
  expect_equal(round(test1, 2), c(36))
})

test_that("RG199R2 returns FF output as 1", {
  test1 <- RG199R2(NULL, 1e19, output = "FF")
  expect_equal(round(test1, 2), c(1))
})

test_that("RG199R2 returns expected margin value", {
  test1 <- RG199R2(20, 1e19, 8, output = "Margin")
  expect_equal(round(test1, 2), c(16))
})

test_that("RG199R2_P1 computes Chemistry Factor for B and W", {
  test1 <- RG199R2_P1(c("B", "W"), 0.1, 0.4, output_unit = "F")
  expect_equal(round(test1, 2), c(65, 97))
})

test_that("RG199R2_P1 returns SD for all product forms", {
  test1 <- RG199R2_P1(c("B", "F", "P", "W"), output = "SD", output_unit = "F")
  expect_equal(round(test1, 2), c(17, 17, 17, 28))
})

test_that("RG199R2_P1 throws error on invalid product_form", {
  expect_error(
    RG199R2_P1("X", 0.2, 0.2),
    regexp = "Invalid"
  )
})

test_that("RG199R2_P2 computes CF from SV data", {
  test1 <- RG199R2_P2(flu2, tts2)
  expect_equal(round(test1, 2), c(134.37))
})

test_that("RG199R2_P2 returns SD for base metal in Celsius", {
  test1 <- RG199R2_P2(flu2, tts2, "B", output = "SD")
  expect_equal(round(test1, 2), c(4.72))
})

test_that("RG199R2_P2 returns SD for base metal in Fahrenheit", {
  test1 <- RG199R2_P2(flu2, tts2, "B", output = "SD", output_unit = "F")
  expect_equal(round(test1, 2), c(8.5))
})

test_that("RG199R2_P2 returns SD for weld metal in Celsius", {
  test1 <- RG199R2_P2(flu2, tts2, "W", output = "SD")
  expect_equal(round(test1, 2), c(7.78))
})

test_that("RG199R2_P2 returns SD for weld metal in Fahrenheit", {
  test1 <- RG199R2_P2(flu2, tts2, "W", output = "SD", output_unit = "F")
  expect_equal(round(test1, 2), c(14))
})

test_that("RG199R2_P2 returns max SD when data scatter is large", {
  test1 <- RG199R2_P2(flu1, tts1, "W", output = "SD", output_unit = "F")
  expect_equal(round(test1, 2), c(28))
})

test_that("RG199R2_P2 throws error when product_form is not unique for SD", {
  expect_error(
    RG199R2_P2(flu2, tts2, c("B", "W"), output = "SD"),
    regexp = "product_unique"
  )
})

test_that("RG199R2_P2 warns on unnecessary product_form for CF", {
  expect_warning(
    RG199R2_P2(flu2, tts2, c("B", "W")),
    regexp = "ignored"
  )
})

test_that("RG199R2_P1 interpolates TTS over fluence", {
  test1 <- RG199R2(RG199R2_P1("B", 0.1, 0.5), flu1)
  expect_equal(round(test1, 2), c(29.89, 37.25, 38.19, 46.43, 48.76, 51.33))
})

test_that("RG199R2_P2 estimates CF from surveillance data", {
  test1 <- RG199R2_P2(flu1, tts1, output = "CF")
  expect_equal(round(test1, 2), 10.34)
})

test_that("RG199R2_P2 default fluence uses SV fluence", {
  test1 <- RG199R2_P2(flu3, tts3)
  expect_equal(round(test1, 2), 178.97)
})

test_that("RG199R2_P2 calculates margin using full pipeline", {
  test1 <- RG199R2(
    cf = RG199R2_P2(flu2, tts2),
    fluence = c(1e17, 1e18, 1e19, 1e20),
    sd = RG199R2_P2(flu2, tts2, "W", output = "SD"),
    output = "Margin")
  expect_equal(round(test1, 2), c(14.73, 15.56, 15.56, 15.56))
})

test_that("RG199R2_P2 returns CF in both Celsius and Fahrenheit", {
  test_c <- RG199R2_P2(flu2, tts2, output_unit = "C")
  test_f <- RG199R2_P2(flu2, tts2, output_unit = "F")
  expect_equal(round(test_c, 2), round(dF_to_dC(test_f), 2))
})

test_that("RG199R2_P1 SD is constant across fluence", {
  test1 <- RG199R2_P1("B", output = "SD", output_unit = "F")
  expect_equal(round(test1, 2), rep(17))
})

test_that("RG199R2 margin computation for multiple fluences", {
  test1 <- RG199R2(
    cf = RG199R2_P2(flu2, tts2),
    fluence = 10^(17:21),
    sd = RG199R2_P2(flu2, tts2, "W", output = "SD"),
    output = "Margin"
  )
  expect_equal(round(test1, 2), c(14.73, 15.56, 15.56, 15.56, 15.56))
})

