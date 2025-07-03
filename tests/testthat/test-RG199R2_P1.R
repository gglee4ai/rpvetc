rg199r2_p1 <- function(..., temperature_unit = "F") RG199R2_P1(..., temperature_unit = temperature_unit)

library(testthat)

# 기본 기능 테스트 ---------------------------------------------------------------

test_that("Computes TTS for base and weld metals", {
  test1 <- rg199r2_p1(c("B", "W"), 0.2, 0.2, 1e19)
  expect_equal(round(test1, 2), c(102, 104))
})

test_that("Computes CF for base and weld metals", {
  test1 <- rg199r2_p1(c("B", "W"), 0.2, 0.2, output = "CF")
  expect_equal(round(test1, 2), c(102, 104))
})

test_that("Computes FF for given fluence values", {
  test1 <- rg199r2_p1(fluence = c(1e19, 2e19), output = "FF")
  expect_equal(round(test1, 2), c(1, 1.19))
})

test_that("Computes SD for base and weld metals", {
  test1 <- rg199r2_p1(c("B", "W"), output = "SD")
  expect_equal(round(test1, 2), c(17, 28))
})

test_that("Computes Margin for base and weld metals", {
  test1 <- rg199r2_p1(c("B", "W"), 0.2, 0.2, 1e19, output = "Margin")
  expect_equal(round(test1, 2), c(34, 56))
})

# 확장 테스트 ---------------------------------------------------------------------

test_that("Computes TTS in Celsius", {
  test1 <- rg199r2_p1("B", 0.2, 0.2, 1e19, temperature_unit = "C")
  expect_equal(round(test1, 1), round(102 * 5/9, 1))
})

test_that("Throws error for mismatched input lengths", {
  expect_error(
    rg199r2_p1(c("B", "W"), 0.2, 0.2, fluence = c(1e19, 1e20, 1e21)),
    regexp = "same length"  # 수정 필요 시 함수 내에서 명시적 stop() 작성
  )
})

test_that("Throws error for invalid product_form", {
  expect_error(
    rg199r2_p1("X", 0.2, 0.2, 1e19),
    regexp = "Invalid product_form"
  )
})

test_that("Handles scalar inputs correctly", {
  test1 <- rg199r2_p1("W", 0.1, 0.5, 1E_19, output = "TTS")
  expect_type(test1, "double")
  expect_length(test1, 1)
})
