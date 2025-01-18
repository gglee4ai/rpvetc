fNP3319 <- function(...) NP3319(..., temperature_unit = "F")

## 단일값 계산
test_that("fluence를 입력하지 않는 경우", {
  test1 <- NP3319("B", 0.1, 0.6, 0)
  expect_equal(round(test1, 2), c(0))
})

test_that("fluence 있을 경우, NP3319 TTS 계산", {
  test1 <- NP3319("B", 0.1, 0.6, 1e18, temperature_unit = "C")
  expect_equal(round(test1, 2), c(10.45))
})

test_that("fluence 있을 경우, NP3319 TTS 화씨 계산", {
  test1 <- NP3319("B", 0.1, 0.6, 1e18, temperature_unit = "F")
  expect_equal(round(test1, 2), c(18.82))
})

test_that("CF 계산", {
  test1 <- fNP3319("B", 0.1, 0.6, 1e19, output = "CF")
  expect_equal(round(test1, 2), c(35.86))
})

test_that("FF 계산", {
  test1 <- fNP3319("B", 0.1, 0.6, 1e19, output = "FF")
  expect_equal(round(test1, 2), c(1))
})

test_that("보조함수 fNP3319 TTS 계산", {
  test1 <- fNP3319("B", 0.1, 0.6, 0, output = "FF")
  expect_equal(round(test1, 2), c(0))
})

## 벡터 테스트
test_that("product_form이 2개 TTS", {
  test1 <- fNP3319(c("B", "W"), 0.1, 0.6, 1e18)
  y <- c(18.82, 22.31)
  expect_equal(round(test1, 2), y)
})

test_that("product_form이 2개 TTS", {
  test1 <- fNP3319(c("B", "W"), 0.1, 0.6, 1e19, output = "CF")
  y <- c(35.86, 68.08)
  expect_equal(round(test1, 2), y)
})

test_that("product_form이 2개 TTS", {
  test1 <- fNP3319("W", 0.1, 0.6, c(0, 1e18, 1e19), output = "TTS")
  y <- c(0, 22.31, 68.07)
  expect_equal(round(test1, 2), y)
})

test_that("product_form이 2개 TTS", {
  test1 <- fNP3319(c("B", "W", "W"), 0.1, 0.6, c(1e18, 1e18, 1e19), output = "TTS")
  y <- c(18.82, 22.31, 68.07)
  expect_equal(round(test1, 2), y)
})
