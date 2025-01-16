fNP3319 <- function(...) NP3319(..., temperature_unit = "F")

## 단일 계산
test_that("섭씨로 NP3319 TTS 계산", {
  test1 <- NP3319("B", 0.1, 0.6, 1e18)
  expect_equal(round(test1, 2), 15.14)
})

test_that("화씨로 NP3319 TTS 계산", {
  test1 <- NP3319("B", 0.1, 0.6, 1e18, temperature_unit = "F")
  expect_equal(round(test1, 2), 27.25)
})

test_that("보조함수 fNP3319 TTS 계산", {
  test1 <- fNP3319("B", 0.1, 0.6, 1e18)
  expect_equal(round(test1, 2), 27.25)
})

test_that("CF 계산", {
  test1 <- fNP3319("B", 0.1, 0.6, 1e18, output = "CF")
  expect_equal(round(test1, 2), 63.81)
})

test_that("FF 계산2", {
  test1 <- fNP3319("B", 0.1, 0.6, 1e18, output = "FF")
  expect_equal(round(test1, 2), 0.43)
})

test_that("FF 계산, 섭씨 단위로", {
  test1 <- NP3319("B", 0.1, 0.6, 1e18, output = "FF", temperature_unit = "C")
  expect_equal(round(test1, 2), 0.43)
})


## 벡터 테스트
p <- c("B", "W")
test_that("product_form이 2개 TTS", {
  test1 <- fNP3319(p, 0.1, 0.6, 1e18)
  expect_equal(round(test1, 2), c(27.25, 56.17))
})

test_that("product_form이 2개 CF", {
  test1 <- fNP3319(p, 0.1, 0.6, 1e18, output = "CF")
  expect_equal(round(test1, 2), c(63.81, 131.53))
})

test_that("product_form이 2개 FF", {
  test1 <- fNP3319(p, 0.1, 0.6, 1e18, output = "FF")
  expect_equal(round(test1, 2), c(0.43, 0.43))
})

test_that("fluence가 2개 CF", {
  test1 <- fNP3319("B", 0.1, 0.6, c(1e18, 1e19), output = "CF")
  expect_equal(round(test1, 2), c(63.81, 63.81))
})

test_that("PF랑 fluence가 각각 2개", {
  test1 <- fNP3319(p, 0.1, 0.6, c(1e18, 1e19), output = "CF")
  expect_equal(round(test1, 2), c(63.81, 131.53))
})

test_that("CF * FF = TTS?", {
  cf <- fNP3319(p, 0.1, 0.6, c(1e18, 1e19), output = "CF")
  ff <- fNP3319(p, 0.1, 0.6, c(1e18, 1e19), output = "FF")
  tts1 <- cf * ff
  tts2 <- fNP3319(p, 0.1, 0.6, c(1e18, 1e19))
  expect_equal(tts1, tts2)
})

