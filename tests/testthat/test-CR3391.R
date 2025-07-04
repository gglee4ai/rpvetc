fCR3391 <- function(...) CR3391(..., output_unit = "F")

## 단일 계산
test_that("섭씨로 CR3391 TTS 계산", {
  test1 <- CR3391("B", 0.1, 0.6, 1e18)
  expect_equal(round(test1, 2), 15.14)
})

test_that("화씨로 CR3391 TTS 계산", {
  test1 <- CR3391("B", 0.1, 0.6, 1e18, output_unit = "F")
  expect_equal(round(test1, 2), 27.25)
})

test_that("보조함수 fCR3391 TTS 계산", {
  test1 <- fCR3391("B", 0.1, 0.6, 1e18)
  expect_equal(round(test1, 2), 27.25)
})

test_that("CF 계산", {
  test1 <- fCR3391("B", 0.1, 0.6, 1e18, output = "CF")
  expect_equal(round(test1, 2), 63.81)
})

test_that("FF 계산2", {
  test1 <- fCR3391("B", 0.1, 0.6, 1e18, output = "FF")
  expect_equal(round(test1, 2), 0.43)
})

test_that("FF 계산, 섭씨 단위로", {
  test1 <- CR3391("B", 0.1, 0.6, 1e18, output = "FF", output_unit = "C")
  expect_equal(round(test1, 2), 0.43)
})

test_that("SD 계산, 섭씨 단위로", {
  test1 <- fCR3391("B", 0.1, 0.6, 1e18, output = "SD")
  expect_equal(round(test1, 2), 17.2)
})

test_that("SD 계산, 섭씨 단위로", {
  test1 <- fCR3391("W", 0.1, 0.6, 1e18, output = "SD")
  expect_equal(round(test1, 2), 28.2)
})


## 벡터 테스트
test_that("product_form이 2개 TTS", {
  test1 <- fCR3391(c("B", "W"), 0.1, 0.6, 1e18)
  expect_equal(round(test1, 2), c(27.25, 56.17))
})

test_that("product_form이 2개 CF", {
  test1 <- fCR3391(c("B", "W"), 0.1, 0.6, 1e18, output = "CF")
  expect_equal(round(test1, 2), c(63.81, 131.53))
})

test_that("product_form이 2개 FF", {
  test1 <- fCR3391(c("B", "W"), 0.1, 0.6, 1e18, output = "FF")
  expect_equal(round(test1, 2), c(0.43, 0.43))
})

test_that("fluence가 2개 CF", {
  test1 <- fCR3391("B", 0.1, 0.6, c(1e18, 1e19), output = "CF")
  expect_equal(round(test1, 2), c(63.81, 63.81))
})

test_that("PF랑 fluence가 각각 2개", {
  test1 <- fCR3391(c("B", "W"), 0.1, c(0.1, 0.6), c(1e18, 1e19), output = "CF")
  expect_equal(round(test1, 2), c(33.45, 131.53))
})

test_that("CF * FF = TTS?", {
  cf <- fCR3391(c("B", "W"), 0.1, 0.6, c(1e18, 1e19), output = "CF")
  ff <- fCR3391(c("B", "W"), 0.1, 0.6, c(1e18, 1e19), output = "FF")
  tts1 <- cf * ff
  tts2 <- fCR3391(c("B", "W"), 0.1, 0.6, c(1e18, 1e19))
  expect_equal(tts1, tts2)
})


## SD 테스트

test_that("CR3391 FF 계산2", {
  test1 <- fCR3391(c("B", "W"), 0.1, 0.6, c(1e18, 1e19), output = "SD")
  expect_equal(round(test1, 2), c(17.2, 28.2))
})

test_that("CR3391 FF 계산2", {
  test1 <- fCR3391("B", 0.1, 0.6, c(1e18, 1e19), output = "SD")
  expect_equal(round(test1, 2), c(17.2, 17.2))
})

test_that("CR3391 FF 계산2", {
  test1 <- fCR3391("B", c(0.1, 0.2), 0.6, c(1e18), output = "SD")
  expect_equal(round(test1, 2), c(17.2, 17.2))
})

test_that("CR3391 FF 계산2", {
  test1 <- fCR3391(c("B", "W"), c(0.1, 0.2), 0.6, c(1e18), output = "SD")
  expect_equal(round(test1, 2), c(17.2, 28.2))
})
