flu1 <- c(0, 1e18, 1e19, 1e20)
tts1 <- c(0, 100, 200, 250)

flu2 <- c(0.54, 1.12, 1.23, 2.94, 3.89, 5.52) * 1e19 # Kori1 F1
tts2 <- c(-9.27, -11.05, 3.12, 12.80, 24.03, 37.56) #

fRG199R2 <- function(...) RG199R2(..., temperature_unit = "F")

# case: 1
test_that("1", {
  test1 <- fRG199R2(fluence = 1e19, CF = 1)
  expect_equal(round(test1, 2), c(1))
})

test_that("1", {
  test1 <- fRG199R2(fluence = flu1, CF = 1)
  expect_equal(round(test1, 2), c(0, 0.42, 1, 1.51))
})

# case 1 > case 2
test_that("CF가 주어질 경우, SV 데이터 보다 우선한다.", {
  test1 <- fRG199R2(SV_flu = flu1, SV_tts = tts1, CF = 1)
  expect_equal(round(test1, 2), c(0, 0.42, 1, 1.51))
})

# case 2
test_that("SV만 제공되는 경우, SV fluence에서 TTS 계산", {
  test1 <- fRG199R2(SV_flu = flu1, SV_tts = tts1)
  expect_equal(round(test1, 2), c(0.00, 74.61, 178.97, 270.89))
})

test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2(fluence = c(0, 1e19, 1e20, 1e21), SV_flu = flu1, SV_tts = tts1)
  expect_equal(round(test1, 2), c(0.0, 178.97, 270.89, 258.69))
})

# case 3
test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2("B", 0.1, 0.6, c(0, 1e19, 1e20, 1e21))
  expect_equal(round(test1, 2), c(0, 65.00, 98.38, 93.95))
})
test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2(c("B", "W"), 0.1, 0.6, c(1e19))
  expect_equal(round(test1, 2), c(65.00, 122.00))
})

test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2(c("B", "W"), 0.1, 0.6, c(1e19), SV_flu = flu1, SV_tts = tts1, CF = 1)
  expect_equal(round(test1, 2), c(1, 1))
})
test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2(c("B", "W"), 0.1, 0.6, c(1e19), CF = 1)
  expect_equal(round(test1, 2), c(1, 1))
})


##
# output = "CF"
# case: 1
test_that("1", {
  test1 <- fRG199R2(fluence = 1e20, CF = 1, output = "CF")
  expect_equal(round(test1, 2), c(1))
})

test_that("1", {
  test1 <- fRG199R2(fluence = flu1, CF = 1, output = "CF")
  expect_equal(round(test1, 2), c(1, 1, 1, 1))
})

# case 2
test_that("SV만 제공되는 경우, SV fluence에서 TTS 계산", {
  test1 <- fRG199R2(SV_flu = flu1, SV_tts = tts1, output = "CF")
  expect_equal(round(test1, 2), c(178.97))
})

test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2(fluence = c(0, 1e19, 1e20, 1e21), SV_flu = flu1, SV_tts = tts1, output = "CF")
  expect_equal(round(test1, 2), c(178.97))
})

# case 3
test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2("B", 0.1, 0.6, c(0, 1e19, 1e20, 1e21), output = "CF")
  expect_equal(round(test1, 2), c(65, 65, 65, 65))
})
test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2(c("B", "W"), 0.1, 0.6, c(1e19), output = "CF")
  expect_equal(round(test1, 2), c(65.00, 122.00))
})

test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2(c("B", "W"), 0.1, 0.6, c(1e19), SV_flu = flu1, SV_tts = tts1, CF = 1, output = "CF")
  expect_equal(round(test1, 2), c(1, 1))
})
test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- fRG199R2(c("B", "W"), 0.1, 0.6, c(1e19), CF = 1, output = "CF")
  expect_equal(round(test1, 2), c(1, 1))
})
