library(testthat)

# RG199R2_P1 테스트 ------------------

rg199r2_p1 <- function(..., temperature_unit = "F") {
  RG199R2_P1(..., temperature_unit = temperature_unit)
}

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


# RG199R2_P2 테스트 ------------------

rg199r2_p2 <- function(..., temperature_unit = "F") {
  RG199R2_P2(..., temperature_unit = temperature_unit)
}

flu1 <- c(0.54, 1.12, 1.23, 2.94, 3.89, 5.52) * 1e19 # Kori1 F1
tts1 <- c(-9.27, -11.05, 3.12, 12.80, 24.03, 37.56) #

flu2 = c(1e18, 1e19, 1e20)
tts2 = c(50, 142, 200)

flu3 <- c(0, 1e18, 1e19, 1e20)
tts3 <- c(0, 100, 200, 250)


test_that("Computes TTS for base and weld metals", {
  test1 <- rg199r2_p2("B", flu1, tts1, c(1e17, 1e18, 1e19, 1e20), )
  expect_equal(round(test1, 2), c(1.13, 4.31, 10.34, 15.65))
})

test_that("Computes CF for base and weld metals", {
  test1 <- rg199r2_p2("B", flu1, tts1, output = "CF")
  expect_equal(round(test1, 2), 10.34)
})

test_that("Computes FF for given fluence values", {
  test1 <- rg199r2_p2(fluence = c(1e19, 2e19), output = "FF")
  expect_equal(round(test1, 2), c(1, 1.19))
})


test_that("SD 2", {
  test1 <- rg199r2_p2("B", flu2, tts2, output = "SD")
  expect_equal(round(test1, 2), 8.5)
})

test_that("SD 2", {
  test1 <- rg199r2_p2("W", flu2, tts2, output = "SD")
  expect_equal(round(test1, 2), 14.0)
})

test_that("Margin", {
  test1 <- rg199r2_p2("W", flu2, tts2, c(1e17, 1e18, 1e19, 1e20), output = "Margin")
  expect_equal(round(test1, 2), c(14.73, 28, 28, 28))
})

# case 2
test_that("SV만 제공되는 경우, SV fluence에서 TTS 계산", {
  test1 <- rg199r2_p2(SV_flu = flu3, SV_tts = tts3)
  expect_equal(round(test1, 2), c(0.00, 74.61, 178.97, 270.89))
})

test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- rg199r2_p2("B", flu3, tts3, c(0, 1e19, 1e20, 1e21))
  expect_equal(round(test1, 2), c(0.0, 178.97, 270.89, 258.69))
})

test_that("SV로 CF 계산후, fluence로 TTS 계산", {
  test1 <- rg199r2_p2("B", flu3, tts3, c(1e19))
  expect_equal(round(test1, 2), 178.97)
})
# test_that("SV로 CF 계산후, fluence로 TTS 계산", {
#   test1 <- rg199r2_p2(c("B", "W"), 0.1, 0.6, c(1e19), CF = 1)
#   expect_equal(round(test1, 2), c(1, 1))
# })
#

# test_that("1", {
#   test1 <- rg199r2_p2(fluence = 1e20, CF = 1, output = "CF")
#   expect_equal(round(test1, 2), c(1))
# })
#
# test_that("1", {
#   test1 <- rg199r2_p2(fluence = flu3, CF = 1, output = "CF")
#   expect_equal(round(test1, 2), c(1, 1, 1, 1))
# })
#
# # case 2
# test_that("SV만 제공되는 경우, SV fluence에서 TTS 계산", {
#   test1 <- rg199r2_p2(SV_flu = flu3, SV_tts = tts3, output = "CF")
#   expect_equal(round(test1, 2), c(178.97))
# })
#
# test_that("SV로 CF 계산후, fluence로 TTS 계산", {
#   test1 <- rg199r2_p2(fluence = c(0, 1e19, 1e20, 1e21), SV_flu = flu3, SV_tts = tts3, output = "CF")
#   expect_equal(round(test1, 2), c(178.97))
# })
#
# # case 3
# test_that("SV로 CF 계산후, fluence로 TTS 계산", {
#   test1 <- rg199r2_p2("B", 0.1, 0.6, c(0, 1e19, 1e20, 1e21), output = "CF")
#   expect_equal(round(test1, 2), c(65, 65, 65, 65))
# })
# test_that("SV로 CF 계산후, fluence로 TTS 계산", {
#   test1 <- rg199r2_p2(c("B", "W"), 0.1, 0.6, c(1e19), output = "CF")
#   expect_equal(round(test1, 2), c(65.00, 122.00))
# })
#
# test_that("SV로 CF 계산후, fluence로 TTS 계산", {
#   test1 <- rg199r2_p2(c("B", "W"), 0.1, 0.6, c(1e19), SV_flu = flu3, SV_tts = tts3, CF = 1, output = "CF")
#   expect_equal(round(test1, 2), c(1, 1))
# })
# test_that("SV로 CF 계산후, fluence로 TTS 계산", {
#   test1 <- rg199r2_p2(c("B", "W"), 0.1, 0.6, c(1e19), CF = 1, output = "CF")
#   expect_equal(round(test1, 2), c(1, 1))
# })
#
# test_that("SD 1", {
#   test1 <- rg199r2_p2(c("B", "W"), output = "SD")
#   expect_equal(round(test1, 2), c(17, 28))
# })
#
#
# test_that("SD 2", {
#   test1 <- rg199r2_p2("B", fluence = 1:5 * 1e19, output = "SD")
#   expect_equal(round(test1, 2), c(17, 17, 17, 17, 17))
# })
#
# test_that("SD 2", {
#   test1 <- rg199r2_p2("B", output = "SD",
#                     SV_flu = c(1e18, 1e19, 1e20), SV_tts = c(50, 142, 200))
#   expect_equal(round(test1, 2), c(8.5, 8.5, 8.5))
# })
#
# test_that("SD 2", {
#   test1 <- rg199r2_p2("B", fluence = 1:5 * 1e19, output = "SD")
#   expect_equal(round(test1, 2), c(17, 17, 17, 17, 17))
# })
#
# test_that("SD 2", {
#   test1 <- rg199r2_p2("B",
#                     output = "SD",
#                     SV_flu = c(1e18, 1e19, 1e20), SV_tts = c(50, 142, 200)
#   )
#   expect_equal(round(test1, 2), c(8.5, 8.5, 8.5))
# })
#
#
# test_that("SD 2", {
#   test1 <- rg199r2_p2("W",
#                     CF = 1, output = "SD",
#                     fluence = 1:5 * 1e19,
#                     SV_flu = c(1e18, 1e19, 1e20), SV_tts = c(50, 142, 200)
#   )
#   expect_equal(round(test1, 2), c(14, 14, 14, 14, 14))
# })
#
#
# test_that("SD 2", {
#   test1 <- rg199r2_p2("B", fluence = 1:5 * 1e19, output = "SD")
#   expect_equal(round(test1, 2), c(17, 17, 17, 17, 17))
# })
#
#
# test_that("SD 2", {
#   df <- data.frame(
#     cf = c(35.8, 10.35, 58.29, 26, 173.33, 199.66, 176.63),
#     fluence = 3.71e19,
#     sd = c(17, 8.5, 8.5, 17, 28, 28, 28)
#   )
#   test1 <- with(df, rg199r2_p2(CF = cf, fluence = fluence, SD = sd, output = "Margin"))
#   expect_equal(round(test1, 2), c(34, 13.87, 17, 34, 56, 56, 56))
# })
