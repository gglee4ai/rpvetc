test_fluence <- c(0.54, 1.12, 1.23, 2.94, 3.89, 5.52) * 1e19 # Kori1 F1
test_tts <- c(-9.27, -11.05, 3.12, 12.80, 24.03, 37.56) #

# test_that("_CF_table", {
#   test1 <- _CF_table("B", 0.2, 1.0)
#   expect_equal(round(test1, 2), 164)
# })

# test_that("_CF_by_SV", {
#   test1 <- _CF_by_SV(test_fluence, test_tts)
#   expect_equal(round(test1, 2), 10.34)
# })

test_that("RG199R2_SV1", {
  test1 <- RG199R2_SV(test_fluence, test_tts, output = "CF", temperature_unit = "F")
  expect_equal(round(test1, 2), 10.34)
})
#
test_that("RG199R2_SV2", {
  test1 <- RG199R2_SV(test_fluence, test_tts, output = "TTS", temperature_unit = "F")
  expect_equal(round(test1, 2), c(8.56, 10.67, 10.94, 13.3, 13.96, 14.7))
})

test_that("RG199R2_SV3", {
  test1 <- RG199R2_SV(c(0, 0.1, 0.2) * 1e19, c(0, 10, 20), output = "TTS", temperature_unit = "F")
  expect_equal(round(test1, 2), c(0, 13.02, 17.79))
})

test_that("RG199R2_SV4", {
  test1 <- RG199R2_SV(test_fluence, test_tts, output = "SD", temperature_unit = "F")
  expect_equal(round(test1, 2), rep(0, 6))
})


# test_that("calculate TTS from RG1.99 Rev.2", {
#   test1 <- ("B", 0.2, 0.18, output = "sigma")
#   expect_equal(round(test1, 2), 9.44)
# })
#
# test_that("calculate TTS from RG1.99 Rev.2", {
#   test1 <- ("W", 0.2, 0.18, output = "sigma")
#   expect_equal(round(test1, 2), 18.89)
# })

# test_that("fluence, tts가 입력되었을 때", {
#   test1 <- (fluence = test_fluence, tts = test_tts, output = "CF")
#   expect_equal(round(test1, 2), 51.83)
# })

