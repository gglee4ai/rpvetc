# test_fluence <- c(0.54, 1.12, 1.23, 2.94, 3.89, 5.52) * 1e19 # Kori1 F1
# test_tts <- c(-9.27, -11.05, 3.12, 12.80, 24.03, 37.56) #
#
# ## check CF
#
# test_that("RG199R2 1", {
#   test1 <- RG199R2("B", 0.1, 0.6, output = "CF", temperature_unit = "F")
#   expect_equal(round(test1, 2), 65)
# })
# test_that("RG199R2 2", {
#   test1 <- RG199R2("B", 0.1, 0.6, 1e19, output = "CF", temperature_unit = "F")
#   expect_equal(round(test1, 2), 65)
# })
# test_that("RG199R2 3", {
#   test1 <- RG199R2("B", 0.1, 0.6, 0.1e19, output = "CF", temperature_unit = "F")
#   expect_equal(round(test1, 2), 65)
# })
#
# sv_flu <- seq(0, 1, 0.2) * 1e19
# sv_tts <- sv_flu * 10 / 1e19
# test_that("RG199R2 4", {
#   test1 <- RG199R2("B", 0.1, 0.6, SV_flu = sv_flu, SV_tts = sv_tts, output = "CF", temperature_unit = "F")
#   expect_equal(round(test1, 2), 7.66)
# })
#
#

#
# f <- seq(0, 1, 0.2) * 1e19
# test_that("RG199R2 1", {
#   test1 <- RG199R2("B", 0.1, 0.6, f, output = "TTS", temperature_unit = "F")
#   expect_equal(round(test1, 2), c(0, 37.01, 48.49, 55.7, 60.93, 65))
# })
#
# test_that("RG199R2 7", {
#   test1 <- RG199R2("B", 0.1, 0.6, SV_flu = f, SV_tts = f * 10 / 1e19, output = "CF", temperature_unit = "F")
#   expect_equal(round(test1, 2), -1)
# })

#
#
# ## test SD
# test_that("RG199R2 1", {
#   test1 <- RG199R2("B", 0.1, 0.6, 1e19, output = "SD", temperature_unit = "F")
#   expect_equal(round(test1, 2), c(34.0))
# })
# test_that("RG199R2 1", {
#   test1 <- RG199R2("W", 0.1, 0.6, c(0, 1e19), output = "SD", temperature_unit = "F")
#   expect_equal(round(test1, 2), c(56.0))
# })
#
# f <- seq(0, 1, 0.2) * 1e19
# test_that("RG199R2 1", {
#   test1 <- RG199R2("B", 0.1, 0.6, f, output = "SD", temperature_unit = "F")
#   expect_equal(round(test1, 2), 34)
# })
#
#
# f <- seq(0, 1, 0.2) * 1e19
# test_that("RG199R2 7", {
#   test1 <- RG199R2("B", 0.1, 0.6, f, SV = f * 10 / 1e19, output = "SD", temperature_unit = "F")
#   expect_equal(round(test1, 2), -1)
# })
#
#
# f <- seq(0, 1, 0.2) * 1e19
# test_that("RG199R2 8", {
#   test1 <- RG199R2(CF = 1, fluence = f, output = "TTS", temperature_unit = "F")
#   expect_equal(round(test1, 2), c(0, 0.57, 0.75, 0.86, 0.94, 1)
# )
# })



#
# test_that("RG199R2 1", {
#   test1 <- RG199R2("B", 0.1, 0.6, test_fluence, outp)
#   expect_equal(round(test1, 2), c(29.89, 37.25, 38.19, 46.43, 48.76, 51.33))
# })
#
# test_that("RG199R2 2", {
#   test1 <- RG199R2(fluence = test_fluence, SV = test_tts)
#   expect_equal(round(test1, 2), c(4.76, 5.93, 6.08, 7.39, 7.76, 8.17))
# })

# test_that("RG199R2 1", {
#   test1 <- RG199R2("B", 0.1, 0.6, fluence = c(0.1, 0.2, 0.3) * 1e19, output = "SD", temperature_unit = "F")
#   expect_equal(round(test1, 2), c(17, 17, 17)
# )
# })
#
# test_that("RG199R2 2", {
#   test1 <- RG199R2(c("W", "B", "W"), 0.1, 0.6, output = "SD", temperature_unit = "C")
#   expect_equal(round(test1, 2), c(15.56, 9.44, 15.56))
# })
#
# test_that("RG199R2_SV 1", {
#   test1 <- RG199R2_SV(test_fluence, test_tts, output = "CF", temperature_unit = "F")
#   expect_equal(round(test1, 2), rep(10.34, 6))
# })
#
# test_that("RG199R2_SV2", {
#   test1 <- RG199R2_SV(test_fluence, test_tts, output = "TTS", temperature_unit = "F")
#   expect_equal(round(test1, 2), c(8.56, 10.67, 10.94, 13.3, 13.96, 14.7))
# })
#
# test_that("RG199R2_SV3", {
#   test1 <- RG199R2_SV(c(0, 0.1, 0.2) * 1e19, c(0, 10, 20), output = "TTS", temperature_unit = "F")
#   expect_equal(round(test1, 2), c(0, 13.02, 17.79))
# })
#
# test_that("RG199R2_SV4", {
#   test1 <- RG199R2_SV(test_fluence, test_tts, output = "SD", temperature_unit = "F")
#   expect_equal(round(test1, 2), rep(-100, 6))
# })


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

