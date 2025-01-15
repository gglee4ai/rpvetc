test_fluence <- c(1.32, 2.76, 3.96, 4.52, 5.72) * 1e19
test_tts <- c(63.27, 40.84, 75.9, 78.85, 78.22)

# test_that("calculate TTS from RG1.99 Rev.2", {
#   test1 <- RG199R2("P", 0.2, 0.18, test_fluence)
#   expect_equal(round(test1, 2), c(107.72, 127.06, 135.40, 138.20, 142.79))
# })

test_that("calculate TTS from RG1.99 Rev.2", {
  test1 <- RG199R2("B", 0, 0, output = "CF", temperature_unit = "F")
  expect_equal(round(test1, 2), 20)
})

# test_that("calculate TTS from RG1.99 Rev.2", {
#   test1 <- RG199R2("B", 0.2, 0.18, output = "sigma")
#   expect_equal(round(test1, 2), 9.44)
# })
#
# test_that("calculate TTS from RG1.99 Rev.2", {
#   test1 <- RG199R2("W", 0.2, 0.18, output = "sigma")
#   expect_equal(round(test1, 2), 18.89)
# })

# test_that("fluence, tts가 입력되었을 때", {
#   test1 <- RG199R2(fluence = test_fluence, tts = test_tts, output = "CF")
#   expect_equal(round(test1, 2), 51.83)
# })

