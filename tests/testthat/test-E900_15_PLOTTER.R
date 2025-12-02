test_that("TTS (total) is computed correctly for base metal P", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 2), c(31.74))
})

test_that("TTS1 is computed correctly for base metal P", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS1")
  expect_equal(round(test1, 2), c(7.85))
})

test_that("TTS2 is computed correctly for base metal P", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS2")
  expect_equal(round(test1, 2), c(23.90))
})

test_that("Standard Deviation (SD) is computed correctly for base metal P", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "SD")
  expect_equal(round(test1, 2), c(11.58))
})

test_that("Repeated test for total TTS (should return same value)", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  expect_equal(round(test1, 2), c(31.74))
})

test_that("Repeated test for TTS1 (should return same value)", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS1")
  expect_equal(round(test1, 2), c(7.85))
})

test_that("Repeated test for TTS2 (should return same value)", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "TTS2")
  expect_equal(round(test1, 2), c(23.90))
})

test_that("Repeated test for SD (should return same value)", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, output = "SD")
  expect_equal(round(test1, 2), c(11.58))
})

test_that("Output in Fahrenheit is converted correctly from Celsius", {
  test1 <- E900_15_plotter("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18,
                   output_unit = "F", temperature_unit = "C")
  expect_equal(round(test1, 2), c(57.14))
})

test_that("TTS computed correctly for multiple product forms with same conditions", {
  test1 <- E900_15_plotter(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)
  y <- c(28.88, 31.74, 34.92)
  expect_equal(round(test1, 2), y)
})

test_that("TTS reflects fluence variation across multiple product forms", {
  test1 <- E900_15_plotter(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, c(0, 1e18, 1e19))
  y <- c(0, 22.91, 52.05)
  expect_equal(round(test1, 2), y)
})

test_that("SD reflects fluence variation across multiple product forms", {
  test1 <- E900_15_plotter(c("F", "P", "W"), 0.2, 0.18, 1.36, 0.012, 290, c(0, 1e18, 1e19),
                   output = "SD", use_names = TRUE)
  y <- c("F" = 0, "P" = 10.98, "W" = 15.71)
  expect_equal(round(test1, 2), y)
})

test_that("e900_15_plotter_tts1 computes correct TTS1 component", {
  val <- e900_15_plotter_tts1("P", Ni = 0.18, Mn = 1.36, P = 0.012, temperature = 290, fluence = 2.56894e18)
  expect_equal(round(val, 2), c(P = 7.85))
})

test_that("e900_15_plotter_tts2 computes correct TTS2 component", {
  val <- e900_15_plotter_tts2("P", Cu = 0.2, Ni = 0.18, P = 0.012, temperature = 290, fluence = 2.56894e18)
  expect_equal(round(val, 2), c(P = 23.90))
})

test_that("e900_15_plotter_tts equals tts1 + tts2", {
  tts_total <- e900_15_plotter_tts("P", Cu = 0.2, Ni = 0.18, Mn = 1.36, P = 0.012, temperature = 290, fluence = 2.56894e18)
  expect_equal(round(tts_total, 2), c(P = 31.74))
})

test_that("e900_15_plotter_sd computes SD correctly", {
  tts_val <- 31.74
  val <- e900_15_plotter_sd("P", tts = tts_val)
  expect_equal(round(val, 2), c(P = 11.58))
})

test_that("e900_15_plotter_cf1 computes normalized CF1 value", {
  val <- e900_15_plotter_cf1("P", Ni = 0.18, Mn = 1.36, P = 0.012, temperature = 290)
  expect_equal(round(val, 3), c(P = 17.015))  # Approximate check
})

test_that("e900_15_plotter_cf2 computes normalized CF2 value", {
  val <- e900_15_plotter_cf2("P", Cu = 0.2, Ni = 0.18, P = 0.012, temperature = 290)
  expect_equal(round(val, 4), c(P =  31.78660))  # Approximate check
})

test_that("e900_15_plotter_ff1 returns normalized fluence function", {
  val <- e900_15_plotter_ff1(2.56894e18)
  expect_equal(round(val, 4), round(0.4611635, 4))
})

test_that("e900_15_plotter_ff2 returns normalized fluence function", {
  val <- e900_15_plotter_ff2(2.56894e18)
  raw <- 113.87 * (log(2.56894e18) - log(4.5e16))
  clamped <- min(max(raw, 0), 612.6)
  expect_equal(round(val, 4), round(clamped / 612.6, 4))
})

test_that("e900_15_plotter_tts_by_cf computes total TTS from CF1 and CF2", {
  cf1 <- 0.2437
  cf2 <- 0.7405
  fluence <- 2.56894e18
  val <- e900_15_plotter_tts_by_cf(cf1, cf2, fluence)
  expect_equal(round(val, 2), round(0.6690987, 2))
})
