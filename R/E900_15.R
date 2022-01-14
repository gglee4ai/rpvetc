#' ASTM E900-15e2
#'
#' Provide TTS  or SD of ASTM E900-15e2.
#'
#' @param product string c("F", "P", "W")
#' @param Cu number *wt%*.
#' @param Ni number weight-percentage
#' @param Mn number weight-percentage
#' @param P number weight-percentage
#' @param temperature number degree Celcius
#' @param fluence number n/cm2
#' @param temperature_unit string c("degC", "degF")
#' @param fluence_area string c("cm2", "m2")
#' @param output string c("tts", "sd", "tts1", "tts2)
#'
#' @return TTS as given temperature_unit
#' @export
#' @examples
#' E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18)  # should be 31.743721
E900_15 <- function(product, Cu, Ni, Mn, P, temperature, fluence,
                    temperature_unit = "degC",
                    fluence_area = "cm2",
                    output = "tts") {

  # check arguments
  pos <- trimws(product)
  if (!all(pos %in% c("F", "P", "W"))) {
    stop('product must be c("F", "P", "W")')
  }

  if (temperature_unit == "degC") {
    temperature <- temperature
  } else if (temperature_unit == "degF") {
    temperature <- (temperature - 32) * 5/9
  } else {
    stop("temperature_unit must be 'degC' or 'degF'")
  }

  if (fluence_area == "cm2")
    fluence <- fluence * 10000  # convert to n/cm2 -> n/m2 in ASTM E900-15
  else if (fluence_area == "m2") {
    fluence <- fluence
  } else {
    stop("fluence_area must be 'cm2' or 'm2'")
  }

  if (!(output %in% c("tts", "sd", "tts1", "tts2"))) {
    stop('output must be in c("tts", "sd", "tts1", "tts2")')
  }

  # calculate ASTM E900-15e2
  pos[pos %in% c("F", "forgings")] <- "1"
  pos[pos %in% c("P", "plates", "SRM")] <- "2"
  pos[pos %in% c("W", "welds")] <- "3"
  pos <- strtoi(pos)

  A_factor <- c(1.011, 1.080, 0.919)  # F, P, W
  TTS1 <- A_factor[pos] *
    (5/9) *
    1.8943e-12 * fluence^0.5695 *
    ((1.8*temperature + 32)/550)^-5.47 *
    (0.09 + P/0.012)^0.216 *
    (1.66 + (Ni^8.54)/0.63)^0.39 *
    (Mn/1.36)^0.3

  B_factor <- c(0.738, 0.819, 0.968)  # F, P, W
  M <- B_factor[pos] *
    pmax(pmin(113.87 * (log(fluence) - log(4.5e20)), 612.6), 0) *
    ((1.8*temperature + 32)/550)^-5.45 *
    (0.1 + P/0.012)^-0.098 *
    (0.168 + (Ni^0.58)/0.63)^0.73
  TTS2 <- (5/9) * pmax(pmin(Cu, 0.28) - 0.053, 0) * M

  TTS <- TTS1 + TTS2

  # choose return value
  if (output == "tts") {
    res <- TTS
  } else if (output == "tts1") {
    res <- TTS1
  } else if (output == "tts2") {
    res <- TTS2
  } else {
    SD_factor1 <- c(6.972, 6.593, 7.681)  # F, P, W
    SD_factor2 <- c(0.199, 0.163, 0.181)  # F, P, W
    res <- SD_factor1[pos] * TTS^SD_factor2[pos]
  }

  # change return value unit
  if (temperature_unit == "degF") {
    res <- res * 1.8  # convert to delta degF
  }
  res
}
