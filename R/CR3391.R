#' CR3391
#'
#' Calculate TTS using CR3391 (NUREG/CR3391 vol.2 by Guthrie (1983).
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
#' E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18) # should be 31.743721
CR3391 <- function(form, Cu, Ni, fl) {
  CF <- ifelse(form %in% c("B", "F", "P"),
    -38.39 + 555.6 * Cu + 480.1 * Cu * tanh(0.353 * Ni / Cu),
    ifelse(form %in% c("W"),
      624 * Cu - 333.1 * sqrt(Cu * Ni) + 251.2 * Ni,
      NA
    )
  )
  FF <- fl^(0.2661 - 0.0449 * log(fl))
  TTS <- CF * FF
  attr(TTS, "CF") <- CF
  attr(TTS, "FF") <- FF
  TTS
}
