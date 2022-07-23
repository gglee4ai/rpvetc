#' ASTM E900-15e2
#'
#' Provide TTS  or SD of ASTM E900-15e2.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param Cu numeric vector wt%
#' @param Ni numeric vector wt%
#' @param Mn numeric vector wt%
#' @param P  numeric vector wt%
#' @param temperature numeric vector
#' @param fluence numeric vector neutron fluence, n/fluence_area
#' @param fluence_area string c("cm2", "m2")
#' @param temperature_unit string c("Celsius", "degF")
#' @param output character c("TTS", "SD", "TTS1", "TTS2")
#'
#' @return TTS or SD as given temperature_unit
#' @export
#' @examples
#' E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18) # should be 31.74387
E900_15 <- function(product_form,
                    Cu,
                    Ni,
                    Mn,
                    P,
                    temperature,
                    fluence,
                    fluence_area = c("cm2", "m2"),
                    temperature_unit = c("Celsius", "Fahrenheit"),
                    output = c("TTS", "SD", "TTS1", "TTS2")) {

  ## check argument types
  stopifnot(is.character(product_form))
  stopifnot(is.numeric(Cu))
  stopifnot(is.numeric(Ni))
  stopifnot(is.numeric(Mn))
  stopifnot(is.numeric(P))
  stopifnot(is.numeric(temperature))
  stopifnot(is.numeric(fluence))
  fluence_area <- match.arg(fluence_area)
  temperature_unit <- match.arg(temperature_unit)
  output <- match.arg(output)

  ## input vectors lengths must be 1L or the same all
  args <- list(product_form, Cu, Ni, Mn, P, temperature, fluence)
  lengths <- vapply(args, length, FUN.VALUE = integer(1L))
  n <- max(lengths)
  stopifnot(all(lengths == 1L | lengths == n))

  ## product_form
  product_form <- trimws(product_form)
  if (!all(product_form %in% c("F", "P", "W"))) {
    stop('product_form must be one of c("F", "P", "W")')
  }

  ## fluence_area
  if (fluence_area == "cm2") { # default, n/cm2 from Plotter-22 macro
    FC <- 3.593e-10
    M_minFlu <- 4.5e16
  } else { # n/m2 from ASTM E900-15e2
    FC <- 1.8943e-12
    M_minFlu <- 4.5e20
  }

  ## temperature_unit
  if (temperature_unit == "Fahrenheit") {
    temperature <- (temperature - 32) * (5 / 9) # convert to Celcius
  }

  ## calculate TTS1
  A_fpw <- c("F" = 1.011, "P" = 1.080, "W" = 0.919)[product_form]
  TTS1 <- A_fpw *
    (5 / 9) *
    FC * fluence^0.5695 * # NOTE FC
    ((1.8 * temperature + 32) / 550)^-5.47 *
    (0.09 + P / 0.012)^0.216 *
    (1.66 + (Ni^8.54) / 0.63)^0.39 *
    (Mn / 1.36)^0.3

  ## calculate TTS2
  B_fpw <- c("F" = 0.738, "P" = 0.819, "W" = 0.968)[product_form]
  M <- B_fpw *
    pmax(pmin(113.87 * (log(fluence) - log(M_minFlu)), 612.6), 0) * # NOTE M_minFlu
    ((1.8 * temperature + 32) / 550)^-5.45 *
    (0.1 + P / 0.012)^-0.098 *
    (0.168 + (Ni^0.58) / 0.63)^0.73
  TTS2 <- (5 / 9) * pmax(pmin(Cu, 0.28) - 0.053, 0) * M

  TTS <- TTS1 + TTS2

  ## choose return value
  if (output == "SD") {
    C_fpw <- c("F" = 6.972, "P" = 6.593, "W" = 7.681)[product_form]
    D_fpw <- c("F" = 0.199, "P" = 0.163, "W" = 0.181)[product_form]
    result <- C_fpw * TTS^D_fpw # calculate SD
  } else if (output == "TTS1") {
    result <- TTS1
  } else if (output == "TTS2") {
    result <- TTS2
  } else {
    result <- TTS # default
  }

  ## change return value along temperature_unit
  if (temperature_unit == "Fahrenheit") {
    result <- result * (9 / 5) # convert to Fahrenheit
  }

  unname(result)
}
