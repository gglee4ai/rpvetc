#' ASTM E900-15e2 with MRP-462 (2021): Flux-Adjusted Transition Temperature Shift (TTS) Model
#'
#' Computes the Transition Temperature Shift (TTS) components (TTS1, TTS2) or Standard Deviation (SD)
#' based on the ASTM E900-15e2 embrittlement model, modified by MRP-462 (2021) to incorporate
#' neutron flux effects.
#'
#' This function estimates radiation-induced embrittlement in reactor pressure vessel (RPV) materials
#' by considering neutron fluence, neutron flux, chemical composition, and temperature.
#' The MRP-462 model introduces flux-sensitive adjustments to improve the prediction accuracy,
#' especially for high-flux irradiation environments.
#'
#' @param product_form Character vector, specifying the product form. Must be one of:
#'   \itemize{
#'     \item \code{"F"} – Forgings
#'     \item \code{"P"} – Plate
#'     \item \code{"W"} – Weld metal
#'   }
#' @param Cu Numeric vector, copper content in weight percent (wt%). Used in TTS2 calculation.
#' @param Ni Numeric vector, nickel content in weight percent (wt%). Used in both TTS1 and TTS2 calculations.
#' @param Mn Numeric vector, manganese content in weight percent (wt%). Used in TTS1 calculation.
#' @param P Numeric vector, phosphorus content in weight percent (wt%). Used in both TTS1 and TTS2 calculations.
#' @param temperature Numeric vector, reactor operating temperature. Unit is defined by \code{temperature_unit}.
#' @param fluence Numeric vector, neutron fluence in n/cm². Must be non-negative.
#' @param flux Numeric vector, neutron flux in n/cm²/s. Must be positive.
#' @param output Character, specifying which property to compute. Must be one of:
#'   \itemize{
#'     \item \code{"TTS"} – Total Transition Temperature Shift (TTS1 + TTS2)
#'     \item \code{"TTS1"} – First component of TTS, including flux effects
#'     \item \code{"TTS2"} – Second component of TTS, including flux effects
#'     \item \code{"SD"} – Standard deviation of the TTS estimation
#'   }
#' @param output_unit Character, specifying the unit of the returned value. Must be one of:
#'   \itemize{
#'     \item \code{"Celsius"} – Return values in degrees Celsius (default)
#'     \item \code{"Fahrenheit"} – Return values in degrees Fahrenheit
#'   }
#' @param temperature_unit Character, specifying the unit of the \code{temperature} input. Must be one of:
#'   \itemize{
#'     \item \code{"Celsius"} – Input temperature is in degrees Celsius (default)
#'     \item \code{"Fahrenheit"} – Input temperature is in degrees Fahrenheit
#'   }
#'
#' @return A numeric vector representing the computed result based on the selected \code{output},
#'         expressed in the unit specified by \code{output_unit}.
#'
#' @details
#' This model builds on ASTM E900-15e2 by incorporating neutron flux as an additional variable in the embrittlement
#' prediction equations. The effect of flux is applied to both TTS1 and TTS2 components to account for dynamic
#' irradiation conditions in modern nuclear plants.
#'
#' @examples
#' # Example 1: Compute total TTS in Celsius with flux effect
#' E900_15_flux("P",
#'   Cu = 0.2, Ni = 0.18, Mn = 1.36, P = 0.012,
#'   temperature = 290, fluence = 2.5e18, flux = 1e13,
#'   output = "TTS", output_unit = "Celsius"
#' )
#'
#' # Example 2: Compute TTS1 in Fahrenheit with input temperature in Fahrenheit
#' E900_15_flux("F",
#'   Cu = 0.15, Ni = 0.2, Mn = 1.4, P = 0.01,
#'   temperature = 570, fluence = 1e19, flux = 5e12,
#'   output = "TTS1", output_unit = "Fahrenheit", temperature_unit = "Fahrenheit"
#' )
#'
#' # Example 3: Compute SD in Celsius
#' E900_15_flux("W",
#'   Cu = 0.25, Ni = 0.3, Mn = 1.5, P = 0.015,
#'   temperature = 275, fluence = 5e18, flux = 2e13,
#'   output = "SD", output_unit = "Celsius"
#' )
#'
#' @seealso \code{\link{ASTM_E900_15}} for the base model without flux correction
#' @export
E900_15_flux <- function(product_form,
                         Cu,
                         Ni,
                         Mn,
                         P,
                         temperature,
                         fluence,
                         flux,
                         output = c("TTS", "SD", "TTS1", "TTS2"),
                         output_unit = c("Celsius", "Fahrenheit"),
                         temperature_unit = c("Celsius", "Fahrenheit")) {
  # Input requirement checks
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)
  temperature_unit <- match.arg(temperature_unit, several.ok = FALSE)

  form <- as.character(product_form)
  if (any(!form %in% c("F", "P", "W"))) {
    stop("Invalid 'product_form'. Must be one of: 'F', 'P', or 'W'.")
  }
  stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
  stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))
  stopifnot(is.numeric(Mn), all(Mn >= 0 & Mn <= 100))
  stopifnot(is.numeric(P), all(P >= 0 & P <= 100))
  stopifnot(is.numeric(temperature), all(temperature >= -273.15))
  stopifnot(is.numeric(fluence), all(fluence >= 0))
  stopifnot(is.numeric(flux), all(flux >= 0))

  # Convert input temperature unit
  if (temperature_unit == "Fahrenheit") {
    temperature <- F_to_C(temperature)
  }

  # Expand vectors
  expanded <- expand_vectors(product_form, Cu, Ni, Mn, P, temperature, fluence, flux)
  pf <- expanded[[1]]
  cu <- expanded[[2]]
  ni <- expanded[[3]]
  mn <- expanded[[4]]
  ps <- expanded[[5]]
  tc <- expanded[[6]]
  fl <- expanded[[7]]
  fx <- expanded[[8]]

  # TTS1
  calc_tts1 <- function(product_form, Ni, Mn, P, temperature, fluence, flux) {
    log10flux <- log10(flux)
    A <- c("F" = 0.785, "P" = 0.774, "W" = 0.798)[product_form]
    out <- A * 4.074E-10 * (fluence)^0.5724 # 4.074E-10 은 cm2으로 미리 계산 from plotter22 macro
    out <- out * ((1.8 * temperature + 32) / 550)^-4.10
    out <- out * (1.62 + P / 0.012)^0.461
    out <- out * (0.29 + (Ni^17.63) / 0.63)^0.107
    out <- out * (Mn / 1.36)^0.44
    out <- out * (log10flux / 11.8)^1.64
    out * (5 / 9)
  }

  # TTS2
  calc_tts2 <- function(product_form, Cu, Ni, P, temperature, fluence, flux) {
    log10flux <- log10(flux)
    B <- c("F" = 0.519, "P" = 0.592, "W" = 0.595)[product_form]
    M <- B * pmax(pmin(73.67 * (log(fluence) - log(3.6e16)), 604.0), 0)
    M <- M * ((1.8 * temperature + 32) / 550)^-3.27
    M <- M * (0.01 + P / 0.012)^-0.026
    M <- M * (0.705 + (Ni^0.64) / 0.63)^1.24
    M <- M * (log10flux / 15.9)^-0.52
    out <- pmax(pmin(Cu, 0.30) - 0.046, 0) * M
    out * (5 / 9)
  }

  # TTS = TTS1 + TTS2
  calc_tts <- function(product_form, Cu, Ni, Mn, P, temperature, fluence, flux) {
    tts1 <- calc_tts1(product_form, Ni, Mn, P, temperature, fluence, flux)
    tts2 <- calc_tts2(product_form, Cu, Ni, P, temperature, fluence, flux)
    tts1 + tts2
  }

  # SD 계산
  calc_sd <- function(product_form, Cu, Ni, Mn, P, temperature, fluence, flux) {
    tts <- calc_tts(product_form, Cu, Ni, Mn, P, temperature, fluence, flux)
    C <- c("F" = 6.818, "P" = 6.293, "W" = 6.862)[product_form]
    D <- c("F" = 0.238, "P" = 0.202, "W" = 0.237)[product_form]
    C * tts^D
  }

  # Output calculation
  result <- switch(output,
    "TTS1" = calc_tts1(pf, ni, mn, ps, tc, fl, fx),
    "TTS2" = calc_tts2(pf, cu, ni, ps, tc, fl, fx),
    "TTS" = calc_tts(pf, cu, ni, mn, ps, tc, fl, fx),
    "SD" = calc_sd(pf, cu, ni, mn, ps, tc, fl, fx)
  )

  # Convert degF to degC if needed
  if (output_unit == "Fahrenheit") {
    result <- dC_to_dF(result)
  }

  unname(result)
}
