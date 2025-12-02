#' ASTM E900-15e2 (2020) Transition Temperature Shift (TTS) and Standard Deviation Calculation
#'
#' Computes either the Transition Temperature Shift (TTS) components (TTS1, TTS2) or
#' the Standard Deviation (SD), based on the ASTM E900-15e2 (2015) embrittlement model.
#'
#' This function calculates radiation-induced embrittlement in reactor pressure vessel (RPV) materials
#' using neutron fluence and chemical composition. It provides estimates for two components of TTS (TTS1 and TTS2),
#' their sum (TTS), and the associated standard deviation (SD).
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
#' @param output Character, specifying which property to compute. Must be one of:
#'   \itemize{
#'     \item \code{"TTS"} – Total Transition Temperature Shift (TTS1 + TTS2)
#'     \item \code{"TTS1"} – First component of TTS
#'     \item \code{"TTS2"} – Second component of TTS
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
#' @param use_names Logical. If \code{TRUE}, result vector retains names (if any). Default is \code{FALSE}.
#'
#' @return A numeric vector of TTS, TTS1, TTS2, or SD in degrees Celsius or Fahrenheit, depending on the specified \code{output_unit}.
#'
#' @details
#' The model is described in ASTM E900-15e2 (2020) and is applicable to RPV steels exposed to neutron irradiation.
#' TTS1 primarily reflects matrix damage and is influenced by Ni, Mn, P, and temperature. TTS2 captures
#' precipitation hardening and is governed by Cu, Ni, and P.
#'
#' @examples
#' # Compute total TTS in Celsius
#' ASTM_E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.5e18, output = "TTS")
#'
#' # Compute TTS1 in Fahrenheit with input temperature also in Fahrenheit
#' ASTM_E900_15("F", 0.15, 0.2, 1.4, 0.01, 570, 1e19,
#'   output = "TTS1", output_unit = "Fahrenheit", temperature_unit = "Fahrenheit"
#' )
#'
#' # Compute SD in Celsius (default temperature input assumed in Celsius)
#' ASTM_E900_15("W", 0.25, 0.3, 1.5, 0.015, 275, 5e18,
#'   output = "SD", output_unit = "Celsius"
#' )
#'
#' @export
ASTM_E900_15 <- function(product_form, # F, P, W
                         Cu, # wt%
                         Ni, # wt%
                         Mn, # wt%
                         P, # wt%
                         temperature, # degC
                         fluence, # n/cm2
                         output = c("TTS", "TTS1", "TTS2", "SD"),
                         output_unit = c("Celsius", "Fahrenheit"),
                         temperature_unit = c("Celsius", "Fahrenheit"),
                         use_names = FALSE) {
  # Match and validate input arguments
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)
  temperature_unit <- match.arg(temperature_unit, several.ok = FALSE)

  # Check product_form
  product_form <- as.character(product_form) # for factor
  if (any(!product_form %in% c("F", "P", "W"))) {
    stop("Invalid 'product_form'. Must be one of: 'F', 'P', or 'W'.")
  }

  # Check numeric validity
  stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
  stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))
  stopifnot(is.numeric(Mn), all(Mn >= 0 & Mn <= 100))
  stopifnot(is.numeric(P), all(P >= 0 & P <= 100))
  stopifnot(is.numeric(temperature), all(temperature >= -273.15))
  stopifnot(is.numeric(fluence), all(fluence >= 0))

  # Convert input temperature unit
  if (temperature_unit == "Fahrenheit") {
    temperature <- F_to_C(temperature)
  }

  # Broadcast shorter vectors to match lengths
  expanded <- expand_vectors(product_form, Cu, Ni, Mn, P, temperature, fluence)
  pf <- expanded[[1]]
  cu <- expanded[[2]]
  ni <- expanded[[3]]
  mn <- expanded[[4]]
  ps <- expanded[[5]]
  tc <- expanded[[6]]
  fl <- expanded[[7]] * 1e4 # n/m^2 (E > 1 Mev)

  # Compute selected output
  result <- switch(output,
    "TTS1" = astm_e900_15_tts1(pf, ni, mn, ps, tc, fl),
    "TTS2" = astm_e900_15_tts2(pf, cu, ni, ps, tc, fl),
    "TTS" = astm_e900_15_tts(pf, cu, ni, mn, ps, tc, fl),
    "SD" = {
      tts <- astm_e900_15_tts(pf, cu, ni, mn, ps, tc, fl)
      astm_e900_15_sd(pf, tts)
    }
  )

  # Convert degF to degC if needed
  if (output_unit == "Fahrenheit") {
    result <- dC_to_dF(result)
  }

  # Return result with or without names
  if (use_names) result else unname(result)
}

# TTS1
astm_e900_15_tts1 <- function(product_form, Ni, Mn, P, temperature, fluence) {
  A <- c("F" = 1.011, "P" = 1.080, "W" = 0.919)[product_form]
  out <- A * 1.8943e-12 * (fluence^0.5695) # fluence n/m2
  out <- out * (((1.8 * temperature + 32) / 550)^(-5.47))
  out <- out * ((0.09 + P / 0.012)^0.216)
  out <- out * ((1.66 + (Ni^8.54) / 0.63)^0.39)
  out <- out * ((Mn / 1.36)^0.3)
  out * (5 / 9)
}

# TTS2
astm_e900_15_tts2 <- function(product_form, Cu, Ni, P, temperature, fluence) {
  B <- c("F" = 0.738, "P" = 0.819, "W" = 0.968)[product_form]
  M <- B * pmax(pmin(113.87 * (log(fluence) - log(4.5e20)), 612.6), 0) # fluence n/m2
  M <- M * (((1.8 * temperature + 32) / 550)^(-5.45))
  M <- M * ((0.1 + P / 0.012)^(-0.098))
  M <- M * ((0.168 + (Ni^0.58) / 0.63)^0.73)
  out <- pmax(pmin(Cu, 0.28) - 0.053, 0) * M
  out * (5 / 9)
}

# TTS = TTS1 + TTS2
astm_e900_15_tts <- function(product_form, Cu, Ni, Mn, P, temperature, fluence) {
  tts1 <- astm_e900_15_tts1(product_form, Ni, Mn, P, temperature, fluence)
  tts2 <- astm_e900_15_tts2(product_form, Cu, Ni, P, temperature, fluence)
  tts1 + tts2
}

# SD
astm_e900_15_sd <- function(product_form, tts) {
  C <- c("F" = 6.972, "P" = 6.593, "W" = 7.681)[product_form]
  D <- c("F" = 0.199, "P" = 0.163, "W" = 0.181)[product_form]
  C * (tts^D)
}
