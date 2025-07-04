#' ASTM E900-15e2 with MRP-462 (2021) Flux Effect on Transition Temperature Shift (TTS)
#'
#' Computes the Transition Temperature Shift (TTS) components (TTS1, TTS2) or Standard Deviation (SD)
#' based on the upgraded ASTM E900-15e2 embrittlement model, as modified in MRP-462 (2021) to incorporate
#' flux effects on embrittlement.
#'
#' The function calculates embrittlement effects in reactor pressure vessel materials using neutron fluence,
#' neutron flux, and material properties. The model introduces flux-dependent terms to improve accuracy.
#'
#' @param product_form character vector, specifying the product form. Must be one of:
#'        \itemize{
#'          \item \code{"F"} - Forgings
#'          \item \code{"P"} - Plate
#'          \item \code{"W"} - Weld metal
#'        }
#' @param Cu numeric vector, copper content in weight percent (wt%). Used in TTS2 calculation.
#' @param Ni numeric vector, nickel content in weight percent (wt%). Used in both TTS1 and TTS2 calculations.
#' @param Mn numeric vector, manganese content in weight percent (wt%). Used in TTS1 calculation.
#' @param P numeric vector, phosphorus content in weight percent (wt%). Used in both TTS1 and TTS2 calculations.
#' @param temperature numeric vector, reactor operating temperature in degrees Celsius (°C) or Fahrenheit (°F).
#'        The unit should be specified using \code{output_unit}.
#' @param fluence numeric vector, neutron fluence in n/cm². Must be non-negative.
#' @param flux numeric vector, neutron flux in n/cm²/s. Must be positive.
#' @param output character, specifying which property to compute. Must be one of:
#'        \itemize{
#'          \item \code{"TTS"}  - Total Transition Temperature Shift (TTS1 + TTS2)
#'          \item \code{"TTS1"} - First component of TTS, incorporating flux effects
#'          \item \code{"TTS2"} - Second component of TTS, incorporating flux effects
#'          \item \code{"SD"}   - Standard Deviation of the TTS estimation
#'        }
#' @param output_unit character, specifying the input and output temperature unit. Must be one of:
#'        \itemize{
#'          \item \code{"Celsius"} - Returns the result in degrees Celsius.
#'          \item \code{"Fahrenheit"} - Returns the result in degrees Fahrenheit.
#'        }
#'
#' @return A numeric vector representing the computed result in the specified \code{output_unit}.
#'         The value corresponds to the selected \code{output} parameter (TTS, TTS1, TTS2, or SD).
#'
#' @examples
#' # Example 1: Compute total TTS for a plate material with flux effect
#' E900_15_flux("P",
#'   Cu = 0.2, Ni = 0.18, Mn = 1.36, P = 0.012,
#'   temperature = 290, fluence = 2.56894e18, flux = 1e13,
#'   output = "TTS", output_unit = "Celsius"
#' )
#'
#' # Example 2: Compute TTS1 component with flux effect
#' E900_15_flux("F",
#'   Cu = 0.15, Ni = 0.2, Mn = 1.4, P = 0.01,
#'   temperature = 300, fluence = 1e19, flux = 5e12,
#'   output = "TTS1", output_unit = "Fahrenheit"
#' )
#'
#' # Example 3: Compute Standard Deviation (SD) for a weld metal under flux effect
#' E900_15_flux("W",
#'   Cu = 0.25, Ni = 0.3, Mn = 1.5, P = 0.015,
#'   temperature = 275, fluence = 5e18, flux = 2e13,
#'   output = "SD", output_unit = "Celsius"
#' )
#'
#' @seealso \code{\link{E900_15}}
#'
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
                      output_unit = c("Celsius", "Fahrenheit")) {
  # Input requirement checks
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)

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
