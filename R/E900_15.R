#' ASTM E900-15e2 (2015) Transition Temperature Shift (TTS) and Standard Deviation Calculation
#'
#' Computes the Transition Temperature Shift (TTS) components (TTS1, TTS2) or Standard Deviation (SD)
#' based on the ASTM E900-15e2 (2015) embrittlement model.
#'
#' The function calculates embrittlement effects in reactor pressure vessel materials
#' using neutron fluence and material properties, providing estimates for TTS1, TTS2,
#' their sum (TTS), and the associated standard deviation (SD).
#'
#' @param product_form Character vector, specifying the product form. Must be one of:
#'        \itemize{
#'          \item \code{"F"} - Forgings
#'          \item \code{"P"} - Plate
#'          \item \code{"W"} - Weld metal
#'        }
#' @param Cu Numeric vector, copper content in weight percent (wt%). Used in TTS2 calculation.
#' @param Ni Numeric vector, nickel content in weight percent (wt%). Used in both TTS1 and TTS2 calculations.
#' @param Mn Numeric vector, manganese content in weight percent (wt%). Used in TTS1 calculation.
#' @param P Numeric vector, phosphorus content in weight percent (wt%). Used in both TTS1 and TTS2 calculations.
#' @param temperature Numeric vector, reactor operating temperature in degrees Celsius (°C).
#' @param fluence Numeric vector, neutron fluence in n/cm². Must be non-negative.
#' @param output Character, specifying which property to compute. Must be one of:
#'        \itemize{
#'          \item \code{"TTS"}  - Total Transition Temperature Shift (TTS1 + TTS2)
#'          \item \code{"TTS1"} - First component of TTS
#'          \item \code{"TTS2"} - Second component of TTS
#'          \item \code{"SD"}   - Standard Deviation of the TTS estimation
#'        }
#' @param output_unit Character, specifying the unit of the output result. Must be one of:
#'        \itemize{
#'          \item \code{"Celsius"} - Output will be returned in degrees Celsius (default)
#'          \item \code{"Fahrenheit"} - Output will be returned in degrees Fahrenheit
#'        }
#'
#' @return A numeric vector representing the computed result (TTS, TTS1, TTS2, or SD),
#'         expressed in the unit specified by \code{output_unit} (Celsius or Fahrenheit).
#'
#' @examples
#' # Example 1: Compute total TTS for a plate material
#' E900_15("P",
#'   Cu = 0.2, Ni = 0.18, Mn = 1.36, P = 0.012,
#'   temperature = 290, fluence = 2.56894e18,
#'   output = "TTS", output_unit = "Celsius"
#' )
#'
#' # Example 2: Compute TTS1 component in Fahrenheit
#' E900_15("F",
#'   Cu = 0.15, Ni = 0.2, Mn = 1.4, P = 0.01,
#'   temperature = 300, fluence = 1e19,
#'   output = "TTS1", output_unit = "Fahrenheit"
#' )
#'
#' # Example 3: Compute TTS2 component
#' E900_15("W",
#'   Cu = 0.25, Ni = 0.3, Mn = 1.5, P = 0.015,
#'   temperature = 275, fluence = 5e18,
#'   output = "TTS2", output_unit = "Celsius"
#' )
#'
#' # Example 4: Compute Standard Deviation (SD) for a weld metal
#' E900_15("W",
#'   Cu = 0.25, Ni = 0.3, Mn = 1.5, P = 0.015,
#'   temperature = 275, fluence = 5e18,
#'   output = "SD", output_unit = "Fahrenheit"
#' )
#'
#' @seealso \code{\link{E900_15_flux}}
#' @export
E900_15 <- function(product_form,
                    Cu,
                    Ni,
                    Mn,
                    P,
                    temperature,
                    fluence,
                    output = c("TTS", "TTS1", "TTS2", "SD"),
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

  # Expand vectors
  expanded <- expand_vectors(product_form, Cu, Ni, Mn, P, temperature, fluence)
  pf <- expanded[[1]]
  cu <- expanded[[2]]
  ni <- expanded[[3]]
  mn <- expanded[[4]]
  ps <- expanded[[5]]
  tc <- expanded[[6]]
  fl <- expanded[[7]]

  # TTS1
  calc_tts1 <- function(product_form, Ni, Mn, P, temperature, fluence) {
    A <- c("F" = 1.011, "P" = 1.080, "W" = 0.919, "B" = 1.0)[product_form]
    out <- A * 3.593e-10 * (fluence^0.5695)
    out <- out * (((1.8 * temperature + 32) / 550)^(-5.47))
    out <- out * ((0.09 + P / 0.012)^0.216)
    out <- out * ((1.66 + (Ni^8.54) / 0.63)^0.39)
    out <- out * ((Mn / 1.36)^0.3)
    out * (5 / 9)
  }

  # TTS2
  calc_tts2 <- function(product_form, Cu, Ni, P, temperature, fluence) {
    B <- c("F" = 0.738, "P" = 0.819, "W" = 0.968, "B" = 1.0)[product_form]
    M <- B * pmax(pmin(113.87 * (log(fluence) - log(4.5e16)), 612.6), 0)
    M <- M * (((1.8 * temperature + 32) / 550)^(-5.45))
    M <- M * ((0.1 + P / 0.012)^(-0.098))
    M <- M * ((0.168 + (Ni^0.58) / 0.63)^0.73)
    out <- pmax(pmin(Cu, 0.28) - 0.053, 0) * M
    out * (5 / 9)
  }

  # TTS = TTS1 + TTS2
  calc_tts <- function(product_form, Cu, Ni, Mn, P, temperature, fluence) {
    tts1 <- calc_tts1(product_form, Ni, Mn, P, temperature, fluence)
    tts2 <- calc_tts2(product_form, Cu, Ni, P, temperature, fluence)
    tts1 + tts2
  }

  # SD
  calc_sd <- function(product_form, Cu, Ni, Mn, P, temperature, fluence) {
    tts <- calc_tts(product_form, Cu, Ni, Mn, P, temperature, fluence)
    C <- c("F" = 6.972, "P" = 6.593, "W" = 7.681)[product_form]
    D <- c("F" = 0.199, "P" = 0.163, "W" = 0.181)[product_form]
    C * (tts^D)
  }

  # Output calculation
  result <- switch(output,
    "TTS1" = calc_tts1(pf, ni, mn, ps, tc, fl),
    "TTS2" = calc_tts2(pf, cu, ni, ps, tc, fl),
    "TTS" = calc_tts(pf, cu, ni, mn, ps, tc, fl),
    "SD" = calc_sd(pf, cu, ni, mn, ps, tc, fl)
  )

  # Convert degF to degC if needed
  if (output_unit == "Fahrenheit") {
    result <- dC_to_dF(result)
  }

  unname(result)
}
