#' Guthrie's Model (NUREG/CR-3391) for Transition Temperature Shift (TTS) Calculation
#'
#' Computes the Transition Temperature Shift (TTS), Chemistry Factor (CF),
#' Fluence Factor (FF), or Standard Deviation (SD) based on the embrittlement model
#' described in NUREG/CR-3391 (1983).
#'
#' This model estimates radiation-induced embrittlement in reactor pressure vessel materials
#' based on copper and nickel content and neutron fluence. It distinguishes between base and weld metals.
#'
#' @param product_form Character vector, one of \code{"B"}, \code{"F"}, \code{"P"}, or \code{"W"}.
#'   \code{"F"} (forgings) and \code{"P"} (plates) are treated as base metal (\code{"B"}).
#' @param Cu Numeric vector, copper content in weight percent (wt%). Must be between 0 and 100.
#' @param Ni Numeric vector, nickel content in weight percent (wt%). Must be between 0 and 100.
#' @param fluence Numeric vector, neutron fluence in n/cm².
#' @param output Character, one of:
#'   \itemize{
#'     \item \code{"TTS"} – Transition Temperature Shift
#'     \item \code{"CF"} – Chemistry Factor
#'     \item \code{"FF"} – Fluence Factor
#'     \item \code{"SD"} – Standard Deviation
#'     \item \code{"Margin"} – Regulatory Margin
#'   }
#' @param temperature_unit Character, one of:
#'   \itemize{
#'     \item \code{"Celsius"} – Return results in degrees Celsius
#'     \item \code{"Fahrenheit"} – Return results in degrees Fahrenheit
#'   }
#'
#' @return A numeric vector of computed values for the selected \code{output}. For TTS, CF, and SD,
#'         the unit depends on \code{temperature_unit}. FF is unitless.
#'
#' @examples
#' CR3391(product_form = "B", Cu = 0.1, Ni = 0.6, fluence = 1e19, output = "TTS")
#' CR3391(product_form = "W", Cu = 0.2, Ni = 0.5, output = "CF")
#' CR3391(fluence = 1e19, output = "FF")
#' CR3391(product_form = "B", output = "SD")
#'
#' @seealso \code{\link{NP3319}}, \code{\link{RG199R2_P1}}, \code{\link{RG199R2_P2}}
#' @export
CR3391 <- function(product_form = NULL,
                   Cu = NULL,
                   Ni = NULL,
                   fluence = NULL,
                   output = c("TTS", "CF", "FF", "SD"),
                   temperature_unit = c("Celsius", "Fahrenheit")) {
  # Input requirement checks
  output <- match.arg(output, several.ok = FALSE)
  temperature_unit <- match.arg(temperature_unit, several.ok = FALSE)

  if (output %in% c("CF", "TTS") &&
    (is.null(product_form) || is.null(Cu) || is.null(Ni))) {
    stop("For CF or TTS calculation, 'product_form', 'Cu', and 'Ni' must be provided.")
  }
  if (output %in% c("TTS", "FF") && is.null(fluence)) {
    stop("For TTS or FF calculation, 'fluence' must be provided.")
  }
  if (output == "SD" && is.null(product_form)) {
    stop("For SD calculation, 'product_form' must be provided.")
  }

  # Standardize product form
  if (!is.null(product_form)) {
    product_form <- to_baseweld(product_form)
  }

  # CF calculation
  calc_cf <- function(product_form, Cu, Ni) {
    stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
    stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))
    base_cf <- -38.39 + 555.6 * Cu + 480.1 * Cu * tanh(0.353 * Ni / Cu)
    weld_cf <- 624 * Cu - 333.1 * sqrt(Cu * Ni) + 251.2 * Ni
    ifelse(product_form == "B", base_cf,
      ifelse(product_form == "W", weld_cf, NA_real_)
    )
  }

  # FFF calculation
  calc_ff <- function(fluence) {
    stopifnot(is.numeric(fluence), all(fluence >= 0))
    f <- fluence / 1e19
    f^(0.2661 - 0.0449 * log(f))
  }

  # TTS calculation
  calc_tts <- function(product_form, Cu, Ni, fluence) {
    cf <- calc_cf(pf, Cu, Ni)
    ff <- calc_ff(fluence)
    cf * ff
  }

  # SD calculation
  calc_sd <- function(product_form) {
    c(B = 17.2, W = 28.2)[product_form]
  }

  # Expand vectors
  expanded <- expand_vectors(product_form, Cu, Ni, fluence)
  pf <- expanded[[1]]
  cu <- expanded[[2]]
  ni <- expanded[[3]]
  fl <- expanded[[4]]

  # Output calculation
  result <- switch(output,
    "TTS" = calc_tts(pf, cu, ni, fl),
    "CF"  = calc_cf(pf, cu, ni),
    "FF"  = calc_ff(fl),
    "SD"  = calc_sd(pf)
  )

  # Convert degF to degC if needed
  if (output %in% c("TTS", "CF", "SD") && temperature_unit == "Celsius") {
    result <- dF_to_dC(result)
  }

  unname(result)
}
