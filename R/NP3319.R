#' Odette's Model (EPRI NP-3319) for Transition Temperature Shift (TTS) Calculation
#'
#' Computes the Transition Temperature Shift (TTS), Chemistry Factor (CF),
#' Fluence Factor (FF), or Standard Deviation (SD) based on the embrittlement model
#' described in EPRI NP-3319 (1984).
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
#' NP3319(product_form = "B", Cu = 0.1, Ni = 0.6, fluence = 1e19, output = "TTS",
#'        temperature_unit = "Celsius")
#' NP3319(product_form = "W", Cu = 0.2, Ni = 0.5, output = "CF", temperature_unit = "Fahrenheit")
#' NP3319(product_form = "F", fluence = 1e19, output = "FF")
#' NP3319(product_form = "B", output = "SD")
#'
#' @seealso \code{\link{CR3391}}, \code{\link{RG199R2_P1}}, \code{\link{RG199R2_P2}}
#'
#' @export

NP3319 <- function(product_form = NULL,
                   Cu = NULL,
                   Ni = NULL,
                   fluence = NULL,
                   output = c("TTS", "CF", "FF", "SD"),
                   temperature_unit = c("Celsius", "Fahrenheit")) {
  output <- match.arg(output, several.ok = FALSE)
  temperature_unit <- match.arg(temperature_unit, several.ok = FALSE)

  # Input requirement checks
  if (output %in% c("TTS", "CF") &&
    (is.null(product_form) || is.null(Cu) || is.null(Ni))) {
    stop("For 'TTS' or 'CF' calculation, 'product_form', 'Cu', and 'Ni' must be provided.")
  }
  if (output %in% c("TTS", "FF") && (is.null(product_form) || is.null(fluence))) {
    stop("For 'TTS' or 'FF' calculation, both 'product_form' and 'fluence' must be provided.")
  }
  if (output == "SD" && is.null(product_form)) {
    stop("For 'SD' calculation, 'product_form' must be provided.")
  }

  # Standardize product_form
  product_form <- to_baseweld(product_form)

  # Error function (erf)
  erf <- function(x) {
    2 * stats::pnorm(x * sqrt(2)) - 1
  }

  # CF calculation
  calc_cf <- function(product_form, Cu, Ni) {
    stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
    stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))

    base_cf <- Cu * 216 * (1 + 0.33 * (erf(0.77 * Ni / Cu - 1) + 1))
    weld_cf <- Cu * 200 * (1 + 1.38 * (erf(0.30 * Ni / Cu - 1) + 1))

    ifelse(product_form == "B", base_cf,
      ifelse(product_form == "W", weld_cf, NA_real_)
    )
  }

  # FF calculation
  calc_ff <- function(product_form, fluence) {
    stopifnot(is.numeric(fluence), all(fluence >= 0))
    f <- fluence / 1e19
    base_ff <- f^0.28
    weld_ff <- (1 - exp(-f / 0.11))^1.36 * f^0.18

    ifelse(product_form == "B", base_ff,
      ifelse(product_form == "W", weld_ff, NA_real_)
    )
  }

  # TTS calculation
  calc_tts <- function(product_form, Cu, Ni, fluence) {
    cf <- calc_cf(product_form, Cu, Ni)
    ff <- calc_ff(product_form, fluence)
    cf * ff
  }

  # SD placeholder
  calc_sd <- function(pf) {
    rep(1, length(pf)) # Placeholder (no source SD)
  }

  # Expand vectors
  args <- expand_vectors(product_form, Cu, Ni, fluence)
  pf <- args[[1]]
  cu <- args[[2]]
  ni <- args[[3]]
  fl <- args[[4]]

  # Output calculation
  result <- switch(output,
    "CF" = calc_cf(pf, cu, ni),
    "FF" = calc_ff(pf, fl),
    "TTS" = calc_tts(pf, cu, ni, fl),
    "SD" = calc_sd(pf)
  )

  # Convert degF to degC if needed
  if (output %in% c("TTS", "CF", "SD") && temperature_unit == "Celsius") {
    result <- dF_to_dC(result)
  }

  unname(result)
}
