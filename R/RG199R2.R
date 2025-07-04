#' Regulatory Guide 1.99 Rev. 2 Position 1.1 Table-based Embrittlement Property Calculator
#'
#' Computes radiation embrittlement-related properties based on the U.S. NRC Regulatory Guide 1.99 Rev. 2 (1988),
#' specifically Position 1.1. This model uses tabulated Chemistry Factors and fixed Standard Deviations
#' based on material composition and neutron fluence.
#'
#' @param product_form Character vector. One of \code{"B"}, \code{"F"}, \code{"P"}, or \code{"W"}.
#'   \code{"F"} (forgings) and \code{"P"} (plates) are treated as base metal (\code{"B"}).
#' @param Cu Numeric vector. Copper content in weight percent (wt%). Must be between 0 and 100.
#' @param Ni Numeric vector. Nickel content in weight percent (wt%). Must be between 0 and 100.
#' @param fluence Numeric vector. Neutron fluence in n/cm².
#' @param output Character. One of:
#'   \itemize{
#'     \item \code{"TTS"} – Transition Temperature Shift
#'     \item \code{"CF"} – Chemistry Factor
#'     \item \code{"FF"} – Fluence Factor
#'     \item \code{"SD"} – Standard Deviation
#'     \item \code{"Margin"} – Regulatory Margin
#'   }
#' @param output_unit Character. Unit of the output result. One of:
#'   \itemize{
#'     \item \code{"Celsius"} – Return results in degrees Celsius (default)
#'     \item \code{"Fahrenheit"} – Return results in degrees Fahrenheit
#'   }
#'
#' @return A numeric vector of computed values for the selected \code{output}. For TTS, CF, and SD,
#'         the unit depends on \code{output_unit}. FF is unitless.
#'
#' @examples
#' # Compute Chemistry Factor in Fahrenheit
#' RG199R2_P1(
#'   product_form = "B", Cu = 0.25, Ni = 0.8, fluence = 1e19,
#'   output = "CF", output_unit = "Fahrenheit"
#' )
#'
#' # Compute TTS in Celsius
#' RG199R2_P1(
#'   product_form = "B", Cu = 0.20, Ni = 0.7, fluence = 5e19,
#'   output = "TTS", output_unit = "Celsius"
#' )
#'
#' @seealso \code{\link{RG199R2_P2}}, \code{\link{NP3319}}, \code{\link{CR3391}}
#' @export
RG199R2_P1 <- function(product_form = NULL, # for CF
                       Cu = NULL, # for CF
                       Ni = NULL, # for CF
                       fluence = NULL, # for FF, TTS, Margin
                       output = c("TTS", "CF", "FF", "SD", "Margin"),
                       output_unit = c("Celsius", "Fahrenheit")) {
  # Input requirement checks
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)

  if (output == "TTS" || output == "Margin") {
    if (is.null(product_form) || is.null(Cu) || is.null(Ni) || is.null(fluence)) {
      stop("For TTS or Margin, please provide 'product_form', 'Cu', 'Ni', and 'fluence'.")
    }
  }
  if (output == "CF") {
    if (is.null(product_form) || is.null(Cu) || is.null(Ni)) {
      stop("For CF calculation, please provide 'product_form', 'Cu', and 'Ni'.")
    }
  }
  if (output == "FF" && is.null(fluence)) {
    stop("For FF calculation, please provide 'fluence'.")
  }
  if (output == "SD" && is.null(product_form)) {
    stop("For SD calculation, please provide 'product_form'.")
  }

  if (!is.null(Cu)) {
    stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
  }
  if (!is.null(Ni)) {
    stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))
  }
  if (!is.null(fluence)) {
    stopifnot(is.numeric(fluence), all(fluence >= 0))
  }

  # Standardize product form
  if (!is.null(product_form)) {
    product_form <- to_baseweld(product_form)
  }

  # Expand vectors
  expanded <- expand_vectors(product_form, Cu, Ni, fluence)
  pf <- expanded[[1]]
  cu <- expanded[[2]]
  ni <- expanded[[3]]
  fl <- expanded[[4]]

  # Output calculation
  result <- switch(output,
    "TTS" = calc_p1_tts(pf, cu, ni, fl),
    "CF" = calc_p1_cf(pf, cu, ni),
    "FF" = calc_ff(fl),
    "SD" = calc_p1_sd(pf),
    "Margin" = calc_p1_margin(pf, cu, ni, fl)
  )

  # Convert degF to degC if needed
  if (output %in% c("TTS", "CF", "SD") && output_unit == "Celsius") {
    result <- dF_to_dC(result)
  }

  unname(result)
}


#' Regulatory Guide 1.99 Rev. 2 Position 2.1 Surveillance-Based Embrittlement Property Calculator
#'
#' Computes embrittlement-related properties using surveillance test data based on an interpretation
#' of U.S. NRC Regulatory Guide 1.99 Rev. 2 (1988), Position 2.1.
#'
#' @param product_form Character vector, one of \code{"B"}, \code{"F"}, \code{"P"}, or \code{"W"}.
#'   \code{"F"} and \code{"P"} are treated as base metal (\code{"B"}). Required for \code{"SD"} and \code{"Margin"}.
#' @param SV_flu Numeric vector, surveillance neutron fluence in n/cm². Must match length of \code{SV_tts}.
#' @param SV_tts Numeric vector, measured transition temperature shift (TTS) in °F.
#' @param fluence Optional numeric vector, neutron fluence in n/cm² for evaluating \code{"TTS"}, \code{"FF"}, or \code{"Margin"}.
#' @param output Character, one of:
#'   \itemize{
#'     \item \code{"TTS"} – Estimated Transition Temperature Shift
#'     \item \code{"CF"} – Back-calculated Chemistry Factor
#'     \item \code{"FF"} – Fluence Factor
#'     \item \code{"SD"} – Standard Deviation
#'     \item \code{"Margin"} – Regulatory Margin (min(TTS, 2×SD))
#'   }
#' @param output_unit Character, one of:
#'   \itemize{
#'     \item \code{"Celsius"} – Return results in degrees Celsius (default)
#'     \item \code{"Fahrenheit"} – Return results in degrees Fahrenheit
#'   }
#'
#' @return A numeric value or vector. For TTS, CF, and SD, the unit depends on \code{output_unit}.
#'         FF is unitless.
#'
#' @details
#' The Chemistry Factor (CF) is back-calculated by minimizing residuals of \eqn{TTS_i ≈ CF × FF(fluence_i)}.
#' SD is determined by residual range compared to fixed thresholds.
#'
#' @examples
#' # Back-calculate CF using surveillance data
#' RG199R2_P2(
#'   SV_flu = c(1e19, 2e19), SV_tts = c(100, 130),
#'   output = "CF", output_unit = "Celsius"
#' )
#'
#' # Estimate TTS at given fluence
#' RG199R2_P2(
#'   SV_flu = c(1e19, 2e19), SV_tts = c(100, 130),
#'   fluence = 3e19, output = "TTS", output_unit = "Celsius"
#' )
#'
#' @seealso \code{\link{RG199R2_P1}}, \code{\link{NP3319}}, \code{\link{CR3391}}
#' @export
RG199R2_P2 <- function(product_form = NULL, # for SD, Margin
                       SV_flu = NULL, # SV_fluence vector
                       SV_tts = NULL, # SV_TTS vector
                       fluence = NULL, # for FF, TTS, Margin,
                       output = c("TTS", "CF", "FF", "SD", "Margin"),
                       output_unit = c("Celsius", "Fahrenheit")) {
  # Input requirement checks
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)

  if (output %in% c("CF", "TTS", "SD", "Margin")) {
    if (is.null(SV_flu) || is.null(SV_tts)) {
      stop("For this output type, both 'SV_flu' and 'SV_tts' must be provided.")
    }
    stopifnot(is.numeric(SV_flu), is.numeric(SV_tts))
    stopifnot(length(SV_flu) == length(SV_tts))
    stopifnot(all(SV_tts[SV_flu == 0] == 0))
    stopifnot(sum(SV_flu > 0) >= 2)
  }

  if (!is.null(fluence)) {
    stopifnot(is.numeric(fluence), all(fluence >= 0))
  }

  if (!is.null(product_form)) {
    product_form <- as.character(product_form)
    unique_form <- unique(product_form)
    if (length(unique_form) != 1) {
      stop("Only one product form must be provided")
    }
    product_form <- to_baseweld(product_form)
  }

  # Standardize product form
  if (!is.null(product_form)) {
    product_form <- to_baseweld(product_form)
  }

  # Expand vectors
  expanded <- expand_vectors(product_form, fluence)
  pf <- expanded[[1]]
  fl <- expanded[[2]]

  # Output calculation
  result <- switch(output,
    "TTS" = calc_p2_tts(SV_flu, SV_tts, fl),
    "CF" = calc_p2_cf(SV_flu, SV_tts),
    "FF" = calc_ff(fl),
    "SD" = calc_p2_sd(pf, SV_flu, SV_tts),
    "Margin" = calc_p2_margin(pf, SV_flu, SV_tts, fl)
  )

  # Convert degF to degC if needed
  if (output %in% c("TTS", "CF", "SD") && output_unit == "Celsius") {
    result <- dF_to_dC(result)
  }

  unname(result)
}


## 2. P1.1 Calculations ----

calc_p1_tts <- function(product_form, Cu, Ni, fluence) {
  cf <- calc_p1_cf(product_form, Cu, Ni)
  calc_tts(cf, fluence)
}


calc_p1_cf <- function(product_form, Cu, Ni) {
  n <- length(product_form)
  cf <- numeric(n)

  for (i in seq_len(n)) {
    pf <- product_form[i]
    cu <- Cu[i]
    ni <- Ni[i]

    cf[i] <- if (pf == "B") {
      calc_p1_cf_base(cu, ni)
    } else {
      calc_p1_cf_weld(cu, ni)
    }
  }

  cf
}


calc_p1_sd <- function(product_form) {
  base_weld <- c("B" = 17, "W" = 28)
  base_weld[product_form]
}


calc_p1_margin <- function(product_form, Cu, Ni, fluence) {
  tts <- calc_p1_tts(product_form, Cu, Ni, fluence) # Calculate TTS
  margin <- 2 * calc_p1_sd(product_form) # Margin is 2 * SD
  ifelse(margin > tts, tts, margin) # The smaller of TTS or Margin
}


calc_p1_cf_base <- function(Cu, Ni) { # not vectorized
  cf_base <- matrix(
    c(
      20, 20, 20, 20, 22, 25, 28, 31, 34, 37, 41, 45, 49,
      53, 57, 61, 65, 69, 73, 78, 82, 86, 91, 95, 100, 104,
      109, 114, 119, 124, 129, 134, 139, 144, 149, 153, 158,
      162, 166, 171, 175, 20, 20, 20, 20, 26, 31, 37, 43, 48,
      53, 58, 62, 67, 71, 75, 80, 84, 88, 92, 97, 102, 107,
      112, 117, 121, 126, 130, 134, 138, 142, 146, 151, 155,
      160, 164, 168, 173, 177, 182, 185, 189, 20, 20, 20, 20,
      26, 31, 37, 44, 51, 58, 65, 72, 79, 85, 91, 99, 104,
      110, 115, 120, 125, 129, 134, 138, 143, 148, 151, 155,
      160, 164, 167, 172, 175, 180, 184, 187, 191, 196, 200,
      203, 207, 20, 20, 20, 20, 26, 31, 37, 44, 51, 58, 65,
      76, 83, 91, 100, 110, 118, 127, 134, 142, 149, 155, 161,
      167, 172, 176, 180, 184, 187, 191, 194, 198, 202, 205,
      209, 212, 216, 220, 223, 227, 231, 20, 20, 20, 20, 26,
      31, 37, 44, 51, 58, 67, 77, 86, 96, 105, 115, 123, 132,
      141, 150, 159, 167, 176, 184, 191, 199, 205, 211, 216,
      221, 225, 228, 231, 234, 238, 241, 245, 248, 250, 254,
      257, 20, 20, 20, 20, 26, 31, 37, 44, 51, 58, 67, 77, 86,
      96, 106, 117, 125, 135, 144, 154, 164, 172, 181, 190,
      199, 208, 216, 225, 233, 241, 249, 255, 260, 264, 268,
      272, 275, 278, 281, 285, 288, 20, 20, 20, 20, 26, 31,
      37, 44, 51, 58, 67, 77, 86, 96, 106, 117, 125, 135, 144,
      154, 165, 176, 184, 194, 204, 214, 221, 230, 239, 248,
      257, 266, 274, 282, 290, 298, 303, 308, 313, 317, 320
    ),
    nrow = 41
  )
  Ni_values <- seq(0, 1.2, 0.2)
  Cu_values <- seq(0, 0.4, 0.01)
  interp2d_linear(Ni_values, Cu_values, cf_base, Ni, Cu)
}


calc_p1_cf_weld <- function(Cu, Ni) { # not vectorized
  cf_weld <- matrix(
    c(
      20, 20, 21, 22, 24, 26, 29, 32, 36, 40, 44, 49, 52, 58,
      61, 66, 70, 75, 79, 83, 88, 92, 97, 101, 105, 110, 113,
      119, 122, 128, 131, 136, 140, 144, 149, 153, 158, 162,
      166, 171, 175, 20, 20, 26, 35, 43, 49, 52, 55, 58, 61,
      65, 68, 72, 76, 79, 84, 88, 92, 95, 100, 104, 108, 112,
      117, 121, 126, 130, 134, 138, 142, 146, 151, 155, 160,
      164, 168, 172, 177, 182, 185, 189, 20, 20, 27, 41, 54,
      67, 77, 85, 90, 94, 97, 101, 103, 106, 109, 112, 115,
      119, 122, 126, 129, 133, 137, 140, 144, 148, 151, 155,
      160, 164, 167, 172, 175, 180, 184, 187, 191, 196, 200,
      203, 207, 20, 20, 27, 41, 54, 68, 82, 95, 106, 115, 122,
      130, 135, 139, 142, 146, 149, 151, 154, 157, 160, 164,
      167, 169, 173, 176, 180, 184, 187, 191, 194, 198, 202,
      205, 209, 212, 216, 220, 223, 227, 231, 20, 20, 27, 41,
      54, 68, 82, 95, 108, 122, 133, 144, 153, 162, 168, 175,
      178, 184, 187, 191, 194, 197, 200, 203, 206, 209, 212,
      216, 218, 222, 225, 228, 231, 234, 238, 241, 245, 248,
      250, 254, 257, 20, 20, 27, 41, 54, 68, 82, 94, 108, 122,
      135, 148, 161, 172, 182, 191, 199, 207, 214, 220, 223,
      229, 232, 236, 239, 243, 246, 249, 251, 254, 257, 260,
      263, 266, 269, 272, 275, 278, 281, 285, 288, 20, 20, 27,
      41, 54, 68, 82, 95, 108, 122, 135, 148, 161, 176, 188,
      200, 211, 221, 230, 238, 245, 252, 257, 263, 268, 272,
      276, 280, 284, 287, 290, 293, 296, 299, 302, 305, 308,
      311, 314, 317, 320
    ),
    nrow = 41
  )
  Ni_values <- seq(0, 1.2, 0.2)
  Cu_values <- seq(0, 0.4, 0.01)
  interp2d_linear(Ni_values, Cu_values, cf_weld, Ni, Cu)
}


# 2 차원 보간 함수
interp2d_linear <- function(x_values, y_values, table, x, y) {
  # Ensure x and y are within the range of x_values and y_values
  if (x < min(x_values) || x > max(x_values)) {
    stop(sprintf(
      "x=%g is out of range [%g, %g].",
      x, min(x_values), max(x_values)
    ))
  }
  if (y < min(y_values) || y > max(y_values)) {
    stop(sprintf(
      "y=%g is out of range [%g, %g].",
      y, min(y_values), max(y_values)
    ))
  }

  # Find the indices of x and y surrounding the input coordinates
  x1_idx <- max(which(x_values <= x))
  x2_idx <- min(which(x_values >= x))
  y1_idx <- max(which(y_values <= y))
  y2_idx <- min(which(y_values >= y))

  # Retrieve the corner values from the table
  x1 <- x_values[x1_idx]
  x2 <- x_values[x2_idx]
  y1 <- y_values[y1_idx]
  y2 <- y_values[y2_idx]

  Q11 <- table[y1_idx, x1_idx]
  Q12 <- table[y2_idx, x1_idx]
  Q21 <- table[y1_idx, x2_idx]
  Q22 <- table[y2_idx, x2_idx]

  # Perform linear interpolation
  if (x1 == x2 && y1 == y2) {
    interpolated_value <- Q11
  } else if (x1 == x2) {
    interpolated_value <- Q11 + (Q12 - Q11) * (y - y1) / (y2 - y1)
  } else if (y1 == y2) {
    interpolated_value <- Q11 + (Q21 - Q11) * (x - x1) / (x2 - x1)
  } else {
    interpolated_value <- (Q11 * (x2 - x) * (y2 - y) +
      Q21 * (x - x1) * (y2 - y) +
      Q12 * (x2 - x) * (y - y1) +
      Q22 * (x - x1) * (y - y1)) / ((x2 - x1) * (y2 - y1))
  }
  interpolated_value
}


## 3. P2.1 Calculations ----

calc_p2_tts <- function(SV_flu, SV_tts, fluence) {
  cf <- calc_p2_cf(SV_flu, SV_tts)
  fl <- if (is.null(fluence)) SV_flu else fluence
  calc_tts(cf, fl)
}


calc_p2_cf <- function(SV_flu, SV_tts) {
  ff <- calc_ff(SV_flu)
  sum(ff * SV_tts) / sum(ff^2) # Returns a single CF value
}


calc_p2_sd <- function(product_form, SV_flu, SV_tts) {
  cf <- calc_p2_cf(SV_flu, SV_tts) # Single CF value
  best_tts <- cf * calc_ff(SV_flu)
  scatter <- abs(SV_tts - best_tts)
  max_scatter <- max(scatter)

  base_weld <- c("B" = 17, "W" = 28)
  threshold <- base_weld[product_form]
  half_val <- threshold / 2 # 8.5 or 14
  if (all(threshold < max_scatter)) threshold else half_val
}


calc_p2_margin <- function(product_form, SV_flu, SV_tts, fluence) {
  tts <- calc_p2_tts(SV_flu, SV_tts, fluence) # Calculate TTS
  margin <- 2 * calc_p2_sd(product_form, SV_flu, SV_tts) # Margin is 2 * SD
  ifelse(margin > tts, tts, margin) # The smaller of TTS or Margin
}


# 4. Common Functions ----
# These are internal-use functions common to Position 1.1 and 1.2 implementations.

# Fluence Factor (FF)
# Computes Fluence Factor from fluence in n/cm² (numeric vector)
calc_ff <- function(fluence) {
  stopifnot(is.numeric(fluence), all(fluence >= 0))
  f <- fluence / 1e19
  f^(0.28 - 0.1 * log10(f))
}


# TTS from CF and fluence
# Generalized computation: TTS = CF × FF
calc_tts <- function(cf, fluence) {
  cf * calc_ff(fluence)
}
