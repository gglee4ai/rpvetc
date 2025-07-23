## 1. RG1.99R2 ----

#' General Embrittlement Property Calculator (Direct CF/Fluence/SD Input)
#'
#' Computes embrittlement-related properties—Transition Temperature Shift (TTS),
#' Fluence Factor (FF), or Regulatory Margin—based on user-supplied Chemistry Factor (CF),
#' fluence, and standard deviation (SD) values.
#'
#' This function does not rely on Position 1.1 or 2.1 models. It is intended for use
#' when CF, SD, and fluence are externally obtained or precomputed.
#'
#' @param cf Numeric vector. Chemistry Factor (in degrees). Required for \code{"TTS"} and \code{"Margin"}.
#' @param fluence Numeric vector. Neutron fluence in n/cm². Required for all output types.
#' @param sd Numeric vector. Standard deviation of the shift (in degrees). Required for \code{"Margin"}.
#' @param output Character string. One of \code{"TTS"}, \code{"FF"}, or \code{"Margin"}.
#' @param output_unit Character. Unit of the returned result. Either \code{"Celsius"} (default) or \code{"Fahrenheit"}.
#' @param input_unit Character. Unit of the input values (\code{cf} and \code{sd}). Either \code{"Celsius"} (default) or \code{"Fahrenheit"}.
#' @param use_names Logical. If \code{TRUE}, preserves input names in the output. Default is \code{FALSE}.
#'
#' @return A numeric vector of computed values. Units depend on \code{output_unit}. If \code{output = "FF"}, result is unitless.
#'
#' @examples
#' # Compute TTS in Celsius
#' RG199R2(cf = 100, fluence = 3e19, output = "TTS",
#'         input_unit = "Fahrenheit", output_unit = "Celsius")
#'
#' # Compute Fluence Factor
#' RG199R2(fluence = 5e19, output = "FF")
#'
#' # Compute Regulatory Margin
#' RG199R2(cf = 80, fluence = 2e19, sd = 17,
#'         input_unit = "Fahrenheit", output = "Margin")
#'
#' @seealso \code{\link{RG199R2_P1}}, \code{\link{RG199R2_P2}}
#' @export
RG199R2 <- function(cf = NULL, # for TTS, Margin
                    fluence = NULL, # for TTS, FF, TTS, Margin n/cm2
                    sd = NULL, # for Margin
                    output = c("TTS", "FF", "Margin"),
                    output_unit = c("Celsius", "Fahrenheit"),
                    input_unit = c("Celsius", "Fahrenheit"),
                    use_names = FALSE) {
  # Match and validate input arguments
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)
  input_unit <- match.arg(input_unit, several.ok = FALSE)

  # Input requirement checks based on output type
  if (output == "TTS" && (is.null(cf) || is.null(fluence))) {
    stop("For 'TTS', please provide both 'cf' and 'fluence'.")
  }
  if (output == "FF" && is.null(fluence)) {
    stop("For 'FF', please provide 'fluence'.")
  }
  if (output == "Margin" && (is.null(cf) || is.null(fluence) || is.null(sd))) {
    stop("For 'Margin', please provide 'cf', 'fluence', and 'sd'.")
  }

  # Check numeric validity
  if (!is.null(cf)) {
    stopifnot(is.numeric(cf))
    if (any(cf < 0, na.rm = TRUE)) {
      stop("CF values must be non-negative.")
    }
  }
  if (!is.null(fluence)) {
    stopifnot(is.numeric(fluence), all(fluence >= 0))
  }
  if (!is.null(sd)) {
    stopifnot(is.numeric(sd), all(sd >= 0))
  }

  # Broadcast shorter vectors to match lengths
  expanded <- expand_vectors(cf, fluence, sd)
  cf <- expanded[[1]]
  fl <- expanded[[2]]
  sd <- expanded[[3]]

  # Compute selected output
  result <- switch(
    output,
    "TTS" = rg199_tts(cf, fl),
    "FF" = rg199_ff(fl),
    "Margin" = rg199_margin(cf, fl, sd)
  )

  # Apply unit conversion if needed
  if (input_unit == "Fahrenheit" && output_unit == "Celsius") {
    result <- dF_to_dC(result)
  } else if (input_unit == "Celsius" && output_unit == "Fahrenheit") {
    result <- dC_to_dF(result)
  }

  # Return result with or without names
  if (use_names) result else unname(result)
}


# Generalized computation: TTS = CF × FF
rg199_tts <- function(cf, fluence) {
  cf * rg199_ff(fluence)
}


# Computes Fluence Factor from fluence in n/cm² (numeric vector)
rg199_ff <- function(fluence) {
  f <- fluence / 1e19
  f^(0.28 - 0.1 * log10(f))
}


# Marging from CF and fluence, SD
rg199_margin <- function(cf, fluence, sd) {
  tts <- rg199_tts(cf, fluence)
  margin <- 2 * sd # Margin is 2 * SD
  ifelse(margin > tts, tts, margin) # The smaller of TTS or Margin
}


## 2. RG1.99R2 P1.1 Calculations ----

#' Regulatory Guide 1.99 Rev. 2 Position 1.1 Table-based Embrittlement Property Calculator
#'
#' Computes radiation embrittlement-related properties based on the U.S. NRC
#' Regulatory Guide 1.99 Rev. 2 (1988), specifically Position 1.1. This model
#' uses tabulated Chemistry Factors (CF) derived from copper and nickel content
#' and fixed Standard Deviations (SD) based on product form.
#'
#' @param product_form Character vector. One of \code{"B"} (base), \code{"F"} (forging),
#'   \code{"P"} (plate), or \code{"W"} (weld). \code{"F"} and \code{"P"} are internally
#'   treated as \code{"B"}.
#' @param Cu Optional numeric vector. Copper content in weight percent (wt%).
#'   Required for \code{output = "CF"}.
#' @param Ni Optional numeric vector. Nickel content in weight percent (wt%).
#'   Required for \code{output = "CF"}.
#' @param output Character. Type of property to compute. One of:
#'   \itemize{
#'     \item \code{"CF"} – Chemistry Factor (°C by default)
#'     \item \code{"SD"} – Standard Deviation (°C by default)
#'   }
#' @param output_unit Character. Desired output unit. Either \code{"Celsius"} or \code{"Fahrenheit"}.
#'   Default is \code{"Celsius"}.
#' @param use_names Logical. Whether to preserve input names in the output vector.
#'   Default is \code{FALSE}.
#'
#' @return A numeric vector of computed values for the selected \code{output}. Unit depends on \code{output_unit}.
#'
#' @examples
#' # Calculate Chemistry Factor in Fahrenheit
#' RG199R2_P1(product_form = "B", Cu = 0.2, Ni = 0.8, output = "CF", output_unit = "Fahrenheit")
#'
#' # Get standard deviation for weld material
#' RG199R2_P1(product_form = "W", output = "SD")
#'
#' @seealso \code{\link{RG199R2}}, \code{\link{RG199R2_P2}}
#' @export

RG199R2_P1 <- function(product_form,
                       Cu = NULL,
                       Ni = NULL,
                       output = c("CF", "SD"),
                       output_unit = c("Celsius", "Fahrenheit"),
                       use_names = FALSE) {

  # Match and validate input arguments
  output <- match.arg(output)
  output_unit <- match.arg(output_unit)

  # Compute selected output
  if (output == "CF") {
    if (is.null(product_form) || is.null(Cu) || is.null(Ni)) {
      stop("For CF calculation, please provide 'product_form', 'Cu', and 'Ni'.")
    }
    stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
    stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))

    expanded <- expand_vectors(product_form, Cu, Ni)
    pf <- expanded[[1]]
    cu <- expanded[[2]]
    ni <- expanded[[3]]
    result <- rg199_p1_cf_degF(pf, cu, ni)

  } else if (output == "SD") {
    result <- rg199_p1_sd_degF(product_form)
  }

  # Apply unit conversion if needed
  if (output_unit == "Celsius") {
    result <- dF_to_dC(result)
  }

  # Return result with or without names
  if (use_names) result else unname(result)
}


# P1.1 CF
rg199_p1_cf_degF <- function(product_form, Cu, Ni) {
  product_form <- to_baseweld(product_form)
  n <- length(product_form)
  cf <- numeric(n)
  for (i in seq_len(n)) {
    pf <- product_form[i]
    cu <- Cu[i]
    ni <- Ni[i]

    cf[i] <- if (pf == "B") rg199_p1_cf_base(cu, ni) else rg199_p1_cf_weld(cu, ni)
  }
  cf
}


# P1.1 SD
rg199_p1_sd_degF <- function(product_form) {
  product_form <- to_baseweld(product_form)
  base_weld <- c("B" = 17, "W" = 28)
  base_weld[product_form]
}


# P1.1 CF base table
rg199_p1_cf_base <- function(Cu, Ni) { # not vectorized
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


# P1.1 CF weld table
rg199_p1_cf_weld <- function(Cu, Ni) { # not vectorized
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


# interpolated 2d
interp2d_linear <- function(x_values, y_values, table, x, y) {
  # Clamp x to [min, max]
  if (x < min(x_values)) {
    message(sprintf("x=%g is below range. Clamped to x=%g.", x, min(x_values)))
    x <- min(x_values)
  } else if (x > max(x_values)) {
    message(sprintf("x=%g is above range. Clamped to x=%g.", x, max(x_values)))
    x <- max(x_values)
  }

  # Clamp y to [min, max]
  if (y < min(y_values)) {
    message(sprintf("y=%g is below range. Clamped to y=%g.", y, min(y_values)))
    y <- min(y_values)
  } else if (y > max(y_values)) {
    message(sprintf("y=%g is above range. Clamped to y=%g.", y, max(y_values)))
    y <- max(y_values)
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


## 3. RG1.99R2 P2.1 Calculations ----

#' Regulatory Guide 1.99 Rev. 2 Position 2.1 Surveillance-Based Embrittlement Property Calculator
#'
#' Computes embrittlement-related properties using surveillance test data based on the interpretation
#' of U.S. NRC Regulatory Guide 1.99 Rev. 2 (1988), Position 2.1. This approach uses fluence and
#' measured transition temperature shift (TTS) data to compute a best-estimate Chemistry Factor (CF)
#' or an appropriate Standard Deviation (SD).
#'
#' @param SV_flu Numeric vector. Surveillance fluence values (n/cm^2).
#' @param SV_tts Numeric vector. Surveillance transition temperature shifts (TTS).
#' @param product_form Character vector of length 1 ("B", "F", "P", or "W") required for SD calculation.
#' @param output Character. One of "CF" or "SD". Determines which property is returned.
#' @param output_unit Character. Desired output unit, either "Celsius" or "Fahrenheit". Default is "Celsius".
#' @param SV_tts_unit Character. Input unit of SV_tts, either "Celsius" or "Fahrenheit". Default is "Celsius".
#' @param use_names Logical. Whether to retain input names in the output. Default is FALSE.
#'
#' @return A single numeric value representing the computed Chemistry Factor (CF) or Standard Deviation (SD).
#'         The unit corresponds to the selected output_unit.
#'
#' @details
#' For "CF" output, at least two nonzero SV_flu values must be provided. All zero-fluence data must have zero TTS.
#' For "SD" output, the product_form must be a single consistent value across the data (either "B" for base metal or "W" for weld).
#' The SD is returned as either the full SD value or half, depending on the scatter of the data.
#'
#' @examples
#' RG199R2_P2(SV_flu = c(1e19, 2e19), SV_tts = c(30, 45), product_form = "B", output = "SD")
#'
#' @seealso \code{\link{RG199R2}}, \code{\link{RG199R2_P1}}
#'
#' @export
RG199R2_P2 <- function(SV_flu, # A numeric vector of length 2 or more
                       SV_tts, # A numeric vector of length 2 or more
                       product_form = NULL, # for SD
                       output = c("CF", "SD"),
                       output_unit = c("Celsius", "Fahrenheit"),
                       SV_tts_unit = c("Celsius", "Fahrenheit"),
                       use_names = FALSE) {

  # Match and validate input arguments
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)
  SV_tts_unit <- match.arg(SV_tts_unit, several.ok = FALSE)

  # Check numeric validity
  stopifnot(is.numeric(SV_flu), is.numeric(SV_tts))
  stopifnot(length(SV_flu) == length(SV_tts))
  stopifnot(all(SV_tts[SV_flu == 0] == 0))
  stopifnot(sum(SV_flu > 0) >= 2)

  if (output == "CF" && !is.null(product_form)) {
    warning("'product_form' is ignored when output = 'CF'.")
  }

  # Temperature convert
  SV_tts_degF <- if (SV_tts_unit == "Celsius") dC_to_dF(SV_tts) else SV_tts

  # check product_form uniqueness
  if (output == "SD") {
    if (is.null(product_form)) {
      stop("For SD calculation, 'product_form' must be provided.")
    }
    product_unique <- unique(product_form)
    stopifnot(length(product_unique) == 1)
  }

  # Output calculation
  result <- switch(output,
    "CF" = rg199_p2_cf(SV_flu, SV_tts_degF),
    "SD" = rg199_p2_sd_degF(SV_flu, SV_tts_degF, product_unique)
  )

  # Convert degF to degC if needed
  if (output_unit == "Celsius") {
    result <- dF_to_dC(result)
  }

  # Return result with or without names
  if (use_names) result else unname(result)
}


# P2.1 CF
rg199_p2_cf <- function(SV_flu, SV_tts) {
  ff <- rg199_ff(SV_flu)
  sum(ff * SV_tts) / sum(ff^2) # Return single value
}


# P2.1 SD
rg199_p2_sd_degF <- function(SV_flu, SV_tts, product_unique) {
  cf <- rg199_p2_cf(SV_flu, SV_tts) # Single value
  best_tts <- cf * rg199_ff(SV_flu)
  scatter <- abs(SV_tts - best_tts)
  max_scatter <- max(scatter)

  product <- to_baseweld(product_unique)
  sd <- c("B" = 17, "W" = 28)[product]
  half_sd <- sd / 2 # 8.5 or 14
  if (all(sd < max_scatter)) sd else half_sd # Return single value
}
