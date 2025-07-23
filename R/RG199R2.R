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
#' @seealso \code{\link{RG199R2_P1}}, \code{\link{RG199R2_P2}}}
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
#' Computes radiation embrittlement-related properties based on the U.S. NRC Regulatory Guide 1.99 Rev. 2 (1988),
#' specifically Position 1.1. This model uses tabulated Chemistry Factors and fixed Standard Deviations
#' based on material composition and neutron fluence.
#'
#' @export
RG199R2_P1 <- function(product_form, # B, F, P, W
                       Cu = NULL, # wt%
                       Ni = NULL, # wt%
                       output = c("CF", "SD"),
                       output_unit = c("Celsius", "Fahrenheit"),
                       use_names = FALSE) {
  # Input requirement checks
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)

  if (output == "CF") {
    if (is.null(product_form) || is.null(Cu) || is.null(Ni)) {
      stop("For CF calculation, please provide 'product_form', 'Cu', and 'Ni'.")
    }
  }

  if (!is.null(Cu)) {
    stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
  }
  if (!is.null(Ni)) {
    stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))
  }

  # Expand vectors
  expanded <- expand_vectors(product_form, Cu, Ni)
  pf <- expanded[[1]]
  cu <- expanded[[2]]
  ni <- expanded[[3]]

  # Output calculation
  result <- switch(output, # degF
    "CF" = rg199_p1_cf_degF(pf, cu, ni),
    "SD" = rg199_p1_sd_degF(pf)
  )

  # Convert degF to degC if needed
  if (output_unit == "Celsius") {
    result <- dF_to_dC(result)
  }

  if (use_names) result else unname(result)
}


# P1.1 CF
rg199_p1_cf_degF <- function(product_form, Cu, Ni) {
  product_form <- to_baseweld(product_form) # B or W
  is_base <- product_form == "B"
  is_weld <- product_form == "W"

  cf <- numeric(length(product_form))
  cf[is_base] <- mapply(rg199_p1_cf_base, Cu[is_base], Ni[is_base])
  cf[is_weld] <- mapply(rg199_p1_cf_weld, Cu[is_weld], Ni[is_weld])

  cf
}

# P1.1 SD
rg199_p1_sd_degF <- function(product_form) {
  product_form <- to_baseweld(product_form)
  base_weld <- c("B" = 17, "W" = 28)
  base_weld[product_form]
}


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
#' of U.S. NRC Regulatory Guide 1.99 Rev. 2 (1988), Position 2.1.
#'
#' The function estimates key embrittlement parameters such as Transition Temperature Shift (TTS),
#' Chemistry Factor (CF), Fluence Factor (FF), Standard Deviation (SD), and Regulatory Margin using
#' surveillance capsule data from reactor pressure vessel materials.
#'

#' @export
#'
#'
#'
#' Return only single value
RG199R2_P2 <- function(SV_flu, # A numeric vector of length 2 or more
                       SV_tts, # A numeric vector of length 2 or more
                       product_form = NULL, # for SD, Margin
                       output = c("CF", "SD"),
                       output_unit = c("Celsius", "Fahrenheit"),
                       SV_tts_unit = c("Celsius", "Fahrenheit")) {
  # Input requirement checks
  output <- match.arg(output, several.ok = FALSE)
  output_unit <- match.arg(output_unit, several.ok = FALSE)
  SV_tts_unit <- match.arg(SV_tts_unit, several.ok = FALSE)

  stopifnot(is.numeric(SV_flu), is.numeric(SV_tts))
  stopifnot(length(SV_flu) == length(SV_tts))
  stopifnot(all(SV_tts[SV_flu == 0] == 0))
  stopifnot(sum(SV_flu > 0) >= 2)

  # Temperature convert
  SV_tts_degF <- if (SV_tts_unit == "Celsius") dC_to_dF(SV_tts) else SV_tts

  # check product_form uniqueness
  if (!is.null(product_form)) {
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

  unname(result)
}


# P2.1 CF
rg199_p2_cf <- function(SV_flu, SV_tts) {
  ff <- rg199_ff(SV_flu)
  sum(ff * SV_tts) / sum(ff^2) # Returns a single CF value
}


# P2.1 SD
rg199_p2_sd_degF <- function(SV_flu, SV_tts, product_unique) {
  cf <- rg199_p2_cf(SV_flu, SV_tts) # Single CF value
  best_tts <- cf * rg199_ff(SV_flu)
  scatter <- abs(SV_tts - best_tts)
  max_scatter <- max(scatter)

  sd <- c("B" = 17, "W" = 28)[product_unique]
  half_sd <- sd / 2 # 8.5 or 14
  if (all(sd < max_scatter)) sd else half_sd
}
