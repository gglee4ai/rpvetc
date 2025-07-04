## 1. Main Entry ----
#' Regulatory Guide 1.99 Rev. 2 Position 1.1 Table-based Embrittlement Property Calculator
#'
#' Computes radiation embrittlement-related properties based on the U.S. NRC *Regulatory Guide 1.99 Revision 2 (1988)*,
#' specifically following the Position 1.1 methodology. This position defines empirical models for evaluating the
#' embrittlement of reactor pressure vessel (RPV) materials, based on material composition and neutron fluence.
#'
#' This is the main interface function for Position 1.1. Given the product form, copper and nickel contents, and
#' neutron fluence, it internally computes the requested output using table-based or empirical expressions defined in the guide.
#' Temperature unit conversion is handled automatically. This function does not use surveillance test data and relies only on
#' Position 1.1 (i.e., tabulated Chemistry Factor and fixed Standard Deviation).
#'
#' @param product_form Character vector specifying the product form. Must be one of \code{"B"} (base metal),
#'   \code{"F"} (forgings), \code{"P"} (plate), or \code{"W"} (weld metal). \code{"F"} and \code{"P"} are internally treated as \code{"B"}.
#' @param Cu Numeric vector of copper content (wt%). Required to compute CF, TTS, and Margin.
#' @param Ni Numeric vector of nickel content (wt%). Required to compute CF, TTS, and Margin.
#' @param fluence Numeric vector of neutron fluence (n/cm\eqn{^2}). Required for FF, TTS, and Margin calculations.
#' @param output Character scalar specifying which property to compute. Must be one of:
#'   \itemize{
#'     \item \code{"TTS"} – Transition Temperature Shift
#'     \item \code{"CF"} – Chemistry Factor
#'     \item \code{"FF"} – Fluence Factor
#'     \item \code{"SD"} – Standard Deviation
#'     \item \code{"Margin"} – Regulatory Margin
#'   }
#' @param temperature_unit Character scalar indicating the output temperature unit. Must be one of:
#'   \code{"Celsius"} or \code{"Fahrenheit"}.
#'
#' @return A numeric vector containing the result corresponding to \code{output}. Units are:
#'   \itemize{
#'     \item \code{"FF"} – unitless
#'     \item others – degrees Celsius or Fahrenheit, depending on \code{temperature_unit}
#'   }
#'
#' @examples
#' # Compute Chemistry Factor using Cu/Ni
#' RG199R2_P1("B", Cu = 0.25, Ni = 0.8, fluence = 1e19, output = "CF", temperature_unit = "Fahrenheit")
#'
#' # Compute Fluence Factor for a given fluence
#' RG199R2_P1(fluence = 1e19, output = "FF")
#'
#' # Compute TTS using Cu/Ni and fluence
#' RG199R2_P1("B", Cu = 0.2, Ni = 0.7, fluence = 5e19, output = "TTS", temperature_unit = "Celsius")
#'
#' # Compute Margin using base metal data
#' RG199R2_P1("B", Cu = 0.15, Ni = 0.9, fluence = 3e19, output = "Margin")
#'
#' @seealso \code{\link{RG199R2_P2}} for Position 2.1 model
#' @export


RG199R2_P1 <- function(
    product_form = NULL, # for CF
    Cu = NULL, # for CF
    Ni = NULL, # for CF
    fluence = NULL, # for FF, TTS, Margin
    output = c("TTS", "CF", "FF", "SD", "Margin"),
    temperature_unit = c("Celsius", "Fahrenheit")) {
  #------------------------#
  # 1) 기본 인수 점검
  #------------------------#
  output <- match.arg(output, c("TTS", "CF", "FF", "SD", "Margin"))
  temperature_unit <- match.arg(temperature_unit, c("Celsius", "Fahrenheit"))

  if (!is.null(product_form)) {
    product_form <- parse_product_form(product_form)
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

  #------------------------------------#
  # 2) 입력값 길이 검사 및 확장
  #------------------------------------#
  # 필요한 인자들의 길이 파악
  arg_list <- list(product_form, Cu, Ni, fluence)
  arg_len <- sapply(arg_list, length)
  max_len <- max(arg_len)
  stopifnot(max_len > 0)

  # 길이가 0 (NULL), 1, max_len 세 가지 경우만 허용
  if (!all(arg_len %in% c(0, 1, max_len))) {
    stop("product_form, Cu, Ni, and fluence must have length 0, 1 or the same length.")
  }

  # 변수 길이 확장
  pf <- rep_expand(product_form, max_len)
  cu <- rep_expand(Cu, max_len)
  ni <- rep_expand(Ni, max_len)
  fl <- rep_expand(fluence, max_len)

  #------------------------------------#
  # 3) 출력값 선택
  #------------------------------------#
  result <- switch(output,
    "TTS" = rg199r2_p1_tts(pf, cu, ni, fl),
    "CF" = rg199r2_p1_cf(pf, cu, ni),
    "FF" = rg199r2_ff(fl),
    "SD" = rg199r2_p1_sd(pf),
    "Margin" = rg199r2_p1_margin(pf, cu, ni, fl)
  )

  #------------------------------------#
  # 4) 결과 온도 변환
  #------------------------------------#
  if (temperature_unit == "Celsius" && output != "FF") {
    result <- result * (5 / 9)
  }
  unname(result)
}


#' Regulatory Guide 1.99 Rev. 2 Position 2.1 Surveillance-Based Embrittlement Property Calculator
#'
#' Computes radiation embrittlement-related properties using plant-specific surveillance data,
#' based on an extended interpretation of the U.S. NRC *Regulatory Guide 1.99 Revision 2 (1988)*.
#'
#' This function is designed for regulatory or engineering assessments using Position 2.1-like methodology,
#' which incorporates actual surveillance fluence–shift data pairs. Using these, the Chemistry Factor (CF)
#' is back-calculated, and used in combination with neutron fluence to derive transition temperature shift (TTS),
#' Fluence Factor (FF), Standard Deviation (SD), and Margin. Temperature unit conversion is handled automatically.
#'
#' @param product_form Optional character vector specifying the product form. Must be one of \code{"B"} (base metal),
#'   \code{"F"} (forgings), \code{"P"} (plate), or \code{"W"} (weld metal). Required only for computing \code{"SD"} and \code{"Margin"}.
#'   Internally, \code{"F"} and \code{"P"} are treated as \code{"B"}.
#' @param SV_flu Numeric vector of surveillance neutron fluence values (n/cm\eqn{^2}). Required for all calculations except FF.
#' @param SV_tts Numeric vector of corresponding measured transition temperature shifts (TTS) in Fahrenheit.
#'   Must be the same length as \code{SV_flu}. Required for all outputs except FF.
#' @param fluence Optional numeric vector of neutron fluence values (n/cm\eqn{^2}) for which properties like FF, TTS, or Margin are to be calculated.
#'   If omitted, the function uses \code{SV_flu} as default.
#' @param output Character scalar indicating which property to compute. Must be one of:
#'   \itemize{
#'     \item \code{"TTS"} – Estimated Transition Temperature Shift
#'     \item \code{"CF"} – Back-calculated Chemistry Factor
#'     \item \code{"FF"} – Fluence Factor
#'     \item \code{"SD"} – Standard Deviation of surveillance data vs. model
#'     \item \code{"Margin"} – Regulatory Margin (minimum of TTS or 2×SD)
#'   }
#' @param temperature_unit Character scalar indicating the output temperature unit. Must be one of:
#'   \code{"Celsius"} or \code{"Fahrenheit"}.
#'
#' @return A numeric vector or scalar corresponding to the requested \code{output}. Units are:
#'   \itemize{
#'     \item \code{"FF"} – unitless
#'     \item others – degrees Celsius or Fahrenheit, depending on \code{temperature_unit}
#'   }
#'
#' @details
#' The Chemistry Factor (CF) is inferred by minimizing the squared residuals of the model:
#' \eqn{TTS_i ≈ CF × FF(fluence_i)}. The Standard Deviation (SD) is selected as either half or full of the
#' fixed threshold (17°F for base metal, 28°F for weld) depending on the maximum residual.
#'
#' For Margin, the smaller of TTS or 2×SD is returned, as per regulatory guidance.
#'
#' @examples
#' # Back-calculate CF from surveillance data
#' RG199R2_P2(SV_flu = c(1e19, 2e19), SV_tts = c(100, 130), output = "CF")
#'
#' # Compute TTS for a given fluence using inferred CF
#' RG199R2_P2(SV_flu = c(1e19, 2e19), SV_tts = c(100, 130), fluence = 3e19, output = "TTS")
#'
#' # Compute Margin for weld metal
#' RG199R2_P2("W", c(1e19, 2e19), c(100, 130), fluence = 3e19, output = "Margin")
#'
#' @seealso \code{\link{RG199R2_P1}} for Position 1.1 (table-based) model
#' @export

RG199R2_P2 <- function(
    product_form = NULL, # for SD, Margin
    SV_flu = NULL, # SV_fluence vector
    SV_tts = NULL, # SV_TTS vector
    fluence = NULL, # for FF, TTS, Margin,
    output = c("TTS", "CF", "FF", "SD", "Margin"),
    temperature_unit = c("Celsius", "Fahrenheit")) {
  #------------------------#
  # 1) 기본 인수 점검
  #------------------------#
  output <- match.arg(output, c("TTS", "CF", "FF", "SD", "Margin"))
  temperature_unit <- match.arg(temperature_unit, c("Celsius", "Fahrenheit"))

  if (!is.null(SV_flu) && !is.null(SV_tts)) {
    stopifnot(is.numeric(SV_flu), is.numeric(SV_tts))
    stopifnot(length(SV_flu) == length(SV_tts))
    stopifnot(all(SV_tts[SV_flu == 0] == 0))
    stopifnot((sum(SV_flu > 0) >= 2)) # At least two valid surveillance data points required
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
    product_form <- parse_product_form(product_form)
  }

  #------------------------------------#
  # 2) 입력값 길이 검사 및 확장
  #------------------------------------#
  # 필요한 인자들의 길이 파악
  arg_list <- list(product_form, fluence)
  arg_len <- sapply(arg_list, length)
  max_len <- max(arg_len)

  # 길이가 0 (NULL), 1, max_len 세 가지 경우만 허용
  if (!all(arg_len %in% c(0, 1, max_len))) {
    stop("product_form, Cu, Ni, and fluence must have length 0, 1 or the same length.")
  }

  # 변수 길이 확장
  pf <- rep_expand(product_form, max_len)
  fl <- rep_expand(fluence, max_len)

  #------------------------------------#
  # 3) 출력값 선택
  #------------------------------------#
  result <- switch(output,
    "TTS" = rg199r2_p2_tts(SV_flu, SV_tts, fl),
    "CF" = rg199r2_p2_cf(SV_flu, SV_tts),
    "FF" = rg199r2_ff(fl),
    "SD" = rg199r2_p2_sd(pf, SV_flu, SV_tts),
    "Margin" = rg199r2_p2_margin(pf, SV_flu, SV_tts, fl)
  )

  #------------------------------------#
  # 4) 결과 온도 변환
  #------------------------------------#
  if (temperature_unit == "Celsius" && output != "FF") {
    result <- result * (5 / 9)
  }
  unname(result)
}


## 2. P1.1 Calculations ----

rg199r2_p1_tts <- function(product_form, Cu, Ni, fluence) {
  cf <- rg199r2_p1_cf(product_form, Cu, Ni)
  calculate_tts(cf, fluence)
}


rg199r2_p1_cf <- function(product_form, Cu, Ni) {
  n <- length(product_form)
  cf <- numeric(n)

  for (i in seq_len(n)) {
    pf <- product_form[i]
    cu <- Cu[i]
    ni <- Ni[i]

    cf[i] <- if (pf == "B") {
      rg199r2_p1_cf_base(cu, ni)
    } else {
      rg199r2_p1_cf_weld(cu, ni)
    }
  }

  cf
}


rg199r2_p1_sd <- function(product_form) {
  base_weld <- c("B" = 17, "W" = 28)
  base_weld[product_form]
}


rg199r2_p1_margin <- function(product_form, Cu, Ni, fluence) {
  tts <- rg199r2_p1_tts(product_form, Cu, Ni, fluence) # Calculate TTS
  margin <- 2 * rg199r2_p1_sd(product_form) # Margin is 2 * SD
  ifelse(margin > tts, tts, margin) # The smaller of TTS or Margin
}


rg199r2_p1_cf_base <- function(Cu, Ni) { # not vectorized
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


rg199r2_p1_cf_weld <- function(Cu, Ni) { # not vectorized
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

rg199r2_p2_tts <- function(SV_flu, SV_tts, fluence) {
  cf <- rg199r2_p2_cf(SV_flu, SV_tts)
  fl <- if (is.null(fluence)) SV_flu else fluence
  calculate_tts(cf, fl)
}


rg199r2_p2_cf <- function(SV_flu, SV_tts) {
  ff <- rg199r2_ff(SV_flu)
  sum(ff * SV_tts) / sum(ff^2) # Returns a single CF value
}


rg199r2_p2_sd <- function(product_form, SV_flu, SV_tts) {
  cf <- rg199r2_p2_cf(SV_flu, SV_tts) # Single CF value
  best_tts <- cf * rg199r2_ff(SV_flu)
  scatter <- abs(SV_tts - best_tts)
  max_scatter <- max(scatter)

  base_weld <- c("B" = 17, "W" = 28)
  threshold <- base_weld[product_form]
  half_val <- threshold / 2 # 8.5 or 14
  if (all(threshold < max_scatter)) threshold else half_val
}


rg199r2_p2_margin <- function(product_form, SV_flu, SV_tts, fluence) {
  tts <- rg199r2_p2_tts(SV_flu, SV_tts, fluence) # Calculate TTS
  margin <- 2 * rg199r2_p2_sd(product_form, SV_flu, SV_tts) # Margin is 2 * SD
  ifelse(margin > tts, tts, margin) # The smaller of TTS or Margin
}


# 4. Utility Functions ----
# These are internal-use functions common to Position 1.1 and 1.2 implementations.

# Fluence Factor (FF)
# Computes Fluence Factor from fluence in n/cm² (numeric vector)
rg199r2_ff <- function(fluence) {
  stopifnot(is.numeric(fluence), all(fluence >= 0))
  f19 <- fluence / 1e19
  f19^(0.28 - 0.1 * log10(f19))
}


# TTS from CF and fluence
# Generalized computation: TTS = CF × FF
calculate_tts <- function(cf, fluence) {
  cf * rg199r2_ff(fluence)
}

# Safe Product Form Parser
# Maps input form ("F", "P") to "B" and validates
parse_product_form <- function(product_form) {
  form <- as.character(product_form)
  if (any(!form %in% c("B", "F", "P", "W"))) {
    stop("Invalid product_form. Must be one of 'B', 'F', 'P', 'W'.")
  }
  ifelse(form == "W", "W", "B")
}

# 벡터 길이 확장
rep_expand <- function(x, max_len) {
  # 길이 1개인 벡터를 max_len으로 확장
  if (length(x) == 1 && max_len > 1) rep(x, max_len) else x
}
