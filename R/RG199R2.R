## Main interface function ----

#' RG199R2
#'
#' Provide TTS, CF, FF, and SD of Regulatory Guide 1.99 Rev. 2 (1988).
#'
#' @param product_form character vector, c("B", F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param fluence numeric vector, n/cm2
#' @param CF numeric vector, temperature unit
#' @param SD numeric vector, temperature unit
#' @param SV_flu numeric vector, n/cm2
#' @param SV_tts numeric vector, temperature unit
#' @param output character c("TTS", "CF", "FF", "SD", "Margin")
#' @param temperature_unit character c("Celsius", "Fahrenheit")
#' @param verbose logical TRUE or FALSE
#'
#' @return various value according to output
#' @export
#'
#' @examples
#' RG199R2("B", 0.2, 0.2, 1e19, temperature_unit = "F") # should be 102
#'
RG199R2 <- function(
    product_form = NULL, # for CF
    Cu = NULL, # for CF
    Ni = NULL, # for CF
    fluence = NULL, # for FF, TTS, Margin
    CF = NULL, # for FF, TTS, Margin
    SD = NULL, # for Margin
    SV_flu = NULL, # for CF, SD
    SV_tts = NULL, # for CF, SD
    output = c("TTS", "CF", "FF", "SD", "Margin"),
    temperature_unit = c("Celsius", "Fahrenheit"),
    verbose = FALSE) {
  #------------------------#
  # 1) 기본 설정
  #------------------------#
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)
  if (!is.null(product_form)) {
    product_form <- as.character(product_form)
    stopifnot(all(product_form %in% c("B", "F", "P", "W")))
    product_form[product_form %in% c("F", "P")] <- "B"
  }

  #------------------------#
  # 2) 계산용 온도차를 화씨로 변환
  #------------------------#
  if (temperature_unit == "Celsius") {
    if (!is.null(SV_tts) && is.numeric(SV_tts)) {
      SV_tts <- SV_tts * (9 / 5)
    }
    if (!is.null(CF) && is.numeric(CF)) {
      CF <- CF * (9 / 5)
    }
    if (!is.null(SD) && is.numeric(SD)) {
      SD <- SD * (9 / 5)
    }
  }

  #------------------------#
  # 3) SV_flu가 있고 fluence가 없으면 fluence를 대체
  #------------------------#
  if (is.numeric(SV_flu) && is.null(fluence)) {
    fluence <- SV_flu
  }

  #------------------------------------#
  # 4) 입력값 길이 검사 및 확장
  #------------------------------------#
  # 필요한 인자들의 길이 파악
  arg_list <- list(product_form, Cu, Ni, fluence, CF, SD)
  arg_len <- sapply(arg_list, length)
  max_len <- max(arg_len)

  # 길이가 0 (NULL), 1, max_len 세 가지 경우만 허용
  if (!all(arg_len %in% c(0, 1, max_len))) {
    stop("product_form, Cu, Ni, fluence, CF and SD must have length 0, 1 or max length.")
  }

  # 벡터 길이 확장
  replicate_to_max <- function(x, max_len) {
    if (length(x) == 1 && max_len > 1) rep(x, max_len) else x # 길이 1개는 max_len으로 확장
  }
  product_form <- replicate_to_max(product_form, max_len)
  Cu <- replicate_to_max(Cu, max_len)
  Ni <- replicate_to_max(Ni, max_len)
  fluence <- replicate_to_max(fluence, max_len)
  CF <- replicate_to_max(CF, max_len)
  SD <- replicate_to_max(SD, max_len)

  #------------------------------------#
  # 5) 중요 계산 함수들(복합 입력 고려)
  #------------------------------------#
  # 5-1) calc_cf: 다양한 CF 계산 종합(°F)
  calc_cf <- function(product_form, Cu, Ni, CF, SV_flu, SV_tts) {
    # Case 1: CF 직접
    if (is.numeric(CF) && all(CF >= 0)) {
      if (verbose) message("Case 1: Using direct CF input.")
      return(CF)
    }

    # Case 2: SV 데이터(감시시험)로부터 CF 추정
    if (is.numeric(SV_flu) && sum(SV_flu > 0) >= 2) {
      if (verbose) message("Case 2: Using SV data to estimate CF.")
      return(rg199r2_cf_by_sv(SV_flu, SV_tts))
    }

    # Case 3: Table로 계산
    if (verbose) message("Case 3: Using RG1.99 R2 table for CF.")
    return(rg199r2_cf_table(product_form, Cu, Ni))
  }

  # 5-2) calc_margin: 다양한 Margin 계산 종합(°F)
  calc_margin <- function(product_form, Cu, Ni, fluence, CF, SD, SV_flu, SV_tts) {
    # CF가 제공되지 않으면, 계산
    if (is.numeric(CF) && all(CF >= 0)) {
      cf <- CF
    } else {
      cf <- calc_cf(product_form, Cu, Ni, CF, SV_flu, SV_tts)
    }

    # SD가 제공되지 않으면 계산
    if (is.numeric(SD) && all(SD >= 0)) {
      sd <- SD
    } else {
      sd <- rg199r2_sd(product_form, SV_flu, SV_tts)
    }

    # Margin 계산값 반환
    rg199r2_margin(cf, fluence, sd)
  }

  #------------------------------------#
  # 결과 선택
  #------------------------------------#
  result <- switch(output,
    "CF" = calc_cf(product_form, Cu, Ni, CF, SV_flu, SV_tts),
    "FF" = rg199r2_ff(fluence),
    "TTS" = {
      cf <- calc_cf(product_form, Cu, Ni, CF, SV_flu, SV_tts)
      rg199r2_tts(cf, fluence)
    },
    "SD" = rg199r2_sd(product_form, SV_flu, SV_tts),
    "Margin" = calc_margin(product_form, Cu, Ni, fluence, CF, SD, SV_flu, SV_tts)
  )

  #------------------------------------#
  # 5) 결과 온도 변환
  #    - 만약 최종 출력을 °C로 원하면
  #    - FF 제외
  #------------------------------------#
  if (temperature_unit == "Celsius" && output %in% c("TTS", "CF", "SD")) {
    result <- result * (5 / 9)
  }

  return(unname(result))
}



## Auxiliary functions ----

#' Fluence Factor Calculation Based on Regulatory Guide 1.99 Rev. 2
#'
#' Computes the fluence factor (FF) according to the guidelines in Regulatory Guide 1.99 Rev. 2.
#' This function calculates FF based on the provided neutron fluence.
#'
#' @param fluence numeric vector, neutron fluence in n/cm². Must be a non-negative value.
#' @return A numeric vector representing the fluence factor (FF).
#' @export
#' @examples
#' # Example: Calculate FF for a given fluence value
#' rg199r2_ff(c(1e18, 5e19, 1e20))
#'
rg199r2_ff <- function(fluence) {
  stopifnot(is.numeric(fluence), all(fluence >= 0))

  fl <- fluence / 1e19
  fl^(0.28 - 0.1 * log10(fl))
}



#' Temperature Transition Shift (TTS) Calculation Based on Regulatory Guide 1.99 Rev. 2
#'
#' Computes the temperature transition shift (TTS) as per the guidelines in Regulatory Guide 1.99 Rev. 2.
#' This function calculates TTS based on the provided chemistry factor (CF) and neutron fluence.
#'
#' @param CF numeric vector, chemistry factor. Must be a non-negative value.
#' @param fluence numeric vector, neutron fluence in n/cm². Must be a non-negative value.
#' @return A numeric vector representing the temperature transition shift (TTS) in degrees Fahrenheit (°F).
#' @export
#' @examples
#' # Example: Calculate TTS for given CF and fluence values
#' rg199r2_tts(c(10, 15), c(1e18, 5e19))
#'
rg199r2_tts <- function(CF, fluence) {
  stopifnot(is.numeric(CF), all(CF >= 0))

  CF * rg199r2_ff(fluence)
}



#' Chemistry Factor Calculation Based on Surveillance Test Results
#'
#' Computes the chemistry factor (CF) using surveillance test results as per the guidelines in Regulatory Guide 1.99 Rev. 2.
#' The calculation requires at least two valid surveillance test results and ensures that fluence values of zero correspond to a temperature transition shift (TTS) of zero.
#'
#' @param SV_flu numeric vector, neutron fluence in n/cm² from surveillance test results. Must be non-negative.
#' @param SV_tts numeric vector, temperature transition shift (TTS) in degrees Fahrenheit (°F) from surveillance test results.
#' Must be non-negative and of the same length as `SV_flu`.
#' @return A single numeric value representing the chemistry factor (CF) in degrees Fahrenheit (°F).
#' @export
#' @examples
#' # Example: Calculate CF using surveillance test results
#' rg199r2_cf_by_sv(c(1e18, 5e19, 1e20), c(10, 50, 100))
#'
rg199r2_cf_by_sv <- function(SV_flu, SV_tts) {
  stopifnot(is.numeric(SV_flu), is.numeric(SV_tts))
  stopifnot(length(SV_flu) == length(SV_tts))
  stopifnot(all(SV_tts[SV_flu == 0] == 0))
  stopifnot((sum(SV_flu > 0) >= 2)) # At least two valid surveillance data points required

  ff <- rg199r2_ff(SV_flu)
  ff2 <- ff^2
  sum(ff * SV_tts) / sum(ff2) # Returns a single CF value
}



#' Chemistry Factor (CF) from Regulatory Guide 1.99 Rev. 2 Tables
#'
#' Computes the chemistry factor (CF) based on material properties and product form using the tables provided in Regulatory Guide 1.99 Rev. 2.
#'
#' @param product_form character vector, specifying the product form. Must be one of \code{"B"} (base metal) or \code{"W"} (weld metal).
#' @param Cu numeric vector, copper content in weight percent (wt%). Must be between 0 and 100.
#' @param Ni numeric vector, nickel content in weight percent (wt%). Must be between 0 and 100.
#' @return A numeric vector representing the chemistry factor (CF) in degrees Fahrenheit (°F).
#' @export
#' @examples
#' # Example: Calculate CF for base and weld metals with given Cu and Ni values
#' rg199r2_cf_table(c("B", "W"), c(0.2, 0.3), c(0.7, 1.0))
#'
rg199r2_cf_table <- function(product_form, Cu, Ni) {
  stopifnot(all(product_form %in% c("B", "W")))
  stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
  stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))

  mapply(function(pf, cu, ni) {
    if (pf == "B") {
      calc_cf_base(cu, ni) # Calculate CF for base metal using table
    } else { # pf == "W"
      calc_cf_weld(cu, ni) # Calculate CF for weld metal using table
    }
  }, product_form, Cu, Ni, SIMPLIFY = TRUE)
}



#' Sigma Delta (SD) Calculation from Surveillance Test Data
#'
#' Computes the sigma delta (SD) based on surveillance test data and product form using Regulatory Guide 1.99 Rev. 2 guidelines.
#' The SD is determined either from the product form alone or using surveillance fluence and TTS data.
#'
#' @param product_form character vector, specifying the product form. Must be one of \code{"B"} (base metal) or \code{"W"} (weld metal).
#' @param SV_flu numeric vector, neutron fluence in n/cm² from surveillance test data. Optional if SD is determined solely by \code{product_form}.
#' @param SV_tts numeric vector, temperature transition shift (TTS) in degrees Fahrenheit (°F) from surveillance test data. Optional if SD is determined solely by \code{product_form}.
#' @return A numeric vector representing the sigma delta (SD) in degrees Fahrenheit (°F).
#' @export
#' @examples
#' # Example 1: Calculate SD based only on product form
#' rg199r2_sd(c("B", "W"))
#'
#' # Example 2: Calculate SD using surveillance test data
#' rg199r2_sd(c("B", "B"), c(1e18, 5e19), c(10, 50))
#'
rg199r2_sd <- function(product_form, SV_flu = NULL, SV_tts = NULL) {
  base_weld <- c("B" = 17, "W" = 28)

  # Case 1: Only product_form is provided
  if (is.null(SV_flu) && is.null(SV_tts)) {
    sd <- base_weld[product_form]
    return(sd)
  }

  # Case 2: Both SV_flu and SV_tts are provided
  unique_form <- unique(product_form)
  stopifnot(length(unique_form) == 1) # Ensure all entries are either "B" or "W"

  cf <- rg199r2_cf_by_sv(SV_flu, SV_tts) # Single CF value
  best_tts <- cf * rg199r2_ff(SV_flu)
  scatter <- abs(SV_tts - best_tts)
  max_scatter <- max(scatter)

  threshold <- base_weld[unique_form] # 17 or 28
  half_val <- threshold / 2 # 8.5 or 14
  sd <- if (max_scatter > threshold) threshold else half_val

  # Expand single SD to match the length of product_form
  sd <- rep(sd, length(product_form))
  return(sd)
}



#' Margin Calculation Based on Regulatory Guide 1.99 Rev. 2
#'
#' Computes the margin for temperature transition shift (TTS) using the chemistry factor (CF), neutron fluence, and sigma delta (SD) as per the guidelines in Regulatory Guide 1.99 Rev. 2.
#'
#' @param CF numeric vector, the chemistry factor (CF) in degrees Fahrenheit (°F). Must be non-negative.
#' @param fluence numeric vector, neutron fluence in n/cm². Must be non-negative.
#' @param SD numeric vector, sigma delta (SD) in degrees Fahrenheit (°F). Must be non-negative.
#' @return A numeric vector representing the computed margin in degrees Fahrenheit (°F).
#' @export
#' @examples
#' # Example: Calculate margin for given CF, fluence, and SD
#' rg199r2_margin(c(10, 15), c(1e18, 5e19), c(5, 7))
#'
rg199r2_margin <- function(CF, fluence, SD) {
  stopifnot(is.numeric(CF), all(CF >= 0))
  stopifnot(is.numeric(fluence), all(fluence >= 0))
  stopifnot(is.numeric(SD), all(SD >= 0))

  tts <- CF * rg199r2_ff(fluence) # Calculate TTS
  margin <- 2 * SD # Margin is 2 * SD
  ifelse(margin > tts, tts, margin) # Return the smaller of TTS or Margin
}



## Internal functions ----

#' Chemistry Factor (CF) Calculation for Base Metal
#'
#' Computes the chemistry factor (CF) for base metals based on the tables provided in Regulatory Guide 1.99 Rev. 2.
#' The CF is determined by interpolating values from the provided table based on input copper (Cu) and nickel (Ni) content.
#'
#' @param Cu numeric vector, copper content in weight percent (wt%). Must be between 0 and 0.4.
#' @param Ni numeric vector, nickel content in weight percent (wt%). Must be between 0 and 1.2.
#' @return A numeric vector representing the chemistry factor (CF) in degrees Fahrenheit (°F).
#' @examples
#' # Example: Calculate CF for base metals with given Cu and Ni values
#' rpvetc:::calc_cf_base(0.1, 0.6)
#'
calc_cf_base <- function(Cu, Ni) { # not vectorized
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
  linear_interpolate_2d(Ni_values, Cu_values, cf_base, Ni, Cu)
}



#' Chemistry Factor (CF) Calculation for Weld Metal
#'
#' Computes the chemistry factor (CF) for weld metals based on the tables provided in Regulatory Guide 1.99 Rev. 2.
#' The CF is determined by interpolating values from the provided table based on input copper (Cu) and nickel (Ni) content.
#'
#' @param Cu numeric vector, copper content in weight percent (wt%). Must be between 0 and 0.4.
#' @param Ni numeric vector, nickel content in weight percent (wt%). Must be between 0 and 1.2.
#' @return A numeric vector representing the chemistry factor (CF) in degrees Fahrenheit (°F).
#' @examples
#' # Example: Calculate CF for weld metals with given Cu and Ni values
#' rpvetc:::calc_cf_weld(0.1, 0.6)
#'
calc_cf_weld <- function(Cu, Ni) { # not vectorized
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
  linear_interpolate_2d(Ni_values, Cu_values, cf_weld, Ni, Cu)
}



#' 2D Linear Interpolation
#'
#' Computes the interpolated value at a given \code{x} and \code{y} coordinate from a 2D table.
#' The table must have \code{x_values} along the columns and \code{y_values} along the rows.
#' This function is not vectorized and handles a single pair of \code{x} and \code{y} values.
#'
#' @param x_values numeric vector, values along the x-axis (columns of the table).
#' @param y_values numeric vector, values along the y-axis (rows of the table).
#' @param table numeric matrix, 2D table containing the data values. Rows correspond to \code{y_values}, columns to \code{x_values}.
#' @param x numeric, the x-coordinate at which to interpolate. Must be within the range of \code{x_values}.
#' @param y numeric, the y-coordinate at which to interpolate. Must be within the range of \code{y_values}.
#' @return Numeric value representing the interpolated result.
#' @examples
#' # Example: Interpolating from a 2D table
#' x_vals <- seq(0, 1, 0.5)
#' y_vals <- seq(0, 1, 0.5)
#' table <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
#' rpvetc:::linear_interpolate_2d(x_vals, y_vals, table, 0.25, 0.75)
#'
linear_interpolate_2d <- function(x_values, y_values, table, x, y) {
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

