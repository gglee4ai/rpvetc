#' RG199R2
#'
#' Provide TTS or CF of Regulatory Guide 1.99 Rev. 2 tables
#'
#' @param product_form character vector, c("B", F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param fluence numeric vector, n/cm2
#' @param SV_flu numeric vector, n/cm2
#' @param SV_tts numeric vector, temperature unit
#' @param CF numeric vector, temperature unit
#' @param output character c("TTS", "CF", "FF", "SD")
#' @param temperature_unit character c("Celsius", "Fahrenheit")
#' @param verbose logical TRUE or FALSE
#' @return various value according to output
#' @export
#' @examples
#' RG199R2("B", 0.2, 0.18, 2.56894e18) # should be 31.74387
RG199R2 <- function(
    product_form = NULL,
    Cu = NULL,
    Ni = NULL,
    fluence = NULL,
    SV_flu = NULL,
    SV_tts = NULL,
    CF = NULL,
    output = c("TTS", "CF", "FF", "SD"),
    temperature_unit = c("Celsius", "Fahrenheit"),
    verbose = FALSE) {
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)

  # SV_flu가 있으면, fluence를 대체
  if (is.null(fluence) && !is.null(SV_flu)) {
    fluence <- SV_flu
  }

  # 기본 온도를 Fahrenheit로 변경
  if (temperature_unit == "Celsius") {
    if (is.numeric(SV_tts)) { # numeric * NULL -> numeric이 되어 버리는 것 방지
      SV_tts <- (9 / 5) * SV_tts
    }
    if (is.numeric(CF)) { # numeric * NULL -> numeric이 되어 버리는 것 방지
      CF <- (9 / 5) * CF
    }
  }

  # 결과 선택
  result <- switch(output,
    "CF" = RG199R2_CF(product_form, Cu, Ni, SV_flu, SV_tts, CF),
    "FF" = RG199R2_FF(fluence),
    "TTS" = {
      cf <- RG199R2_CF(product_form, Cu, Ni, SV_flu, SV_tts, CF)
      RG199R2_TTS(cf, fluence)
    },
    "SD" = {
      cf <- RG199R2_CF(product_form, Cu, Ni, SV_flu, SV_tts, CF)
      ff <- RG199R2_TTS(fluence)
      tts <- cf * ff
      RG199R2_SD()
    }
  )

  # 최종값 변환
  if (output %in% c("TTS", "CF", "SD") && temperature_unit == "Celsius") {
    result <- (5 / 9) * result
  }

  return(unname(result))
}



#' RG199R2_FF
#'
#' Provide fluence factor
#'
#' @param fluence numeric vector, n/cm2
#' @return FF
RG199R2_FF <- function(fluence) {
  stopifnot(is.numeric(fluence))
  stopifnot(all(fluence >= 0))

  fl <- fluence / 1e19
  ff <- fl^(0.28 - 0.1 * log10(fl))
  return(ff)
}



#' RG199R2_TTS
#'
#' Provide TTS from CF and fluence
#'
#' @param CF numeric vector, degF
#' @param fluence numeric vector, n/cm2
#' @return output TTS as degF
RG199R2_TTS <- function(CF, fluence) {
  stopifnot(is.numeric(CF))
  stopifnot(all(CF >= 0))

  # 벡터 길이 체크
  arg_len <- c(length(CF), length(fluence))
  max_len <- max(arg_len)
  if (!all(arg_len == 1L | arg_len == max_len)) {
    stop("The lengths of calculated CF and fluence must be 1 or the same.")
  }

  ff <- RG199R2_FF(fluence)
  tts <- CF * ff
  return(tts)
}



#' RG199R2_SD
#'
#' Provide SD
#'
#' @return SD as degF
RG199R2_SD <- function() {
  4
}



#' RG199R2_CF
#'
#' Provide chemistry factor
#'
#' @param product_form character vector, c("B", F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param SV_flu numeric vector, n/cm2
#' @param SV_tts numeric vector, degF
#' @param CF numeric vector, degF
#' @return CF as degF
RG199R2_CF <- function(
    product_form = NULL, Cu = NULL, Ni = NULL,
    SV_flu = NULL, SV_tts = NULL,
    CF = NULL) {
  ## Case 1: CF가 주어졌을 때
  if (!is.null(CF)) {
    stopifnot(is.numeric(CF))
    stopifnot(all(CF >= 0))
    return(CF)
  }

  ## Case 2: 2차 이상의 감시시험 결과가 있을 경우
  if (!is.null(SV_flu) && !is.null(SV_tts)) {
    stopifnot(is.numeric(SV_flu))
    stopifnot(is.numeric(SV_tts))
    stopifnot(length(SV_flu) == length(SV_tts))
    stopifnot(SV_tts[SV_flu == 0] == 0)

    if (sum(SV_flu > 0) >= 2) {
      cf <- RG199R2_CF_by_SV(SV_flu, SV_tts)
      return(cf)
    }
  }

  ## Case 3: Table로 부터 직접 CF 계산
  product_form <- as.character(product_form)
  stopifnot(all(product_form %in% c("B", "F", "P", "W")))
  stopifnot(is.numeric(Cu))
  stopifnot(is.numeric(Ni))

  arg_len <- c(length(product_form), length(Cu), length(Ni))
  max_len <- max(arg_len)
  if (!all(arg_len == 1L | arg_len == max_len)) {
    stop("The lengths of product_form, Cu, and Ni must be 1 or the same.")
  }

  cf <- RG199R2_CF_table(product_form, Cu, Ni)
  return(cf)
}

#' RG199R2_CF_by_SV
#'
#' Provide CF calculated using TTS of the surveillance test results
#'
#' @param SV_flu numeric vector, n/cm2
#' @param SV_tts numeric vector, degF
#' @return CF as degF
RG199R2_CF_by_SV <- function(SV_flu, SV_tts) {
  fl <- SV_flu / 1e19
  ff <- fl^(0.28 - 0.1 * log10(fl))
  ff2 <- ff^2
  cf <- sum(ff * SV_tts) / sum(ff2)
  return(cf)
}



#' RG199R2_CF_table
#'
#' Provide CF from the tables of Regulatory Guide 1.99 Rev. 2.
#'
#' @param product_form character vector, c("B", "F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @return CF as degF
RG199R2_CF_table <- function(product_form, Cu, Ni) {
  result <- mapply(function(pf, cu, ni) {
    if (pf %in% c("B", "F", "P")) {
      RG199R2_CF_base(cu, ni)
    } else if (pf %in% c("W")) {
      RG199R2_CF_weld(cu, ni)
    }
  }, product_form, Cu, Ni, SIMPLIFY = TRUE)

  return(result)
}



#' RG199R2_CF_base
#'
#' Provide CF of base from the tables of Regulatory Guide 1.99 Rev. 2.
#'
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @return CF as degF
RG199R2_CF_base <- function(Cu, Ni) { # not vectorized
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
  cf <- linear_interpolate_2d(Ni_values, Cu_values, cf_base, Ni, Cu)
  return(unname(cf))
}



#' RG199R2_CF_weld
#'
#' Provide CF of weld from the tables of Regulatory Guide 1.99 Rev. 2.
#'
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @return CF as degF
RG199R2_CF_weld <- function(Cu, Ni) { # not vectorized
  cf_weld <- matrix(c(
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
  ), nrow = 41)
  Ni_values <- seq(0, 1.2, 0.2)
  Cu_values <- seq(0, 0.4, 0.01)

  # CF 계산
  cf <- linear_interpolate_2d(Ni_values, Cu_values, cf_weld, Ni, Cu)
  return(unname(cf))
}



#' linear_interpolate_2d
#'
#' Provide interpolated value from 2D table (not vectorize).
#'
#' @param x_values, numeric vector, x축 값들
#' @param y_values, numeric vector, y축 값들
#' @param table, numeric matrix, 2D 테이블 (행: y축, 열: x축)
#' @param x, numeric, 보간하려는 x 지점
#' @param y, numeric, 보간하려는 y 지점
#' @return interpoated value
linear_interpolate_2d <- function(x_values, y_values, table, x, y) {
  # x, y가 범위를 벗어나는지 체크해서 에러 발생
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

  # 또는 x와 y의 좌표가 범위를 벗어날 경우 경계값으로 클램핑
  # x <- max(min(x, max(x_values)), min(x_values))
  # y <- max(min(y, max(y_values)), min(y_values))

  # x와 y의 인덱스를 찾기
  x1_idx <- max(which(x_values <= x))
  x2_idx <- min(which(x_values >= x))
  y1_idx <- max(which(y_values <= y))
  y2_idx <- min(which(y_values >= y))

  # x와 y의 인접 값 가져오기
  x1 <- x_values[x1_idx]
  x2 <- x_values[x2_idx]
  y1 <- y_values[y1_idx]
  y2 <- y_values[y2_idx]

  # 테이블에서 코너 값 가져오기
  Q11 <- table[y1_idx, x1_idx]
  Q12 <- table[y2_idx, x1_idx]
  Q21 <- table[y1_idx, x2_idx]
  Q22 <- table[y2_idx, x2_idx]

  # 선형 보간 계산
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

  return(interpolated_value)
}
