#' RG199R2
#'
#' Provide TTS or CF of Regulatory Guide 1.99 Rev. 2
#'
#' @param product_form character vector, c("B", F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param fluence numeric vector, n/cm2
#' @param tts numeric vector, TTS from surveillance tests
#' @param output string c("TTS", "CF", "sigma)
#' @param temperature_unit string c("Celcius", "Fahrenheit")
#' @return TTS or CF as given condition
#' @export
#' @examples
#' RG199R2("B", 0.2, 0.18, 2.56894e18) # should be 31.74387
RG199R2 <- function(
    product_form = NULL,
    Cu = NULL,
    Ni = NULL,
    fluence = NULL,
    tts = NULL,
    output = c("TTS", "CF", "sigma"),
    temperature_unit = c("Celcius", "Fahrenheit")) {
  ## 매개변수 기본값 처리
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)

  ## 입력 유효성 검사
  if (is.null(tts)) {
    stopifnot(is.character(product_form) | is.factor(product_form))
    stopifnot(is.numeric(Cu) & Cu >= 0)
    stopifnot(is.numeric(Ni) & Ni >= 0)
  } else {
    stopifnot(is.numeric(tts))
  }

  if (is.null(fluence)) {
    fluence <- 1e19
  } else {
    stopifnot(is.numeric(fluence) & fluence >= 0)
  }

  # tts가 있을 경우 fluence와 길이가 같아야 한다.
  if (!is.null(fluence) && !is.null(tts)) {
    stopifnot(length(fluence) == length(tts))
  }

  fl <- fluence / 1e19
  ff <- fl^(0.28 - 0.1 * log10(fl))

  ## Chemistry factor 계산
  if (is.null(tts)) {
    # Chemistry Factor 계산
    product_form <- as.character(product_form)
    product_form[product_form == "F"] <- "B"
    product_form[product_form == "P"] <- "B"
    CF <- mapply(CF_from_table, product_form, Cu, Ni)
  } else {
    # TTS로부터 CF 계산
    ff2 <- ff^2
    CF <- sum(ff * tts) / sum(ff2)
  }

  ## TTS 계산
  TTS <- CF * ff  # degF

  ## sigma 계산
  if (is.null(tts)) {
    sigma <- c(B = 17, W = 34)[product_form] # degF
  } else {
    sigma <- c(B = 17, W = 34)[product_form] # degF
  }

  ## 결과 선택
  result <- switch(output,
    TTS = TTS,
    CF = CF,
    sigma = sigma
  )

  ## 온도 단위 변환
  if (temperature_unit == "Celcius") {
    result <- result * (5 / 9)
  }

  unname(result)
}

linear_interpolate_2d <- function(x_values, y_values, table, x, y) {
  # x_values: x축 값들
  # y_values: y축 값들
  # table: 2D 테이블 (행: y축, 열: x축)
  # x, y: 보간하려는 지점

  # x와 y의 좌표가 범위를 벗어날 경우 경계값으로 클램핑
  x <- max(min(x, max(x_values)), min(x_values))
  y <- max(min(y, max(y_values)), min(y_values))

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

CF_from_table <- function(product_form, Cu, Ni) {
  # pf <- match.arg(product_form)
  CF_base <- matrix(
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
  CF_weld <- matrix(c(
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
  if (product_form == "W") {
    table <- CF_weld
  } else {
    table <- CF_base
  }
  linear_interpolate_2d(Ni_values, Cu_values, table, Ni, Cu)
}
