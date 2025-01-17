#' ASTM E900-15e2
#'
#' Provide TTS or SD of ASTM E900-15e2.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param Mn numeric vector, wt%
#' @param P  numeric vector, wt%
#' @param temperature numeric vector, temperature_unit
#' @param fluence numeric vector, n/cm2
#' @param output character c("TTS", "TTS1", "TTS2", "SD")
#' @param temperature_unit character c("Celsius", "Fahrenheit")
#' @return TTS or SD as given temperature_unit
#' @export
#' @examples
#' E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18) # should be 31.74387
E900_15 <- function(product_form,
                    Cu,
                    Ni,
                    Mn,
                    P,
                    temperature,
                    fluence,
                    output = c("TTS", "SD", "TTS1", "TTS2"),
                    temperature_unit = c("Celsius", "Fahrenheit")) {
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)

  # 입력변수 검사
  product_form <- as.character(product_form)
  stopifnot(product_form %in% c("B", "F", "P", "W"))
  stopifnot(is.numeric(Cu), Cu >= 0, Cu <= 100)
  stopifnot(is.numeric(Ni), Ni >= 0, Ni <= 100)
  stopifnot(is.numeric(Mn), Mn >= 0, Mn <= 100)
  stopifnot(is.numeric(P), P >= 0, P <= 100)
  stopifnot(is.numeric(temperature))
  stopifnot(is.numeric(fluence), fluence >= 0)

  ## 벡터 크기 검사
  args <- list(product_form, Cu, Ni, Mn, P, temperature, fluence)
  arg_len <- sapply(args, length)
  max_len <- max(arg_len)
  stopifnot(all(arg_len == 1L | arg_len == max_len))

  # 입력변수 벡터크기를 동일화
  if (length(product_form) < max_len) {
    product_form <- rep(product_form, max_len)
  }
  if (length(Cu) < max_len) {
    Cu <- rep(Cu, max_len)
  }
  if (length(Ni) < max_len) {
    Ni <- rep(Ni, max_len)
  }
  if (length(Mn) < max_len) {
    Mn <- rep(Mn, max_len)
  }
  if (length(P) < max_len) {
    P <- rep(P, max_len)
  }
  if (length(temperature) < max_len) {
    temperature <- rep(temperature, max_len)
  }
  if (length(fluence) < max_len) {
    fluence <- rep(fluence, max_len)
  }

  # 기본온도를 섭씨로 변환
  if (temperature_unit == "Fahrenheit") {
    temperature <- (5 / 9) * (temperature - 32)
  }

  # TTS1 계산
  tts1 <- E900_15_TTS1(product_form, Ni, Mn, P, temperature, fluence)

  # TTS2 계산
  tts2 <- E900_15_TTS2(product_form, Cu, Ni, P, temperature, fluence)

  # TTS 계산
  tts <- tts1 + tts2

  # SD 계산
  sd <- E900_15_SD(product_form, tts)

  # 결과 선택
  result <- switch(output,
    TTS = tts,
    TTS1 = tts1,
    TTS2 = tts2,
    SD = sd,
  )

  # 온도 변환
  if (temperature_unit == "Fahrenheit") {
    result <- (9 / 5) * result
  }

  return(unname(result))
}



#' E900_15_TTS
#'
#' Calculate TTS of ASTM E900-15, vectorized.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param Mn numeric vector, wt%
#' @param P numeric vector, wt%
#' @param temperature numeric vector, temperature_unit
#' @param fluence numeric vector, n/cm2
#' @return TTS (degC)
E900_15_TTS <- function(product_form, Cu, Ni, Mn, P, temperature, fluence) {
  TTS1 <- E900_15_TTS1(product_form, Ni, Mn, P, temperature, fluence)
  TTS2 <- E900_15_TTS2(product_form, Cu, Ni, P, temperature, fluence)
  TTS <- TTS1 + TTS2
  return(unname(TTS))
}



#' E900_15_TTS1
#'
#' Calculate TTS1 of ASTM E900-15.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param Ni numeric vector, wt%
#' @param Mn numeric vector, wt%
#' @param P numeric vector, wt%
#' @param temperature numeric vector, temperature_unit
#' @param fluence numeric vector, n/cm2
#' @return TTS1 (degC)
E900_15_TTS1 <- function(product_form, Ni, Mn, P, temperature, fluence) {
  A_fpw <- c("F" = 1.011, "P" = 1.080, "W" = 0.919)[product_form]
  TTS1 <- A_fpw * 3.593E-10 * (fluence)^0.5695 # 3.593e10 은 cm2으로 미리 계산 from plotter22 macro
  TTS1 <- TTS1 * ((1.8 * temperature + 32) / 550)^-5.47
  TTS1 <- TTS1 * (0.09 + P / 0.012)^0.216
  TTS1 <- TTS1 * (1.66 + (Ni^8.54) / 0.63)^0.39
  TTS1 <- TTS1 * (Mn / 1.36)^0.3
  TTS1 <- TTS1 * (5 / 9)
  return(unname(TTS1))
}



#' E900_15_TTS2
#'
#' Calculate TTS2 of ASTM E900-15.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param P numeric vector, wt%
#' @param temperature numeric vector, temperature_unit
#' @param fluence numeric vector, n/cm2
#' @return TTS2 (degC)
E900_15_TTS2 <- function(product_form, Cu, Ni, P, temperature, fluence) {
  B_fpw <- c("F" = 0.738, "P" = 0.819, "W" = 0.968)[product_form]
  M <- B_fpw * pmax(pmin(113.87 * (log(fluence * 1e4) - log(4.5e20)), 612.6), 0)
  M <- M * ((1.8 * temperature + 32) / 550)^-5.45
  M <- M * (0.1 + P / 0.012)^-0.098
  M <- M * (0.168 + (Ni^0.58) / 0.63)^0.73
  TTS2 <- pmax(pmin(Cu, 0.28) - 0.053, 0) * M
  TTS2 <- TTS2 * (5 / 9)
  return(unname(TTS2))
}



#' E900_15_SD
#'
#' Calculate SD of ASTM E900-15, vectorized.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param TTS numeric vector, degC
#' @return SD of TTS (degC)
E900_15_SD <- function(product_form, TTS) {
  C_fpw <- c("F" = 6.972, "P" = 6.593, "W" = 7.681)[product_form]
  D_fpw <- c("F" = 0.199, "P" = 0.163, "W" = 0.181)[product_form]
  SD <- C_fpw * TTS^D_fpw
  unname(SD)
}
