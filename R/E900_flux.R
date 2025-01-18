#' E900_flux
#'
#' Provide TTS (TTS1+TTS2) or SD of ASTM E900-15e2 upgraded in MRP-462 (2021).
#'
#' @param product_form character vector c("F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param Mn numeric vector, wt%
#' @param P  numeric vector, wt%
#' @param temperature numeric vector (°C or °F)
#' @param fluence numeric vector, n/cm2
#' @param flux numeric vector, n/cm2/s
#' @param output character c("TTS", "TTS1", "TTS2", "SD")
#' @param temperature_unit character c("Celsius", "Fahrenheit")
#'
#' @return numeric vector (°C or °F) of TTS / TTS1 / TTS2 / SD
#' @export
#'
#' @examples
#' E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, 1e13) # should be 31.81079
#'
E900_flux <- function(product_form,
                      Cu,
                      Ni,
                      Mn,
                      P,
                      temperature,
                      fluence,
                      flux,
                      output = c("TTS", "SD", "TTS1", "TTS2"),
                      temperature_unit = c("Celsius", "Fahrenheit")) {
  #------------------------------------#
  # 0) 기본 설정
  #------------------------------------#
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)

  #------------------------------------#
  # 1) 입력값 검증
  #------------------------------------#
  product_form <- as.character(product_form)
  stopifnot(product_form %in% c("F", "P", "W"))
  stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
  stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))
  stopifnot(is.numeric(Mn), all(Mn >= 0 & Mn <= 100))
  stopifnot(is.numeric(P), all(P >= 0 & P <= 100))
  stopifnot(is.numeric(temperature))
  stopifnot(is.numeric(fluence), all(fluence >= 0))
  stopifnot(is.numeric(flux), all(flux > 0))

  #------------------------------------#
  # 2) 온도 입력을 "섭씨"로 통일
  #------------------------------------#
  if (temperature_unit == "Fahrenheit") {
    # °F -> °C
    temperature <- (5 / 9) * (temperature - 32)
  }

  #------------------------------------#
  # 2) 입력값 길이 검사
  #------------------------------------#
  arg_list <- list(product_form, Cu, Ni, Mn, P, temperature, fluence, flux)
  arg_len <- sapply(arg_list, length)
  max_len <- max(arg_len)

  # 길이가 1, max_len 두 가지 경우만 허용
  stopifnot(all(arg_len %in% c(1L, max_len)))

  # 벡터 길이 확장
  replicate_to_max <- function(x, max_len) {
    if (length(x) < max_len) rep(x, length.out = max_len) else x
  }
  product_form <- replicate_to_max(product_form, max_len)
  Cu <- replicate_to_max(Cu, max_len)
  Ni <- replicate_to_max(Ni, max_len)
  Mn <- replicate_to_max(Mn, max_len)
  P <- replicate_to_max(P, max_len)
  temperature <- replicate_to_max(temperature, max_len)
  fluence <- replicate_to_max(fluence, max_len)
  flux <- replicate_to_max(flux, max_len)

  #------------------------------------#
  # 4) 주요 계산 함수들
  #    - 모두 "temperature"를 섭씨로 취급
  #------------------------------------#

  # TTS1
  calc_tts1 <- function(product_form, Ni, Mn, P, temperature, fluence, flux) {
    log10flux <- log10(flux)
    A <- c("F" = 0.785, "P" = 0.774, "W" = 0.798)[product_form]
    out <- A * 4.074E-10 * (fluence)^0.5724 # 4.074E-10 은 cm2으로 미리 계산 from plotter22 macro
    out <- out * ((1.8 * temperature + 32) / 550)^-4.10
    out <- out * (1.62 + P / 0.012)^0.461
    out <- out * (0.29 + (Ni^17.63) / 0.63)^0.107
    out <- out * (Mn / 1.36)^0.44
    out <- out * (log10flux / 11.8)^1.64
    out * (5 / 9) # 최종 결과는 degC
  }

  # TTS2
  calc_tts2 <- function(product_form, Cu, Ni, P, temperature, fluence, flux) {
    log10flux <- log10(flux)
    B <- c("F" = 0.519, "P" = 0.592, "W" = 0.595)[product_form]
    M <- B * pmax(pmin(73.67 * (log(fluence) - log(3.6e16)), 604.0), 0)
    M <- M * ((1.8 * temperature + 32) / 550)^-3.27
    M <- M * (0.01 + P / 0.012)^-0.026
    M <- M * (0.705 + (Ni^0.64) / 0.63)^1.24
    M <- M * (log10flux / 15.9)^-0.52
    out <- pmax(pmin(Cu, 0.30) - 0.046, 0) * M
    out * (5 / 9) # 최종 결과는 degC
  }

  # TTS = TTS1 + TTS2
  calc_tts <- function(product_form, Cu, Ni, Mn, P, temperature, fluence, flux) {
    tts1 <- calc_tts1(product_form, Ni, Mn, P, temperature, fluence, flux)
    tts2 <- calc_tts2(product_form, Cu, Ni, P, temperature, fluence, flux)
    tts1 + tts2 # 최종 결과는 degC
    # 최종 결과는 degC
  }

  # SD 계산
  calc_sd <- function(product_form, TTS) {
    C <- c("F" = 6.818, "P" = 6.293, "W" = 6.862)[product_form]
    D <- c("F" = 0.238, "P" = 0.202, "W" = 0.237)[product_form]
    C * TTS^D # 최종 결과는 degC
  }

  #------------------------------------#
  # 5) switch(output)에 따라 계산
  #------------------------------------#
  result <- switch(output,
    "TTS1" = calc_tts1(product_form, Ni, Mn, P, temperature, fluence, flux),
    "TTS2" = calc_tts2(product_form, Cu, Ni, P, temperature, fluence, flux),
    "TTS" = calc_tts(product_form, Cu, Ni, Mn, P, temperature, fluence, flux),
    "SD" = {
      tts <- calc_tts(product_form, Cu, Ni, Mn, P, temperature, fluence, flux)
      calc_sd(product_form, tts)
    }
  )

  #------------------------------------#
  # 6) 결과 온도 변환
  #    - 만약 최종 출력을 °F로 원하면
  #      TTS/SD 결과(= °C)를 (×9/5)
  #------------------------------------#
  if (temperature_unit == "Fahrenheit") {
    result <- result * (9 / 5)
  }

  #------------------------------------#
  # 7) 반환
  #------------------------------------#
  return(unname(result))
}
