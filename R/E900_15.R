#' E900_15
#'
#' Provide TTS (TTS1+TTS2) or SD of ASTM E900-15e2.
#'
#' @param product_form character vector c("F","P","W").
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param Mn numeric vector, wt%
#' @param P  numeric vector, wt%
#' @param temperature numeric vector (°C or °F)
#' @param fluence numeric vector, n/cm2
#' @param output character c("TTS", "TTS1", "TTS2", "SD")
#' @param temperature_unit character c("Celsius", "Fahrenheit")
#'
#' @return numeric vector (°C or °F) of TTS / TTS1 / TTS2 / SD
#' @export
#'
#' @examples
#' E900_15("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18) # should be 31.74387
#'
E900_15 <- function(product_form,
                    Cu,
                    Ni,
                    Mn,
                    P,
                    temperature,
                    fluence,
                    output = c("TTS", "TTS1", "TTS2", "SD"),
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

  #------------------------------------#
  # 2) 온도 입력을 "섭씨"로 통일
  #
  #    - 원코드 로직상, TTS1/TTS2 계산에서
  #      temperature를 "화씨"로 가정하여 식을 썼으므로,
  #      만약 사용자가 섭씨로 넣었다면 => 변환해서 화씨로 쓸 수 있음.
  #    - 그러나 코드 내부는 "((1.8*temp +32)/550)" 등을 계산했는데,
  #      이미 temp가 섭씨라고 가정하는 로직...
  #
  #    => 여기서는 "최초에 temperature_unit" 보고
  #       'Celsius'면 그대로, 'Fahrenheit'면 °F→°C 변환.
  #    => TTS1, TTS2 안에서는 "temperature"가 항상 섭씨라고 가정 후,
  #       공식 내에서 (1.8 * temp + 32) 해줌.
  #
  #    (원 코드에서는 temperature_unit=="Fahrenheit"면
  #     temperature<-(5/9)*(temperature-32) 했지만,
  #     TTS1, TTS2 내부에 또 (1.8*temperature+32)가 있어서
  #     사실상 '두 번' 변환되는 꼴이었음)
  #
  #    => 여기서는 "입력단계"에서 명시적으로 °F→°C 변환
  #       → TTS1, TTS2 계산식은 temp를 섭씨라고 가정하고
  #         (1.8 * temp + 32)/550 등의 식을 적용.
  #------------------------------------#
  if (temperature_unit == "Fahrenheit") {
    # °F -> °C
    temperature <- (5 / 9) * (temperature - 32)
  }

  #------------------------------------#
  # 2) 입력값 길이 검사
  #------------------------------------#
  arg_list <- list(product_form, Cu, Ni, Mn, P, temperature, fluence)
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

  #------------------------------------#
  # 4) 주요 계산 함수들
  #    - 모두 "temperature"를 섭씨로 취급
  #------------------------------------#

  # TTS1
  calc_tts1 <- function(product_form, Ni, Mn, P, temperature, fluence) {
    A <- c("F" = 1.011, "P" = 1.080, "W" = 0.919, "B" = 1.0)[product_form]
    out <- A * 3.593e-10 * (fluence^0.5695)
    out <- out * (((1.8 * temperature + 32) / 550)^(-5.47))
    out <- out * ((0.09 + P / 0.012)^0.216)
    out <- out * ((1.66 + (Ni^8.54) / 0.63)^0.39)
    out <- out * ((Mn / 1.36)^0.3)
    out * (5 / 9) # 최종 결과는 degC
  }

  # TTS2
  calc_tts2 <- function(product_form, Cu, Ni, P, temperature, fluence) {
    B <- c("F" = 0.738, "P" = 0.819, "W" = 0.968, "B" = 1.0)[product_form]
    M <- B * pmax(pmin(113.87 * (log(fluence) - log(4.5e16)), 612.6), 0)
    M <- M * (((1.8 * temperature + 32) / 550)^(-5.45))
    M <- M * ((0.1 + P / 0.012)^(-0.098))
    M <- M * ((0.168 + (Ni^0.58) / 0.63)^0.73)
    out <- pmax(pmin(Cu, 0.28) - 0.053, 0) * M
    out * (5 / 9) # 최종 결과는 degC
  }

  # TTS = TTS1 + TTS2
  calc_tts <- function(product_form, Cu, Ni, Mn, P, temperature, fluence) {
    tts1 <- calc_tts1(product_form, Ni, Mn, P, temperature, fluence)
    tts2 <- calc_tts2(product_form, Cu, Ni, P, temperature, fluence)
    tts1 + tts2 # 최종 결과는 degC
  }

  # SD
  calc_sd <- function(product_form, TTS) {
    C <- c("F" = 6.972, "P" = 6.593, "W" = 7.681)[product_form]
    D <- c("F" = 0.199, "P" = 0.163, "W" = 0.181)[product_form]
    C * (TTS^D) # 최종 결과는 degC
  }

  #------------------------------------#
  # 5) switch(output)에 따라 계산
  #------------------------------------#
  result <- switch(output,
    "TTS1" = calc_tts1(product_form, Ni, Mn, P, temperature, fluence),
    "TTS2" = calc_tts2(product_form, Cu, Ni, P, temperature, fluence),
    "TTS" = calc_tts(product_form, Cu, Ni, Mn, P, temperature, fluence),
    "SD" = {
      tts <- calc_tts(product_form, Cu, Ni, Mn, P, temperature, fluence)
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
