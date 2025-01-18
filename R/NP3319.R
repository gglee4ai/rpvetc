#' NP3319
#'
#' Provide TTS, CF, FF, or SD of Odette's model in EPRI NP-3319 (1984).
#'
#' @param product_form character vector c("B","F","P","W")
#' @param Cu numeric vector, wt% (0~100). Not required if only \code{output="FF"}.
#' @param Ni numeric vector, wt% (0~100). Not required if only \code{output="FF"}.
#' @param fluence numeric vector (n/cm2). Required unless you set \code{output="CF"} only.
#' @param output character c("TTS", "CF", "FF", "SD")
#' @param temperature_unit character c("Celsius", "Fahrenheit")
#'
#' @return Numeric vector of CF, FF, or TTS in the specified unit.
#' @export
#'
#' @examples
#' # 1) TTS 계산 (모든 인자 필요)
#' NP3319(product_form = "B", Cu = 0.1, Ni = 0.6, fluence = 1e19)
#'
#' # 2) FF만 계산 (Cu, Ni 불필요)
#' NP3319(product_form = "F", fluence = 1e19, output = "FF")
#'
NP3319 <- function(product_form = NULL,
                   Cu = NULL,
                   Ni = NULL,
                   fluence = NULL,
                   output = c("TTS", "CF", "FF", "SD"),
                   temperature_unit = c("Celsius", "Fahrenheit")) {
  #-------------------------------#
  # 0) 기본 설정, 온도 기준 degF
  #-------------------------------#
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)

  # 에러 함수(erf) 정의
  erf <- function(x) {
    2 * stats::pnorm(x * sqrt(2)) - 1
  }

  #--------------------------#
  # 1) product_form 전처리
  #--------------------------#
  #  - "B","F","P","W" 중 하나이며, F/P는 B로 통합
  if (!is.null(product_form)) {
    stopifnot(all(product_form %in% c("B", "F", "P", "W")))
    # F, P -> B
    product_form[product_form %in% c("F", "P")] <- "B"
  }

  #--------------------------#
  # 2) 입력값 길이 검사
  #--------------------------#
  #  - 길이가 0, 1, 또는 max_len인지 확인
  arg_list <- list(product_form, Cu, Ni, fluence)
  arg_len <- sapply(arg_list, length)
  max_len <- max(arg_len)

  # 길이가 0(NULL), 1, max_len 세 가지 경우만 허용
  stopifnot(all(arg_len %in% c(0, 1, max_len)))

  # 벡터 길이 확장
  replicate_to_max <- function(x, max_len) {
    if (is.null(x)) {
      return(x)
    }
    if (length(x) == 1 && max_len > 1) rep(x, max_len) else x
  }
  product_form <- replicate_to_max(product_form, max_len)
  Cu <- replicate_to_max(Cu, max_len)
  Ni <- replicate_to_max(Ni, max_len)
  fluence <- replicate_to_max(fluence, max_len)

  #-------------------------------#
  # 3) 주요 계산 함수들(중복된 인자 검사는 제외)
  #-------------------------------#
  # (1) CF 계산
  calc_cf <- function(product_form, Cu, Ni) {
    stopifnot(!is.null(Cu), is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
    stopifnot(!is.null(Ni), is.numeric(Ni), all(Ni >= 0 & Ni <= 100))

    base_cf <- Cu * 216 * (1 + 0.33 * (erf(0.77 * Ni / Cu - 1) + 1))
    weld_cf <- Cu * 200 * (1 + 1.38 * (erf(0.30 * Ni / Cu - 1) + 1))

    ifelse(product_form == "B", base_cf,
      ifelse(product_form == "W", weld_cf, NA_real_)
    )
  }

  # (2) FF 계산
  calc_ff <- function(product_form, fluence) {
    stopifnot(is.numeric(fluence), all(fluence >= 0))

    fl <- fluence / 1e19
    base_ff <- fl^0.28
    weld_ff <- (1 - exp(-fl / 0.11))^1.36 * fl^0.18

    ifelse(product_form == "B", base_ff,
      ifelse(product_form == "W", weld_ff, NA_real_)
    )
  }

  # (3) TTS 계산
  calc_tts <- function(product_form, Cu, Ni, fluence) {
    # 각 인수 검증은 함수 호출에서 수행함
    cf <- calc_cf(product_form, Cu, Ni)
    ff <- calc_ff(product_form, fluence)
    cf * ff
  }

  # (4) SD 계산
  calc_sd <- function(product_form) {
    1
  }


  #-------------------------------#
  # 4) switch(output)에 따라 계산
  #-------------------------------#
  result <- switch(output,
    "CF" = calc_cf(product_form, Cu, Ni),
    "FF" = calc_ff(product_form, fluence),
    "TTS" = calc_tts(product_form, Cu, Ni, fluence),
    "SD" = calc_sd(product_form)
  )

  #------------------------------------#
  # 5) 결과 온도 변환
  #    - 만약 최종 출력을 °C로 원하면
  #    - FF 제외
  #------------------------------------#
  if (temperature_unit == "Celsius" && output %in% c("TTS", "CF", "SD")) {
    result <- result * (5 / 9)
  }

  #-------------------------------#
  # 6) 반환
  #-------------------------------#
  return(unname(result))
}
