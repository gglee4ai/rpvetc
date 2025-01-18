#' CR3391
#'
#' Calculate TTS using NUREG/CR3391 vol.2 report by Guthrie (1983).
#'
#' @param product_form character vector. One of c("B", "F", "P", "W").
#' @param Cu numeric vector, wt% (0~100)
#' @param Ni numeric vector, wt% (0~100)
#' @param fluence numeric vector n/cm2 (0 이상)
#' @param output character c("TTS", "CF", "FF", "SD")
#' @param temperature_unit character c("Celsius", "Fahrenheit")
#'
#' @return A numeric vector corresponding to \code{output} in the specified \code{temperature_unit}.
#' @export
#'
#' @examples
#' CR3391("B", 0.1, 0.6, 1e19, output = "TTS")
CR3391 <- function(product_form = NULL,
                   Cu = NULL,
                   Ni = NULL,
                   fluence = NULL,
                   output = c("TTS", "CF", "FF", "SD"),
                   temperature_unit = c("Celsius", "Fahrenheit")) {
  # 1) match.arg
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)

  #--------------------------#
  # 2) 입력값 길이 검사
  #--------------------------#
  #  - 길이가 0, 1, 또는 max_len인지 확인
  args <- list(product_form, Cu, Ni, fluence)
  arg_len <- sapply(args, length)
  max_len <- max(arg_len)

  # 길이가 0, 1, max_len 세 가지 경우만 허용
  stopifnot(all(arg_len %in% c(0, 1, max_len)))

  #--------------------------#
  # 3) product_form 전처리
  #--------------------------#
  #  - "B","F","P","W" 중 하나이며, F/P는 B로 통합
  if (length(product_form) > 0) {
    stopifnot(all(product_form %in% c("B", "F", "P", "W")))
    # F, P → B
    product_form[product_form %in% c("F", "P")] <- "B"
  }

  #--------------------------#
  # 4) 길이 확장 (rep) 함수
  #--------------------------#
  replicate_to_max <- function(x, max_len) {
    if (length(x) == 0) {
      return(rep(NA_real_, max_len))
    }
    if (length(x) == 1 && max_len > 1) {
      return(rep(x, max_len))
    }
    x
  }

  # 한 번에 확장해서 변수 재할당
  product_form <- replicate_to_max(product_form, max_len)
  Cu <- replicate_to_max(Cu, max_len)
  Ni <- replicate_to_max(Ni, max_len)
  fluence <- replicate_to_max(fluence, max_len)

  #--------------------------#
  # 5) 보조 계산 함수 정의
  #--------------------------#
  # (1) CF 계산
  calc_cf <- function(form, cu, ni) {
    stopifnot(is.numeric(cu), all(cu >= 0 & cu <= 100))
    stopifnot(is.numeric(ni), all(ni >= 0 & ni <= 100))

    ifelse(
      form == "B", # Base
      -38.39 + 555.6 * cu + 480.1 * cu * tanh(0.353 * ni / cu),
      ifelse(
        form == "W", # Weld
        624 * cu - 333.1 * sqrt(cu * ni) + 251.2 * ni,
        NA_real_
      )
    )
  }

  # (2) FF 계산
  calc_ff <- function(flu) {
    stopifnot(is.numeric(flu), all(flu >= 0))
    fl <- flu / 1e19
    fl^(0.2661 - 0.0449 * log(fl))
  }

  # (3) SD 계산
  calc_sd <- function(form) {
    # Base(B)=17.2, Weld(W)=28.2
    # form이 "B"나 "W" 중 하나라고 가정, 그 외 NA
    # 이름을 사용하므로 [form] 접근 가능
    # form이 "B" 또는 "W"가 아니라면 NA를 반환해야 할 수도 있음
    out <- c(B = 17.2, W = 28.2)[form]
    ifelse(is.na(out), NA_real_, out) # B나 W가 아니면 NA
  }

  #--------------------------#
  # 6) switch에 따라 결과 계산
  #--------------------------#
  result <- switch(output,
    "CF" = {
      calc_cf(product_form, Cu, Ni)
    },
    "FF" = {
      calc_ff(fluence)
    },
    "TTS" = {
      cf <- calc_cf(product_form, Cu, Ni)
      ff <- calc_ff(fluence)
      cf * ff
    },
    "SD" = {
      calc_sd(product_form)
    }
  )

  #--------------------------#
  # 7) 온도 변환
  #    - TTS/CF/SD만 온도 차이이므로 변환
  #--------------------------#
  if (temperature_unit == "Celsius" && output %in% c("TTS", "CF", "SD")) {
    # 화씨 -> 섭씨: 온도 차이이므로 × 5/9
    result <- result * (5 / 9)
  }

  #--------------------------#
  # 8) 반환
  #--------------------------#
  return(unname(result))
}
