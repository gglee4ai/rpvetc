#' Guthrie's Model (NUREG/CR-3391) Calculations
#'
#' Computes Temperature Transition Shift (TTS), Chemistry Factor (CF), Fluence Factor (FF), or Standard Deviation (SD)
#' based on Guthrie's embrittlement model described in NUREG/CR-3391 (1983).
#'
#' This function provides calculations for base and weld metals using different empirical formulas
#' depending on material properties such as copper (Cu) and nickel (Ni) content, and neutron fluence.
#'
#' @param product_form character vector, specifying the product form. Must be one of:
#'        \itemize{
#'          \item \code{"B"} - Base metal
#'          \item \code{"F"} - Forgings (treated as \code{"B"})
#'          \item \code{"P"} - Plate (treated as \code{"B"})
#'          \item \code{"W"} - Weld metal
#'        }
#' @param Cu numeric vector, copper content in weight percent (wt%). Must be between 0 and 100.
#' @param Ni numeric vector, nickel content in weight percent (wt%). Must be between 0 and 100.
#' @param fluence numeric vector, neutron fluence in n/cm². Must be non-negative.
#' @param output character, specifying which property to compute. Must be one of:
#'        \itemize{
#'          \item \code{"TTS"} - Temperature Transition Shift
#'          \item \code{"CF"} - Chemistry Factor
#'          \item \code{"FF"} - Fluence Factor
#'          \item \code{"SD"} - Standard Deviation of the TTS estimation
#'        }
#' @param temperature_unit character, specifying the output temperature unit. Must be one of:
#'        \itemize{
#'          \item \code{"Celsius"} - Returns the result in degrees Celsius.
#'          \item \code{"Fahrenheit"} - Returns the result in degrees Fahrenheit.
#'        }
#'
#' @return A numeric vector containing the computed result based on the selected \code{output} parameter.
#'         The unit depends on \code{temperature_unit}, except for Fluence Factor (\code{"FF"}), which is unitless.
#'
#' @examples
#' # Example 1: Compute TTS for a base metal
#' CR3391(product_form = "B", Cu = 0.1, Ni = 0.6, fluence = 1e19, output = "TTS")
#'
#' # Example 2: Compute CF (Chemistry Factor)
#' CR3391(product_form = "W", Cu = 0.2, Ni = 0.5, output = "CF")
#'
#' # Example 3: Compute FF (Fluence Factor) with only fluence
#' CR3391(fluence = 1e19, output = "FF")
#'
#' # Example 4: Compute SD (Standard Deviation) for base metal
#' CR3391(product_form = "B", output = "SD")
#'
#' @seealso \code{\link{NP3319}}, \code{\link{RG199R2}}
#'
#' @export
CR3391 <- function(product_form = NULL,
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

  #--------------------------#
  # 1) product_form 전처리
  #--------------------------#
  #  - "B","F","P","W" 중 하나이며, F/P는 B로 통합
  if (!is.null(product_form)) {
    product_form <- as.character(product_form)
    stopifnot(all(product_form %in% c("B", "F", "P", "W")))
    product_form[product_form %in% c("F", "P")] <- "B"
  }

  #--------------------------#
  # 2) 입력값 길이 검사
  #--------------------------#
  #  - 길이가 0, 1, 또는 max_len인지 확인
  args <- list(product_form, Cu, Ni, fluence)
  arg_len <- sapply(args, length)
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

  #--------------------------#
  # 3) 주요 계산 함수들(중복된 인자 검사는 제외)
  #--------------------------#
  # (1) CF 계산
  calc_cf <- function(product_form, Cu, Ni) {
    stopifnot(is.numeric(Cu), all(Cu >= 0 & Cu <= 100))
    stopifnot(is.numeric(Ni), all(Ni >= 0 & Ni <= 100))

    base_cf <- -38.39 + 555.6 * Cu + 480.1 * Cu * tanh(0.353 * Ni / Cu)
    weld_cf <- 624 * Cu - 333.1 * sqrt(Cu * Ni) + 251.2 * Ni

    ifelse(product_form == "B", base_cf,
      ifelse(product_form == "W", weld_cf, NA_real_)
    )
  }

  # (2) FF 계산
  calc_ff <- function(fluence) {
    stopifnot(is.numeric(fluence), all(fluence >= 0))

    fl <- fluence / 1e19
    fl^(0.2661 - 0.0449 * log(fl))
  }

  # (3) TTS 계산
  calc_tts <- function(product_form, Cu, Ni, fluence) {
    # 각 인수 검증은 함수 호출에서 수행함
    cf <- calc_cf(product_form, Cu, Ni)
    ff <- calc_ff(fluence)
    cf * ff
  }

  # (4) SD 계산
  calc_sd <- function(product_form) {
    c(B = 17.2, W = 28.2)[product_form]
  }

  #--------------------------#
  # 6) switch에 따라 결과 계산
  #--------------------------#
  result <- switch(output,
    "CF" = calc_cf(product_form, Cu, Ni),
    "FF" = calc_ff(fluence),
    "TTS" = calc_tts(product_form, Cu, Ni, fluence),
    "SD" = calc_sd(product_form)
  )

  #------------------------------------#
  # 7) 결과 온도 변환
  #    - 만약 최종 출력을 °C로 원하면
  #    - FF 제외
  #------------------------------------#
  if (temperature_unit == "Celsius" && output %in% c("TTS", "CF", "SD")) {
    result <- result * (5 / 9)
  }

  #--------------------------#
  # 8) 반환
  #--------------------------#
  return(unname(result))
}
