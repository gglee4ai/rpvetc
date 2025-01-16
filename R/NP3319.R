#' NP3319
#'
#' Calculate TTS using NP3319(EPRI NP-3319 TTS model by Odette, 1984).
#'
#' @param product_form character vector c("B", "F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param fluence numeric vector n/cm2
#' @param output string c("TTS", "CF", "FF")
#' @param temperature_unit character vector, c("Celsius", "Fahrenheit")
#'
#' @return output as given unit
#' @export
#' @examples
#' NP3319("B", 0.1, 0.6, 1e19)
NP3319 <- function(product_form,
                   Cu,
                   Ni,
                   fluence = 0,
                   output = c("TTS", "CF", "FF"),
                   temperature_unit = c("Celsius", "Fahrenheit")) {
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)

  erf <- function(x) 2 * stats::pnorm(x * sqrt(2)) - 1 # Basic error function

  # 입력변수 검사
  stopifnot(product_form %in% c("B", "F", "P", "W"))
  stopifnot(is.numeric(Cu), Cu >= 0, Cu <= 100)
  stopifnot(is.numeric(Ni), Ni >= 0, Ni <= 100)
  stopifnot(is.numeric(fluence), fluence >= 0)

  # 벡터 크기 검사
  arg_len <- c(length(product_form), length(Cu), length(Ni), length(fluence))
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
  if (length(fluence) < max_len) {
    fluence <- rep(fluence, max_len)
  }

  # product_form을 단순화
  product_form[product_form %in% c("F", "P")] <- "B"

  cf <- ifelse(
    test = product_form %in% c("B"),
    yes = {
      # Base 일 때의 공식
      Cu * 216 * (1 + 0.33 * (erf(0.77 * Ni / Cu - 1) + 1))
    },
    no = ifelse(
      test = product_form %in% c("W"),
      yes = {
        # Weld 일 때의 공식
        Cu * 200 * (1 + 1.38 * (erf(0.30 * Ni / Cu - 1) + 1))
      },
      no = NA_real_
    )
  )
  names(cf) <- rep("CF \u00b0F", max_len)

  # fluence factor 계산
  fl <- fluence / 1e19
  ff <- ifelse(
    test = product_form %in% c("B"),
    yes = {
      # Base 일 때의 공식
      fl^0.28
    },
    no = ifelse(
      test = product_form %in% c("W"),
      yes = {
        # Weld 일 때의 공식
        (1 - exp(-fl / 0.11))^1.36 * fl^0.18
      },
      no = NA_real_
    )
  )
  names(ff) <- rep("FF", max_len)

  # TTS 계산
  tts <- cf * ff
  names(tts) <- rep("TTS \u00b0F", max_len)

  # SD 계산
  # sd <- c(B = 17.2, W = 28.2)[product_form]

  # 결과 선택
  result <- switch(output,
    TTS = tts,
    CF = cf,
    FF = ff,
    # SD = sd
  )

  # 온도 변환
  if (output != "FF" && temperature_unit == "Celsius") {
    result <- (5 / 9) * result
    names(result) <- gsub("\u00b0F", "\u00b0C", names(result))
  }

  return(result)
}
