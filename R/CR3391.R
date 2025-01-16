#' CR3391
#'
#' Calculate TTS using NUREG/CR3391 vol.2 report by Guthrie (1983).
#'
#' @param product_form character vector c("B", "F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param fluence nummeric vector n/cm2
#' @param output string c("TTS", "CF", "FF", "SD")
#' @param temperature_unit character vector, c("Celsius", "Fahrenheit")
#'
#' @return TTS as given temperature_unit
#' @export
#' @examples
#' CR3391("B", 0.1, 0.6, 1e19)
CR3391 <- function(product_form,
                   Cu,
                   Ni,
                   fluence,
                   output = c("TTS", "CF", "FF", "SD"),
                   temperature_unit = c("Celsius", "Fahrenheit")) {
  output <- match.arg(output)
  temperature_unit <- match.arg(temperature_unit)

  # 입력변수 검사
  stopifnot(product_form %in% c("B", "F", "P", "W"))
  stopifnot(is.numeric(Cu), Cu >= 0, Cu <= 100)
  stopifnot(is.numeric(Ni), Ni >= 0, Ni <= 100)
  stopifnot(is.numeric(fluence), fluence >= 0)

  # 벡터 크기 검사
  arg_len <- c(length(product_form), length(Cu), length(Ni), length(fluence))
  max_len <- max(arg_len)
  stopifnot(all(arg_len == 1L | arg_len == max_len))

  # product_form을 단순화
  product_form[product_form == "F"] <- "B"
  product_form[product_form == "P"] <- "B"

  cf <- ifelse(
    test = product_form %in% c("B"),
    yes = {
      # Base 일 때의 공식
      -38.39 + 555.6 * Cu + 480.1 * Cu * tanh(0.353 * Ni / Cu)
    },
    no = ifelse(
      test = product_form %in% c("W"),
      yes = {
        # Weld 일 때의 공식
        624 * Cu - 333.1 * sqrt(Cu * Ni) + 251.2 * Ni
      },
      no = NA_real_
    )
  )

  # fluence factor 계산
  fl <- fluence / 1e19
  ff <- fl^(0.2661 - 0.0449 * log(fl)) # fluence factor

  # TTS 계산
  tts <- cf * ff

  # SD 계산
  sd <- c(B = 17.2, W = 28.2)[product_form]

  # 출력 길이가 최대 길이보다 짧으면 맞추기
  if (length(tts) < max_len) tts <- rep(tts, max_len)
  if (length(cf) < max_len) cf <- rep(cf, max_len)
  if (length(ff) < max_len) ff <- rep(ff, max_len)
  if (length(sd) < max_len) sd <- rep(sd, max_len)

  # 결과 선택
  result <- switch(output,
    TTS = tts,
    CF = cf,
    FF = ff,
    SD = sd
  )

  # 온도 변환
  if (output != "FF" && temperature_unit == "Celsius") {
    result <- result * (5 / 9)
  }

  return(unname(result))
}
