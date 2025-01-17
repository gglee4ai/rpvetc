#'E900_flux
#'
#' Provide TTS or SD of ASTM E900-15e2 upgraded in MRP-462.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param Cu numeric vector, wt%
#' @param Ni numeric vector, wt%
#' @param Mn numeric vector, wt%
#' @param P  numeric vector, wt%
#' @param temperature numeric vector, temperature_unit
#' @param fluence numeric vector, n/cm2
#' @param flux numeric vector, n/cm2/s
#' @param output character c("TTS", "TTS1", "TTS2", "SD")
#' @param temperature_unit character c("Celsius", "Fahrenheit")
#' @return TTS or SD as given temperature_unit
#' @export
#' @examples
#' E900_flux("P", 0.2, 0.18, 1.36, 0.012, 290, 2.56894e18, ) # should be 31.74387
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
  stopifnot(is.numeric(flux), flux >= 0)

  # 벡터 크기 검사
  args <- list(product_form, Cu, Ni, Mn, P, temperature, fluence, flux)
  arg_len <- sapply(args, length)
  max_len <- max(arg_len)
  stopifnot(all(arg_len == 1L | arg_len == max_len))

  # 입력변수 벡터 크기를 동일화
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

  # 기본온도를 섭씨로 변환
  if (temperature_unit == "Fahrenheit") {
    temperature <- (5 / 9) * (temperature - 32)
  }

  # TTS1 계산
  tts1 <- E900_flux_TTS1(product_form, Ni, Mn, P, temperature, fluence, flux)

  # TTS2 계산
  tts2 <- E900_flux_TTS2(product_form, Cu, Ni, P, temperature, fluence, flux)

  # TTS 계산
  tts <- tts1 + tts2

  # SD 계산
  sd <- E900_flux_SD(product_form, tts)

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



#' E900_flux_TTS
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
#' @param flux numeric vector, n/cm2/s
#' @return TTS (degC)
E900_flux_TTS <- function(product_form, Cu, Ni, Mn, P, temperature, fluence, flux) {
  TTS1 <- E900_flux_TTS1(product_form, Ni, Mn, P, temperature, fluence)
  TTS2 <- E900_flux_TTS2(product_form, Cu, Ni, P, temperature, fluence)
  TTS <- TTS1 + TTS2
  return(unname(TTS))
}



#' E900_flux_TTS1
#'
#' Calculate TTS1 of ASTM E900-15.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param Ni numeric vector, wt%
#' @param Mn numeric vector, wt%
#' @param P numeric vector, wt%
#' @param temperature numeric vector, temperature_unit
#' @param fluence numeric vector, n/cm2
#' @param flux numeric vector, n/cm2/s
#' @return TTS1 (degC)
E900_flux_TTS1 <- function(product_form, Ni, Mn, P, temperature, fluence, flux) {
  log10flux <- log10(flux)
  A <- c("F" = 0.785, "P" = 0.774, "W" = 0.798)[product_form]
  TTS1 <- A * 4.074E-10 * (fluence)^0.5724 # 4.074E-10 은 cm2으로 미리 계산 from plotter22 macro
  TTS1 <- TTS1 * ((1.8 * temperature + 32) / 550)^-4.10
  TTS1 <- TTS1 * (1.62 + P / 0.012)^0.461
  TTS1 <- TTS1 * (0.29 + (Ni^17.63) / 0.63)^0.107
  TTS1 <- TTS1 * (Mn / 1.36)^0.44
  TTS1 <- TTS1 * (log10flux / 11.8)^1.64
  TTS1 <- TTS1 * (5 / 9)
  return(unname(TTS1))
}



#' E900_flux_TTS2
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
E900_flux_TTS2 <- function(product_form, Cu, Ni, P, temperature, fluence, flux) {
  log10flux <- log10(flux)
  B <- c("F" = 0.519, "P" = 0.592, "W" = 0.595)[product_form]
  M <- B * pmax(pmin(73.67 * (log(fluence) - log(3.6e16)), 604.0), 0)
  M <- M * ((1.8 * temperature + 32) / 550)^-3.27
  M <- M * (0.01 + P / 0.012)^-0.026
  M <- M * (0.705 + (Ni^0.64) / 0.63)^1.24
  M <- M * (log10flux / 15.9)^-0.52
  TTS2 <- pmax(pmin(Cu, 0.30) - 0.046, 0) * M
  TTS2 <- TTS2 * (5 / 9)
  return(unname(TTS2))
}



#' E900_flux_SD
#'
#' Calculate SD of ASTM E900-15, vectorized.
#'
#' @param product_form character vector c("F", "P", "W")
#' @param TTS numeric vector, degC
#' @return SD of TTS (degC)
E900_flux_SD <- function(product_form, TTS) {
  C <- c("F" = 6.818, "P" = 6.293, "W" = 6.862)[product_form]
  D <- c("F" = 0.238, "P" = 0.202, "W" = 0.237)[product_form]
  SD <- C * TTS^D
  unname(SD)
}
