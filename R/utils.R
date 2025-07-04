
#' 입력 벡터들을 가장 긴 길이에 맞게 확장하는 함수
#' @param ... 확장할 벡터들의 리스트
#' @return 길이가 확장된 벡터들의 리스트
expand_vectors <- function(...) {
  args <- list(...)
  arg_len <- sapply(args, length)
  max_len <- max(arg_len[arg_len > 0], 0)

  if (max_len > 0 && !all(arg_len %in% c(0, 1, max_len))) {
    stop("모든 입력 인자는 길이가 0, 1 또는 동일한 최대 길이를 가져야 합니다.")
  }

  lapply(args, function(x) {
    if (is.null(x) || length(x) == 0) return(x)
    if (length(x) == 1 && max_len > 1) rep(x, max_len) else x
  })
}

#' Product Form을 "B"(Base) 또는 "W"(Weld)로 표준화하는 함수
#' @param product_form 문자열 벡터 ("B", "F", "P", "W")
#' @return "B" 또는 "W"로 변환된 문자열 벡터
to_baseweld <- function(product_form) {
  form <- as.character(product_form)
  if (any(!form %in% c("B", "F", "P", "W"))) {
    stop("잘못된 product_form 입니다. 'B', 'F', 'P', 'W' 중 하나여야 합니다.")
  }
  ifelse(form == "W", "W", "B")
}




F_to_C <- function(x) {
  (5 / 9) * (x - 32)
}

C_to_F <- function(x) {
  (9 / 5) * x + 32
}

dF_to_dC <- function(x) {
  x * 5 / 9
}

dC_to_dF <- function(x) {
  x * 9 / 5
}

ksi_to_MPa <- function(x) {
  x * 6.89476 # from internet 20250424
}

MPa_to_ksi <- function(x) {
  x / 6.89476 # from internet 20250424
}

ftlb_to_J <- function(x) {
  x * 1.35582
}

J_to_ftlb <- function(x) {
  x / 1.35582
}


#' @export
conv <- list(
  F_to_C = F_to_C,
  C_to_F = C_to_F,
  dF_to_dC = dF_to_dC,
  dC_to_dF = dC_to_dF,
  ksi_to_MPa = ksi_to_MPa,
  MPa_to_ksi = MPa_to_ksi,
  ftlb_to_J = ftlb_to_J,
  J_to_ftlb = J_to_ftlb
)

