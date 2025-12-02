# -------------------------------
# Unit Conversion Functions
# -------------------------------

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
  x * 6.89476
}

MPa_to_ksi <- function(x) {
  x / 6.89476
}

ftlb_to_J <- function(x) {
  x * 1.35582
}

J_to_ftlb <- function(x) {
  x / 1.35582
}

# -------------------------------
# Unit Conversion Registry
# -------------------------------

#' @title Unit Conversion Function Registry
#' @description A list of commonly used unit conversion functions. Each element is named by its conversion direction.
#'
#' @examples
#' unitconv$F_to_C(212) # 100
#' unitconv$ksi_to_MPa(10) # 68.9476
#' unitconv$ftlb_to_J(20) # 27.1164
#'
#' @export
unitconv <- list(
  F_to_C = F_to_C,
  C_to_F = C_to_F,
  dF_to_dC = dF_to_dC,
  dC_to_dF = dC_to_dF,
  ksi_to_MPa = ksi_to_MPa,
  MPa_to_ksi = MPa_to_ksi,
  ftlb_to_J = ftlb_to_J,
  J_to_ftlb = J_to_ftlb
)
