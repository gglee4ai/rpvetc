# -------------------------------
# Vector Expansion and Standardization
# -------------------------------

# Expand input vectors to the maximum common length
#
# Ensures all input vectors are of the same length by replicating elements
# of length 1 to match the maximum non-zero length among the inputs.
#
# @param ... A list of vectors to be expanded.
# @return A list of vectors, all of the same length.
expand_vectors <- function(...) {
  args <- list(...)
  arg_len <- sapply(args, length)
  max_len <- max(arg_len[arg_len > 0], 0)

  if (max_len > 0 && !all(arg_len %in% c(0, 1, max_len))) {
    stop("All input vectors must be of length 0, 1, or the same maximum length.")
  }

  lapply(args, function(x) {
    if (is.null(x) || length(x) == 0) {
      return(x)
    }
    if (length(x) == 1 && max_len > 1) rep(x, max_len) else x
  })
}

# Standardize product form to "B" (Base) or "W" (Weld)
#
# Converts product form identifiers to a simplified form used internally.
#
# @param product_form A character vector with values "B", "F", "P", or "W".
# @return A character vector with values either "B" or "W".
to_baseweld <- function(product_form) {
  form <- as.character(product_form)
  if (any(!form %in% c("B", "F", "P", "W"))) {
    stop("Invalid 'product_form'. Must be one of: 'B', 'F', 'P', or 'W'.")
  }
  ifelse(form == "W", "W", "B")
}

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
#' conv$F_to_C(212) # 100
#' conv$ksi_to_MPa(10) # 68.9476
#' conv$ftlb_to_J(20) # 27.1164
#'
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
