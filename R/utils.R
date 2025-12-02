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
