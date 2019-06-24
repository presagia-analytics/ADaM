# Find numerically encoded columns
numerically_encoded_cols <- function(x) {
  num_enc <- grep("N$", colnames(x))
  num_enc_candidate <- colnames(x)[num_enc]
  num_enc_no_n <- gsub("N$", "", num_enc_candidate)
  num_enc_candidate[num_enc_no_n %in% colnames(x)]
}

#' Remove numerically encoded columns
#' 
#' @param x the ADaM formatted data.frame which may have numerically encoded
#' columns.
#' @param verbose do you want the columns being removed printed? (default FALSE)
#' @importFrom crayon italic
#' @export
remove_numerically_encoded_columns <- function(x, verbose = FALSE) {
  nec <- numerically_encoded_cols(x)
  if (verbose) {
    if (length(nec) > 0) {
      cat(italic("\tDropping numerically encoded columns:\n\t\t", 
                 paste(nec, collapse = "\n\t\t"), "\n", sep = ""))
    } else {
      cat(italic("\tNo numerically encoded columns to drop.\n"))
    }
  }
  x[, setdiff(colnames(x), numerically_encoded_cols(x))]
}

