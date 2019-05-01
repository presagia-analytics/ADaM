#' Print a message
#' 
#' @param x the data the function will return
#' @param msg the message to cat.
#' @param verbose should the message be cat'ed? Default TRUE.
#' @param style the crayon style to use when printing. Default reset.
#' @param ... other paramters passed to cat.
#' @importFrom crayon reset
tcat <- function(x, msg, verbose = TRUE, style = reset, ...) {
  if (verbose) {
    cat(style(msg), ...)
  }
  x
}

#' Normalize an ADam dataset.
#' 
#' @param x an ADaM formatted data.frame.
#' @param on which variable should be collpased on? (Default: "USUBJID")
#' @param collapse_name the variable name of the collapsed sub-data.frames.
#' @param remove_equiv_columns should equivalent columns be removed?
#' @param remove_numerically_encoded should numerically encoded columns be 
#' removed? Default TRUE
#' @param keep_cols column names that should be kept.
#' @param verbose should information about dropped columns be printed? 
#' (default FALSE)
#' @return The collapsed data.frame with numerically encoded columens removed.
#' @importFrom dplyr %>% mutate_if mutate_at
#' @importFrom crayon green
#' @importFrom normalizer collapse_rows remove_equiv_columns
#' @export
normalize_adam <- function(x, on = "USUBJID", collapse_name, 
  remove_equiv_columns = TRUE, remove_numerically_encoded = TRUE,
  keep_cols = character(), verbose = FALSE) {

  . <- NULL
  if (missing(collapse_name)) {
    collapse_name <- as.character(as.list(match.call())$x)
  }
  to_factor <- function(x) {
    col_types <- sapply(x, class)
    colnames(x)[colnames(x) != on & col_types == "character"]
  }

  if (remove_numerically_encoded) {
    x <- x %>% 
      tcat("Removing numerically encoded columns.\n", verbose = verbose,
           style = green) %>%
      remove_numerically_encoded_columns(verbose = verbose)
  }
  if (remove_equiv_columns) {
    x %>%
      tcat("Removing equivalent columns.\n", verbose = verbose,
           style = green) %>%
      remove_equiv_columns(verbose = verbose, keep_cols = keep_cols)
  }
 
  if (verbose) { 
    cat(green("Mutating character columns to factors.\n"))
    ma <- to_factor(x)
    if (length(ma) == 0) {
      cat(italic("No variables to turn into factor."))
    } else {
      cat(
        italic("\tThe following variables will be turned into factors:\n\t\t"))
      cat(italic(paste(ma, collapse = "\n\t\t")))
      cat("\n")
    }
  }

  x %>%
    mutate_at(to_factor(.), as.factor) %>%
    tcat("Collapsing rows.\n", verbose = verbose, style = green) %>%
    collapse_rows(on, collapse_name) 
}

#' @importFrom crayon red
#' @importFrom equivalent equiv
check_common_variable_equivalence <- function(x, on) {
  dup_violations <- c()
  if (length(x) > 1) {
    all_names <- unlist(lapply(x, colnames))
    dup_names <- setdiff(all_names[duplicated(all_names)], on)
    for (dn in dup_names) {
      dup_inds <- which(unlist(lapply(x, function(d) dn %in% colnames(d))))
      for (di in dup_inds[-1]) {
        if (!equiv(x[[dup_inds[1]]][[dn]][order(x[[dup_inds[1]]][[on]])], 
                   x[[di]][[dn]][order(x[[dup_inds[1]]][[on]])])) {
          dup_violations <- c(dup_violations, dn)
          break
        }
      }
    }
  }
  dup_violations
}

#' Consolidate multiple data sets
#'
#' @param ... a set of ADaM formatted data.frames.
#' @param on which variable should be collpased on? (Default: "USUBJID")
#' @return A single data.frame composed of the collapsed and merged input
#' data.frames.
#' @importFrom dplyr full_join
#' @importFrom crayon red
#' @export
consolidate_adam <- function(..., on = "USUBJID") {
  # Get the set of data sets.
  arg_list <- as.list(...)

  # Make sure we have an on variable in each data set.
  name_check <- vapply(arg_list, function(x) on %in% names(x), FALSE)
  if (!isTRUE(all(name_check))) {
    stop(red(
      paste("Join variable missing in data set", which(name_check != TRUE))))
  }

  # Make sure if a variable appears in more than one data set it is the 
  # same in each data set.
  violations <- check_common_variable_equivalence(arg_list, on = on)
  if (length(violations) > 0) {
    stop(red("The following variables appear in multiple data sets but are ",
             "not consistent\n  and can't be merged:\n\t", 
             paste(violations, collapse= "\n\t"), 
             sep = ""))
  }

  col_names <- list(colnames(arg_list[[1]]))
  all_col_names <- unique(unlist(lapply(arg_list, colnames)))
  all_col_names <- setdiff(all_col_names, colnames(arg_list[[1]]))
  for (i in 2:length(arg_list)) {
    keep_col_names <- intersect(colnames(arg_list[[i]]), all_col_names)
    col_names <- c(col_names, list(c(on, keep_col_names)))
    all_col_names <- setdiff(all_col_names, keep_col_names)
  }
  keep <- vapply(col_names, function(x) length(x) > 1, FALSE)
  col_names <- col_names[keep]
  arg_list <- arg_list[keep]
  ret <- arg_list[[1]][,col_names[[1]]]
  for (i in 2:length(arg_list)) {
    ret <- full_join(ret, arg_list[[i]][,col_names[[i]]], by = on)
  }
  ret
}

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

