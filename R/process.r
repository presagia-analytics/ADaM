#' Print a message
#' 
#' @param x the data the function will return
#' @param msg the message to cat.
#' @param verbose should the message be cat'ed? Default TRUE.
#' @param ... other paramters passed to cat.
tcat <- function(x, msg, verbose = TRUE, ...) {
  if (verbose) {
    cat(msg, ...)
  }
  x
}

#' Normalize an ADam dataset.
#' 
#' @param x an ADaM formatted data.frame.
#' @param on which variable should be collpased on? (Default: "USUBJID")
#' @param collapse_name the variable name of the collapsed sub-data.frames.
#' @param verbose should information about dropped columns be printed? 
#' (default FALSE)
#' @return The collapsed data.frame with numerically encoded columens removed.
#' @importFrom dplyr %>% mutate_if mutate_at
#' @export
normalize_adam <- function(x, on = "USUBJID", collapse_name, verbose = FALSE) {
  . <- NULL
  if (missing(collapse_name)) {
    collapse_name <- as.character(as.list(match.call())$x)
  }
  to_factor <- function(x) {
    col_types <- sapply(x, class)
    to_factor <- colnames(x)[colnames(x) != on & col_types == "character"]
  }
  x %>% 
    tcat("Removing numerically encoded columns.\n", verbose = verbose) %>%
    remove_numerically_encoded_columns(verbose = verbose) %>% 
    tcat("Removing equivalent columns.\n", verbose = verbose) %>%
    remove_equiv_columns(verbose = verbose) %>%
    tcat("Mutating character columns to factors.\n", verbose = verbose) %>%
    mutate_at(to_factor(.), as.factor) %>%
    tcat("Collapsing rows.\n", verbose = verbose) %>%
    collapse_rows(on, collapse_name) 
}

#' Consolidate multiple data sets
#'
#' @param ... a set of ADaM formatted data.frames.
#' @param on which variable should be collpased on? (Default: "USUBJID")
#' @return A single data.frame composed of the collapsed and merged input
#' data.frames.
#' @importFrom dplyr full_join
#' @export
consolidate_adam <- function(..., on = "USUBJID") {
  # Get the set of data sets.
  arg_list <- as.list(...)

  # Make sure we have an on variable in each data set.
  name_check <- vapply(arg_list, function(x) on %in% names(x), FALSE)
  if (!isTRUE(all(name_check))) {
    stop(paste("Join variable missing in data set", which(name_check != TRUE)))
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

#' Collapse the rows of a data.frame object
#' 
#' @param x and ADaM formatted data.frame
#' @param key which variable should be collpased on? (Default: "USUBJID")
#' @param collapse_name the variable name of the collapsed sub-data.frames.
#' @importFrom foreach foreach %do%
#' @export
collapse_rows <- function(x, key = "USUBJID", collapse_name = "data") {
  svs <- NULL
  sv <- c(key, collapsible_vars(x, key))
  nsv <- setdiff(colnames(x), sv)
  sv_split <- split(seq_len(nrow(x)), Reduce(paste, x[,sv]))
  foreach(svs = sv_split, .combine = rbind) %do% {
    l <- list(x[svs, nsv])
    ret <- x[svs[1], sv]
    ret[[collapse_name]] <- l
    ret
  }
}

# For data frames with repeated subject ids.

#' @importFrom foreach foreach %do%
collapsible_vars <- function(x, group_var) {
  s <- NULL
  spl <- split(1:nrow(x), x[,group_var])
  check_vars <- setdiff(colnames(x), group_var)
  check_vals <- foreach (s = spl, .combine = `&`) %do% {
    unlist(lapply(x[s, check_vars],
      function(x) {
        isTRUE(all(x == x[1])) | all(is.na(x))
      }))
  }
  check_vars[check_vals]
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
#' @export
remove_numerically_encoded_columns <- function(x, verbose = FALSE) {
  nec <- numerically_encoded_cols(x)
  if (verbose) {
    if (length(nec) > 0) {
      print(paste("Dropping numerically encoded columns", 
                  paste(nec, collapse = " ")))
    } else {
      print("No numerically encoded columns to drop.")
    }
  }
  x[, setdiff(colnames(x), numerically_encoded_cols(x))]
}


