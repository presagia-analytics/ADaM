
#' Normalize an ADam dataset.
#' 
#' @examples
#' data(adorirr)
#' normalize_adam(adorirr)
#' @importFrom dplyr %>%
#' @export
normalize_adam <- function(x, on = "USUBJID", collapse_name) {
  if (missing(collapse_name)) {
    collapse_name <- as.character(as.list(match.call())$x)
  }
  x %>% 
    remove_numerically_encoded_columns %>% 
    collapse_rows(on, collapse_name)
}

#' Consolidate multiple data sets
#'
#' @examples
#' data(adorirr, adefirr, adsl)
#' ruca <- list(normalize_adam(adorirr), normalize_adam(adefirr), 
#'              normalize_adam(adsl))
#' x <- consolidate_adam(ruca)
#' @importFrom tidyr nest_
#' @importFrom dplyr full_join
#' @importFrom purrr map2 
#' @export
consolidate_adam <- function(..., on = "USUBJID") {
  # Get the set of data sets.
   browser() 
  arg_list <- as.list(...)

  # Make sure we have an on variable in each data set.
  name_check <- vapply(arg_list, function(x) on %in% names(x), FALSE)
  if (!isTRUE(all(name_check))) {
    stop(paste("Join variable missing in data set", which(name_check != TRUE)))
  }

  col_names <- list(colnames(arg_list[[1]]))
  for (i in 2:length(arg_list)) {
    col_names <- c(col_names, 
                   list(c(on, setdiff(colnames(arg_list[[i]]), 
                                      colnames(arg_list[[i-1]])))))
  }
  ret <- arg_list[[1]][,col_names[[1]]]
  for (i in 2:length(arg_list)) {
    ret <- full_join(ret, arg_list[[i]][,col_names[[i]]], by = on)
  }
  ret
}

#' Collapse the rows of a data.frame object
#'  
#' @examples
#' data(adorirr)
#' collapse_rows(adorirr)
#' @importFrom tidyr nest
#' @export
collapse_rows <- function(x, key = "USUBJID", collapse_name = "data") {
  sv <- c(key, collapsable_vars(x, key))
  nsv <- setdiff(colnames(x), sv)
  if (length(nsv) > 0) {
    x <- nest_(x, collapse_name, colnames(x)[match(nsv, colnames(x))])
  }
  x
}

# For data frames with repeated subject ids.

#' @importFrom foreach foreach %do%
#' @export
collapsable_vars <- function(x, group_var) {
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

#' Find numerically encoded columns
#' 
#' @export
numerically_encoded_cols <- function(x) {
  num_enc <- grep("N$", colnames(x))
  num_enc_candidate <- colnames(x)[num_enc]
  num_enc_no_n <- gsub("N$", "", num_enc_candidate)
  num_enc_candidate[num_enc_no_n %in% colnames(x)]
}

#' Remove numerically encoded columns
#' 
#' @examples
#' data(adorirr)
#' remove_numerically_encoded_columns(adorirr)
#' @export
remove_numerically_encoded_columns <- function(x) {
  x[, setdiff(colnames(x), numerically_encoded_cols(x))]
}


