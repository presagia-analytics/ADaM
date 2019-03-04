
attr_or_na <- function(x, attr) {
  ret <- c()
  for (j in seq_len(ncol(x))) {
    v <- attributes(x[[j]])[[attr]]
    if (length(v) > 1) {
      v <- paste(v, collapse = ":")
    }
    if (is.null(v)) {
      v <- NA
    }
    ret <- c(ret, v)
  }
  if (ncol(x) != length(ret)) browser()
  ret
}

#' @title Describe an ADaM data set
#'
#' @description An ADaM-formatted .sas7bdat file includes a minimal amount
#' of information about each of the variables, which are stored as
#' attributes. This fuction "describes" these data sets and includes
#' the variable names, descriptions, types, and other summary information.
#' @param x the ADaM-formatted data.frame.
#' @param data_name the name of the data set to describe. If not null, then
#' a column, data_name is added and the value is repeated. Otherwise,
#' a new column is not added. Default NULL.
#' @return A tibble with description information.
#' @export
describe_adam <- function(x, data_name = NULL) {
  ret <- tibble(var_name = colnames(x))
  if(!is.null(data_name)) {
    ret <- cbind(tibble(data_name = rep(data_name, ncol(x))), ret)
  }
  ret$type <- unlist(lapply(x, typeof))
  all_attr <- c()
  for (j in seq_len(ncol(x))) {
    all_attr <- c(all_attr, list(attributes(x[[j]])))
  }
  all_attr_names <- unique(unlist(lapply(all_attr, names)))
  for (an in all_attr_names) {
    ret[[an]] <- attr_or_na(x, an)
  }
  ret
}

#' @title Consolidate Mulitple Data Set Descriptions
#'
#' @param ... a set of ADaM formatted data.frames.
#' @return A single data.frame composed of descriptions of all variables
#' for all data sets specified.
#' @importFrom dplyr bind_rows
#' @export
consolidated_describe_adam <- function(...) {
  arg_list <- as.list(...)
  aln <- names(arg_list)
  ret <- NULL
  bind_rows(Map(function(i) describe_adam(arg_list[[i]], aln[i]), 
                seq_along(arg_list)))
}
