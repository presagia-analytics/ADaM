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

#' Find Variables Appearing Multiple Data Sets
#'
#' @param x the list of data.frames.
#' @param on the collapse variable.
#' @param x_names the names of the data sets. (Default names(x))
#' @importFrom dplyr full_join
#' @importFrom crayon red
#' @importFrom equivalent equiv
#' @export
repeat_vars <- function(x, on, x_names = names(x)) {
  if (is.null(x_names)) {
    stop(red("The supplied list must have names."))
  }
  dup_violations <- list()
  if (length(x) > 1) {
    dup_names <- setdiff(dup_vars(x), on)
    for (dn in dup_names) {
      dup_inds <- which(unlist(lapply(x, function(d) dn %in% colnames(d))))
      dup_ret <- as_tibble(x[[dup_inds[1]]][,c(on, dn)])
      colnames(dup_ret)[2] <- x_names[dup_inds[1]]
      for (di in dup_inds[-1]) {
        nd <- x[[di]][, c(on, dn)]
        colnames(nd)[2] <- x_names[di]
        dup_ret <- full_join(dup_ret, nd, by = on)
      }
      dup_ret$var <- dn
      dup_ret <- dup_ret[, c(1, ncol(dup_ret), 
                             setdiff(seq_len(ncol(dup_ret)), 
                                     c(1, ncol(dup_ret))))]
      dup_violations <- c(dup_violations, list(dup_ret))
    }
    names(dup_violations) <- dup_names
  }
  dup_violations
}

#' Variables Duplicated Across Data Sets
#'
#' @param x the list of data.frames.
#' @export
dup_vars <- function(x) {
  all_names <- unlist(lapply(x, colnames))
  unique(all_names[duplicated(all_names)])
}

#' @importFrom equivalent has_equiv_column
#' @importFrom crayon yellow
#' @importFrom stats na.omit
handle_repeated_vars <- function(arg_list, rvs, on) {
  new_arg <- NULL
  for (rv in rvs) {
    if (isTRUE(all(has_equiv_column(rv)[-(1:3)]))) {
      new_cols <- rv[,c(1, 3)]
      colnames(new_cols)[2] <- rv[[2]][1]
    } else {
      warning(yellow("No equivalence found for variable", rv$var[1], 
                     "using most complete data set."))
      na_counts <- unlist(lapply(rv, function(x) sum(is.na(x))))
      min_na_val <- min(na_counts[-(1:2)])
      rv <- rv[, na_counts <= min_na_val]
      if (isTRUE(all(has_equiv_column(rv)[-(1:3)]))) {
        new_cols <- rv[,c(1, 3)]
        colnames(new_cols)[2] <- rv[[2]][1]
      } else {
        stop(red("Contradictions in repeated variables.\n", 
                 "  You need to manually fix variable: ", rv[[2]][1], 
                 "\n  It appears in data sets:\n\t",
                 paste(names(na_counts)[-(1:2)], collapse = "\n\t"),
                 "\n", sep = ""))
      }
    }
    if (is.null(new_arg)) {
      new_arg <- new_cols
    } else {
      new_arg <- full_join(new_arg, new_cols, by = on)
    }
  }
  for (i in seq_along(arg_list)) {
    rem_inds <- na.omit(match(names(rvs), colnames(arg_list[[i]])))
    arg_list[[i]] <- arg_list[[i]][, -rem_inds]
  }
  c(arg_list, list(new_arg))
}

#' Find the Data Sets with Conflicting Columns
#' 
#' @param x the list of ADaM data sets.
#' @param on which variable should be collpased on? 
#' @importFrom tibble as_tibble
#' @export
contradicting_vars <- function(x, on) {
  rvs <- repeat_vars(x, on = on)
  ret <- lapply(rvs, function(rv) {
    r <- c()
    if (!isTRUE(all(has_equiv_column(rv)[-(1:3)]))) {
      colnames(rv)[-(1:2)]
    }
  })
  ret[unlist(lapply(ret, function(x) length(x) > 0))]
}

#' Consolidate multiple data sets
#'
#' @param ... a set of ADaM formatted data.frames.
#' @param on which variable should be collpased on? (Default: "USUBJID")
#' @param verbose should extra information be provided? (Default: TRUE)
#' @return A single data.frame composed of the collapsed and merged input
#' data.frames.
#' @importFrom dplyr full_join
#' @importFrom crayon red
#' @export
consolidate_adam <- function(..., on = "USUBJID", verbose = FALSE) {
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
  rvs <- repeat_vars(arg_list, on = on)

  if (length(rvs) > 0) {
    if (verbose) {
      cat(italic("\tHandling repeated variables."))
    }
    arg_list <- handle_repeated_vars(arg_list, rvs, on)
  }  
#  if (length(violations) > 0) {
#    stop(red("The following variables appear in multiple data sets but are ",
#             "not consistent\n  and can't be merged:\n\t", 
#             paste(violations, collapse= "\n\t"), 
#             sep = ""))
#  }

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

