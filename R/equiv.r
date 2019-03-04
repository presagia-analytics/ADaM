
#' @title Test to see if two vectors are equivalent.
#'
#' @description Two numeric vectors are equivalent if their correlation 
#' coefficient is 1 or -1. Two character or factor vectors are equivalent if 
#' their is a mapping between labels that makes them the same.
#' @param x the first vector to test for equivalence.
#' @param y the second vector to test for equivalence.
#' @param factor_equiv_character should factors be treated as equivalend
#' to characters? (default TRUE)
#' @param ... other arguments for type-specific equivalence.
#' @return A boolean is returned indicating whether or not the two
#' vectors are equivalent.
#' @examples
#' a <- rnorm(10)
#' b <- 2 * rnorm(10) + 4
#' # TRUE because they are the same (up to an affine transformation).
#' equiv(a, b)
#' 
#' a <- c("a", "a", "b", "c")
#' b <- c("b", "b", "a", "c")
#' # TRUE because they are the same (up to a label change).
#' equiv(a, b)
#' @export
equiv <- function(x, y, factor_equiv_character = TRUE, ...) {
  UseMethod("equiv")
}

#' @importFrom crayon yellow
#' @export
equiv.default <- function(x, y, factor_equiv_character = TRUE, ...) {
  if (isTRUE(any(class(x) == class(y)))) { 
    warning(yellow(paste("Don't know how to test for equivalence between", 
                  class(x), "and", class(y))))
  }
  FALSE
}

#' @export
equiv.Date <- function(x, y, factor_equiv_character = TRUE, ...) {
  ret <- FALSE
  if (inherits(y, "Date")) {
    ret <- equiv(as.numeric(x), as.numeric(y))
  }
  ret
}

#' @importFrom stats cor
#' @export
equiv.numeric <- function(x, y, factor_equiv_character = TRUE, ...) {
  ret <- FALSE
  if (is.numeric(y)) {
    x_nas <- which(is.na(x))
    y_nas <- which(is.na(y))
    if (length(x_nas) == length(y_nas) && isTRUE(all(x_nas == y_nas))) {
      x <- na.omit(x)
      y <- na.omit(y)
      if (length(unique(x)) == 1 || length(unique(y)) == 1) {
        ret <- isTRUE(
          all.equal(x, y, check.attributes = FALSE, use.names = FALSE))
      } else if (isTRUE(all.equal(abs(cor(x, y)), 1))) {
        ret <- TRUE
      }
    }
  }
  ret
}

table_equiv <- function(x, y) {
  ret <- FALSE
  if (length(x) == length(y) && length(unique(x)) == length(unique(y))) {
    x_nas <- which(is.na(x))
    y_nas <- which(is.na(y))
    if (length(x_nas) == length(y_nas) && isTRUE(all(x_nas == y_nas))) {
      wts <- as.matrix(table(x, y))
      row_all_zero_except_one <- apply(wts, 1, function(x) sum(x != 0) == 1)
      col_all_zero_except_one <- apply(wts, 2, function(x) sum(x != 0) == 1)
      if (isTRUE(all(row_all_zero_except_one)) && 
          isTRUE(all(row_all_zero_except_one))) {

        row_non_zero <- sort(apply(wts, 1, function(z) which(z != 0)))
        col_non_zero <- sort(apply(wts, 2, function(z) which(z != 0)))

        if (isTRUE(all(row_non_zero == seq_along(row_non_zero))) &&
            isTRUE(all(col_non_zero == seq_along(col_non_zero)))) {
          
          ret <- TRUE
        }
      }
    }
  }
  ret
}

#' @export
equiv.character <- function(x, y, factor_equiv_character = TRUE, ...) {
  ret <- FALSE
  if (is.factor(y) && factor_equiv_character) {
    y <- as.character(y)
  }
  if (is.character(y)) {
    ret <- table_equiv(x, y)
  }
  ret
}

#' @export
equiv.factor <- function(x, y, factor_equiv_character = TRUE, ...) {
  ret <- FALSE
  if (is.character(y) && factor_equiv_character) {
    y <- as.factor(y)
  }
  if (is.factor(y)) {
    if (isTRUE(length(levels(x)) == length(levels(y)) && 
        all(levels(x) == levels(y)))) {
      ret <- table_equiv(x, y)
    }
  }
  ret
}

#' @export
equiv.tibble <- function(x, y, factor_equiv_character = TRUE, ...) {
  ret <- FALSE
  if (ncol(x) == 1) {
    x <- as.vector(x[,1])
  }
  if (!is.vector(y)) {
    if (inherits(y, "tibble") && ncol(y) == 1) {
      y <- as.vector(y[,1])
    }
  }
  if (is.vector(x) && is.vector(y)) {
    ret <- equiv(x, y)
  }
  ret
}

#' @title Find equivalent columns in a matrix or data.frame
#' 
#' @description Test all column combinations to find out which ones are 
#' equivalent. An upper-triangular matrix is returned with TRUE indicating
#' columns that are equivalent. Note that the main diagonal along with the
#' lower triangular values are always FALSE.
#' @param x a matrix or data.frame
#' @examples
#' 
#' iris$Sepal.Length2 <- 3 * iris$Sepal.Length + 3
#' equiv_columns(iris)
#' 
#' @return a symmetric boolean matrix where the rows and columns correspond 
#' to the columns of x and the elements correspond to whether or not the 
#' columns are equivalent
#' @export
equiv_columns <- function(x) {
  ret <- matrix(FALSE, nrow = ncol(x), ncol = ncol(x))
  if (!is.null(colnames(x))) {
    colnames(ret) <- rownames(ret) <- colnames(x)
  }
  for (i in seq_len(ncol(x))[-ncol(x)]) {
    for (j in (i+1):ncol(x)) {
      ret[i, j] <- equiv(as.data.frame(x)[,i], as.data.frame(x)[,j])
    }
  }
  ret
}

#' @title Remove redundant equivalent columns
#'
#' @description Find the equivalant columns of a data.frame. Keep the first 
#' remove the rest.
#' @param x a data.frame that may have repeated, equivalent columns.
#' @param verbose should information about dropped columns be printed? 
#' (default FALSE)
#' @examples
#' 
#' iris$Sepal.Length2 <- 3 * iris$Sepal.Length + 3
#' remove_equiv_columns(iris)
#' 
#' @return a data frame where redundant columns have been dropeed.
#' @export
remove_equiv_columns <- function(x, verbose = FALSE) {
  ecm <- equiv_columns(x)
  redundant_cols <- apply(ecm, 2, any)
  if (verbose) {
    if (sum(redundant_cols) > 0) {
      cat(green(paste("Dropping redundant columns", 
                  paste(colnames(x)[redundant_cols], collapse = " ")), "\n"))
    } else {
      cat(green("No redundant columns to drop.\n"))
    }
  }
  x[,!redundant_cols]
}


