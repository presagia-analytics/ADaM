
#' @title Test to see if two vectors are equivalent.
#'
#' @description Two numeric vectors are equivalent if their correlation 
#' coefficient is 1 or -1. Two character or factor vectors are equivalent if 
#' their is a mapping between labels that makes them the same.
#' @param x the first vector to test for equivalence.
#' @param y the second vector to test for equivalence.
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
equiv <- function(x, y) {
  UseMethod("equiv")
}

#' @export
equiv.default <- function(x, y) {
  stop(paste("Don't know how to test for equivalence between", class(x), "and",
             "class(y)"))
}

#' @export
equiv.numeric <- function(x, y) {
  ret <- FALSE
  if (is.numeric(y)) {
    x_nas <- which(is.na(x))
    y_nas <- which(is.na(y))
    if (length(x_nas) == length(y_nas) && isTRUE(all(x_nas == y_nas))) {
      x <- na.omit(x)
      y <- na.omit(y)
      if (isTRUE(all.equal(abs(cor(x, y)), 1))) {
        ret <- TRUE
      }
    }
  }
  ret
}

#' @importFrom lpSolve lp.assign
#' @export
equiv.character <- function(x, y) {
  ret <- FALSE
  if (is.character(y)) {
    x_nas <- which(is.na(x))
    y_nas <- which(is.na(y))
    if (length(x_nas) == length(y_nas) && isTRUE(all(x_nas == y_nas))) {
      wts <- table(x, y)
      if (nrow(wts) == ncol(wts) && 
          all.equal(lp.assign(wts, "max")$objval, sum(wts))) {
        ret <- TRUE
      }
    }
  }
  ret
}

#' @importFrom lpSolve lp.assign
#' @export
equiv.factor <- function(x, y) {
  ret <- FALSE
  if (is.factor(y)) {
    x_nas <- which(is.na(x))
    y_nas <- which(is.na(y))
    if (length(x_nas) == length(y_nas) && isTRUE(all(x_nas == y_nas))) {
      wts <- table(x, y)
      if (nrow(wts) == ncol(wts) && 
          all.equal(lp.assign(wts, "max")$objval, sum(wts))) {
        ret <- TRUE
      }
    }
  }
  ret
}

#' @export
equiv.tibble <- function(x, y) {
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
      ret[i, j] <- equiv(x[,i], x[,j])
    }
  }
  ret
}

#' @title Remove redundant equivalent columns
#'
#' @description Find the equivalant columns of a data.frame. Keep the first 
#' remove the rest.
#' @param x a data.frame that may have repeated, equivalent columns.
#' @examples
#' 
#' iris$Sepal.Length2 <- 3 * iris$Sepal.Length + 3
#' remove_equiv_columns(iris)
#' 
#' @return a data frame where redundant columns have been dropeed.
#' @export
remove_equiv_columns <- function(x) {
  ecm <- equiv_columns(x)
  redundant_cols <- apply(ecm, 2, any)
  x[,!redundant_cols]
}


