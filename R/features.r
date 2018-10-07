#' Clovis ADORIRR trajetory clean
#' @importFrom stats na.omit
#' @importFrom dplyr %>% arrange
#' @export
clean_traj <- function(x) {
  ret <- x[as.numeric(x$AVALC) < 5 , c("ADY", "AVALC")] %>%
    arrange(ADY) %>%
    na.omit
}


# ADORIRR
#' @export
best_resp <- function(x) {
  ret <- as.factor(NA)
  if (nrow(x) > 0) {
    min_fac_level <- min(as.integer(x$AVALC))
    if (min_fac_level < 5) {
      ret <- x$AVALC[which.min(x$AVALC)]
    }
  }
  ret
}

# ADEFIRR
#' @export
get_duration_of_response <- function(x) {
  ret <- as.double(NA)
  dor_ind <- which(x$PARAMN == 2)
  if (length(dor_ind)) {
    ret <- x$AVAL[dor_ind]
  }
  ret
}

# ADEFIRR
#' @export
get_duration_of_response_censor <- function(x) {
  ret <- as.double(NA)
  dor_ind <- which(x$PARAMN == 2)
  if (length(dor_ind)) {
    ret <- x$CNSR[dor_ind]
  }
  ret
}

# ADEFIRR
#' @export
get_progression_free_survival <- function(x) {
  ret <- as.double(NA)
  pfs_ind <- which(x$PARAMN == 1) 
  if (length(pfs_ind)) {
    ret <- x$AVAL[pfs_ind]
  }
  ret
}

# ADEFIRR
#' @export
get_progression_free_survival_event <- function(x) {
  ret <- as.double(NA)
  pfs_ind <- which(x$PARAMN == 1) 
  if (length(pfs_ind)) {
    ret <- x$AVAL[pfs_ind]
  }
  ret
}

# ADEFIRR
#' @export
get_progression_free_survival_censor <- function(x) {
  ret <- NA
  pfs_ind <- which(x$PARAMN == 1) 
  if (length(pfs_ind)) {
    ret <- x$CNSR[pfs_ind]
  }
  ret
}

