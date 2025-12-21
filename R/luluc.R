#' Build LUC matrix
#'
#' Build land-use change (LUC) transition matrix from patterns.
#'
#' @param patterns A list of patterns.
#' Each pattern must be a sequence of transition values whose size is a multiple of \code{n_lu}x\code{n_lu}.
#' The \code{n_lu} first values are the transitions from each LU to the first LU, and so on.
#' If the sequence contains more years than \code{n_years}, it will be truncated.
#' @param n_lu Number of land use types (LU).
#' @param n_years Number of years (i.e. length of the 3rd dimension).
#' @param out For internal use only.
#'
#' @return An \code{n_lu}x\code{n_lu}x\code{n_years} transition matrix.
#' @export
#'
#' @examples
#' # Example of building a 6 year-long transition matix consisting of 6 times 2x2 matrices
#'
#' # A one time transfer of 0.5 of the total cell fraction from LU 2 to LU 1
#' pattern1 <- c(0, 0, 0.5, 0)
#' # The null pattern (no transition)
#' null_pattern <- rep(0, 4)
#' # A repeated self-transition of 0.1 of the total cell fraction from LU 2 to LU 2 every other year
#' pattern2 <- rep(c(c(0, 0, 0, 0.1), null_pattern), 3)
#'
#' # Building the transition matrix
#' build_luc_matrix(list(pattern1, pattern2), 2, 6)

# 'patterns' must be a list
build_luc_matrix <- function(patterns, n_lu, n_years, out=vector()) {
  # Convenience method to prep the transitions
  prep_pattern <- function(pattern) {
    if (length(pattern) %% n_lu ^ 2 != 0) {
      stop(paste0('Error: transition patterns should be a multiple of the square of the number of LUs (', n_lu, '*', n_lu, '), but got ', length(pattern), '.'))
    }
    length(pattern) <- n_years * n_lu ^ 2
    pattern[is.na(pattern)] <- 0
    return(pattern)
  }

  if (length(patterns) > 0) {
    build_luc_matrix(patterns[-1], n_lu, n_years, prep_pattern(out) + prep_pattern(patterns[1][[1]]))
  } else {
    return(array(out, c(n_lu, n_lu, n_years)))
  }
}