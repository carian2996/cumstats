#' Cumulative Median
#'
#' Returns a vector whose elements are the cumulative \emph{median} of the
#' elements of the argument.
#'
#' @param x a numeric vector
#'
#' @return A numeric vector of the same length as \code{x}. An \code{NA} value
#'    in \code{x} causes the corresponding and following elements of the return
#'    value to be \code{NA}.
#'
#' @references
#' Kotz, S., Balakrishnan, N., Read, C.B, Vidakovic, B., Johnson, N.L. (2006)
#'    \emph{Encyclopedia of Statistical Sciences.} Wiley, New Jersey.
#'
#' @examples
#' cummedian(c(9, 1, 4, 0, 3, NA, 8, 5))
#'
#' @seealso \code{\link{cummean}} for cummulative arithmetic mean, and
#'    '\code{\link{cumquant}} for cummulative quantile.
#'
#' @author Arturo Erdely, \email{arturo.erdely@@comunidad.unam.mx}
#'
#' @export
#'

cummedian <-
function(x) sapply(seq_along(x), function(k, z) stats::median(z[1:k]), z = x)
