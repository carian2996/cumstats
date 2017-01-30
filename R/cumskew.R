#' Cumulative Skewness
#'
#' Returns a vector whose elements are the cumulative \emph{skewness} of the
#' elements of the argument.
#'
#' @param x a numeric vector
#'
#' @return A numeric vector of the same length as \code{x}. An \code{NA} value
#'    in \code{x} causes the corresponding and following elements of the return
#'    value to be \code{NA}. The first entry is always \code{NaN} since skewness
#'    requires at least two different values.
#'
#' @references
#' Kotz, S., Balakrishnan, N., Read, C.B, Vidakovic, B., Johnson, N.L. (2006)
#'    \emph{Encyclopedia of Statistical Sciences.} Wiley, New Jersey.
#'
#' @examples
#' cumskew(c(9, 1, 4, 0, 3, NA, 8, 5))
#'
#' @seealso \code{\link{skewness}}
#'
#' @author Arturo Erdely, \email{arturo.erdely@@comunidad.unam.mx}
#'
#' @export
#'

cumskew <-
function(x) sapply(seq_along(x), function(k, z) skewness(z[1:k]), z = x)
