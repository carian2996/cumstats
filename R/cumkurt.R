#' Cumulative Kurtosis
#'
#' Returns a vector whose elements are the cumulative \emph{kurtosis} of the
#' elements of the argument.
#'
#' @param x a numeric vector
#'
#' @return A numeric vector of the same length as \code{x}. An \code{NA} value
#'    in \code{x} causes the corresponding and following elements of the return
#'    value to be \code{NA}. The first entry is always \code{NaN} since kurtosis
#'    requires at least two different values.
#'
#' @references
#' Kotz, S., Balakrishnan, N., Read, C.B, Vidakovic, B., Johnson, N.L. (2006)
#'    \emph{Encyclopedia of Statistical Sciences.} Wiley, New Jersey.
#'
#' @examples
#' cumkurt(c(9, 1, 4, 0, 3, NA, 8, 5))
#'
#' @seealso \code{\link{kurtosis}}
#'
#' @author Arturo Erdely, \email{arturo.erdely@@comunidad.unam.mx}
#'
#' @export
#'

cumkurt <-
function(x) sapply(seq_along(x), function(k, z) kurtosis(z[1:k]), z = x)
